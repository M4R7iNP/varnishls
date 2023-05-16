use crate::{
    parser, static_autocomplete_items,
    varnish_builtins::{
        self, get_backend_field_types, get_probe_field_types, scope_contains_type,
        scope_contains_writable, Type,
    },
};

use log::debug;
use ropey::Rope;
use std::{
    collections::VecDeque,
    sync::{Arc, Mutex},
};
use tower_lsp::lsp_types::*;
use tree_sitter::{InputEdit, Node, Parser, Point, Query, QueryCursor, Tree};

#[derive(Clone, Copy)]
pub enum FileType {
    Vcl,
    Vtc,
}

#[derive(Clone)]
pub struct Document {
    version: i32,
    parser: Arc<Mutex<Parser>>,
    pub rope: Rope,
    pub ast: Tree,
    pub url: Url,
    pub filetype: FileType,
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub ident_str: String,
    pub r#type: Box<Type>,
    pub line_num: usize,
    pub doc_url: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Reference {
    pub ident_str: String,
    pub line_num: usize,
    pub doc_url: Option<String>,
    pub uri: Location,
}

#[derive(Debug)]
pub struct LintError {
    pub message: String,
    pub severity: DiagnosticSeverity,
    pub loc: Location,
}

// reserved keywords, without top-level only declarations
const RESERVED_KEYWORDS: &[&str] = &[
    "if", "set", "new", "call", "else", "elsif", "unset", "include", "return",
];

pub fn get_node_text<'a>(rope: &'a Rope, node: &'a Node) -> String {
    let mut text = rope.byte_slice(node.byte_range()).to_string();
    if let Some((first_part, _)) = text.split_once('\n') {
        text = first_part.to_string();
    }

    if RESERVED_KEYWORDS.contains(&text.as_str()) {
        return "".into();
    }

    text
}

unsafe impl Send for Document {}
unsafe impl Sync for Document {}

impl Document {
    pub fn new(url: Url, text: String) -> Self {
        let mut filetype = FileType::Vcl;
        if url.path().ends_with(".vtc") {
            filetype = FileType::Vtc;
        }

        let mut parser = match filetype {
            FileType::Vcl => parser::vcl(),
            FileType::Vtc => parser::vtc(),
        };

        let ast = parser.parse(&text, None).unwrap();
        let parser = Arc::new(Mutex::new(parser));
        let rope = Rope::from(text);

        Self {
            version: 0,
            rope,
            parser,
            ast,
            url,
            filetype: FileType::Vcl,
        }
    }

    pub fn version(&self) -> i32 {
        self.version
    }

    pub fn edit(&mut self, version: i32, edits: impl Iterator<Item = (Option<Range>, String)>) {
        for (range, text) in edits {
            match range {
                Some(range) => {
                    self.edit_range(version, range, text);
                }
                None => {
                    self.edit_fulltext(version, text);
                }
            }
        }
    }

    fn edit_range(&mut self, _version: i32, range: Range, text: String) {
        // let start = position_to_offset(&self.rope, range.start);
        // let end = position_to_offset(&self.rope, range.end);
        // let new_end_byte = start + text.as_bytes().len();

        // let mut new_rope = self.rope.clone();
        let mut new_ast = self.ast.clone();

        let old_end_position_point = Point {
            row: range.end.line as usize,
            column: range.end.character as usize,
        };

        let start_col = self
            .rope
            .line(range.start.line as usize)
            .utf16_cu_to_char(range.start.character as usize);
        let end_col = self
            .rope
            .line(range.end.line as usize)
            .utf16_cu_to_char(range.end.character as usize);
        let start = self.rope.line_to_char(range.start.line as usize) + start_col;
        let end = self.rope.line_to_char(range.end.line as usize) + end_col;
        let old_end_byte = self.rope.char_to_byte(end);
        self.rope.remove(start..end);
        self.rope.insert(start, text.as_str());
        let start_byte = self.rope.char_to_byte(start);
        let new_end_byte = start_byte + text.as_bytes().len();
        let new_end_line = self.rope.byte_to_line(new_end_byte);
        let new_end_col =
            self.rope.byte_to_char(new_end_byte) - self.rope.line_to_char(new_end_line);
        let new_end_position_point = Point {
            row: new_end_line,
            column: new_end_col,
        };

        new_ast.edit(&InputEdit {
            start_byte,
            old_end_byte,
            new_end_byte,
            start_position: Point {
                row: range.start.line as usize,
                column: range.start.character as usize,
            },
            old_end_position: old_end_position_point,
            new_end_position: new_end_position_point,
        });

        let new_new_ast = self
            .parser
            .lock()
            .unwrap()
            .parse_with(
                &mut |offset, _pos| {
                    let (chunk, chunk_byte_idx, _, _) = self.rope.chunk_at_byte(offset);
                    &chunk.as_bytes()[(offset - chunk_byte_idx)..]
                },
                Some(&new_ast),
            )
            .unwrap();

        // self.rope = new_rope;
        self.ast = new_new_ast;
    }

    fn edit_fulltext(&mut self, _version: i32, text: String) {
        let rope = Rope::from(text.clone());
        let ast = self.parser.lock().unwrap().parse(&text, None).unwrap();
        self.rope = rope;
        self.ast = ast;
    }

    pub fn get_type_at_point(&self, point: Point, scope: Type) -> Option<Type> {
        let mut node = self
            .ast
            .root_node()
            .descendant_for_point_range(point, point)?;
        // let Type::Obj(obj) = scope;
        loop {
            if matches!(node.kind(), "ident_call_expr") {
                let ident_node = node.child_by_field_name("ident")?;
                let ident = get_node_text(&self.rope, &ident_node);
                let parts = ident.split('.').collect::<Vec<&str>>();
                let r#type = lookup_from_scope(&scope, parts);
                if r#type.is_some() {
                    return r#type.cloned();
                }
            }
            if matches!(node.kind(), "nested_ident" | "ident") {
                let ident = get_node_text(&self.rope, &node);
                let parts = ident.split('.').collect::<Vec<&str>>();
                let r#type = lookup_from_scope(&scope, parts);
                if r#type.is_some() {
                    return r#type.cloned();
                }
            }
            node = match node.parent() {
                Some(node) => node,
                None => break,
            }
        }
        None
    }

    pub fn get_ident_at_point(&self, point: Point) -> Option<String> {
        let node = self
            .ast
            .root_node()
            .descendant_for_point_range(point, point)?;
        let name = get_node_text(&self.rope, &node)
            .split('.')
            .take(1)
            .next()
            .unwrap()
            .to_string();
        Some(name)
    }

    pub fn get_definition_by_name(&self, name: &String) -> Option<(Point, Point)> {
        let q = Query::new(
            self.ast.language(),
            &format!(
                r#"
                [
                    (toplev_declaration (_ ident: (ident) @ident (#eq? @ident "{}")))
                    (new_stmt (ident) @left (#eq? @left "{}"))
                ] @node
                "#,
                name, name
            ),
        );
        if let Err(err) = q {
            debug!("Error: {}", err);
            return None;
        }
        let q = q.unwrap();
        let mut qc = QueryCursor::new();
        let str = self.rope.to_string();
        let str_bytes = str.as_bytes();
        let all_matches = qc.matches(&q, self.ast.root_node(), str_bytes);
        let capt_idx = q.capture_index_for_name("node").unwrap();

        for each_match in all_matches {
            if let Some(capture) = each_match.captures.iter().find(|c| c.index == capt_idx) {
                let range = capture.node.range();
                let line = range.start_point.row;
                let col = range.start_point.column;
                debug!(
                    "[Line: {:?}, Col: {:?}] Matching source code: `{:?}`",
                    line,
                    col,
                    &str[range.start_byte..range.end_byte]
                );
                return Some((range.start_point, range.end_point));
            }
        }

        None
    }

    pub fn get_definition_by_point(&self, point: Point) -> Option<(Point, Point)> {
        let name = self.get_ident_at_point(point)?;
        self.get_definition_by_name(&name)
    }

    pub fn get_error_ranges(&self) -> Vec<LintError> {
        let ast = self.ast.clone();

        let full_text = self.rope.to_string();
        let mut cursor = ast.walk();
        let mut error_ranges = Vec::new();
        let mut recurse = true;

        loop {
            if (recurse && cursor.goto_first_child()) || cursor.goto_next_sibling() {
                recurse = true;
            } else if cursor.goto_parent() {
                recurse = false;
            } else {
                break;
            }

            let node = cursor.node();
            let range = Range::new(
                Position {
                    line: node.start_position().row as u32,
                    character: node.start_position().column as u32,
                },
                Position {
                    line: node.end_position().row as u32,
                    character: node.end_position().column as u32,
                },
            );

            if node.is_error() {
                error_ranges.push(LintError {
                    message: "Syntax error".to_string(),
                    loc: Location {
                        uri: self.url.to_owned(),
                        range,
                    },
                    severity: DiagnosticSeverity::ERROR,
                });
                recurse = false;
                continue;
            }

            if node.is_missing() {
                error_ranges.push(LintError {
                    message: format!("Expected {}", node.kind()),
                    loc: Location {
                        uri: self.url.to_owned(),
                        range,
                    },
                    severity: DiagnosticSeverity::ERROR,
                });
                recurse = false;
                continue;
            }

            match node.kind() {
                "set_stmt" => {
                    let left = node.child_by_field_name("left");
                    if left.is_none() {
                        error_ranges.push(LintError {
                            message: "Missing set property".to_string(),
                            loc: Location {
                                uri: self.url.to_owned(),
                                range,
                            },
                            severity: DiagnosticSeverity::ERROR,
                        });
                        continue;
                    }

                    let right = node.child_by_field_name("right");
                    if right.is_none() {
                        error_ranges.push(LintError {
                            message: "Missing set value".to_string(),
                            loc: Location {
                                uri: self.url.to_owned(),
                                range,
                            },
                            severity: DiagnosticSeverity::ERROR,
                        });
                        continue;
                    }
                }
                "new_stmt" => {
                    let left = node.child_by_field_name("ident");
                    if left.is_none() {
                        error_ranges.push(LintError {
                            message: "Missing identifier".to_string(),
                            loc: Location {
                                uri: self.url.to_owned(),
                                range,
                            },
                            severity: DiagnosticSeverity::ERROR,
                        });
                        continue;
                    }

                    let right = node.child_by_field_name("def_right");
                    if right.is_none() {
                        error_ranges.push(LintError {
                            message: "Missing value".to_string(),
                            loc: Location {
                                uri: self.url.to_owned(),
                                range,
                            },
                            severity: DiagnosticSeverity::ERROR,
                        });
                        continue;
                    }
                }
                "backend_property" => {
                    let left_ident = match node.child_by_field_name("left") {
                        None => {
                            error_ranges.push(LintError {
                                message: "Missing backend property field".to_string(),
                                loc: Location {
                                    uri: self.url.to_owned(),
                                    range,
                                },
                                severity: DiagnosticSeverity::ERROR,
                            });
                            continue;
                        }
                        Some(left_node) => &full_text[left_node.byte_range()],
                    };

                    let parent_parent_node_kind = node.parent().unwrap().kind();
                    let map = match parent_parent_node_kind {
                        "probe_declaration" => get_probe_field_types(),
                        _ => get_backend_field_types(),
                    };

                    let r#type = map.get(left_ident);

                    let Some(right_node) = node.child_by_field_name("right") else {
                        error_ranges.push(LintError {
                            message: "Missing backend property value".to_string(),
                            loc: Location {
                                uri: self.url.to_owned(),
                                range,
                            },
                            severity: DiagnosticSeverity::ERROR,
                        });
                        continue;
                    };

                    if right_node.kind() == "string_list" && left_ident != "request" {
                        error_ranges.push(LintError {
                            message: format!("Property {left_ident} cannot contain string list"),
                            loc: Location {
                                uri: self.url.to_owned(),
                                range,
                            },
                            severity: DiagnosticSeverity::ERROR,
                        });
                    }

                    match r#type {
                        None => {
                            error_ranges.push(LintError {
                                message: format!(
                                    "Backend property «{}» does not exist",
                                    left_ident
                                ),
                                loc: Location {
                                    uri: self.url.to_owned(),
                                    range,
                                },
                                severity: DiagnosticSeverity::ERROR,
                            });
                        }
                        Some(r#type) => {
                            let Some(right_node_type) = node_to_type(&right_node) else {
                                error_ranges.push(LintError {
                                    message: format!("Unexpected value"),
                                    loc: Location {
                                        uri: self.url.to_owned(),
                                        range,
                                    },
                                    severity: DiagnosticSeverity::ERROR,
                                });
                                continue;
                            };
                            if !right_node_type.can_this_cast_into(r#type) {
                                error_ranges.push(LintError {
                                    message: format!(
                                        "Expected {}, found {}",
                                        r#type, right_node_type
                                    ),
                                    loc: Location {
                                        uri: self.url.to_owned(),
                                        range,
                                    },
                                    severity: DiagnosticSeverity::ERROR,
                                });
                            }
                        }
                    }
                }
                "nested_ident" | "ident" => {
                    let text = &self.rope.to_string()[node.byte_range()];
                    if RESERVED_KEYWORDS.contains(&text) {
                        error_ranges.push(LintError {
                            message: "Reserved keyword".to_string(),
                            loc: Location {
                                uri: self.url.to_owned(),
                                range,
                            },
                            severity: DiagnosticSeverity::ERROR,
                        });
                        continue;
                    }
                    if text.ends_with('.') {
                        error_ranges.push(LintError {
                            message: "Identifier ending with dot?".to_string(),
                            loc: Location {
                                uri: self.url.to_owned(),
                                range,
                            },
                            severity: DiagnosticSeverity::WARNING,
                        });
                        continue;
                    }

                    let toplev_decl = {
                        let mut node = node;
                        loop {
                            match node.parent() {
                                Some(parent_node) if parent_node.kind() == "toplev_declaration" => {
                                    break;
                                }
                                Some(parent_node) => {
                                    node = parent_node;
                                }
                                None => break,
                            }
                        }
                        node
                    };

                    if toplev_decl.kind() == "sub_declaration" {
                        if let Some(ident_node) = toplev_decl.child_by_field_name("ident") {
                            let sub_name_string = get_node_text(&self.rope, &ident_node);
                            let sub_name = sub_name_string.as_str();
                            let parts = text.split('.').collect::<Vec<&str>>();
                            if sub_name.starts_with("vcl_") {
                                let exists_in_sub = match parts[0] {
                                    "req" => matches!(
                                        sub_name,
                                        "vcl_recv"
                                            | "vcl_deliver"
                                            | "vcl_synth"
                                            | "vcl_miss"
                                            | "vcl_hit"
                                            | "vcl_pass"
                                            | "vcl_purge"
                                            | "vcl_pipe"
                                            | "vcl_hash"
                                    ),
                                    "bereq" => matches!(
                                        sub_name,
                                        "vcl_backend_fetch"
                                            | "vcl_backend_response"
                                            | "vcl_pipe"
                                            | "vcl_backend_error"
                                    ),
                                    "beresp" => matches!(
                                        sub_name,
                                        "vcl_backend_response" | "vcl_backend_error"
                                    ),
                                    "resp" => {
                                        matches!(sub_name, "vcl_deliver" | "vcl_miss" | "vcl_synth")
                                    }
                                    "obj" => matches!(sub_name, "vcl_hit" | "vcl_deliver"),
                                    _ => true,
                                };

                                if !exists_in_sub {
                                    error_ranges.push(LintError {
                                        message: format!(
                                            "«{}» does not exist in «{}»",
                                            parts[0], sub_name
                                        ),
                                        loc: Location {
                                            uri: self.url.to_owned(),
                                            range,
                                        },
                                        severity: DiagnosticSeverity::ERROR,
                                    });
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        error_ranges
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        return self
            .get_error_ranges()
            .iter()
            .map(|lint_error| Diagnostic {
                range: lint_error.loc.range,
                severity: Some(lint_error.severity),
                message: lint_error.message.to_owned(),
                ..Diagnostic::default()
            })
            .collect();
    }

    pub fn get_vmod_imports(&self) -> Vec<String> {
        let ast = self.ast.clone();
        let q = Query::new(ast.language(), "(import_declaration (ident) @ident)").unwrap();
        let mut qc = QueryCursor::new();
        let str = self.rope.to_string();
        let str_bytes = str.as_bytes();
        let all_matches = qc.matches(&q, ast.root_node(), str_bytes);
        let capt_idx = q.capture_index_for_name("ident").unwrap();

        let mut import_names: Vec<String> = Vec::new();
        for each_match in all_matches {
            for capture in each_match.captures.iter().filter(|c| c.index == capt_idx) {
                let range = capture.node.range();
                let text = &str[range.start_byte..range.end_byte];
                import_names.push(text.to_string());
            }
        }

        import_names
    }

    pub fn get_includes(&self) -> VecDeque<String> {
        let ast = self.ast.clone();
        let q = Query::new(ast.language(), "(include_declaration (string) @string)").unwrap();
        let mut qc = QueryCursor::new();
        let str = self.rope.to_string();
        let str_bytes = str.as_bytes();
        let all_matches = qc.matches(&q, ast.root_node(), str_bytes);
        let capt_idx = q.capture_index_for_name("string").unwrap();

        let mut include_names: VecDeque<String> = VecDeque::new();
        for each_match in all_matches {
            for capture in each_match.captures.iter().filter(|c| c.index == capt_idx) {
                let range = capture.node.range();
                let text = &str[range.start_byte..range.end_byte];
                include_names.push_back(text.trim_matches('"').to_string());
            }
        }

        include_names
    }

    pub fn get_subroutines(&self) -> Vec<String> {
        let ast = self.ast.clone();
        let q = Query::new(ast.language(), "(sub_declaration (ident) @ident)").unwrap();
        let mut qc = QueryCursor::new();
        let str = self.rope.to_string();
        let str_bytes = str.as_bytes();
        let all_matches = qc.matches(&q, ast.root_node(), str_bytes);
        let capt_idx = q.capture_index_for_name("ident").unwrap();

        let mut import_names: Vec<String> = Vec::new();
        for each_match in all_matches {
            for capture in each_match.captures.iter().filter(|c| c.index == capt_idx) {
                let range = capture.node.range();
                let text = &str[range.start_byte..range.end_byte];
                import_names.push(text.to_string());
            }
        }

        import_names
    }

    pub fn get_all_definitions(&self, scope_with_vmods: &Type) -> Vec<Definition> {
        let q = Query::new(
            self.ast.language(),
            // "[(toplev_declaration (_ (ident) @ident ) @node), (new_stmt (ident) @ident) @node]",
            r#"
            [
                (toplev_declaration (_ (ident) @ident ) @node)
                (new_stmt ident: (ident) @ident def_right: (ident_call_expr ident: (_) @def_ident)) @node
            ]
            "#,
        );
        if let Err(err) = q {
            debug!("Error: {}", err);
            return vec![];
        }
        let q = q.unwrap();

        let mut qc = QueryCursor::new();
        let str = self.rope.to_string();
        let str_bytes = str.as_bytes();
        let all_matches = qc.matches(&q, self.ast.root_node(), str_bytes);
        let node_capt_idx = q.capture_index_for_name("node").unwrap();
        let ident_capt_idx = q.capture_index_for_name("ident").unwrap();
        let def_ident_capt_idx = q.capture_index_for_name("def_ident").unwrap();

        let mut defs: Vec<Definition> = Vec::new();
        for each_match in all_matches {
            // debug!("match: {:?}", each_match);
            let node_capture = each_match
                .captures
                .iter()
                .find(|c| c.index == node_capt_idx)
                .unwrap();
            let ident_capture = each_match
                .captures
                .iter()
                .find(|c| c.index == ident_capt_idx)
                .unwrap();
            let text = &str[ident_capture.node.byte_range()];
            let line_num = node_capture.node.start_position().row;
            let r#type: Box<Type> = match node_capture.node.kind() {
                "sub_declaration" => Box::new(Type::Sub),
                "acl_declaration" => Box::new(Type::Acl),
                "backend_declaration" => Box::new(Type::Backend),
                "probe_declaration" => Box::new(Type::Probe),
                "new_stmt" => {
                    let def_ident_capture = each_match
                        .captures
                        .iter()
                        .find(|c| c.index == def_ident_capt_idx)
                        .unwrap();
                    let def_text_range = def_ident_capture.node.range();
                    let def_text = &str[def_text_range.start_byte..def_text_range.end_byte];
                    let r#type = lookup_from_scope(scope_with_vmods, def_text.split('.').collect());

                    if r#type.is_none() {
                        continue;
                    }

                    let r#type = r#type.unwrap();
                    let mut r#type_box = Box::new(r#type.to_owned());

                    if let Type::Func(func) = r#type {
                        if let Some(func_return) = func.r#return.clone() {
                            r#type_box = func_return;
                        }
                    }

                    r#type_box
                }
                _ => continue,
            };

            defs.push(Definition {
                ident_str: text.to_string(),
                line_num,
                doc_url: Some(self.url.to_string()),
                r#type,
            });
        }

        defs
    }

    pub fn get_references_for_ident(&self, ident: &str) -> Vec<Reference> {
        let q = Query::new(
            self.ast.language(),
            &format!(r#"((ident) @ident (#eq? @ident "{}"))"#, ident),
        );
        if let Err(err) = q {
            debug!("Error: {}", err);
            return vec![];
        }
        let q = q.unwrap();

        let mut qc = QueryCursor::new();
        let str = self.rope.to_string();
        let str_bytes = str.as_bytes();
        let all_matches = qc.matches(&q, self.ast.root_node(), str_bytes);
        let ident_capt_idx = q.capture_index_for_name("ident").unwrap();
        let mut refs: Vec<Reference> = Vec::new();
        for each_match in all_matches {
            let ident_capture = each_match
                .captures
                .iter()
                .find(|c| c.index == ident_capt_idx)
                .unwrap();
            let text = &str[ident_capture.node.byte_range()];
            let line_num = ident_capture.node.start_position().row;
            refs.push(Reference {
                ident_str: text.to_string(),
                line_num,
                doc_url: Some(self.url.to_string()),
                uri: Location {
                    uri: self.url.to_owned(),
                    range: Range {
                        start: point_to_position(ident_capture.node.start_position()),
                        end: point_to_position(ident_capture.node.end_position()),
                    },
                },
            });
        }
        refs
    }

    /**
     * Expand identifiers into req, res etc. and their properties.
     */
    pub fn autocomplete_for_pos(
        &self,
        pos: Position,
        global_scope: Type,
    ) -> Option<Vec<CompletionItem>> {
        debug!("starting autocomplete2");

        let mut target_point = Point {
            row: pos.line as usize,
            column: pos.character as usize,
        };

        let target_row = self.rope.line(target_point.row);
        if matches!(target_row.get_char(target_point.column), None | Some('\n'))
            && target_point.column > 0
        {
            debug!("decrementing by one");
            target_point.column -= 1;
        }

        debug!("target point: {}", target_point);

        let mut text = "".to_string();
        let full_text = self.rope.to_string();
        let mut search_type: Option<Type> = None;
        let mut must_be_writable = false;
        let mut keyword_suggestions = vec![];

        /*
        let narrowest_node = self
            .ast
            .root_node()
            .descendant_for_point_range(target_point, target_point)?;
        */
        let mut cursor = self.ast.root_node().walk();
        // walk down, then up again
        while cursor.goto_first_child_for_point(target_point).is_some() {}
        loop {
            let node = cursor.node();
            debug!("visiting node {:?}", node);

            let parent_node = match node.parent() {
                Some(node) => node,
                _ => break,
            };

            match parent_node.kind() {
                "set_stmt" => {
                    let mut field = cursor.field_name();

                    // When the node under the cursor is «;», select the right field instead
                    if node.kind() == ";" || node.kind() == "=" {
                        field = Some("right");
                    }

                    match field {
                        None | Some("left") => {
                            // autocomplete for left part of set statements must be writeable
                            must_be_writable = true;
                        }
                        Some("right") => {
                            // text = get_node_text(&self.rope, &node);
                            if let Some(ctx_node) = parent_node.child_by_field_name("left") {
                                let ctx_node_str = &full_text[ctx_node.byte_range()];
                                if ["req.http.", "resp.http.", "bereq.http.", "beresp.http."]
                                    .iter()
                                    .any(|variable| ctx_node_str.starts_with(variable))
                                {
                                    search_type = Some(Type::String);
                                } else {
                                    search_type =
                                        lookup_from_scope_by_str(&global_scope, ctx_node_str)
                                            .map(|t| t.to_owned());
                                    // ignore objects here
                                    if let Some(Type::Obj(_obj)) = search_type {
                                        search_type = None;
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
                "backend_property" | "backend_declaration" | "probe_declaration" => {
                    let mut field = cursor.field_name();
                    if node.kind() == ";" || node.kind() == "=" {
                        field = Some("right");
                    } else if node.kind() == "." {
                        field = Some("left");
                        // node = node.parent().unwrap();
                    }

                    match field {
                        Some("left") => {
                            text = get_node_text(&self.rope, &node);
                            let text = text.strip_prefix('.').unwrap_or(text.as_str());
                            let parent_parent_node_kind = parent_node.parent().unwrap().kind();
                            let r#type = match parent_parent_node_kind {
                                "probe_declaration" => Type::Probe,
                                _ => Type::Backend,
                            };
                            return Some(get_probe_backend_fields(r#type, text));
                        }
                        Some("right") => {
                            if let Some(ctx_node) = parent_node.child_by_field_name("left") {
                                let ctx_node_str = &full_text[ctx_node.byte_range()];
                                if ctx_node_str == "probe" {
                                    search_type = Some(Type::Probe);
                                }
                            }
                        }
                        _ => {}
                    };
                }
                "call_stmt" => {
                    debug!(
                        "reached call statement ({:?}) ({:?}) ({:?})",
                        node,
                        parent_node,
                        cursor.field_name()
                    );
                    search_type = Some(Type::Sub);
                }
                "ret_stmt" => {
                    debug!(
                        "reached return statement ({:?}) ({:?}) ({:?})",
                        node,
                        parent_node,
                        cursor.field_name()
                    );
                    return Some(
                        varnish_builtins::RETURN_METHODS
                            .iter()
                            .map(|field| CompletionItem {
                                label: field.to_string(),
                                detail: Some(field.to_string()),
                                kind: Some(CompletionItemKind::FUNCTION),
                                ..Default::default()
                            })
                            .collect(),
                    );
                }
                "source_file" => {
                    return Some(static_autocomplete_items::source_file());
                }
                "sub_declaration" | "if_stmt" | "elsif_stmt" | "else_stmt" => {
                    keyword_suggestions.append(&mut static_autocomplete_items::subroutine());
                }
                _ => {
                    if parent_node.kind() == "new_stmt" || node.kind() == "new_stmt" {
                        debug!("huhh: {:?} ({:?})", node, cursor.field_name());
                        match cursor.field_name() {
                            Some("ident") => {
                                return Some(vec![]);
                            }
                            Some("def_right") => search_type = Some(Type::Func(Default::default())),
                            _ => {}
                        }
                    }
                }
            }

            // now, try to gather best identifier for autocomplete
            match node.kind() {
                "nested_ident" => {
                    text = get_node_text(&self.rope, &node);
                    debug!("got text for nested_ident {:?} {}", node, text);
                }
                "ident" if node.parent().unwrap().kind() != "nested_ident" => {
                    text = get_node_text(&self.rope, &node);
                    debug!("got text for ident {:?} {}", node, text);
                }
                _ => {}
            }

            let node_range = node.start_position().row..node.end_position().row;
            if node_range.contains(&target_point.row) {
                break;
            }

            if !cursor.goto_parent() {
                debug!("no parent??");
                break;
            }
        }

        // debug!("jaggu: {:?}", search_type);
        // let full_text = self.rope.to_string();
        // text = &full_text[node.start_byte()..node.end_byte()];
        debug!("text: «{:?}»", text);

        // identifiers written so far (split by dot)
        let idents: Vec<&str> = text.split('.').collect();
        // get scope from identifiers written so far (excluding the last one)
        let scope = lookup_from_scope(&global_scope, idents[0..idents.len() - 1].to_vec());

        // debug!("scope: {:?}", scope);

        // partial match on last identifier written
        let last_part = idents[idents.len() - 1];

        return match scope.unwrap() {
            Type::Obj(obj) => {
                let mut suggestions = obj
                    .properties
                    .range(last_part.to_string()..)
                    .take_while(|(key, _v)| key.starts_with(last_part))
                    .filter(|(_prop_name, property)| {
                        // debug!("{}: {:?}", _prop_name, property);
                        // try to match only backends when autocompleting for e.g. req.backend_hint
                        if let Some(ref search_type) = search_type {
                            let has_type = scope_contains_type(property, search_type, true);
                            debug!("{} has type {}? {}", _prop_name, search_type, has_type);
                            has_type
                        } else if must_be_writable {
                            !obj.read_only || scope_contains_writable(property)
                        } else {
                            // match on everything
                            true
                        }
                    })
                    .map(|(prop_name, property)| CompletionItem {
                        label: prop_name.to_string(),
                        detail: Some(match property {
                            Type::Func(_func) => format!("{}", property),
                            Type::Backend => format!("BACKEND {}", prop_name),
                            _ => format!("{} {}", property, prop_name),
                        }),
                        kind: Some(match property {
                            Type::Func(_func) => CompletionItemKind::FUNCTION,
                            Type::Obj(_obj) => CompletionItemKind::STRUCT,
                            Type::Sub => CompletionItemKind::FUNCTION,
                            _ => CompletionItemKind::PROPERTY,
                        }),
                        insert_text: Some(match property {
                            Type::Func(func) => {
                                let args_str = func
                                    .args
                                    .iter()
                                    .enumerate()
                                    .map_while(|(idx, arg)| {
                                        if arg.optional || arg.default_value.is_some() {
                                            return None;
                                        }
                                        let mut str = format!("${{{}", idx + 1).to_string();
                                        if let Some(ref name) = arg.name {
                                            str.push_str(&format!(":{name}"));
                                        }
                                        str.push('}');
                                        Some(str)
                                    })
                                    .collect::<Vec<_>>()
                                    .join(", ");
                                format!("{}({})", prop_name, args_str)
                            }
                            _ => prop_name.to_string(),
                        }),
                        insert_text_format: Some(InsertTextFormat::SNIPPET),
                        documentation: match property {
                            Type::Func(func) => func.doc.to_owned().map(|doc| {
                                Documentation::MarkupContent(MarkupContent {
                                    kind: MarkupKind::Markdown,
                                    value: doc,
                                })
                            }),
                            _ => None,
                        },
                        ..Default::default()
                    })
                    .collect::<Vec<_>>();
                if obj.name == *"GLOBAL" {
                    suggestions.append(&mut keyword_suggestions);
                }
                Some(suggestions)
            }
            _ => return None,
        };
    }
}

fn lookup_from_scope<'a>(global_scope: &'a Type, idents: Vec<&'a str>) -> Option<&'a Type> {
    let mut scope = global_scope;
    for ident in idents.as_slice().iter() {
        let new_scope: Option<&Type> = match scope {
            Type::Obj(obj) => obj.properties.get(*ident),
            _ => None,
        };

        if let Some(new_scope) = new_scope {
            scope = new_scope;
        }
    }

    Some(scope)
}

fn lookup_from_scope_by_str<'a>(global_scope: &'a Type, idents: &'a str) -> Option<&'a Type> {
    return lookup_from_scope(global_scope, idents.split('.').collect());
}

fn point_to_position(point: Point) -> Position {
    Position {
        line: point.row as u32,
        character: point.column as u32,
    }
}

fn get_probe_backend_fields(r#type: Type, text: &str) -> Vec<CompletionItem> {
    let map = match r#type {
        Type::Probe => get_probe_field_types(),
        _ => get_backend_field_types(),
    };
    map.keys()
        .filter(|field| field.starts_with(text))
        .map(|field| CompletionItem {
            label: field.to_string(),
            detail: Some(field.to_string()),
            kind: Some(CompletionItemKind::PROPERTY),
            ..Default::default()
        })
        .collect()
}

pub fn node_to_type(node: &Node) -> Option<Type> {
    match node.kind() {
        "string" => Some(Type::String),
        "string_list" => Some(Type::String),
        "number" => Some(Type::Number),
        "duration" => Some(Type::Duration),
        "bool" => Some(Type::Bool),
        "literal" => node_to_type(&node.child(0)?),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::varnish_builtins::get_varnish_builtins;
    use crate::varnish_builtins::{Func, Obj};

    use super::*;

    #[test]
    fn autocomplete_expands_h_to_http() {
        let doc = Document::new(
            Url::parse("file:///test.vcl").unwrap(),
            r#"
sub vcl_recv {
    set req.h
}
"#
            .to_string(),
        );
        let result = doc
            .autocomplete_for_pos(
                Position {
                    line: 2,
                    character: 12,
                },
                get_varnish_builtins(),
            )
            .unwrap();
        println!("result: {:?}", result);
        assert_eq!(result.len(), 1);
        // assert_eq!(result[0].label, "http");
    }

    #[test]
    fn autocomplete_lists_all_on_req() {
        let doc = Document::new(
            Url::parse("file:///test.vcl").unwrap(),
            r#"
sub vcl_recv {
    set req.
}
"#
            .to_string(),
        );
        let result = doc
            .autocomplete_for_pos(
                Position {
                    line: 2,
                    character: 11,
                },
                get_varnish_builtins(),
            )
            .unwrap();
        assert!(!result.is_empty());
    }

    #[test]
    fn get_all_vmod_imports() {
        let doc = Document::new(
            Url::parse("file:///test.vcl").unwrap(),
            r#"
import brotli;
import jwt;
import xkey;
"#
            .to_string(),
        );
        let result = doc.get_vmod_imports();
        println!("imports: {:?}", result);
        assert_eq!(result.len(), 3);
        assert_eq!(result[0], "brotli");
        assert_eq!(result[1], "jwt");
        assert_eq!(result[2], "xkey");
    }

    #[test]
    fn get_all_subroutines() {
        let doc = Document::new(
            Url::parse("file:///test.vcl").unwrap(),
            r#"
sub vcl_init {}
sub vcl_recv {}
sub my_custom_sub {}
"#
            .to_string(),
        );
        let result = doc.get_subroutines();
        println!("subroutines: {:?}", result);
        assert_eq!(result.len(), 3);
        assert_eq!(result[0], "vcl_init");
        assert_eq!(result[1], "vcl_recv");
        assert_eq!(result[2], "my_custom_sub");
    }

    #[test]
    fn get_definition_for_sub() {
        let doc = Document::new(
            Url::parse("file:///test.vcl").unwrap(),
            r#"
sub my_custom_sub {}
sub vcl_recv {
    call my_custom_sub;
}
"#
            .to_string(),
        );
        let result = doc
            .get_definition_by_point(Point { row: 3, column: 9 })
            .unwrap();
        assert_eq!(result.0.row, 1);
        assert_eq!(result.0.column, 0);
        assert_eq!(result.1.row, 1);
        assert_eq!(result.1.column, 20);
    }

    #[test]
    fn get_all_definitions_works() {
        let doc = Document::new(
            Url::parse("file:///test.vcl").unwrap(),
            r#"
acl my_ips {}
sub my_custom_sub {}
sub vcl_recv {}
"#
            .to_string(),
        );
        let result = doc.get_all_definitions(&get_varnish_builtins());
        assert_eq!(result.len(), 3);
    }

    #[test]
    fn nested_identifier_before_if_works() {
        let doc = Document::new(
            Url::parse("file:///test.vcl").unwrap(),
            r#"
acl a_not_this {}
backend a_match_this {}
sub vcl_recv {
    if (req.

    if (true) {}
}
"#
            .to_string(),
        );
        let result = doc.autocomplete_for_pos(
            Position {
                line: 4,
                character: 11,
            },
            get_varnish_builtins(),
        );
        println!("result: {:?}", result);
        assert_eq!(
            result.unwrap()[0].detail,
            Some("BACKEND backend_hint".to_string())
        );
    }

    #[test]
    fn autocomplete_backend_after_equal_sign() {
        let mut scope = get_varnish_builtins();
        if let Type::Obj(ref mut obj) = scope {
            obj.properties
                .insert("localhost".to_string(), Type::Backend);
            obj.properties
                .insert("localhost_probe".to_string(), Type::Probe);
        }

        let doc = Document::new(
            Url::parse("file:///test.vcl").unwrap(),
            r#"
backend localhost {}
sub vcl_recv {
    set req.backend_hint = 
}
"#
            .to_string(),
        );
        let result = doc.autocomplete_for_pos(
            Position {
                line: 3,
                character: 26,
            },
            scope,
        );
        println!("result: {:?}", result);
        let result = result.unwrap();
        /*
        assert_eq!(
            result.first().unwrap().detail,
            Some("localhost".to_string())
        );
        assert_eq!(result.len(), 1);
        */
        assert!(!result.is_empty());
        assert!(result.iter().any(|item| item.label == *"localhost"));
        assert!(!result.iter().any(|item| item.label == *"localhost_probe"));
    }

    #[test]
    fn autocomplete_backend_def_properties() {
        let doc = Document::new(
            Url::parse("file:///test.vcl").unwrap(),
            r#"
backend localhost {
    .po
}
"#
            .to_string(),
        );
        let result = doc.autocomplete_for_pos(
            Position {
                line: 2,
                character: 6,
            },
            get_varnish_builtins(),
        );
        println!("result: {:?}", result);
        let result = result.unwrap();
        assert_eq!(result.first().unwrap().detail, Some("port".to_string()));
        assert_eq!(result.len(), 1);
    }

    #[test]
    fn lists_all_includes() {
        let doc = Document::new(
            Url::parse("file:///test.vcl").unwrap(),
            r#"
include "config/acl.vcl";
sub vcl_recv {
    if (req.url = "/health") {
        include "config/healthcheck.vcl";
    }
}
"#
            .to_string(),
        );
        let result = doc.get_includes();
        println!("result: {:?}", result);
        assert_eq!(result, vec!["config/acl.vcl", "config/healthcheck.vcl"]);
    }

    #[test]
    fn list_all_new_objs() {
        let doc = Document::new(
            Url::parse("file:///test.vcl").unwrap(),
            r#"
sub vcl_init {
    new static_web = directors.round_robin();
}
"#
            .to_string(),
        );
        let result = doc.get_all_definitions(&Type::Obj(Obj {
            name: "test".to_string(),
            properties: BTreeMap::from([(
                "directors".to_string(),
                Type::Obj(Obj {
                    name: "mock_directors".to_string(),
                    properties: BTreeMap::from([(
                        "round_robin".to_string(),
                        Type::Func(Func {
                            r#return: Some(Box::new(Type::Obj(Obj {
                                name: "mock_round_robin_director".to_string(),
                                properties: BTreeMap::from([(
                                    "backend".to_string(),
                                    Type::Func(Default::default()),
                                )]),
                                ..Default::default()
                            }))),
                            ..Default::default()
                        }),
                    )]),
                    ..Default::default()
                }),
            )]),
            ..Default::default()
        }));
        println!("result: {:?}", result);
        assert_eq!(result.len(), 2);
    }
}
