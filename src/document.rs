use crate::{
    parser::parser,
    varnish_builtins::{scope_contains, Func, Obj, Type, BACKEND_FIELDS, PROBE_FIELDS},
};

use log::debug;
use std::{
    collections::VecDeque,
    sync::{Arc, Mutex},
};
use tower_lsp::lsp_types::*;
use tree_sitter::{InputEdit, Node, Parser, Point, Query, QueryCursor, Tree};
use xi_rope::{rope::Utf16CodeUnitsMetric, Interval, Rope};

#[derive(Clone)]
pub struct Document {
    version: i32,
    parser: Arc<Mutex<Parser>>,
    pub rope: Rope,
    pub ast: Tree,
    pub url: Option<Url>,
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub ident_str: String,
    pub r#type: Box<Type>,
    pub line_num: usize,
    pub doc_url: Option<String>,
}

#[derive(Debug)]
pub struct LintError {
    message: String,
    severity: DiagnosticSeverity,
    range: Range,
}

// reserved keywords, without top-level only declarations
const RESERVED_KEYWORDS: &'static [&str] = &[
    "if", "set", "new", "call", "else", "elsif", "unset", "include", "return",
];

pub fn get_node_text(rope: &Rope, node: &Node) -> String {
    let mut str = &rope.to_string()[node.byte_range()];
    if let Some((first_part, _)) = str.split_once('\n') {
        str = first_part;
    }

    if RESERVED_KEYWORDS.contains(&str) {
        return "".to_string();
    }

    return str.to_string();
}

unsafe impl Send for Document {}
unsafe impl Sync for Document {}

impl Document {
    pub fn new(text: String) -> Self {
        let mut parser = parser();
        let ast = parser.parse(&text, None).unwrap();
        let rope = Rope::from(text);
        let parser = Arc::new(Mutex::new(parser));
        Self {
            version: 0,
            rope,
            parser,
            ast,
            url: None,
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
        let start = position_to_offset(&self.rope, range.start);
        let end = position_to_offset(&self.rope, range.end);
        let new_end_byte = start + text.as_bytes().len();

        let mut new_rope = self.rope.clone();
        let mut new_ast = self.ast.clone();

        new_rope.edit(Interval { start, end }, text);
        new_ast.edit(&InputEdit {
            start_byte: start,
            old_end_byte: end,
            new_end_byte,
            start_position: offset_to_point(&self.rope, end),
            old_end_position: offset_to_point(&self.rope, end),
            new_end_position: offset_to_point(&new_rope, new_end_byte),
        });

        let new_new_ast = self
            .parser
            .lock()
            .unwrap()
            .parse_with(
                &mut |offset, _pos| get_chunk(&new_rope, offset),
                Some(&new_ast),
            )
            .unwrap();

        self.rope = new_rope;
        self.ast = new_new_ast;
    }

    fn edit_fulltext(&mut self, _version: i32, text: String) {
        let rope = Rope::from(text.clone());
        let ast = self.parser.lock().unwrap().parse(&text, None).unwrap();
        self.rope = rope;
        self.ast = ast;
    }

    pub fn get_ident_at_point(&self, point: Point) -> Option<String> {
        let node = self
            .ast
            .root_node()
            .descendant_for_point_range(point, point)?;
        let name = get_node_text(&self.rope, &node);
        return Some(name);
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
            for capture in each_match.captures.iter().filter(|c| c.index == capt_idx) {
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

        return None;
    }

    pub fn get_definition_by_point(&self, point: Point) -> Option<(Point, Point)> {
        let name = self.get_ident_at_point(point)?;
        return self.get_definition_by_name(&name);
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
                    range,
                    severity: DiagnosticSeverity::ERROR,
                });
                recurse = false;
                continue;
            }

            if node.is_missing() {
                error_ranges.push(LintError {
                    message: format!("Expected {}", node.kind()),
                    range,
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
                            range,
                            severity: DiagnosticSeverity::ERROR,
                        });
                        continue;
                    }

                    let right = node.child_by_field_name("right");
                    if right.is_none() {
                        error_ranges.push(LintError {
                            message: "Missing set value".to_string(),
                            range,
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
                            range,
                            severity: DiagnosticSeverity::ERROR,
                        });
                        continue;
                    }

                    let right = node.child_by_field_name("def_right");
                    if right.is_none() {
                        error_ranges.push(LintError {
                            message: "Missing value".to_string(),
                            range,
                            severity: DiagnosticSeverity::ERROR,
                        });
                        continue;
                    }
                }
                "backend_property" => {
                    let left = node.child_by_field_name("left");
                    match left {
                        None => {
                            error_ranges.push(LintError {
                                message: "Missing backend property field".to_string(),
                                range,
                                severity: DiagnosticSeverity::ERROR,
                            });
                            continue;
                        }
                        Some(left_node) => {
                            let parent_parent_node_kind = node.parent().unwrap().kind();
                            let fields = match parent_parent_node_kind {
                                "probe_declaration" => PROBE_FIELDS,
                                _ => BACKEND_FIELDS,
                            };

                            let text = &full_text[left_node.byte_range()];
                            if !fields.iter().any(|field| field == &text) {
                                error_ranges.push(LintError {
                                    message: format!("Backend property «{}» does not exist", text),
                                    range,
                                    severity: DiagnosticSeverity::ERROR,
                                });
                            }
                        }
                    }

                    let right = node.child_by_field_name("right");
                    if right.is_none() {
                        error_ranges.push(LintError {
                            message: "Missing backend property value".to_string(),
                            range,
                            severity: DiagnosticSeverity::ERROR,
                        });
                        continue;
                    }
                }
                "nested_ident" | "ident" => {
                    let text = &self.rope.to_string()[node.byte_range()];
                    if RESERVED_KEYWORDS.contains(&text) {
                        error_ranges.push(LintError {
                            message: "Reserved keyword".to_string(),
                            range,
                            severity: DiagnosticSeverity::ERROR,
                        });
                        continue;
                    }
                    if text.ends_with('.') {
                        error_ranges.push(LintError {
                            message: "Identifier ending with dot?".to_string(),
                            range,
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
                            let sub_name = get_node_text(&self.rope, &ident_node);
                            let parts = text.split('.').collect::<Vec<&str>>();
                            if sub_name.starts_with("vcl_") {
                                let exists_in_sub = match parts[0] {
                                    "req" => match sub_name.as_str() {
                                        "vcl_recv" | "vcl_deliver" | "vcl_synth" | "vcl_miss"
                                        | "vcl_hit" | "vcl_pass" | "vcl_purge" | "vcl_pipe"
                                        | "vcl_hash" => true,
                                        _ => false,
                                    },
                                    "bereq" => match sub_name.as_str() {
                                        "vcl_backend_fetch"
                                        | "vcl_backend_response"
                                        | "vcl_pipe"
                                        | "vcl_backend_error" => true,
                                        _ => false,
                                    },
                                    "beresp" => match sub_name.as_str() {
                                        "vcl_backend_response" | "vcl_backend_error" => true,
                                        _ => false,
                                    },
                                    "resp" => match sub_name.as_str() {
                                        "vcl_deliver" | "vcl_miss" | "vcl_synth" => true,
                                        _ => false,
                                    },
                                    "obj" => match sub_name.as_str() {
                                        "vcl_hit" | "vcl_deliver" => true,
                                        _ => false,
                                    },
                                    _ => true,
                                };

                                if !exists_in_sub {
                                    error_ranges.push(LintError {
                                        message: format!(
                                            "«{}» does not exist in «{}»",
                                            parts[0], sub_name
                                        ),
                                        range,
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

        return error_ranges;
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        return self
            .get_error_ranges()
            .iter()
            .map(|lint_error| Diagnostic {
                range: lint_error.range,
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

    pub fn get_all_definitions<'a>(&self, scope_with_vmods: &'a Type) -> Vec<Definition> {
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
            let text_range = ident_capture.node.range();
            let text = &str[text_range.start_byte..text_range.end_byte];
            let line_num = node_capture.node.start_position().row;
            let r#type = match node_capture.node.kind() {
                "sub_declaration" => Some(Type::Sub),
                "acl_declaration" => Some(Type::Acl),
                "backend_declaration" => Some(Type::Backend),
                "probe_declaration" => Some(Type::Probe),
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

                    defs.push(Definition {
                        ident_str: text.to_string(),
                        line_num,
                        doc_url: self.url.as_ref().and_then(|url| Some(url.to_string())),
                        r#type: r#type_box,
                    });

                    continue;
                }
                _ => None,
            };

            if r#type.is_some() {
                defs.push(Definition {
                    ident_str: text.to_string(),
                    line_num,
                    doc_url: self.url.as_ref().and_then(|url| Some(url.to_string())),
                    r#type: Box::new(r#type.unwrap()),
                });
            }
        }

        defs
    }

    /**
     * Expand identifiers into req, res etc. and their properties.
     */
    pub fn autocomplete_for_pos(
        &self,
        pos: Position,
        global_scope: Type,
    ) -> Option<Vec<CompletionItem>> {
        debug!("starting autocomplete");
        let ast = self.ast.clone();
        let target_point = Point {
            row: pos.line as usize,
            column: pos.character as usize,
        };

        // let mut ctx_node: Option<Node> = None;
        let mut cursor = ast.walk();
        let mut node: Node;
        let mut text = "".to_string();
        let full_text = self.rope.to_string();
        // let mut ctx_text: Option<&str> = None;
        let mut search_type: Option<&Type> = None;
        let mut ignore_type: Option<Type> = None;
        let mut last_loop = false;
        while !last_loop {
            let idx_opt = cursor.goto_first_child_for_point(target_point);
            node = cursor.node();
            debug!("visiting node {:?}", node);

            // fix when cursor is at ;
            if node.kind() == ";" {
                node = node.prev_sibling().unwrap();
                debug!("fixing ; by going to node {:?}", node);
                last_loop = true;
            }

            // try to fix when tree-sitter says we're on a closing curly bracket
            /*
            if node.kind() == "}" {
                node = node.prev_sibling().unwrap();
                match node.descendant_for_point_range(target_point, target_point) {
                    Some(possible_node) => {
                        node = possible_node;
                        debug!("hmm {:?}", possible_node);
                    },
                    _ => {
                        debug!("meh");
                    }
                }
                debug!("fixing }} by going to node {:?}", node);
                last_loop = true;
            }
            */

            // gather context for autocomplete
            if let Some(parent_node) = node.parent() {
                match parent_node.kind() {
                    "set_stmt" => {
                        let mut field = cursor.field_name();

                        // When the node under the cursor is «;», select the right field instead
                        if node.kind() == ";" || node.kind() == "=" {
                            field = Some("right");
                        }

                        match field {
                            Some("left") => {
                                // ignore functions
                                ignore_type = Some(Type::Func(Func {
                                    ..Default::default()
                                }));
                            }
                            Some("right") => {
                                // text = get_node_text(&self.rope, &node);
                                if let Some(ctx_node) = parent_node.child_by_field_name("left") {
                                    let ctx_node_str = &full_text[ctx_node.byte_range()];
                                    search_type =
                                        lookup_from_scope_by_str(&global_scope, &ctx_node_str);
                                    // ignore objects here
                                    match search_type {
                                        Some(Type::Obj(_obj)) => {
                                            search_type = None;
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    "backend_property" => {
                        debug!("found backend_property {:?}", cursor.field_name());
                        let mut field = cursor.field_name();
                        if node.kind() == ";" || node.kind() == "=" {
                            field = Some("right");
                        }

                        match field {
                            Some("left") => {
                                text = get_node_text(&self.rope, &node);
                                let parent_parent_node_kind = parent_node.parent().unwrap().kind();
                                let fields = match parent_parent_node_kind {
                                    "probe_declaration" => PROBE_FIELDS,
                                    _ => BACKEND_FIELDS,
                                };
                                return Some(
                                    fields
                                        .iter()
                                        .filter(|field| field.starts_with(&text))
                                        .map(|field| CompletionItem {
                                            label: field.to_string(),
                                            detail: Some(field.to_string()),
                                            ..Default::default()
                                        })
                                        .collect(),
                                );
                            }
                            Some("right") => {
                                if let Some(ctx_node) = parent_node.child_by_field_name("left") {
                                    let ctx_node_str = &full_text[ctx_node.byte_range()];
                                    debug!("probe or backend. {:?} {}", ctx_node, ctx_node_str);
                                    if ctx_node_str == "probe" {
                                        search_type = Some(&Type::Probe);
                                    }
                                }
                            }
                            _ => {}
                        };
                    }
                    "call_stmt" => {
                        search_type = Some(&Type::Sub);
                    }
                    _ => {}
                }
            }

            // now, try to gather best identifier for autocomplete
            match node.kind() {
                "nested_ident" => {
                    text = get_node_text(&self.rope, &node);
                    debug!("got text for nested_ident {:?} {}", node, text);
                    break;
                }
                "ident" if node.parent().unwrap().kind() != "nested_ident" => {
                    text = get_node_text(&self.rope, &node);
                    debug!("got text for ident {:?} {}", node, text);
                }
                _ => {}
            }

            if idx_opt.is_none() {
                // has reached the end
                break;
            }
        }

        // debug!("jaggu: {:?}", search_type);
        // let full_text = self.rope.to_string();
        // text = &full_text[node.start_byte()..node.end_byte()];
        // debug!("text: {:?}", text);

        // identifiers written so far
        let idents: Vec<&str> = text.split('.').collect(); // split by dot
                                                           // get scope from identifiers written so far (excluding the last one)
        let scope = lookup_from_scope(&global_scope, idents[0..idents.len() - 1].to_vec());

        if scope.is_none() {
            return None;
        }

        // partial match on last identifier written
        let last_part = idents[idents.len() - 1];

        return match scope.unwrap() {
            Type::Obj(obj) => Some(
                obj.properties
                    .range(last_part.to_string()..)
                    .take_while(|(key, _v)| key.starts_with(&last_part))
                    .filter(|(_prop_name, property)| {
                        // debug!("{}: {:?}", _prop_name, property);
                        // try to match only backends when autocompleting for e.g. req.backend_hint
                        return if let Some(search_type) = search_type {
                            let has_type = scope_contains(property, search_type, true);
                            debug!("{} has type {}? {}", _prop_name, search_type, has_type);
                            has_type
                        } else if let Some(ref ignore_type) = ignore_type {
                            return !property.is_same_type_as(ignore_type);
                        } else {
                            // match on everything
                            true
                        };
                    })
                    .map(|(prop_name, property)| CompletionItem {
                        label: prop_name.to_string(),
                        detail: Some(match property {
                            Type::Func(_func) => format!("{}", property),
                            Type::Backend => format!("BACKEND {}", prop_name),
                            _ => format!("{} {}", property, prop_name.to_string()),
                        }),
                        kind: Some(match property {
                            Type::Func(_func) => CompletionItemKind::FUNCTION,
                            Type::Obj(_obj) => CompletionItemKind::STRUCT,
                            Type::Sub => CompletionItemKind::FUNCTION,
                            _ => CompletionItemKind::PROPERTY,
                        }),
                        insert_text: Some(match property {
                            Type::Func(_func) => format!("{}(", prop_name.to_string()),
                            _ => prop_name.to_string(),
                        }),
                        ..Default::default()
                    })
                    .collect(),
            ),
            _ => return None,
        };
    }
}

fn position_to_offset(rope: &Rope, pos: Position) -> usize {
    let line_offset = rope.offset_of_line(pos.line as usize);
    let line_slice = rope.slice(line_offset..);
    let char_offset = line_slice.count_base_units::<Utf16CodeUnitsMetric>(pos.character as usize);
    line_offset + char_offset
}

fn offset_to_point(rope: &Rope, offset: usize) -> Point {
    let row = rope.line_of_offset(offset);
    let column = offset - rope.offset_of_line(row);
    Point { row, column }
}

/*
fn offset_to_position(rope: &Rope, offset: usize) -> Position {
    let row = rope.line_of_offset(offset);
    let column = offset - rope.offset_of_line(row);
    Position::new(row as u32, column as u32)
}
*/

fn get_chunk(rope: &Rope, offset: usize) -> &str {
    let cursor = xi_rope::Cursor::new(&rope, offset);
    if let Some((node, idx)) = cursor.get_leaf() {
        &node[idx..]
    } else {
        ""
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

    return Some(scope);
}
fn lookup_from_scope_by_str<'a>(global_scope: &'a Type, idents: &'a str) -> Option<&'a Type> {
    return lookup_from_scope(global_scope, idents.split('.').collect());
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::varnish_builtins::get_varnish_builtins;

    use super::*;

    #[test]
    fn autocomplete_expands_h_to_http() {
        let doc = Document::new(
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
        assert_eq!(result.len(), 2);
        // assert_eq!(result[0].label, "http");
    }

    #[test]
    fn autocomplete_lists_all_on_req() {
        let doc = Document::new(
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
        assert!(result.len() > 0);
    }

    #[test]
    fn get_all_vmod_imports() {
        let doc = Document::new(
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
        assert!(result.len() > 0);
        assert!(result
            .iter()
            .any(|item| item.label == "localhost".to_string()));
        assert!(!result
            .iter()
            .any(|item| item.label == "localhost_probe".to_string()));
    }

    #[test]
    fn autocomplete_backend_def_properties() {
        let doc = Document::new(
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
                                    Type::Func(Func {
                                        ..Default::default()
                                    }),
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
