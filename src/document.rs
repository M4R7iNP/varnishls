use crate::{
    config::LintConfig,
    parser, static_autocomplete_items,
    varnish_builtins::{
        self, get_backend_field_types, get_probe_field_types, AutocompleteSearchOptions,
        Definition, Definitions, HasTypeProperties, Type,
    },
};

use log::debug;
use ropey::{iter::Chunks, Rope};
use std::iter::Iterator;
use std::sync::{Arc, Mutex};
use tower_lsp::lsp_types::*;
use tree_sitter::{InputEdit, Node, Parser, Point, Query, QueryCursor, TextProvider, Tree};

const VCL_QUERY: &str = include_str!("../vendor/tree-sitter-vcl/queries/semantic_tokens.scm");
const VTC_QUERY: &str = include_str!("../vendor/tree-sitter-vtc/queries/highlights.scm"); // TODO:

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
    pub pos_from_main_doc: Option<NestedPos>,
}

#[derive(Debug, Clone)]
pub struct Include {
    pub uri: Url,
    pub nested_pos: NestedPos,
}

#[derive(Debug, Clone)]
pub struct VmodImport {
    pub name: String,
    pub loc: Location,
    pub nested_pos: Option<NestedPos>,
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

pub type NestedPos = Vec<(usize, usize)>;

// Reserved keywords: words you can't name e.g. a backend, subroutine etc.
const RESERVED_KEYWORDS: &[&str] = &[
    "if", "set", "new", "call", "else", "elsif", "unset", "include", "return", "sub", "acl",
    "backend", // "probe", // FIXME: probe is a valid field name
    "req", "resp", "bereq", "beresp", "client", "server",
];

pub const LEGEND_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::VARIABLE,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::STRING,
    SemanticTokenType::COMMENT,
    SemanticTokenType::NUMBER,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::REGEXP,
    SemanticTokenType::NUMBER,
    // SemanticTokenType::new("delimiter"),
];

pub const LEGEND_MODIFIERS: &[SemanticTokenModifier] = &[SemanticTokenModifier::DEFAULT_LIBRARY];

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
    pub fn new(url: Url, text: String, nested_pos: Option<NestedPos>) -> Self {
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
            filetype,
            pos_from_main_doc: nested_pos,
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
        let mut new_ast = self.ast.clone();

        let start_line = self.rope.line(range.start.line as usize);
        let start_char = start_line.utf16_cu_to_char(range.start.character as usize);
        let start_position = Point {
            row: range.start.line as usize,
            column: start_line.char_to_byte(start_char),
        };

        let old_end_char = self
            .rope
            .get_line(range.end.line as usize)
            .map(|line| line.utf16_cu_to_char(range.end.character as usize))
            .unwrap_or_else(|| {
                // Edge case: fix for one-past-the-end line.
                let len_lines = self.rope.len_lines();
                if range.end.character == 0 && (range.end.line as usize) == len_lines {
                    return 0;
                }
                // trigger the error if it is way beyond
                self.rope.line(range.end.line as usize);
                unreachable!();
            });

        let start = self.rope.line_to_char(range.start.line as usize) + start_char;
        let end = self.rope.line_to_char(range.end.line as usize) + old_end_char;

        let old_end_position = Point {
            row: range.end.line as usize,
            column: self.rope.char_to_byte(end) - self.rope.line_to_byte(range.end.line as usize),
        };

        let old_end_byte = self.rope.char_to_byte(end);
        self.rope.remove(start..end);
        self.rope.insert(start, text.as_str());
        let start_byte = self.rope.char_to_byte(start);
        let new_end_byte = start_byte + text.len();
        let new_end_line = self.rope.byte_to_line(new_end_byte);

        let new_end_position = Point {
            row: new_end_line,
            column: new_end_byte - self.rope.line_to_byte(new_end_line),
        };

        new_ast.edit(&InputEdit {
            start_byte,
            old_end_byte,
            new_end_byte,
            start_position,
            old_end_position,
            new_end_position,
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

    pub fn edit_fulltext(&mut self, _version: i32, text: String) {
        let rope = Rope::from(text.clone());
        let ast = self.parser.lock().unwrap().parse(&text, None).unwrap();
        self.rope = rope;
        self.ast = ast;
    }

    pub fn get_type_at_point(&self, point: Point, scope: Definitions) -> Option<Type> {
        let mut node = self
            .ast
            .root_node()
            .descendant_for_point_range(point, point)?;

        loop {
            if matches!(node.kind(), "ident_call_expr") {
                let ident_node = node.child_by_field_name("ident")?;
                let ident = get_node_text(&self.rope, &ident_node);
                let parts = ident.split('.').collect::<Vec<&str>>();
                let r#type = scope.get_type_property_by_nested_idents(parts);
                if r#type.is_some() {
                    return r#type.cloned();
                }
            } else if matches!(node.kind(), "nested_ident" | "ident") {
                let ident = get_node_text(&self.rope, &node);
                let parts = ident.split('.').collect::<Vec<&str>>();
                let r#type = scope.get_type_property_by_nested_idents(parts);
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

    /// get identifier at point, only first part of nested idents
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

    pub fn get_definition_by_name(&self, name: &str) -> Option<(Point, Point)> {
        let name_escaped = name.replace('"', "\\\"");
        let q = Query::new(
            self.ast.language(),
            &format!(
                r#"
                [
                    (toplev_declaration (_ ident: (ident) @ident (#eq? @ident "{name_escaped}")))
                    (new_stmt (ident) @left (#eq? @left "{name_escaped}"))
                ] @node
                "#,
            ),
        );
        if let Err(err) = q {
            log::error!("Failed exec query for goto definition: {}", err);
            return None;
        }
        let q = q.unwrap();
        let mut qc = QueryCursor::new();
        let all_matches = qc.matches(&q, self.ast.root_node(), self);
        let capt_idx = q.capture_index_for_name("node").unwrap();

        for each_match in all_matches {
            if let Some(capture) = each_match.captures.iter().find(|c| c.index == capt_idx) {
                let range = capture.node.range();
                return Some((range.start_point, range.end_point));
            }
        }

        None
    }

    pub fn get_definition_by_point(&self, point: Point) -> Option<(Point, Point)> {
        let name = self.get_ident_at_point(point)?;
        self.get_definition_by_name(&name)
    }

    // TODO: only check definition line numbers for certain types
    // TO CHECK: probes, ident_call_exprs
    // TO NOT CHECK: backends, subroutines
    pub fn get_error_ranges(
        &self,
        global_scope: Definitions,
        config: &LintConfig,
    ) -> Vec<LintError> {
        let mut cursor = self.ast.walk();
        let mut error_ranges = Vec::new();
        let mut recurse = true;

        loop {
            if (recurse && cursor.goto_first_child()) || cursor.goto_next_sibling() {
                recurse = true;
            } else if cursor.goto_parent() {
                recurse = false;
                continue;
            } else {
                break;
            }

            let node = cursor.node();
            macro_rules! add_error {
                (range: $range:expr, severity: $severity:expr, $($arg:tt)+) => {
                    error_ranges.push(LintError {
                        message: format!($($arg)+),
                        loc: Location {
                            uri: self.url.to_owned(),
                            range: $range,
                        },
                        severity: $severity,
                    });
                };
                (node: $node:expr, $($arg:tt)+) => {
                    let range = ts_range_to_lsp_range($node.range());
                    add_error!(range: range, severity: DiagnosticSeverity::ERROR, $($arg)+);
                };
                ($($arg:tt)+) => {
                    let range = ts_range_to_lsp_range(node.range());
                    add_error!(range: range, severity: DiagnosticSeverity::ERROR, $($arg)+);
                }
            }

            macro_rules! add_hint {
                (node: $node:expr, $($arg:tt)+) => {
                    let range = ts_range_to_lsp_range($node.range());
                    add_error!(range: range, severity: DiagnosticSeverity::HINT, $($arg)+);
                };
                ($($arg:tt)+) => {
                    let range = ts_range_to_lsp_range(node.range());
                    add_error!(range: range, severity: DiagnosticSeverity::HINT, $($arg)+);
                }
            }

            if node.is_error() {
                add_error!("Syntax error");
                recurse = false;
                continue;
            }

            if node.is_missing() {
                add_error!("Expected {}", node.kind());
                recurse = false;
                continue;
            }

            match node.kind() {
                "set_stmt" => {
                    let Some(left_node) = node.child_by_field_name("left") else {
                        add_error!("Missing set property");
                        continue;
                    };

                    let Some(right_node) = node.child_by_field_name("right") else {
                        add_error!("Missing set value");
                        continue;
                    };

                    let left_ident_text = {
                        if matches!(left_node.kind(), "ident" | "nested_ident") {
                            get_node_text(&self.rope, &left_node).to_lowercase()
                        } else {
                            continue;
                        }
                    };

                    let left_parts = left_ident_text.split('.').collect::<Vec<_>>();
                    if vec!["req", "bereq", "resp", "beresp"].contains(&left_parts[0]) {
                        // Deprecated headers (from MDN)
                        if matches!(left_parts.get(1), Some(&"http"))
                            && matches!(
                                left_parts.get(2),
                                Some(&"content-dpr")
                                    | Some(&"dnt")
                                    | Some(&"dpr")
                                    | Some(&"large-allocation")
                                    | Some(&"pragma")
                                    | Some(&"sec-ch-ua-full-version")
                                    | Some(&"tk")
                                    | Some(&"viewport-width")
                                    | Some(&"width")
                            )
                        {
                            let hdr = left_parts[2];
                            add_hint!(node: left_node, "Deprecated header {hdr}");
                        }

                        // Hit-for-pass is usually better than no cache at all
                        if left_parts.get(1) == Some(&"ttl") {
                            let right_text = get_node_text(&self.rope, &right_node);
                            if matches!(right_text.as_str(), "0s" | "0") {
                                add_hint!("Consider using .cacheable = false instead");
                            }
                        }

                        // Vary on header with many arbitrary values
                        if left_parts.get(1) == Some(&"vary") {
                            let right_text = get_node_text(&self.rope, &right_node);
                            if matches!(
                                right_text.as_str().to_lowercase().as_str(),
                                "*" | "user-agent" | "accept-encoding" | "accept-language"
                            ) {
                                add_hint!("Consider filtering header before vary");
                            }
                        }

                        // Rewriting req.url also changes the cache key, so requests hitting that
                        // cache key might get a different response that intended.
                        if config.no_rewrite_req_url && left_ident_text.as_str() == "req.url" {
                            let toplev_decl = get_toplev_declaration_from_node(node);
                            if toplev_decl.kind() == "sub_declaration" {
                                if let Some(ident_node) = toplev_decl.child_by_field_name("ident") {
                                    let sub_name = get_node_text(&self.rope, &ident_node);
                                    if sub_name == "vcl_recv" {
                                        add_hint!("[no_rewrite_req_url] Don't rewrite req.url. Rather, make a concious decision whether to edit the cache key or just the backend url.");
                                    }
                                }
                            }
                        }
                    }
                }
                "new_stmt" => {
                    let left = node.child_by_field_name("ident");
                    if left.is_none() {
                        add_error!("Missing identifier");
                        continue;
                    }

                    let right = node.child_by_field_name("def_right");
                    if right.is_none() {
                        add_error!("Missing value");
                        continue;
                    }
                }
                "backend_property" => {
                    let left_ident = match node.child_by_field_name("left") {
                        None => {
                            add_error!("Missing backend property field");
                            continue;
                        }
                        Some(ref left_node) => get_node_text(&self.rope, left_node),
                    };

                    let parent_parent_node_kind = node.parent().unwrap().kind();
                    let map = match parent_parent_node_kind {
                        "probe_declaration" | "inline_probe" => get_probe_field_types(),
                        _ => get_backend_field_types(),
                    };

                    let r#type = map.get(left_ident.as_str());

                    let Some(right_node) = node.child_by_field_name("right") else {
                        add_error!("Missing backend property value");
                        continue;
                    };

                    // only request can contain multiline strings
                    if right_node.kind() == "string_list" && left_ident != "request" {
                        add_error!("Property {left_ident} cannot contain string list");
                    }

                    match r#type {
                        None => {
                            add_error!("Backend property «{}» does not exist", left_ident);
                        }
                        Some(r#type) => {
                            if right_node.kind() == "ident" {
                                let right_ident = get_node_text(&self.rope, &right_node);
                                let Some(ident_type) = global_scope.get_type_property(&right_ident) else {
                                    add_error!(node: right_node, "Undefined value");
                                    continue;
                                };
                                if !ident_type.can_this_cast_into(r#type) {
                                    add_error!(
                                        node: right_node,
                                        "Expected {}, found {}",
                                        r#type,
                                        ident_type
                                    );
                                }
                                continue;
                            }

                            let Some(right_node_type) = node_to_type(&right_node) else {
                                add_error!("Unexpected value"); // unrecognized type
                                continue;
                            };
                            if !right_node_type.can_this_cast_into(r#type) {
                                add_error!("Expected {}, found {}", r#type, right_node_type);
                            }
                        }
                    }
                }
                "call_stmt" => {
                    let Some(ident_node) = node.child_by_field_name("ident") else {
                        add_error!("Missing identifier");
                        continue;
                    };
                    let ident = get_node_text(&self.rope, &ident_node);
                    let Some(definition) = global_scope.get(ident.as_str()) else {
                        add_error!("Undefined subroutine");
                        continue;
                    };

                    match &*definition.r#type {
                        Type::Sub => {
                            /*
                            if let Some(ref doc_nested_pos) = self.pos_from_main_doc {
                                let mut use_nested_pos = doc_nested_pos.clone();
                                use_nested_pos.push(point_to_tuple(node.start_position()));

                                if definition
                                    .nested_pos
                                    .as_ref()
                                    .is_some_and(|nested_pos| nested_pos.gt(&use_nested_pos))
                                {
                                    let line = definition.loc.as_ref().unwrap().range.start.line;
                                    add_error!("sub {ident} is defined at line {line}");
                                }
                            }
                            */
                        }
                        t => {
                            add_error!("{ident} is of type {t}");
                        }
                    }
                }
                // lint ident_call_expr (e.g. brotli.init(BOTH, br_q = 1))
                "ident_call_expr" => {
                    let Some(ident_node) = node.child_by_field_name("ident") else {
                        continue;
                    };
                    let full_ident = get_node_text(&self.rope, &ident_node);
                    let ident_parts = full_ident.split('.').collect::<Vec<_>>();
                    // check first part exists (e.g. «brotli»)
                    let Some(definition) = global_scope.get(ident_parts[0]) else {
                        add_error!("{} undefined", ident_parts[0]);
                        continue;
                    };

                    // check it is defined above current line
                    if let Some(ref doc_nested_pos) = self.pos_from_main_doc {
                        let mut call_nested_pos = doc_nested_pos.clone();
                        call_nested_pos.push(point_to_tuple(node.start_position()));

                        if let Some(ref nested_pos) = definition.nested_pos {
                            if nested_pos.gt(&call_nested_pos) {
                                let line = definition.loc.as_ref().unwrap().range.start.line;
                                let filename = definition
                                    .loc
                                    .as_ref()
                                    .and_then(|loc| {
                                        loc.uri
                                            .path_segments()
                                            .and_then(|path_segments| path_segments.last())
                                    })
                                    .unwrap_or("<UNKNOWN>");

                                add_error!(
                                    "{} is defined in {} at line {}",
                                    ident_parts[0],
                                    filename,
                                    line + 1
                                );
                            }
                        }
                    }

                    // check method exists (init of brotli.init())
                    let Some(Type::Func(func)) = global_scope.get_type_property_by_nested_idents(ident_parts) else {
                        add_error!("{full_ident} is not a method");
                        continue;
                    };

                    // check required args are provided
                    let required_args = func
                        .args
                        .iter()
                        .filter(|arg| !arg.optional && arg.default_value.is_none())
                        .collect::<Vec<_>>();
                    let Some(args_node) = node.child_by_field_name("args") else {
                        debug!("could not find args node");
                        continue;
                    };
                    let mut args_cursor = self.ast.walk();
                    let arg_nodes = args_node
                        .children_by_field_name("arg", &mut args_cursor)
                        .collect::<Vec<Node>>();
                    if required_args.len() > arg_nodes.len() {
                        let missing_arg = required_args[arg_nodes.len()];
                        debug!("missing_arg: {:?}", missing_arg);
                        let mut message = "Missing required argument".to_string();
                        if let Some(ref missing_arg_name) = missing_arg.name {
                            message.push(' ');
                            message.push_str(missing_arg_name);
                        }
                        add_error!("{}", message);
                    }

                    // check the arguments exists and that their provided type is correct
                    let mut arg_idx = 0;
                    for arg_node in arg_nodes {
                        let arg;
                        let arg_value_node;
                        if arg_node.kind() == "func_call_named_arg" {
                            let Some(arg_name_node) = arg_node.child_by_field_name("arg_name") else { continue; };
                            let Some(_arg_value_node) = arg_node.child_by_field_name("arg_value") else {
                                add_error!(node: arg_node, "Expected value");
                                continue;
                            };
                            arg_value_node = _arg_value_node;
                            let arg_name =
                                &self.rope.byte_slice(arg_name_node.byte_range()).to_string();
                            let Some(_arg) = func
                                .args
                                .iter()
                                .find(|arg| arg.name.as_ref().map(|name| name.eq(arg_name.as_str())).unwrap_or(false)) else {
                                add_error!(node: arg_node, "No such argument named {arg_name}");
                                continue;
                            };
                            arg = _arg;
                        } else {
                            arg = {
                                let Some(found_arg) = &func.args.get(arg_idx) else {
                                    add_error!(node: arg_node, "Argument {} not found", arg_idx + 1);
                                    continue;
                                };
                                found_arg
                            };
                            arg_idx += 1;
                            arg_value_node = arg_node;
                        }

                        if let Some(arg_type) = arg.r#type.as_ref() {
                            if let Type::Enum(enum_values) = arg_type {
                                // validate enums
                                if arg_value_node.kind() != "ident" {
                                    add_error!(node: arg_node, "Enum not found");
                                    continue;
                                }
                                let enum_value = get_node_text(&self.rope, &arg_value_node);
                                if !enum_values.contains(&enum_value.to_string()) {
                                    add_error!(
                                        node: arg_node,
                                        "Enum value {} not found. Must be one of: {}",
                                        enum_value,
                                        arg.r#type.as_ref().unwrap()
                                    );
                                    continue;
                                }
                            } else if matches!(
                                arg_value_node.kind(),
                                "literal" | "ident" | "nested_ident"
                            ) {
                                // check provided type can cast into argument type
                                let Some(arg_value_type) = node_to_type(&arg_value_node).or_else(|| {
                                    let ident = get_node_text(&self.rope, &arg_value_node);
                                    let ident_parts = ident.split('.').collect::<Vec<_>>();
                                    global_scope.get_type_property_by_nested_idents(ident_parts.clone()).cloned()
                                }) else {
                                    add_error!(node: arg_value_node, "Not found");
                                    continue;
                                };
                                if !arg_value_type.can_this_cast_into(arg_type) {
                                    add_error!(
                                        node: arg_value_node,
                                        "{} cannot cast into {}",
                                        arg_value_type,
                                        arg_type
                                    );
                                }
                            }
                        }
                    }
                }
                "nested_ident" | "ident" => {
                    // manually get ident text without get_node_text here, to check for reserved
                    // keywords
                    let text = self.rope.byte_slice(node.byte_range()).to_string();
                    if RESERVED_KEYWORDS.contains(&text.as_str()) {
                        add_error!("Reserved keyword");
                        continue;
                    }
                    if text.ends_with('.') {
                        add_error!("Identifier ending with dot?");
                        continue;
                    }

                    // check whether e.g. req/resp is allowed from this builtin subroutine
                    // TODO: check where custom subroutines are called from
                    let toplev_decl = get_toplev_declaration_from_node(node);
                    if toplev_decl.kind() == "sub_declaration" {
                        if let Some(ident_node) = toplev_decl.child_by_field_name("ident") {
                            let sub_name = &*get_node_text(&self.rope, &ident_node);
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
                                    add_error!("«{}» does not exist in «{}»", parts[0], sub_name);
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

    pub fn diagnostics(
        &self,
        global_scope: Definitions,
        lint_config: &LintConfig,
    ) -> Vec<Diagnostic> {
        return self
            .get_error_ranges(global_scope, lint_config)
            .iter()
            .map(|lint_error| Diagnostic {
                range: lint_error.loc.range,
                severity: Some(lint_error.severity),
                message: lint_error.message.to_owned(),
                ..Diagnostic::default()
            })
            .collect();
    }

    pub fn get_vmod_imports(&self) -> Vec<VmodImport> {
        let q = Query::new(self.ast.language(), "(import_declaration (ident) @ident)").unwrap();
        let mut qc = QueryCursor::new();
        let all_matches = qc.matches(&q, self.ast.root_node(), self);
        let capt_idx = q.capture_index_for_name("ident").unwrap();

        let mut imports = Vec::new();
        for each_match in all_matches {
            for capture in each_match.captures.iter().filter(|c| c.index == capt_idx) {
                let ts_range = capture.node.range();
                let name = get_node_text(&self.rope, &capture.node).to_string();
                let range = ts_range_to_lsp_range(ts_range);
                imports.push(VmodImport {
                    name,
                    loc: Location {
                        uri: self.url.to_owned(),
                        range,
                    },
                    nested_pos: self.pos_from_main_doc.as_ref().map(|pos| {
                        let mut pos = pos.clone();
                        pos.push((ts_range.end_point.row, ts_range.end_point.column));
                        pos
                    }),
                });
            }
        }

        imports
    }

    pub fn get_includes(&self, root_uri: &Url, nested_pos: &NestedPos) -> Vec<Include> {
        let q = Query::new(
            self.ast.language(),
            "(include_declaration (string) @string)",
        )
        .unwrap();
        let mut qc = QueryCursor::new();
        let all_matches = qc.matches(&q, self.ast.root_node(), self);
        let capt_idx = q.capture_index_for_name("string").unwrap();

        let mut includes = Vec::new();
        for each_match in all_matches {
            for capture in each_match.captures.iter().filter(|c| c.index == capt_idx) {
                let range = capture.node.range();
                let text = get_node_text(&self.rope, &capture.node);
                let path = text.trim_matches('"');
                let uri = if path.starts_with("./") || path.starts_with("../") {
                    // relative to current vcl
                    self.url.join(path).unwrap()
                } else {
                    root_uri.join(path).unwrap()
                };
                let mut nested_pos = nested_pos.clone();
                nested_pos.push((range.start_point.row, range.start_point.column));
                includes.push(Include { uri, nested_pos });
            }
        }

        includes
    }

    pub fn get_subroutines(&self) -> Vec<String> {
        let q = Query::new(self.ast.language(), "(sub_declaration (ident) @ident)").unwrap();
        let mut qc = QueryCursor::new();
        let all_matches = qc.matches(&q, self.ast.root_node(), self);
        let capt_idx = q.capture_index_for_name("ident").unwrap();

        let mut import_names: Vec<String> = Vec::new();
        for each_match in all_matches {
            for capture in each_match.captures.iter().filter(|c| c.index == capt_idx) {
                let text = get_node_text(&self.rope, &capture.node).to_string();
                import_names.push(text);
            }
        }

        import_names
    }

    pub fn get_all_definitions(&self, scope_with_vmods: &Definitions) -> Vec<Definition> {
        let q = Query::new(
            self.ast.language(),
            r#"
                (toplev_declaration (_ (ident) @ident ) @node)
                (new_stmt ident: (ident) @ident def_right: (ident_call_expr ident: (_) @def_ident)) @node
            "#,
        );
        if let Err(err) = q {
            debug!("Error: {}", err);
            return vec![];
        }
        let q = q.unwrap();

        let mut qc = QueryCursor::new();
        let all_matches = qc.matches(&q, self.ast.root_node(), self);
        let node_capt_idx = q.capture_index_for_name("node").unwrap();
        let ident_capt_idx = q.capture_index_for_name("ident").unwrap();
        let def_ident_capt_idx = q.capture_index_for_name("def_ident").unwrap();

        let mut defs: Vec<Definition> = Vec::new();
        for each_match in all_matches {
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
            let text = get_node_text(&self.rope, &ident_capture.node);
            let loc = Location {
                uri: self.url.clone(),
                range: ts_range_to_lsp_range(node_capture.node.range()),
            };
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
                    let def_text = get_node_text(&self.rope, &def_ident_capture.node);
                    let Some(r#type) = scope_with_vmods
                        .get_type_property_by_nested_idents(def_text.split('.').collect()) else {
                        continue;
                    };

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
                r#type,
                loc: Some(loc),
                nested_pos: self.pos_from_main_doc.as_ref().map(|nested_pos| {
                    let mut nested_pos = nested_pos.clone();
                    // nested_pos.push(node_capture.node.range().into());
                    let range = node_capture.node.range();
                    nested_pos.push((range.start_point.row, range.start_point.column));
                    nested_pos
                }),
            });
        }

        defs
    }

    pub fn get_references_for_ident(&self, ident: &str) -> Vec<Reference> {
        let q = Query::new(
            self.ast.language(),
            &format!(
                r#"
                [
                    ((ident) @ident (#eq? @ident "{ident}"))
                    ((nested_ident) @nested_ident (#match? @nested_ident "^{ident}\\b"))
                ]
                "#
            ),
        );
        if let Err(err) = q {
            debug!("Error: {}", err);
            return vec![];
        }
        let q = q.unwrap();

        let mut qc = QueryCursor::new();
        let all_matches = qc.matches(&q, self.ast.root_node(), self);
        let ident_capt_idx = q.capture_index_for_name("ident").unwrap();
        let nested_ident_capt_idx = q.capture_index_for_name("nested_ident").unwrap();
        let mut refs: Vec<Reference> = Vec::new();
        for each_match in all_matches {
            let ident_capture = each_match
                .captures
                .iter()
                .find(|c| c.index == ident_capt_idx || c.index == nested_ident_capt_idx)
                .unwrap();
            let text = get_node_text(&self.rope, &ident_capture.node);
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
        global_scope: Definitions,
    ) -> Option<Vec<CompletionItem>> {
        debug!("starting autocomplete2");

        let mut target_point = Point {
            row: pos.line as usize,
            column: pos.character as usize,
        };

        let target_row = self.rope.line(target_point.row);
        if target_point.column > 0
            && matches!(
                target_row.get_char(target_point.column),
                None | Some('\n') | Some(',') | Some(')')
            )
            && !matches!(target_row.get_char(target_point.column - 1), Some('('))
        {
            debug!(
                "decrementing by one ({:?})",
                target_row.get_char(target_point.column)
            );
            target_point.column -= 1;
        }

        debug!("target point: {}", target_point);

        let mut text = "".to_string();
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
        debug!("cursor.node(): {:?}", cursor.node());
        if matches!(cursor.node().kind(), "," | ")") {
            // move cursor one point back
            if let Some(prev_sibling) = cursor.node().prev_sibling() {
                if prev_sibling.kind() != "(" {
                    debug!("prev_sibling: {prev_sibling:?}");
                    cursor.reset(prev_sibling);
                }
            }
        }
        let mut stop = false;
        while !stop {
            let node = cursor.node();
            debug!("visiting node {:?}", node);

            let Some(parent_node) = node.parent() else {
                break;
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
                            if let Some(ctx_node) = parent_node.child_by_field_name("left") {
                                let ctx_node_str = get_node_text(&self.rope, &ctx_node);
                                let left_type = global_scope.get_type_property_by_nested_idents(
                                    ctx_node_str.split('.').collect(),
                                );

                                if let Some(Type::Obj(obj)) = left_type {
                                    // Filter by string for headers
                                    if obj.is_http_headers {
                                        search_type = Some(Type::String);
                                    }
                                } else {
                                    // Autocomplete same type as left type
                                    search_type = left_type.cloned();
                                }
                            }
                        }
                        _ => {}
                    }
                    stop = true;
                }
                // Autocomplete backend/probe properties
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
                                let ctx_node_str = get_node_text(&self.rope, &ctx_node);
                                if ctx_node_str == "probe" {
                                    search_type = Some(Type::Probe);
                                }
                                stop = true;
                            }
                        }
                        _ => {}
                    };
                }
                // Filter by subroutines for call statements
                "call_stmt" => {
                    search_type = Some(Type::Sub);
                    stop = true;
                }
                // Autocomplete return methods
                "ret_stmt" => {
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
                // Autocomplete named arguments
                "func_call_args" => {
                    let func_node = find_parent(node, "ident_call_expr".to_string()).unwrap();
                    let func_ident =
                        get_node_text(&self.rope, &func_node.child_by_field_name("ident").unwrap());
                    if let Some(Type::Func(func)) = global_scope
                        .get_type_property_by_nested_idents(func_ident.split('.').collect())
                    {
                        // Add to suggestions
                        keyword_suggestions = func
                            .args
                            .iter()
                            .filter_map(|arg| {
                                arg.name.as_ref().map(|name| CompletionItem {
                                    label: name.to_string(),
                                    kind: Some(CompletionItemKind::PROPERTY), // closest?
                                    label_details: Some(CompletionItemLabelDetails {
                                        detail: Some("Parameter".to_string()),
                                        description: None,
                                    }),
                                    insert_text: Some(format!("{name} = ")),
                                    detail: arg.r#type.as_ref().map(|t| format!("{t}")),
                                    ..Default::default()
                                })
                            })
                            .collect();
                    }
                }
                // Autocomplete enum arguments
                "func_call_named_arg" => {
                    if cursor.field_name() == Some("arg_value") || node.kind() == "=" {
                        let arg_name = get_node_text(
                            &self.rope,
                            &node
                                .parent()
                                .unwrap()
                                .child_by_field_name("arg_name")
                                .unwrap(),
                        );
                        let func_node = find_parent(node, "ident_call_expr".to_string()).unwrap();
                        let func_ident = get_node_text(
                            &self.rope,
                            &func_node.child_by_field_name("ident").unwrap(),
                        );
                        if let Some(Type::Func(func)) = global_scope
                            .get_type_property_by_nested_idents(func_ident.split('.').collect())
                        {
                            if let Some(arg) = func
                                .args
                                .iter()
                                .find(|arg| arg.name.as_ref() == Some(&arg_name))
                            {
                                if let Some(Type::Enum(enum_values)) = arg.r#type.as_ref() {
                                    return Some(
                                        enum_values
                                            .iter()
                                            .map(|enum_value| CompletionItem {
                                                label: enum_value.to_string(),
                                                kind: Some(CompletionItemKind::ENUM),
                                                ..Default::default()
                                            })
                                            .collect(),
                                    );
                                }
                            }
                        }
                    }
                }
                "source_file" => {
                    return Some(static_autocomplete_items::source_file());
                }
                "sub_declaration" | "if_stmt" | "elsif_stmt" | "else_stmt" => {
                    if !text.contains('.') {
                        keyword_suggestions.append(&mut static_autocomplete_items::subroutine());
                    }
                    stop = true;
                }
                _ => {
                    if parent_node.kind() == "new_stmt" || node.kind() == "new_stmt" {
                        match cursor.field_name() {
                            Some("ident") => {
                                return Some(vec![]);
                            }
                            Some("def_right") => search_type = Some(Type::Func(Default::default())),
                            _ => {}
                        }
                        stop = true;
                    }
                }
            }

            // now, try to gather best identifier for autocomplete
            match node.kind() {
                "nested_ident" => {
                    text = get_node_text(&self.rope, &node).to_string();
                    debug!("got text for nested_ident {:?} {}", node, text);
                }
                "ident" if node.parent().unwrap().kind() != "nested_ident" => {
                    text = get_node_text(&self.rope, &node).to_string();
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
        debug!("text: «{:?}»", text);

        // identifiers written so far (split by dot)
        let idents: Vec<&str> = text.split('.').collect();

        let mut suggestions = global_scope
            .get_type_properties_by_idents(
                idents,
                AutocompleteSearchOptions {
                    search_type,
                    must_be_writable: Some(must_be_writable),
                },
            )?
            .iter()
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
                                let mut str = format!("${{{}", idx + 1);
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

        suggestions.append(&mut keyword_suggestions);

        Some(suggestions)
    }

    pub fn get_semantic_tokens(&self) -> Vec<SemanticToken> {
        let node = self.ast.root_node();
        let mut query_str = VCL_QUERY.to_string();
        if matches!(self.filetype, FileType::Vtc) {
            query_str.push_str(VTC_QUERY);
        }
        let q = Query::new(self.ast.language(), query_str.as_str()).unwrap();
        let mut qc = QueryCursor::new();

        let names = q.capture_names();

        #[derive(Eq, PartialEq, PartialOrd, Ord)]
        struct Token {
            line: usize,
            start: usize,
            token_type: usize, // LEGEND_TYPES is ordered, so we can order by this for precendence
            modifier: usize,
            length: usize,
        }

        let mut tokens = vec![];
        for c in qc.matches(&q, node, self).flat_map(|m| m.captures) {
            let node = c.node;
            let range = node.range();
            let mut capture_name = names.get(c.index as usize).unwrap().splitn(2, '.');
            let name = capture_name.next().unwrap();
            let modifier_name = capture_name.next();
            let Some(token_type) = LEGEND_TYPES.iter().position(|s| s.as_str() == name) else {
                continue;
            };
            let modifier = modifier_name
                .and_then(|modifier_name| {
                    LEGEND_MODIFIERS
                        .iter()
                        .position(|s| s.as_str() == modifier_name)
                })
                .unwrap_or(0);

            for row in range.start_point.row..=range.end_point.row {
                let start = if row == range.start_point.row {
                    let start_line = self.rope.line(row);
                    start_line.char_to_utf16_cu(start_line.byte_to_char(range.start_point.column))
                } else {
                    0
                };

                let end_line = self.rope.line(row);
                let end = if row == range.end_point.row {
                    end_line.char_to_utf16_cu(end_line.byte_to_char(range.end_point.column))
                } else {
                    end_line.len_utf16_cu()
                };

                tokens.push(Token {
                    start,
                    line: row,
                    length: end - start,
                    token_type,
                    modifier,
                });
            }
        }

        tokens.sort_unstable();

        let mut prev_line = 0;
        let mut prev_start = 0;
        tokens
            .into_iter()
            .map(|tok| {
                let delta_line = tok.line - prev_line;
                let delta_start = if delta_line == 0 {
                    tok.start - prev_start
                } else {
                    tok.start
                };
                let semtok = SemanticToken {
                    delta_start: delta_start as u32,
                    delta_line: delta_line as u32,
                    length: tok.length as u32,
                    token_type: tok.token_type as u32,
                    token_modifiers_bitset: tok.modifier as u32,
                };

                prev_line = tok.line;
                prev_start = tok.start;
                semtok
            })
            .collect()
    }
}

pub struct RopeChunkBytesIterator<'a> {
    chunks: Chunks<'a>,
}

impl<'a> Iterator for RopeChunkBytesIterator<'a> {
    type Item = &'a [u8];
    fn next(&mut self) -> Option<Self::Item> {
        self.chunks.next().map(|chunk| chunk.as_bytes())
    }
}

// TextProvider for tree-sitter to use ropey as efficiently as possible
impl<'a> TextProvider<'a> for &'a Document {
    type I = RopeChunkBytesIterator<'a>;
    fn text(&mut self, node: Node) -> Self::I {
        RopeChunkBytesIterator {
            chunks: self.rope.byte_slice(node.byte_range()).chunks(),
        }
    }
}

impl PartialEq for VmodImport {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for VmodImport {}

// Misc helper functions

fn point_to_position(point: Point) -> Position {
    Position {
        line: point.row as u32,
        character: point.column as u32,
    }
}

fn ts_range_to_lsp_range(input: tree_sitter::Range) -> Range {
    Range {
        start: point_to_position(input.start_point),
        end: point_to_position(input.end_point),
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
        "inline_probe" => Some(Type::Probe),
        "literal" => node_to_type(&node.child(0)?),
        _ => None,
    }
}

fn point_to_tuple(point: Point) -> (usize, usize) {
    (point.row, point.column)
}

fn get_toplev_declaration_from_node(node: Node) -> Node {
    let mut node = node;
    loop {
        let Some(parent_node) = node.parent() else {
            break;
        };
        if parent_node.kind() == "toplev_declaration" {
            break;
        }
        node = parent_node;
    }
    node
}

fn find_parent(node: Node, kind: String) -> Option<Node> {
    let mut node = node;
    loop {
        let Some(parent_node) = node.parent() else {
            return None;
        };
        node = parent_node;
        if node.kind() == kind {
            return Some(node);
        }
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
            None,
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
        assert!(result.iter().any(|item| item.label == "http"));
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
            None,
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
            None,
        );
        let vmod_names: Vec<_> = doc
            .get_vmod_imports()
            .iter()
            .map(|import| import.name.clone())
            .collect();
        println!("imports: {:?}", vmod_names);
        assert_eq!(vmod_names, vec!["brotli", "jwt", "xkey"]);
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
            None,
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
            None,
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
            None,
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
            None,
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

        scope.properties.insert(
            "localhost".to_string(),
            Definition::new_builtin("localhost".to_string(), Type::Backend),
        );
        scope.properties.insert(
            "localhost_probe".to_string(),
            Definition::new_builtin("localhost_probe".to_string(), Type::Probe),
        );

        let doc = Document::new(
            Url::parse("file:///test.vcl").unwrap(),
            r#"
backend localhost {}
sub vcl_recv {
    set req.backend_hint = 
}
"#
            .to_string(),
            None,
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
            None,
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
            Url::parse("file:///etc/varnish/vg/varnish.vcl").unwrap(),
            r#"
include "config/acl.vcl";
sub vcl_recv {
    if (req.url == "/health") {
        include "config/healthcheck.vcl";
    }

    if (req.http.host == "www.vg.no") {
        include "./www_vg_no_routing.vcl";
    }
}
"#
            .to_string(),
            None,
        );
        let result = doc.get_includes(&Url::parse("file:///etc/varnish/").unwrap(), &vec![]);
        println!("result: {:?}", result);
        assert_eq!(
            result
                .iter()
                .map(|inc| inc.uri.to_string())
                .collect::<Vec<_>>(),
            vec![
                "file:///etc/varnish/config/acl.vcl",
                "file:///etc/varnish/config/healthcheck.vcl",
                "file:///etc/varnish/vg/www_vg_no_routing.vcl",
            ]
        );
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
            None,
        );
        let result = doc.get_all_definitions(&Definitions {
            properties: BTreeMap::from([(
                "directors".to_string(),
                Definition {
                    ident_str: "directors".to_string(),
                    r#type: Box::new(Type::Obj(Obj {
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
                    })),
                    loc: None,
                    nested_pos: None,
                },
            )]),
            ..Default::default()
        });
        println!("result: {:?}", result);
        assert_eq!(result.len(), 2);
    }

    #[test]
    fn backend_with_probe_works() {
        let doc = Document::new(
            Url::parse("file:///test.vcl").unwrap(),
            r#"
probe my_probe {
    .request =
        "GET /healthz HTTP/1.1"
        "Host: localhost"
        "User-Agent: varnish-probe"
        "Connection: close";
    .interval = 1s;
    .timeout = 3.0s;
    .window = 5;
    .threshold = 3;
    .initial = 3;
    .expected_response = 200;
}

backend my_backend {
    .host = "195.88.54.16";
    .port = "80";
    .probe = my_probe;
}
"#
            .to_string(),
            None,
        );

        let mut defs = get_varnish_builtins();
        let doc_defs = doc.get_all_definitions(&defs);
        assert_eq!(doc_defs.len(), 2, "Should return two definitions");
        let mut map: BTreeMap<String, Definition> = BTreeMap::from_iter(
            doc_defs
                .iter()
                .map(|def| (def.ident_str.to_string(), def.clone())),
        );
        defs.properties.append(&mut map);
        let errors = doc.diagnostics(defs, &Default::default());
        assert_eq!(errors.len(), 0, "Should produce no errors");
    }

    #[test]
    fn replace_document_edge_case() {
        let mut doc = Document::new(
            Url::parse("file:///test.vcl").unwrap(),
            "\n".to_string(),
            None,
        );

        // insert second newline
        doc.edit_range(
            1,
            Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 0,
                },
            },
            "\n".to_string(),
        );
        // insert third newline
        doc.edit_range(
            2,
            Range {
                start: Position {
                    line: 1,
                    character: 0,
                },
                end: Position {
                    line: 1,
                    character: 0,
                },
            },
            "\n".to_string(),
        );

        // now, delete everything
        doc.edit_range(
            3,
            Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 3,
                    character: 0,
                },
            },
            "".to_string(),
        );

        // now, our state is empty, but the editor probably has an empty newline

        // insert new newlines, spanning the newline the editor has, but we don't
        doc.edit_range(
            4,
            Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 1,
                    character: 0,
                },
            },
            "\n\n".to_string(),
        );
    }
}
