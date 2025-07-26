use crate::{
    config::LintConfig,
    parser,
    safe_regex::{SafeRegexError, is_regex_safe},
    static_autocomplete_items,
    varnish_builtins::{
        self, AutocompleteSearchOptions, Definition, Definitions, HasTypeProperties, Type,
        get_backend_field_types, get_probe_field_types,
    },
};

use log::{debug, error};
use ropey::{Rope, iter::Chunks};
use serde::{Deserialize, Serialize};
use std::sync::{Arc, Mutex};
use std::{cmp::Ordering, iter::Iterator, path::PathBuf};
use streaming_iterator::{StreamingIterator, convert as convert_to_streaming_iterator};
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
    pub path: Arc<PathBuf>,
    pub filetype: FileType,
    pub pos_from_main_doc: NestedPos,
}

#[derive(Debug, Clone)]
pub struct Include {
    pub url: Option<Url>,
    pub path: PathBuf,
    pub nested_pos: NestedPos,
}

unsafe impl Send for Include {}
unsafe impl Sync for Include {}

#[derive(Debug, Clone)]
pub struct VmodImport {
    pub name: String,
    pub loc: Location,
    pub nested_pos: NestedPos,
}

#[derive(Debug, Clone)]
pub struct Reference {
    pub ident_str: String,
    pub line_num: usize,
    pub doc_path: Arc<PathBuf>,
    pub uri: Location,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum LintErrorInternalType {
    PreferElseIf = 1,
    PreferLowercaseHeader = 2,
}

#[derive(Debug)]
pub struct LintError {
    pub message: String,
    pub severity: Option<DiagnosticSeverity>,
    pub loc: Location,
    pub data: Option<DiagnosticData>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiagnosticData {
    pub r#type: LintErrorInternalType,
    pub quickfix_label: String,
    pub replace_with: String,
}

pub type NestedPos = Vec<(usize, usize)>;

// Reserved keywords: words you can't name e.g. a backend, subroutine etc.
const RESERVED_KEYWORDS: &[&str] = &[
    "if", "set", "new", "call", "else", "elsif", "unset", "include", "return", "sub", "acl",
    "backend",
    // "probe", // FIXME: probe is a valid field name
    // "req", "resp", "bereq", "beresp", "client", "server", // FIXME: actually valid identifier
];

// Sorted most to least important works in vscode
pub const LEGEND_TYPES: &[SemanticTokenType] = &[
    // SemanticTokenType::new("delimiter"),
    SemanticTokenType::NUMBER,
    SemanticTokenType::REGEXP,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::NUMBER,
    SemanticTokenType::COMMENT,
    SemanticTokenType::STRING,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::VARIABLE,
];

pub const LEGEND_MODIFIERS: &[SemanticTokenModifier] = &[
    SemanticTokenModifier::DEFAULT_LIBRARY,
    SemanticTokenModifier::DECLARATION,
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
            path: Arc::new(url.to_file_path().unwrap()),
            url,
            filetype,
            pos_from_main_doc: nested_pos.unwrap_or_default(),
        }
    }

    pub fn version(&self) -> i32 {
        self.version
    }

    pub fn edit(&mut self, _version: i32, edits: impl Iterator<Item = (Option<Range>, String)>) {
        for (range, text) in edits {
            match range {
                Some(range) => {
                    self.edit_range(range, text);
                }
                None => {
                    self.edit_fulltext(text);
                }
            }
        }
    }

    fn edit_range(&mut self, range: Range, text: String) {
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
            .parse_with_options(
                &mut |offset, _pos| {
                    let (chunk, chunk_byte_idx, _, _) = self.rope.chunk_at_byte(offset);
                    &chunk.as_bytes()[(offset - chunk_byte_idx)..]
                },
                Some(&new_ast),
                None,
            )
            .unwrap();

        // self.rope = new_rope;
        self.ast = new_new_ast;
    }

    pub fn edit_fulltext(&mut self, text: String) {
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

    /// get identifier at point
    pub fn get_ident_at_point_full(&self, point: Point) -> Option<String> {
        let node = self
            .ast
            .root_node()
            .descendant_for_point_range(point, point)?;
        let name = get_node_text(&self.rope, &node);
        Some(name)
    }

    pub fn get_definition_by_name(&self, name: &str) -> Option<(Point, Point)> {
        let name_escaped = name.replace('"', "\\\"");
        let q = Query::new(
            &self.ast.language(),
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
            log::error!("Failed exec query for goto definition: {err}");
            return None;
        }
        let q = q.unwrap();
        let mut qc = QueryCursor::new();
        let mut all_matches = qc.matches(&q, self.ast.root_node(), self);
        let capt_idx = q.capture_index_for_name("node").unwrap();

        while let Some(each_match) = all_matches.next() {
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
        global_scope: &Definitions,
        config: &LintConfig,
    ) -> Vec<LintError> {
        debug!("get_error_ranges start");
        let mut cursor = self.ast.walk();
        let mut error_ranges = Vec::new();
        let mut recurse = true;
        struct VarnishlsIgnore {
            pub line: usize,
        }
        let mut varnishls_ignore: Option<VarnishlsIgnore> = None;
        let mut toplev_node = self.ast.root_node();
        let mut is_at_top = true;

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
            if node.kind() == "toplev_declaration" {
                is_at_top = true;
            } else if is_at_top {
                toplev_node = node;
                is_at_top = false;
            }

            macro_rules! get_location {
                (node: $node:expr) => {
                    Location {
                        uri: self.url.to_owned(),
                        range: ts_range_to_lsp_range($node.range()),
                    }
                };
                () => {
                    get_location(node: node)
                }
            }

            macro_rules! add_error {
                (node: $node:expr, severity: $severity:expr, $($arg:tt)+) => {
                    let severity = $severity;
                    if (severity.is_some()) {
                        error_ranges.push(LintError {
                            message: format!($($arg)+),
                            loc: get_location!(node: $node),
                            severity,
                            data: None,
                        });
                    }
                };
                (node: $node:expr, $($arg:tt)+) => {
                    add_error!(node: $node, severity: Some(DiagnosticSeverity::ERROR), $($arg)+);
                };
                ($($arg:tt)+) => {
                    add_error!(node: node, severity: Some(DiagnosticSeverity::ERROR), $($arg)+);
                }
            }

            macro_rules! add_hint {
                (node: $node:expr, $($arg:tt)+) => {
                    add_error!(node: $node, severity: Some(DiagnosticSeverity::HINT), $($arg)+);
                };
                ($($arg:tt)+) => {
                    add_error!(node: node, severity: Some(DiagnosticSeverity::HINT), $($arg)+);
                }
            }

            // skip this node if ignored
            if let Some(ignore) = varnishls_ignore.as_ref() {
                match ignore.line.cmp(&node.start_position().row) {
                    Ordering::Less => {
                        varnishls_ignore = None;
                    }
                    Ordering::Equal => continue,
                    Ordering::Greater => unreachable!(),
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

                    if !matches!(left_node.kind(), "ident" | "nested_ident") {
                        continue;
                    }

                    let left_ident_text = get_node_text(&self.rope, &left_node);

                    let left_parts = left_ident_text.split('.').collect::<Vec<_>>();
                    let first_left_part = left_parts[0].to_lowercase();
                    if ["req", "bereq", "resp", "beresp", "obj"].contains(&first_left_part.as_str())
                    {
                        let is_http = left_parts.get(1).is_some_and(|ident| *ident == "http");

                        // Deprecated headers (from MDN)
                        const DEPRECATED_HEADERS: &[&str] = &[
                            "content-dpr",
                            "dnt",
                            "dpr",
                            "large-allocation",
                            "pragma",
                            "sec-ch-ua-full-version",
                            "tk",
                            "viewport-width",
                            "width",
                        ];
                        if is_http
                            && left_parts
                                .get(2)
                                .is_some_and(|left_part_2| DEPRECATED_HEADERS.contains(left_part_2))
                        {
                            let hdr = left_parts[2];
                            add_hint!(node: left_node, "Deprecated header {hdr}");
                        }

                        // Hit-for-pass is usually better than no cache at all
                        if left_parts.get(1) == Some(&"ttl") {
                            let right_text = get_node_text(&self.rope, &right_node);
                            if matches!(right_text.as_str(), "0s" | "0") {
                                add_hint!(
                                    "Consider using {first_left_part}.cacheable = false instead"
                                );
                            }
                        }

                        // Vary on header with many arbitrary values
                        if is_http && left_parts.get(2) == Some(&"vary") {
                            let right_text = get_node_text(&self.rope, &right_node);
                            const BAD_VARY_VALUES: &[&str] = &[
                                "\"*\"",
                                "\"user-agent\"",
                                "\"accept-encoding\"",
                                "\"accept-language\"",
                            ];
                            if BAD_VARY_VALUES.contains(&right_text.to_lowercase().as_str()) {
                                add_hint!("Consider filtering header before vary");
                            }
                        }

                        // Rewriting req.url also changes the cache key, so requests hitting that
                        // cache key might get a different response that intended.
                        if config.no_rewrite_req_url.is_enabled()
                            && left_ident_text.as_str() == "req.url"
                            && toplev_node.kind() == "sub_declaration"
                            && let Some(ident_node) = toplev_node.child_by_field_name("ident")
                            && get_node_text(&self.rope, &ident_node) == "vcl_recv"
                        {
                            add_error!(
                                node: node,
                                severity: config.no_rewrite_req_url.lsp_severity(),
                                "[no_rewrite_req_url] Don't rewrite req.url. Rather, make a consious decision whether to edit the cache key or just the backend url."
                            );
                        }

                        if config.prefer_lowercase_headers.is_enabled()
                            && is_http
                            && left_parts
                                .get(2)
                                .and_then(|header_name| header_name.find(char::is_uppercase))
                                .is_some()
                        {
                            error_ranges.push(LintError {
                                message: "Prefer lowercase headers".into(),
                                loc: get_location!(node: left_node),
                                severity: config.prefer_lowercase_headers.lsp_severity(),
                                data: Some(DiagnosticData {
                                    r#type: LintErrorInternalType::PreferLowercaseHeader,
                                    quickfix_label: "Downcase".into(),
                                    replace_with: get_node_text(&self.rope, &left_node)
                                        .to_lowercase(),
                                }),
                            });
                        }

                        if config.prefer_custom_headers_without_prefix.is_enabled() && is_http {
                            if let Some(hdr) = left_parts.get(2) {
                                let hdr = hdr.to_lowercase();
                                if hdr.starts_with("x-") && !hdr.starts_with("x-forwarded-") {
                                    add_error!(
                                        node: node,
                                        severity: config.prefer_custom_headers_without_prefix.lsp_severity(),
                                        "Prefer custom headers without the «X-»-prefix"
                                    );
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

                    let Some(right_node) = node.child_by_field_name("right") else {
                        add_error!("Missing backend property value");
                        continue;
                    };

                    // only request can contain multiline strings
                    if right_node.kind() == "string_list" && left_ident != "request" {
                        add_error!("Property {left_ident} cannot contain string list");
                    }

                    let parent_parent_node_kind = node.parent().unwrap().kind();
                    let map = match parent_parent_node_kind {
                        "probe_declaration" | "inline_probe" => get_probe_field_types(),
                        _ => get_backend_field_types(),
                    };

                    let Some(r#type) = map.get(left_ident.as_str()) else {
                        add_error!("Backend property «{}» does not exist", left_ident);
                        continue;
                    };

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
                "elsif_stmt" => {
                    let Some(keyword_node) = node.child_by_field_name("keyword") else {
                        error!("could not find keyword for elsif_stmt");
                        continue;
                    };

                    let keyword = self.rope.byte_slice(keyword_node.byte_range()).to_string();
                    if config.prefer_else_if.is_enabled() && keyword != "else if" {
                        error_ranges.push(LintError {
                            message: "Prefer «else if»".into(),
                            loc: get_location!(node: keyword_node),
                            severity: config.prefer_else_if.lsp_severity(),
                            data: Some(DiagnosticData {
                                r#type: LintErrorInternalType::PreferElseIf,
                                quickfix_label: format!(
                                    "Replace {} with else if",
                                    get_node_text(&self.rope, &keyword_node)
                                ),
                                replace_with: "else if".into(),
                            }),
                        });
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
                        add_error!(node: ident_node, "{} is undefined", ident_parts[0]);
                        continue;
                    };

                    // check it is defined above current line
                    let mut call_nested_pos = self.pos_from_main_doc.clone();
                    call_nested_pos.push(point_to_tuple(node.start_position()));

                    if definition.nested_pos.gt(&call_nested_pos) {
                        let line = definition.loc.as_ref().unwrap().range.start.line;
                        let filename = definition
                            .loc
                            .as_ref()
                            .and_then(|loc| {
                                loc.uri
                                    .path_segments()
                                    .and_then(|mut path_segments| path_segments.next_back())
                            })
                            .unwrap_or("<UNKNOWN>");

                        add_error!(
                            "{} is defined in {} at line {}",
                            ident_parts[0],
                            filename,
                            line + 1
                        );
                    }

                    // check method exists (init of brotli.init())
                    let Some(Type::Func(func)) =
                        global_scope.get_type_property_by_nested_idents(ident_parts)
                    else {
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
                        debug!("missing_arg: {missing_arg:?}");
                        let mut message = "Missing required argument".to_string();
                        if let Some(ref missing_arg_name) = missing_arg.name {
                            message.push(' ');
                            message.push_str(missing_arg_name);
                        }
                        add_error!("{message}");
                    }

                    // check the arguments exists and that their provided type is correct
                    let mut arg_idx = 0;
                    for arg_node in arg_nodes {
                        let arg;
                        let arg_value_node;
                        if arg_node.kind() == "func_call_named_arg" {
                            let Some(arg_name_node) = arg_node.child_by_field_name("arg_name")
                            else {
                                continue;
                            };
                            let Some(_arg_value_node) = arg_node.child_by_field_name("arg_value")
                            else {
                                add_error!(node: arg_node, "Expected value");
                                continue;
                            };
                            arg_value_node = _arg_value_node;
                            let arg_name =
                                &self.rope.byte_slice(arg_name_node.byte_range()).to_string();
                            let Some(_arg) = func.args.iter().find(|arg| {
                                arg.name
                                    .as_ref()
                                    .map(|name| name.eq(arg_name.as_str()))
                                    .unwrap_or(false)
                            }) else {
                                add_error!(node: arg_node, "No such argument named {arg_name}");
                                continue;
                            };
                            arg = _arg;
                        } else {
                            arg = {
                                let Some(found_arg) = &func.args.get(arg_idx) else {
                                    add_error!(node: arg_node, "Extraneous argument");
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
                                let Some(arg_value_type) =
                                    node_to_type(&arg_value_node).or_else(|| {
                                        let ident = get_node_text(&self.rope, &arg_value_node);
                                        let ident_parts = ident.split('.').collect::<Vec<_>>();
                                        global_scope
                                            .get_type_property_by_nested_idents(ident_parts.clone())
                                            .cloned()
                                    })
                                else {
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

                        if let Some(restricted) = func.restricted.as_ref() {
                            if toplev_node.kind() == "sub_declaration" {
                                if let Some(ident_node) = toplev_node.child_by_field_name("ident") {
                                    let sub_name = &*get_node_text(&self.rope, &ident_node);
                                    if sub_name.starts_with("vcl_") {
                                        let mut search = vec![sub_name];
                                        let mapping = [
                                            ("vcl_recv", "client"),
                                            ("vcl_deliver", "client"),
                                            ("vcl_backend_fetch", "backend"),
                                            ("vcl_backend_response", "backend"),
                                            ("vcl_init", "housekeeping"),
                                            ("vcl_fini", "housekeeping"),
                                        ];

                                        mapping
                                            .iter()
                                            .filter(|(search_sub_name, _)| {
                                                search_sub_name == &sub_name
                                            })
                                            .for_each(|(_, alias)| search.push(alias));

                                        if !search
                                            .iter()
                                            .any(|search| restricted.contains(&search.to_string()))
                                        {
                                            add_error!(node: node, "Cannnot be called from {sub_name}");
                                        }
                                    }
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
                    if toplev_node.kind() == "sub_declaration" {
                        if let Some(ident_node) = toplev_node.child_by_field_name("ident") {
                            let sub_name = &*get_node_text(&self.rope, &ident_node);
                            let parts = text.split('.').collect::<Vec<&str>>();
                            if sub_name.starts_with("vcl_") {
                                let exists_in_sub = match parts[0] {
                                    "req" | "req_top" => matches!(
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

                        if node.parent().is_some_and(|parent_node| {
                            matches!(
                                parent_node.kind(),
                                "set_stmt"
                                    | "binary_expression"
                                    | "parenthesized_expression"
                                    | "neg_expr"
                            )
                        }) {
                            let ident = get_node_text(&self.rope, &node);
                            let ident_parts = ident.split('.').collect::<Vec<_>>();
                            if global_scope
                                .get_type_property_by_nested_idents(ident_parts.clone())
                                .is_none()
                            {
                                add_error!(node: node, "Not found");
                                continue;
                            }
                        }
                    }
                }
                "rmatch" | "nmatch" => {
                    if config.invalid_regex.is_enabled() {
                        // right-hand side is the regex
                        let re_node = node
                            .parent()
                            .unwrap()
                            .parent()
                            .unwrap()
                            .child_by_field_name("right");
                        if let Some(re_node) = re_node {
                            let re_str = get_node_text(&self.rope, &re_node);
                            match is_regex_safe(re_str) {
                                Err(SafeRegexError::ParseError) => {
                                    add_error!(
                                        node: re_node,
                                        severity: config.invalid_regex.lsp_severity(),
                                        "Regex might be invalid"
                                    );
                                }
                                Err(SafeRegexError::StarHeightError) => {
                                    add_error!(
                                        node: re_node,
                                        severity: config.slow_regex.lsp_severity(),
                                        "Regex might be exponentially slow"
                                    );
                                }
                                Err(SafeRegexError::TooManyRepititions) => {
                                    add_error!(
                                        node: re_node,
                                        severity: config.slow_regex.lsp_severity(),
                                        "Regex might be slow (too many repititions)"
                                    );
                                }
                                _ => {}
                            }
                        }
                    }
                }
                "COMMENT" => {
                    let content = get_node_text(&self.rope, &node);
                    if content.contains("varnishls-ignore-next-line") {
                        varnishls_ignore = Some(VarnishlsIgnore {
                            line: node.end_position().row + 1,
                        });
                    }
                }
                "vtc_statement" | "vtc_block_statement" => {
                    let start_line_idx = node.start_position().row;
                    let end_line_idx = node.end_position().row;
                    if start_line_idx != end_line_idx {
                        // make sure newlines are escaped (except in blocks)
                        let text = self.rope.byte_slice(node.byte_range()).to_string();
                        let chars = text.chars();
                        let mut block_depth = 0;
                        let mut prev_char = '\0';
                        for (idx, char) in chars.enumerate() {
                            if char == '{' {
                                block_depth += 1;
                            }
                            if char == '}' {
                                block_depth -= 1;
                            }
                            if block_depth == 0 && idx > 0 && char == '\n' && prev_char != '\\' {
                                add_error!("Unescaped newline");
                            }
                            prev_char = char;
                        }
                    }
                }
                _ => {}
            }
        }

        debug!("get_error_ranges done!");
        error_ranges
    }

    pub fn diagnostics(
        &self,
        global_scope: Definitions,
        lint_config: &LintConfig,
    ) -> Vec<Diagnostic> {
        if !lint_config.enabled {
            return vec![];
        }

        self.get_error_ranges(&global_scope, lint_config)
            .iter()
            .map(|lint_error| Diagnostic {
                range: lint_error.loc.range,
                severity: lint_error.severity,
                message: lint_error.message.to_owned(),
                data: lint_error
                    .data
                    .as_ref()
                    .and_then(|data| serde_json::to_value(data).ok()),
                ..Diagnostic::default()
            })
            .collect()
    }

    pub fn get_vmod_imports(&self) -> Vec<VmodImport> {
        let q = Query::new(&self.ast.language(), "(import_declaration (ident) @ident)").unwrap();
        let mut qc = QueryCursor::new();
        let mut all_matches = qc.matches(&q, self.ast.root_node(), self);
        let capt_idx = q.capture_index_for_name("ident").unwrap();

        let mut imports = Vec::new();
        while let Some(each_match) = all_matches.next() {
            for capture in each_match.captures.iter().filter(|c| c.index == capt_idx) {
                let ts_range = capture.node.range();
                let name = get_node_text(&self.rope, &capture.node).to_string();
                let range = ts_range_to_lsp_range(ts_range);
                let mut nested_pos = self.pos_from_main_doc.clone();
                nested_pos.push(point_to_tuple(ts_range.start_point));
                imports.push(VmodImport {
                    name,
                    loc: Location {
                        uri: self.url.to_owned(),
                        range,
                    },
                    nested_pos,
                });
            }
        }

        imports
    }

    pub fn get_includes(&self) -> Vec<Include> {
        let q = Query::new(
            &self.ast.language(),
            "(include_declaration (string) @string)",
        )
        .unwrap();
        let mut qc = QueryCursor::new();
        let mut all_matches = qc.matches(&q, self.ast.root_node(), self);
        let capt_idx = q.capture_index_for_name("string").unwrap();

        let mut includes = Vec::new();
        while let Some(each_match) = all_matches.next() {
            for capture in each_match.captures.iter().filter(|c| c.index == capt_idx) {
                let range = capture.node.range();
                let text = get_node_text(&self.rope, &capture.node);
                let path_str = text.trim_matches('"');
                let path = PathBuf::from(path_str);
                let url = if path.starts_with("./") || path.starts_with("../") {
                    // relative to current vcl
                    self.url.join(path_str).ok()
                } else {
                    None
                };

                let mut nested_pos = self.pos_from_main_doc.clone();
                nested_pos.push(point_to_tuple(range.start_point));
                includes.push(Include {
                    url,
                    path,
                    nested_pos,
                });
            }
        }

        includes
    }

    pub fn to_include(&self) -> Include {
        Include {
            url: Some(self.url.to_owned()),
            path: (*self.path).to_owned(),
            nested_pos: self.pos_from_main_doc.to_owned(),
        }
    }

    pub fn get_subroutines(&self) -> Vec<String> {
        let q = Query::new(&self.ast.language(), "(sub_declaration (ident) @ident)").unwrap();
        let mut qc = QueryCursor::new();
        let mut all_matches = qc.matches(&q, self.ast.root_node(), self);
        let capt_idx = q.capture_index_for_name("ident").unwrap();

        let mut import_names: Vec<String> = Vec::new();
        while let Some(each_match) = all_matches.next() {
            for capture in each_match.captures.iter().filter(|c| c.index == capt_idx) {
                let text = get_node_text(&self.rope, &capture.node).to_string();
                import_names.push(text);
            }
        }

        import_names
    }

    pub fn get_all_definitions(&self, scope_with_vmods: &Definitions) -> Vec<Definition> {
        let q = Query::new(
            &self.ast.language(),
            r#"
                (toplev_declaration (_ (ident) @ident ) @node)
                (new_stmt ident: (ident) @ident def_right: (ident_call_expr ident: (_) @def_ident)) @node
            "#,
        );
        if let Err(err) = q {
            debug!("Error: {err}");
            return vec![];
        }
        let q = q.unwrap();

        let mut qc = QueryCursor::new();
        let mut all_matches = qc.matches(&q, self.ast.root_node(), self);
        let node_capt_idx = q.capture_index_for_name("node").unwrap();
        let ident_capt_idx = q.capture_index_for_name("ident").unwrap();
        let def_ident_capt_idx = q.capture_index_for_name("def_ident").unwrap();

        let mut defs: Vec<Definition> = Vec::new();
        while let Some(each_match) = all_matches.next() {
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
                        .get_type_property_by_nested_idents(def_text.split('.').collect())
                    else {
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

            let mut nested_pos = self.pos_from_main_doc.clone();
            nested_pos.push(point_to_tuple(node_capture.node.start_position()));

            defs.push(Definition {
                ident_str: text.to_string(),
                r#type,
                loc: Some(loc),
                nested_pos,
            });
        }

        defs
    }

    pub fn get_references_for_ident(&self, ident: &str) -> Vec<Reference> {
        let escaped_regex_ident = regex_syntax::escape(ident);
        let q = Query::new(
            &self.ast.language(),
            &format!(
                r#"
                [
                    ((ident) @ident (#match? @ident "^(?i){escaped_regex_ident}$"))
                    ((nested_ident) @nested_ident (#match? @nested_ident "^(?i){escaped_regex_ident}\\b"))
                ]
                "#
            ),
        );
        if let Err(err) = q {
            debug!("Error: {err}");
            return vec![];
        }
        let q = q.unwrap();

        let mut qc = QueryCursor::new();
        let mut all_matches = qc.matches(&q, self.ast.root_node(), self);
        let ident_capt_idx = q.capture_index_for_name("ident").unwrap();
        let nested_ident_capt_idx = q.capture_index_for_name("nested_ident").unwrap();
        let mut refs: Vec<Reference> = Vec::new();
        while let Some(each_match) = all_matches.next() {
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
                doc_path: self.path.clone(),
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

        debug!("target point: {target_point}");

        let mut text = "".to_string();
        let mut search_type: Option<Type> = None;
        let mut must_be_writable = false;
        let mut keyword_suggestions = vec![];

        let mut cursor = self.ast.root_node().walk();
        // Walk down the ast tree to the narrowest node (then up again later.)
        while cursor.goto_first_child_for_point(target_point).is_some() {
            let node = cursor.node();
            let range = node.range();
            if range.start_point > target_point || range.end_point < target_point {
                cursor.goto_parent();
                break;
            }
        }

        // Move the cursor one node back if comma or end parenthesis
        if matches!(cursor.node().kind(), "," | ")") {
            // move cursor one point back
            if let Some(prev_sibling) = cursor.node().prev_sibling() {
                if prev_sibling.kind() != "(" {
                    debug!("prev_sibling: {prev_sibling:?}");
                    cursor.reset(prev_sibling);
                }
            }
        }

        // Walk up the tree from the narrowest node
        let mut stop = false;
        while !stop {
            let node = cursor.node();
            debug!("visiting node {node:?}");

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
                    debug!("got text for nested_ident {node:?} {text}");
                }
                "ident" if node.parent().unwrap().kind() != "nested_ident" => {
                    text = get_node_text(&self.rope, &node).to_string();
                    debug!("got text for ident {node:?} {text}");
                }
                "sub_declaration" | "if_stmt" | "elsif_stmt" | "else_stmt" => {
                    if !text.contains('.') {
                        keyword_suggestions.append(&mut static_autocomplete_items::subroutine());
                    }
                    stop = true;
                }
                "source_file" => {
                    return Some(static_autocomplete_items::source_file());
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

        debug!("text: «{text}»");

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
                    Type::Func(_func) => format!("{property}"),
                    Type::Backend => format!("BACKEND {prop_name}"),
                    _ => format!("{property} {prop_name}"),
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
                        format!("{prop_name}({args_str})")
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
        let q = Query::new(&self.ast.language(), query_str.as_str()).unwrap();
        let mut qc = QueryCursor::new();

        let names = q.capture_names();

        #[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
        struct Token {
            line: usize,
            start: usize,
            token_type: usize, // LEGEND_TYPES is ordered, so we can order by this for precendence
            modifier_bitset: usize,
            length: usize,
        }

        let mut tokens = vec![];
        let mut captures = qc
            .matches(&q, node, self)
            .flat_map(|m| convert_to_streaming_iterator(m.captures));
        let mut prev_node: Option<Node> = None;
        while let Some(c) = captures.next() {
            let node = c.node;
            let range = node.range();
            let mut capture_name = names.get(c.index as usize).unwrap().splitn(2, '.');
            let name = capture_name.next().unwrap();
            let modifier_name = capture_name.next();
            let Some(token_type) = LEGEND_TYPES.iter().position(|s| s.as_str() == name) else {
                continue;
            };
            let modifier = modifier_name.and_then(|modifier_name| {
                LEGEND_MODIFIERS
                    .iter()
                    .position(|s| s.as_str() == modifier_name)
            });

            let mut modifier_bitset = 0;
            if let Some(modifier) = modifier {
                modifier_bitset |= 1 << modifier;
            }

            let is_multiline = range.start_point.row != range.end_point.row;
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

                if !is_multiline && prev_node.is_some_and(|prev_node| prev_node == node) {
                    let prev_token: &mut Token = tokens.last_mut().unwrap();
                    if prev_token.token_type == token_type {
                        prev_token.modifier_bitset |= modifier_bitset;
                        continue;
                    }
                }

                tokens.push(Token {
                    start,
                    line: row,
                    length: end - start,
                    token_type,
                    modifier_bitset,
                });
                prev_node = Some(node);
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
                    token_modifiers_bitset: tok.modifier_bitset as u32,
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
impl<'a> TextProvider<&'a [u8]> for &'a Document {
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

impl Include {
    pub fn resolve(mut self, vcl_paths: &[PathBuf]) -> Self {
        if self.url.is_none() {
            self.url = vcl_paths.iter().find_map(|search_path| {
                let path = search_path.join(&self.path);
                if path.exists() {
                    Url::from_file_path(path).ok()
                } else {
                    None
                }
            });
        }
        self
    }
}

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
        "bytes" => Some(Type::Bytes),
        "bool" => Some(Type::Bool),
        "inline_probe" => Some(Type::Probe),
        "literal" => node_to_type(&node.child(0)?),
        _ => None,
    }
}

fn point_to_tuple(point: Point) -> (usize, usize) {
    (point.row, point.column)
}

fn find_parent(node: Node, kind: String) -> Option<Node> {
    let mut node = node;
    loop {
        let parent_node = node.parent()?;
        node = parent_node;
        if node.kind() == kind {
            return Some(node);
        }
    }
}

#[cfg(test)]
mod tests {
    use insta::{assert_snapshot, glob};
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
        println!("result: {result:?}");
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
        println!("imports: {vmod_names:?}");
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
        println!("subroutines: {result:?}");
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
        println!("result: {result:?}");
        assert_eq!(
            result.unwrap()[0].detail,
            Some("BACKEND backend_hint".to_string())
        );
    }

    #[ignore]
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
        println!("result: {result:?}");
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
        println!("result: {result:?}");
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
        let includes = doc.get_includes();
        println!("includes: {includes:?}");
        assert_eq!(
            includes
                .iter()
                .map(|inc| inc.path.to_string_lossy())
                .collect::<Vec<_>>(),
            vec![
                "config/acl.vcl",
                "config/healthcheck.vcl",
                "./www_vg_no_routing.vcl",
            ]
        );

        /*
        let includes = includes
            .into_iter()
            .map(|include| include.resolve(&vec!["/etc/varnish/".into()].into()))
            .collect::<Vec<_>>();
        */

        assert_eq!(
            includes
                .iter()
                .map(|inc| inc.url.as_ref().map(|url| url.to_string()))
                .collect::<Vec<_>>(),
            vec![
                None, // Some("file:///etc/varnish/acl.vcl".to_string()),
                None, // Some("file:///etc/varnish/healthcheck.vcl".to_string()),
                Some("file:///etc/varnish/vg/www_vg_no_routing.vcl".to_string()),
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
                    nested_pos: Default::default(),
                },
            )]),
        });
        println!("result: {result:?}");
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
            Range::new(Position::new(0, 0), Position::new(0, 0)),
            "\n".to_string(),
        );

        assert_eq!(doc.rope.to_string(), "\n\n");

        // insert third newline
        doc.edit_range(
            Range::new(Position::new(1, 0), Position::new(1, 0)),
            "\n".to_string(),
        );

        assert_eq!(doc.rope.to_string(), "\n\n\n");

        // now, delete everything
        doc.edit_range(
            Range::new(Position::new(0, 0), Position::new(3, 0)),
            "".to_string(),
        );
        // now, our state is empty, but the editor probably has an empty newline

        assert_eq!(doc.rope.to_string(), "");

        // insert new newlines, spanning the newline the editor has, but we don't
        doc.edit_range(
            Range::new(Position::new(0, 0), Position::new(1, 0)),
            "\n\n".to_string(),
        );

        assert_eq!(doc.rope.to_string(), "\n\n");
    }

    #[test]
    fn lint_tests() {
        glob!("lint_tests/*.vcl", |input_path| {
            let doc = Document::new(
                Url::parse("file:///test.vcl").unwrap(),
                std::fs::read_to_string(input_path).unwrap(),
                None,
            );

            let defs = get_varnish_builtins();
            let errors = doc.diagnostics(defs, &Default::default());
            let errors_json = serde_json::to_string_pretty(&errors).unwrap();
            assert_snapshot!(errors_json);
        });
    }
}
