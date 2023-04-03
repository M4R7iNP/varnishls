use crate::{
    parser::parser,
    varnish_builtins::{scope_contains_backend, Func, Obj, Type},
};

use tower_lsp::lsp_types::*;

use std::sync::{Arc, Mutex};
use tree_sitter::{InputEdit, Node, Parser, Point, Query, QueryCursor, Tree};
use xi_rope::{rope::Utf16CodeUnitsMetric, Interval, Rope};

#[derive(Clone)]
pub struct Document {
    version: i32,
    parser: Arc<Mutex<Parser>>,
    pub rope: Rope,
    pub ast: Tree,
    pub url: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub ident_str: String,
    pub r#type: Box<Type>,
    pub line_num: usize,
    pub doc_url: Option<String>,
}

pub fn get_node_text(rope: &Rope, node: &Node) -> String {
    if node.kind() == "ident" {
        let start = node.start_byte();
        let end = node.end_byte();
        let slice = rope.slice_to_cow(start..end).to_mut().clone();
        return slice;
    }
    return "".to_string();
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
        let mut node: Option<Node> = None;
        let mut cursor = self.ast.walk();
        loop {
            let idx_opt = cursor.goto_first_child_for_point(point);
            if idx_opt.is_none() {
                break;
            }
            node = Some(cursor.node());
        }

        if node.is_none() {
            return None;
        }

        let node = node.unwrap();
        let name = get_node_text(&self.rope, &node);
        return Some(name);
    }

    pub fn get_definition_by_name(&self, name: String) -> Option<(Point, Point)> {
        let q = Query::new(
            self.ast.language(),
            &format!(
                r#"
                [
                    (toplev_declaration (_ (ident) @ident (#eq? @ident "{}")))
                    (new_stmt (ident) @ident (#eq? @ident "{}"))
                ] @node
                "#,
                name, name
            ),
        )
        .unwrap();
        let mut qc = QueryCursor::new();
        let str = self.rope.to_string();
        let str_bytes = str.as_bytes();
        let all_matches = qc.matches(&q, self.ast.root_node(), str_bytes);
        let capt_idx = q.capture_index_for_name("node").unwrap();

        for each_match in all_matches {
            for capture in each_match.captures.iter().filter(|c| c.index == capt_idx) {
                // TODO: check if parent node is a declaration
                let range = capture.node.range();
                let text = &str[range.start_byte..range.end_byte];
                let line = range.start_point.row;
                let col = range.start_point.column;
                println!(
                    "[Line: {:?}, Col: {:?}] Matching source code: `{:?}`",
                    line, col, text
                );
                return Some((range.start_point, range.end_point));
            }
        }

        return None;
    }

    pub fn get_definition_by_point(&self, point: Point) -> Option<(Point, Point)> {
        let name = self.get_ident_at_point(point)?;
        return self.get_definition_by_name(name);
    }

    pub fn get_error_ranges(&self) -> Option<Vec<(Point, Point)>> {
        let ast = self.ast.clone();
        let error_ranges = get_error_ranges(&ast);

        /*
        let q = Query::new(ast.language(), "(ERROR) @error").unwrap();
        let mut qc = QueryCursor::new();
        let capt_idx = q.capture_index_for_name("error").unwrap();
        let str = self.rope.to_string();
        let str_bytes = str.as_bytes();
        let all_matches = qc.matches(&q, ast.root_node(), str_bytes);


        for each_match in all_matches {
            for capture in each_match.captures.iter().filter(|c| c.index == capt_idx) {
                let range = capture.node.range();
                error_ranges.push((range.start_point, range.end_point))
            }
        }
        */

        return Some(error_ranges);
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        return self
            .get_error_ranges()
            .unwrap()
            .iter()
            .map(|(start, end)| Diagnostic {
                range: Range::new(
                    Position {
                        line: start.row as u32,
                        character: start.column as u32,
                    },
                    Position {
                        line: end.row as u32,
                        character: end.column as u32,
                    },
                ),
                severity: Some(DiagnosticSeverity::ERROR),
                message: "Syntax error".into(),
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

    pub fn get_includes(&self) -> Vec<String> {
        let ast = self.ast.clone();
        let q = Query::new(ast.language(), "(include_declaration (string) @string)").unwrap();
        let mut qc = QueryCursor::new();
        let str = self.rope.to_string();
        let str_bytes = str.as_bytes();
        let all_matches = qc.matches(&q, ast.root_node(), str_bytes);
        let capt_idx = q.capture_index_for_name("string").unwrap();

        let mut include_names: Vec<String> = Vec::new();
        for each_match in all_matches {
            for capture in each_match.captures.iter().filter(|c| c.index == capt_idx) {
                let range = capture.node.range();
                let text = &str[range.start_byte..range.end_byte];
                include_names.push(text.trim_matches('"').to_string());
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

    pub fn get_new_objs<'a>(&self, scope_with_vmods: &'a Type) -> Vec<Definition> {
        let ast = self.ast.clone();
        let q = Query::new(
            ast.language(),
            "(new_stmt ident: (ident) @ident def_right: (ident_call_expr ident: (_) @def_ident)) @node",
        )
        .unwrap();

        let mut qc = QueryCursor::new();
        let str = self.rope.to_string();
        let str_bytes = str.as_bytes();
        let all_matches = qc.matches(&q, ast.root_node(), str_bytes);
        // let node_capt_idx = q.capture_index_for_name("node").unwrap();
        let ident_capt_idx = q.capture_index_for_name("ident").unwrap();
        let def_ident_capt_idx = q.capture_index_for_name("def_ident").unwrap();

        let mut defs: Vec<Definition> = Vec::new();
        for each_match in all_matches {
            println!("match: {:?}", each_match);
            let ident_capture = each_match
                .captures
                .iter()
                .find(|c| c.index == ident_capt_idx)
                .unwrap();
            let def_ident_capture = each_match
                .captures
                .iter()
                .find(|c| c.index == def_ident_capt_idx)
                .unwrap();
            let text_range = ident_capture.node.range();
            let def_text_range = def_ident_capture.node.range();
            let text = &str[text_range.start_byte..text_range.end_byte];
            let def_text = &str[def_text_range.start_byte..def_text_range.end_byte];
            let line_num = ident_capture.node.start_position().row;

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
                doc_url: self.url.clone(),
                r#type: r#type_box,
            });
        }

        defs
    }

    pub fn get_all_definitions(&self) -> Vec<Definition> {
        let ast = self.ast.clone();
        let q = Query::new(
            ast.language(),
            // "[(toplev_declaration (_ (ident) @ident ) @node), (new_stmt (ident) @ident) @node]",
            "(toplev_declaration (_ (ident) @ident ) @node)",
        )
        .unwrap();

        let mut qc = QueryCursor::new();
        let str = self.rope.to_string();
        let str_bytes = str.as_bytes();
        let all_matches = qc.matches(&q, ast.root_node(), str_bytes);
        let node_capt_idx = q.capture_index_for_name("node").unwrap();
        let ident_capt_idx = q.capture_index_for_name("ident").unwrap();

        let mut defs: Vec<Definition> = Vec::new();
        for each_match in all_matches {
            println!("match: {:?}", each_match);
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
                // "new_stmt" => Some(Type::UnresolvedNew),
                _ => None,
            };

            if r#type.is_some() {
                defs.push(Definition {
                    ident_str: text.to_string(),
                    line_num,
                    doc_url: self.url.clone(),
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
        let ast = self.ast.clone();
        let target_point = Point {
            row: pos.line as usize,
            column: pos.character as usize,
        };

        let mut node: Option<Node> = None;
        // let mut ctx_node: Option<Node> = None;
        let mut cursor = ast.walk();
        let mut text = "".to_string();
        let full_text = self.rope.to_string();
        // let mut ctx_text: Option<&str> = None;
        let mut search_type: Option<&Type> = None;
        loop {
            let idx_opt = cursor.goto_first_child_for_point(target_point);
            if idx_opt.is_none() {
                let mut node = cursor.node();

                /*
                println!(
                    "idx_opt.is_none() -> break. ended up at {:?}, search searching for {}",
                    node.range(),
                    target_point
                );
                */

                let parent_node = node.parent()?;
                match parent_node.kind() {
                    "set_stmt" => match cursor.field_name() {
                        Some("operator") => {
                            text = "".to_string();
                            if let Some(ctx_node) = parent_node.child_by_field_name("left") {
                                let ctx_node_str = &full_text[ctx_node.byte_range()];
                                search_type =
                                    lookup_from_scope_by_str(&global_scope, &ctx_node_str);
                            }
                            break;
                        }
                        Some("right") => {
                            text = get_node_text(&self.rope, &node);
                            if let Some(ctx_node) = parent_node.child_by_field_name("left") {
                                let ctx_node_str = &full_text[ctx_node.byte_range()];
                                search_type =
                                    lookup_from_scope_by_str(&global_scope, &ctx_node_str);
                            }
                            break;
                        }
                        _ => {
                            text = full_text[node.byte_range()].to_string();
                        }
                    },
                    "nested_ident" => {
                        node = node.parent().unwrap();
                        text = full_text[node.byte_range()].to_string();
                    }
                    _ => {
                        text = full_text[node.byte_range()].to_string();
                    }
                }
                break;
            }
            let search_node = cursor.node();
            // let field_name = cursor.field_name();

            // println!("walking: {}", search_node.kind());
            // println!("field_name: {:?}", field_name);
            /*
            match search_node.kind() {
                "set_stmt" => {
                    println!("HMMMMMMMMMMMM {:?}", search_node);
                    // break;
                }
                "nested_ident" | "ident" => {
                    node = Some(search_node);
                    text = full_text[search_node.byte_range()].to_string();
                    println!("matched on idents ({}) ({:?})", text, search_node);

                    if field_name.is_some() && field_name.unwrap() == "right" {
                        let left_node = search_node
                            .parent()
                            .unwrap()
                            .child_by_field_name("left")
                            .unwrap();

                        ctx_node = Some(left_node);
                    }

                    break;
                }
                _ => {}
            }
            */
            node = Some(search_node);
        }

        // println!("jaggu: {:?}", search_type);
        if node.is_none() {
            return None;
        }

        let node = node.unwrap();
        // let full_text = self.rope.to_string();
        // text = &full_text[node.start_byte()..node.end_byte()];
        // println!("text: {:?}", text);

        if node.parent()?.kind() == "call_stmt" {
            if let Type::Obj(obj) = global_scope {
                return Some(
                    obj.properties
                        .iter()
                        .filter(|(_ident, r#type)| match r#type {
                            Type::Sub => true,
                            _ => false,
                        })
                        .map(|(ident, _type)| CompletionItem {
                            label: ident.clone(),
                            detail: Some(ident.clone()),
                            kind: Some(CompletionItemKind::FUNCTION),
                            ..Default::default()
                        })
                        .collect(),
                );
            }
        } else {
            // identifiers written so far
            let idents: Vec<&str> = text.split('.').collect(); // split by dot
            let scope = lookup_from_scope(&global_scope, idents[0..idents.len() - 1].to_vec());

            if scope.is_some() {
                // partial match on last identifier written
                let last_part = idents[idents.len() - 1];

                return match scope.unwrap() {
                    Type::Obj(obj) => Some(
                        obj.properties
                            .range(last_part.to_string()..)
                            .take_while(|(key, _v)| key.starts_with(&last_part))
                            .filter(|(_prop_name, property)| {
                                // try to match only backends when autocompleting for e.g. req.backend_hint
                                return if let Some(search_type) = search_type {
                                    match (search_type, property) {
                                        (Type::Backend, Type::Backend) => true,
                                        (Type::Backend, Type::Director) => true,
                                        (Type::Backend, Type::Obj(_)) => {
                                            scope_contains_backend(&property)
                                        }
                                        _ => false,
                                    }
                                } else {
                                    // match on everything
                                    true
                                };
                            })
                            .map(|(prop_name, property)| CompletionItem {
                                label: prop_name.to_string(),
                                // detail: Some(prop_name.to_string()),
                                detail: Some(match property {
                                    Type::Func(func) => {
                                        format!(
                                            "{}{}",
                                            prop_name.to_string(),
                                            func.signature.clone().unwrap_or("()".to_string())
                                        )
                                    }
                                    /*
                                    Type::Backend => {
                                        format!("BACKEND {}", prop_name)
                                    }
                                    */
                                    _ => prop_name.to_string(),
                                }),
                                kind: Some(match property {
                                    Type::Func(_func) => CompletionItemKind::FUNCTION,
                                    Type::Obj(_obj) => CompletionItemKind::STRUCT,
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
        /*} else {
            return Some(vec![CompletionItem {
                label: format!("kind: {}, text: {}", node.kind().to_string(), text),
                detail: Some(node.kind().to_string()),
                kind: Some(CompletionItemKind::PROPERTY),
                ..Default::default()
            }]);
        }*/

        return None;
    }
}

/*
fn edit_range(doc: &Document, version: i32, range: Range, text: String) -> Document {
    let start = position_to_offset(&doc.rope, range.start);
    let end = position_to_offset(&doc.rope, range.end);
    let new_end_byte = start + text.as_bytes().len();

    let mut new_rope = doc.rope.clone();
    let mut new_ast = doc.ast.clone();

    new_rope.edit(Interval { start, end }, text);
    new_ast.edit(&InputEdit {
        start_byte: start,
        old_end_byte: end,
        new_end_byte,
        start_position: offset_to_point(&doc.rope, end),
        old_end_position: offset_to_point(&doc.rope, end),
        new_end_position: offset_to_point(&new_rope, new_end_byte),
    });

    let new_ast = doc
        .parser
        .lock()
        .unwrap()
        .parse_with(
            &mut |offset, _pos| get_chunk(&new_rope, offset),
            Some(&new_ast),
        )
        .unwrap();

    Document {
        version,
        parser: doc.parser.clone(),
        rope: new_rope,
        ast: new_ast,
    }
}

fn edit_fulltext(doc: &Document, version: i32, text: String) -> Document {
    let rope = Rope::from(text.clone());
    let ast = doc.parser.lock().unwrap().parse(&text, None).unwrap();
    Document {
        version,
        parser: doc.parser.clone(),
        rope,
        ast,
    }
}
*/

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

pub fn get_error_ranges(tree: &Tree) -> Vec<(Point, Point)> {
    let mut cursor = tree.walk();
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
        if node.is_error() || node.is_missing() || node.kind() == "incomplete_ident" {
            error_ranges.push((node.start_position(), node.end_position()));
            recurse = false;
        }
    }

    error_ranges
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
        let result = doc.get_all_definitions();
        assert_eq!(result.len(), 3);
    }

    #[test]
    fn nested_identifier_before_if_works() {
        let doc = Document::new(
            r#"
acl a_not_this {}
backend a_match_this {}
sub vcl_recv {
    if (bereq.

    if (true) {}
}
"#
            .to_string(),
        );
        let result = doc.autocomplete_for_pos(
            Position {
                line: 4,
                character: 13,
            },
            get_varnish_builtins(),
        );
        println!("result: {:?}", result);
        assert_eq!(result.unwrap()[0].detail, Some("backend".to_string()));
    }

    #[test]
    fn autocomplete_backend_after_equal_sign() {
        let mut scope = get_varnish_builtins();
        if let Type::Obj(ref mut obj) = scope {
            obj.properties
                .insert("localhost".to_string(), Type::Backend);
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
    }

    /*
    #[test]
    fn autocomplete_backend_def_properties() {
        let doc = Document::new(
            r#"
backend localhost {
    .po
}
"#
            .to_string(),
            get_varnish_builtins()
        );
        let result = doc.autocomplete_for_pos(
            Position {
                line: 2,
                character: 6,
            },
            scope,
        );
        println!("result: {:?}", result);
        let result = result.unwrap();
        assert_eq!(result.first().unwrap().detail, Some("port".to_string()));
        assert_eq!(result.len(), 1);
    }
    */

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
        let result = doc.get_new_objs(&Type::Obj(Obj {
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
        assert_eq!(result.len(), 1);
    }
}
