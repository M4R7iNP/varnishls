use crate::{
    parser::parser,
    varnish_builtins::{self, Type},
};

use tower_lsp::lsp_types::*;

use std::{
    collections::hash_map::HashMap,
    sync::{Arc, Mutex},
};
use tree_sitter::{InputEdit, Node, Parser, Point, Query, QueryCursor, Tree};
use xi_rope::{rope::Utf16CodeUnitsMetric, Interval, Rope};

#[derive(Clone)]
pub struct Document {
    version: i32,
    parser: Arc<Mutex<Parser>>,
    pub rope: Rope,
    pub ast: Tree,
}

pub fn get_node_text(rope: &Rope, node: &Node) -> String {
    let mut cursor = node.walk();
    let mut text = String::new();
    let mut depth = 0;
    let mut recurse = true;

    /*
    while depth >= 0 {
        if recurse && cursor.goto_first_child() {
            recurse = true;
            depth += 1;
        } else if depth > 0 && cursor.goto_next_sibling() {
            recurse = true;
        } else if depth > 0 && cursor.goto_parent() {
            recurse = false;
            depth -= 1;
            continue;
        } else {
            break;
        }
        */

    let node = cursor.node();
    if node.kind() == "ident" {
        let start = node.start_byte();
        let end = node.end_byte();
        // let length = end - start;
        let slice = rope.slice_to_cow(start..end).to_mut().clone();
        return slice;
    }
    // }
    return text;
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
        }
    }

    pub fn version(&self) -> i32 {
        self.version
    }

    pub fn edit(
        &self,
        version: i32,
        edits: impl Iterator<Item = (Option<Range>, String)>,
    ) -> Document {
        edits.fold(self.clone(), |doc, (range, text)| match range {
            Some(range) => edit_range(&doc, version, range, text),
            None => edit_fulltext(&doc, version, text),
        })
    }

    pub fn get_definition(&self, point: Point) -> Option<(Point, Point)> {
        let ast = self.ast.clone();
        let mut node: Option<Node> = None;
        let mut cursor = ast.walk();
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

        let q = Query::new(
            ast.language(),
            &format!(
                r#"
                [
                    (toplev_declaration (_ (ident) @ident (#eq? @ident \"{}\")))
                    (new_stmt (ident) @ident (#eq? @ident \"{}\"))
                ] @node
                "#,
                name, name
            ),
        )
        .unwrap();
        let mut qc = QueryCursor::new();
        let str = self.rope.to_string();
        let str_bytes = str.as_bytes();
        let all_matches = qc.matches(&q, ast.root_node(), str_bytes);
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

    /**
     * Expand identifiers into req, res etc. and their properties.
     */
    pub fn autocomplete_for_pos(&self, pos: Position) -> Option<Vec<CompletionItem>> {
        let ast = self.ast.clone();
        let target_point = Point {
            row: pos.line as usize,
            column: pos.character as usize,
        };

        let mut node: Option<Node> = None;
        let mut cursor = ast.walk();
        loop {
            let idx_opt = cursor.goto_first_child_for_point(target_point);
            if idx_opt.is_none() {
                break;
            }
            let search_node = cursor.node();
            match search_node.kind() {
                "nested_ident" | "ident" => {
                    node = Some(search_node);
                    break;
                }
                _ => {}
            }
            node = Some(search_node);
        }

        if node.is_none() {
            return None;
        }

        let node = node.unwrap();
        let text = &self.rope.to_string()[node.start_byte()..node.end_byte()];

        if node.parent()?.kind() == "call_stmt" {
            return Some(
                self.get_subroutines()
                    .iter()
                    .map(|sub_name| CompletionItem {
                        label: sub_name.clone(),
                        detail: Some(sub_name.clone()),
                        kind: Some(CompletionItemKind::FUNCTION),
                        ..Default::default()
                    })
                    .collect(),
            );
        } else if node.kind() == "ident" || node.kind() == "nested_ident" {
            let parts: Vec<&str> = text.split('.').collect(); // split by dot
            let global = varnish_builtins::get_varnish_builtins();
            let mut scope = Some(&global);
            for part in parts.as_slice()[0..parts.len() - 1].iter() {
                let scope_unwrapped = scope.unwrap();
                let new_scope: Option<&Type> = match scope_unwrapped {
                    Type::Obj(obj) => obj.properties.get(*part),
                    _ => None,
                };
                if new_scope.is_some() {
                    scope = match new_scope.unwrap() {
                        Type::Obj(_obj) => new_scope,
                        _ => None,
                    };
                }

                if scope.is_none() {
                    break;
                }
            }

            if scope.is_some() {
                let last_part = parts[parts.len() - 1];
                return match scope.unwrap() {
                    Type::Obj(obj) => Some(
                        obj.properties
                            .keys()
                            .filter(|alt| alt.starts_with(&last_part))
                            .map(|str| str.to_string())
                            .map(|property| CompletionItem {
                                label: property.clone(),
                                detail: Some(property.clone()),
                                kind: Some(CompletionItemKind::PROPERTY),
                                ..Default::default()
                            })
                            .collect(),
                    ),
                    _ => return None,
                };
            }
        }

        return None;
    }
}

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

fn offset_to_position(rope: &Rope, offset: usize) -> Position {
    let row = rope.line_of_offset(offset);
    let column = offset - rope.offset_of_line(row);
    Position::new(row as u32, column as u32)
}

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
        if node.is_error() || node.is_missing() {
            error_ranges.push((node.start_position(), node.end_position()));
            recurse = false;
        }
    }

    error_ranges
}

#[cfg(test)]
mod tests {
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
            .autocomplete_for_pos(Position {
                line: 2,
                character: 12,
            })
            .unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].label, "http");
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
            .autocomplete_for_pos(Position {
                line: 2,
                character: 11,
            })
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
}
