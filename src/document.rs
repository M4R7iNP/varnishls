use crate::parser::parser;

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
        println!("{}", node.kind());
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
        println!("hei {:?}: {} og {}", node, name, node.kind());

        let q = Query::new(
            ast.language(),
            &format!("(_ (ident) @ident (#eq? @ident \"{}\"))", name),
        )
        .unwrap();
        let mut qc = QueryCursor::new();
        let str = self.rope.to_string();
        let str_bytes = str.as_bytes();
        let all_matches = qc.matches(&q, ast.root_node(), str_bytes);
        let capt_idx = q.capture_index_for_name("ident").unwrap();

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

        // TODO: return None?
        return Some((node.start_position(), node.end_position()));
    }

    /*
    pub fn get_vmod_imports(&self) {
        let ast = self.ast.clone();
        let mut cursor = ast.root_node();
    }
    */
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
