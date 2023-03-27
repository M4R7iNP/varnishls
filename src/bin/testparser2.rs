use tree_sitter::Point;
use vcl_parser::parser;
use vcl_parser::document::Document;
use std::fs;

pub fn main() {
    let src = fs::read_to_string("/home/martin/vcl-parser/test.vcl").expect("Could not read VCL");
    let doc = Document::new(src);
    // println!("{:?}", doc);
    /*
    println!("{:?}", doc.get_definition(Point {
        row: 54,
        column: 14,
    }));
    */
    println!("{:?}", doc.get_error_ranges());
    // parser::parse(&src);
}
