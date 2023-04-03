use std::fs;
use varnish_lsp::document::Document;

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
