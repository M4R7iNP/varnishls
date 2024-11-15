use tree_sitter::Parser;
use tree_sitter_vcl;
use tree_sitter_vtc;

pub fn vcl() -> Parser {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_vcl::LANGUAGE.into())
        .unwrap();
    parser
}

pub fn vtc() -> Parser {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_vtc::LANGUAGE.into())
        .unwrap();
    parser
}
