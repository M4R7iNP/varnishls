use tree_sitter::Parser;
use tree_sitter_vcl;
use tree_sitter_vtc;

pub fn vcl() -> Parser {
    let language = tree_sitter_vcl::language();
    let mut parser = Parser::new();
    parser.set_language(language).unwrap();
    parser
}

pub fn vtc() -> Parser {
    let language = tree_sitter_vtc::language();
    let mut parser = Parser::new();
    parser.set_language(language).unwrap();
    parser
}
