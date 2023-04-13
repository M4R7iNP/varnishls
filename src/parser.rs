use tree_sitter::{Parser, Tree};
use tree_sitter_vcl;
use tree_sitter_vtc;

pub fn parse(src: &str) -> Tree {
    let mut parser = Parser::new();
    let language = tree_sitter_vcl::language();
    parser.set_language(language).unwrap();
    let ast = parser.parse(&src, None).unwrap();
    let root_node = ast.root_node();
    println!("{:?}", root_node);
    println!("AST: {:?}", ast);
    return ast;
}

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
