use tree_sitter::{Parser, Tree};
use tree_sitter_vcl;

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

pub fn parser() -> Parser {
    let language = tree_sitter_vcl::language();
    let mut parser = Parser::new();
    parser.set_language(language).unwrap();
    parser
}
