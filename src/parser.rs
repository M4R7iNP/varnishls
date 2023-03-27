use tree_sitter::{Language, Parser, Tree};

extern "C" {
    fn tree_sitter_vcl() -> Language;
}

pub fn parse(src: &str) -> Tree {
    let mut parser = Parser::new();
    let language = unsafe { tree_sitter_vcl() };
    parser.set_language(language).unwrap();
    let ast = parser.parse(&src, None).unwrap();
    let root_node = ast.root_node();
    println!("{:?}", root_node);
    println!("AST: {:?}", ast);
    return ast;
}

pub fn parser() -> Parser {
    let language = unsafe { tree_sitter_vcl() };
    let mut parser = Parser::new();
    parser.set_language(language).unwrap();
    parser
}
