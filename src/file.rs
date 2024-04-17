use std::fs;

use crate::ast::ASTNode;
use crate::lexer::build_lexer;
use crate::parser::parser_from;

pub fn parse_file(path: &str) -> Vec<ASTNode> {
    let input = fs::read_to_string(path).unwrap();
    let lexer = build_lexer(&input);
    let mut parser = parser_from(lexer.map(|res| res.unwrap().token));

    parser.program()
}
