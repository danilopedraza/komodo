use std::fs;

use crate::{
    ast::ASTNode,
    builtin::standard_env,
    error::Error,
    exec::exec,
    lexer::{build_lexer, Token},
    parser::{parser_from, Parser},
    weeder::postprocess,
};

fn collect_nodes<T: Iterator<Item = Result<Token, Error>>>(
    parser: Parser<T>,
) -> Result<Vec<ASTNode>, Error> {
    parser.collect()
}

pub fn run(path: &str) -> Result<(), Error> {
    let input = fs::read_to_string(path).unwrap();
    let lexer = build_lexer(&input);
    let parser = parser_from(lexer);
    let nodes = collect_nodes(parser)?;

    let mut env = standard_env();
    for node in nodes {
        exec(&postprocess(node), &mut env)?;
    }

    Ok(())
}
