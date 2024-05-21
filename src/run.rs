use crate::{
    ast::ASTNode,
    builtin::standard_env,
    env::Environment,
    error::Error,
    exec::exec,
    lexer::{build_lexer, Token},
    object::Object,
    parser::{parser_from, Parser},
    weeder::postprocess,
};

fn collect_nodes<T: Iterator<Item = Result<Token, Error>>>(
    parser: Parser<T>,
) -> Result<Vec<ASTNode>, Error> {
    parser.collect()
}

pub fn run(source: &str) -> Result<Object, Error> {
    let lexer = build_lexer(source);
    let parser = parser_from(lexer);
    let nodes = collect_nodes(parser)?;

    let mut env = standard_env();
    let mut obj = Object::empty_tuple();
    for node in nodes {
        obj = run_node(node, &mut env)?;
    }

    Ok(obj)
}

pub fn run_node(node: ASTNode, env: &mut Environment) -> Result<Object, Error> {
    exec(&postprocess(node), env)
}
