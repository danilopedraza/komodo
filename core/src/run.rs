use crate::{
    cst::CSTNode,
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
) -> Result<Vec<CSTNode>, Error> {
    parser.collect()
}

pub fn run(source: &str, env: &mut Environment) -> Result<(), Error> {
    let lexer = build_lexer(source);
    let parser = parser_from(lexer);
    let nodes = collect_nodes(parser)?;

    for node in nodes {
        run_node(node, env)?;
    }

    Ok(())
}

pub fn run_node(node: CSTNode, env: &mut Environment) -> Result<Object, Error> {
    exec(&postprocess(node), env)
}
