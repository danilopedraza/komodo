use crate::{
    ast::{ASTNode, ASTNodeKind},
    cst::CSTNode,
    env::Environment,
    error::Error,
    exec::exec,
    lexer::{build_lexer, Token},
    object::Object,
    parser::{parser_from, Parser},
    weeder::rewrite,
};

fn collect_nodes<T: Iterator<Item = Result<Token, Error>>>(
    parser: Parser<T>,
) -> Result<Vec<ASTNode>, Error> {
    let cst_nodes: Result<Vec<CSTNode>, Error> = parser.collect();

    cst_nodes?.into_iter().map(rewrite).collect()
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

pub fn run_node(node: ASTNode, env: &mut Environment) -> Result<Object, Error> {
    exec(&node, env)
}

pub fn import_from(source: &str, values: &[String], env: &mut Environment) -> Result<(), Error> {
    let lexer = build_lexer(source);
    let parser = parser_from(lexer);
    let nodes = collect_nodes(parser)?;

    let mut temp_env = Environment::default();

    for node in nodes {
        match &node.kind {
            ASTNodeKind::Let_ { left: _, right: _ } => {
                run_node(node, &mut temp_env)?;
            }
            ASTNodeKind::ImportFrom {
                source: _,
                values: _,
            } => {
                run_node(node, &mut temp_env)?;
            }
            _ => continue,
        }
    }

    for value in values {
        match temp_env.get(value) {
            Some(obj) => env.set(value, obj.to_owned()),
            None => todo!(),
        }
    }

    Ok(())
}
