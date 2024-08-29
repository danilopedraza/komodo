use std::{fs, path::Path};

use crate::{
    ast::{ASTNode, ASTNodeKind},
    cst::CSTNode,
    env::{EnvResponse, Environment, ExecContext},
    error::Error,
    exec::exec,
    lexer::{Lexer, Token},
    object::Object,
    parser::Parser,
    weeder::rewrite,
};

fn collect_nodes<T: Iterator<Item = Result<Token, Error>>>(
    parser: Parser<T>,
) -> Result<Vec<ASTNode>, Error> {
    let cst_nodes: Result<Vec<CSTNode>, Error> = parser.collect();

    cst_nodes?.into_iter().map(rewrite).collect()
}

pub fn run(source: &str, env: &mut Environment) -> Result<(), Error> {
    let lexer = Lexer::from(source);
    let parser = Parser::from(lexer);
    let nodes = collect_nodes(parser)?;

    for node in nodes {
        run_node(node, env)?;
    }

    Ok(())
}

pub fn run_node(node: ASTNode, env: &mut Environment) -> Result<Object, Error> {
    exec(&node, env)
}

fn is_std_module(module_name: &str) -> bool {
    module_name == "utils"
}

static STDLIB_PATH: &str = "../std/";

fn get_module_code(module_name: &str, env: &Environment) -> Result<String, Error> {
    match &env.ctx {
        ExecContext::File { reference_path } => {
            let path = if is_std_module(module_name) {
                Path::new(STDLIB_PATH).join(Path::new(&format!("{module_name}.smtc")))
            } else {
                reference_path.join(Path::new(&format!("{module_name}.smtc")))
            };
            println!("{}", path.to_str().unwrap());
            let source = fs::read_to_string(path).unwrap();
            Ok(source)
        }
        ExecContext::Repl => todo!(),
    }
}

pub fn import_from(
    module_name: &str,
    values: &[String],
    env: &mut Environment,
) -> Result<(), Error> {
    let source = get_module_code(module_name, env)?;
    let lexer = Lexer::from(source.as_str());
    let parser = Parser::from(lexer);
    let nodes = collect_nodes(parser)?;

    let mut temp_env = Environment::default();

    for node in nodes {
        match &node.kind {
            ASTNodeKind::Declaration { .. } | ASTNodeKind::ImportFrom { .. } => {
                run_node(node, &mut temp_env)?;
            }
            _ => continue,
        }
    }

    for value in values {
        match temp_env.get(value) {
            EnvResponse::Mutable(obj) => env.set_mutable(value, obj.to_owned()),
            EnvResponse::Inmutable(obj) => env.set_inmutable(value, obj.to_owned()),
            EnvResponse::NotFound => todo!(),
        }
    }

    Ok(())
}
