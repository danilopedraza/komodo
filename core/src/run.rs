use std::{fmt, fs, path::Path};

use crate::{
    ast::{ASTNode, ASTNodeKind},
    cst::CSTNode,
    env::{EnvResponse, Environment},
    error::{Error, Position},
    exec::exec,
    lexer::{Lexer, Token},
    object::Object,
    parser::Parser,
    weeder::rewrite,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ImportError {
    SymbolNotFound { module: String, symbol: String },
}

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

static STDLIB_PATH: &str = "../std/";

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ModuleAddress {
    StandardLibrary { name: String },
    LocalPath { path: String },
}

impl fmt::Display for ModuleAddress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::StandardLibrary { name } => name.fmt(f),
            Self::LocalPath { path } => path.fmt(f),
        }
    }
}

fn get_module_code(module: &ModuleAddress, env: &Environment) -> Result<String, Error> {
    let reference_path = &env.ctx.reference_path;

    let path = match module {
        ModuleAddress::StandardLibrary { name } => {
            Path::new(STDLIB_PATH).join(Path::new(&format!("{name}.komodo")))
        }
        ModuleAddress::LocalPath { path } => reference_path.join(Path::new(&path)),
    };

    let source = fs::read_to_string(path).unwrap();
    Ok(source)
}

pub fn import_from(
    module: &ModuleAddress,
    values: &[(String, Position)],
    env: &mut Environment,
) -> Result<(), Error> {
    let source = get_module_code(module, env)?;
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

    for (value, position) in values {
        match temp_env.get(value) {
            EnvResponse::Mutable(obj) => env.set_mutable(value, obj.to_owned()),
            EnvResponse::Inmutable(obj) => env.set_inmutable(value, obj.to_owned()),
            EnvResponse::NotFound => {
                let module = module.to_string();
                let symbol = value.to_string();
                return Err(Error::new(
                    ImportError::SymbolNotFound { module, symbol }.into(),
                    *position,
                ));
            }
        }
    }

    Ok(())
}
