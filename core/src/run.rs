use std::{
    env::{self, VarError},
    fmt::{self, Debug},
    fs,
    path::{Path, PathBuf},
};

use crate::{
    ast::ASTNode,
    builtin::standard_env,
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
    exec(&node, env).map(|(obj, _)| obj)
}

static STDLIB_PATH: &str = "/usr/local/bin/komodo/";
static STDLIB_PATH_VAR: &str = "KOMODO_STD";

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ModuleAddress {
    StandardLibrary { name: String },
    LocalPath { path: PathBuf },
}

impl ModuleAddress {
    pub fn absolute_path(&self, reference_path: PathBuf, stdlib_path: PathBuf) -> PathBuf {
        match self {
            Self::LocalPath { path } if path.is_absolute() => path.to_path_buf(),
            Self::LocalPath { path } => reference_path.join(path),
            Self::StandardLibrary { name } => {
                stdlib_path.join(Path::new(&format!("{name}.komodo")))
            }
        }
    }
}

impl fmt::Display for ModuleAddress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::StandardLibrary { name } => Debug::fmt(&name, f),
            Self::LocalPath { path } => path.fmt(f),
        }
    }
}

fn get_std_path(env_var: Result<String, VarError>) -> PathBuf {
    match env_var {
        Ok(path) if path.is_empty() => Path::new(STDLIB_PATH).to_path_buf(),
        Ok(path) => Path::new(&path).to_path_buf(),
        Err(VarError::NotPresent) => Path::new(STDLIB_PATH).to_path_buf(),
        Err(VarError::NotUnicode(_)) => {
            eprintln!(
                "The environment variable '{STDLIB_PATH_VAR}' is present, but it's not Unicode!"
            );
            eprintln!("Proceeding with the default directory: {STDLIB_PATH}");
            Path::new(STDLIB_PATH).to_path_buf()
        }
    }
}

fn get_module_code(module: &ModuleAddress, reference_path: &Path) -> Result<String, Error> {
    let path = match module {
        ModuleAddress::StandardLibrary { name } => {
            get_std_path(env::var(STDLIB_PATH_VAR)).join(Path::new(&format!("{name}.komodo")))
        }
        ModuleAddress::LocalPath { path } => reference_path.join(Path::new(&path)),
    };

    let source = fs::read_to_string(path)?;
    Ok(source)
}

pub fn import_from(
    module: &ModuleAddress,
    values: &[(String, Position)],
    env: &mut Environment,
) -> Result<(), Error> {
    let ctx = env.ctx();
    let source = get_module_code(module, &ctx.reference_path)?;
    let mut temp_env = standard_env(ctx);
    run(&source, &mut temp_env)?;

    for (value, position) in values {
        match temp_env.get(value) {
            EnvResponse::Mutable((obj, addr), _) => env.set_mutable(value, (obj.to_owned(), addr)),
            EnvResponse::Inmutable((obj, addr), _) => {
                env.set_inmutable(value, (obj.to_owned(), addr))
            }
            EnvResponse::NotFound => {
                let module = module.to_string();
                let symbol = value.to_string();
                return Err(Error::with_position(
                    ImportError::SymbolNotFound { module, symbol }.into(),
                    *position,
                ));
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn import_relative_path() {
        assert_eq!(
            ModuleAddress::LocalPath {
                path: "./foo.komodo".into()
            }
            .absolute_path(
                Path::new("/bar").to_path_buf(),
                Path::new("/std/").to_path_buf(),
            ),
            Path::new("/bar/foo.komodo"),
        );
    }

    #[test]
    fn import_absolute_path() {
        assert_eq!(
            ModuleAddress::LocalPath {
                path: "/foo.komodo".into()
            }
            .absolute_path(
                Path::new("/bar/").to_path_buf(),
                Path::new("/std").to_path_buf(),
            ),
            Path::new("/foo.komodo"),
        );
    }

    #[test]
    fn import_std_module() {
        assert_eq!(
            ModuleAddress::StandardLibrary {
                name: "utils".into()
            }
            .absolute_path(
                Path::new("/bar/").to_path_buf(),
                Path::new("/std/").to_path_buf(),
            ),
            Path::new("/std/utils.komodo"),
        );
    }
}
