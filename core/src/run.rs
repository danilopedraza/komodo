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
    env::{EnvResponse, Environment, ExecContext},
    error::{Error, Position},
    exec::exec,
    lexer::{Lexer, Token},
    object::Object,
    parser::Parser,
    std::math::komodo_math,
    weeder::{rewrite, WeederError},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ImportError {
    SymbolNotFound { module: String, symbol: String },
}

fn collect_nodes<T: Iterator<Item = Result<Token, Error>>>(
    parser: Parser<T>,
) -> Result<Vec<ASTNode>, Error> {
    let path = parser.path();
    let cst_nodes: Result<Vec<CSTNode>, Error> = parser.collect();

    let res: Result<Vec<ASTNode>, (WeederError, Position)> =
        cst_nodes?.into_iter().map(rewrite).collect();

    match res {
        Err((err, pos)) => Err(Error::with_position(err.into(), pos, path)),
        Ok(val) => Ok(val),
    }
}

pub fn run(source: &str, env: &mut Environment) -> Result<(), Error> {
    let lexer = Lexer::from((source, env.file_path()));
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

static STDLIB_PATH: &str = "/usr/local/lib/komodo/";
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

fn get_module_code(path: &Path) -> Result<String, Error> {
    let source = fs::read_to_string(path)?;
    Ok(source)
}

fn module_env(path: &PathBuf, env: &Environment) -> Result<Environment, Error> {
    let reference_path = env.ctx().reference_path;
    let abs_path = reference_path.join(Path::new(path));
    let source = get_module_code(&abs_path)?;
    let mut temp_env = standard_env(ExecContext::new(abs_path, reference_path));
    run(&source, &mut temp_env)?;

    Ok(temp_env)
}

fn get_module_env(module: &ModuleAddress, env: &Environment) -> Result<Environment, Error> {
    match module {
        ModuleAddress::LocalPath { path } => module_env(path, env),
        ModuleAddress::StandardLibrary { name } => match name.as_str() {
            "math" => Ok(komodo_math(env.ctx())),
            name => {
                let path = get_std_path(env::var(STDLIB_PATH_VAR))
                    .join(Path::new(&format!("{name}.komodo")));
                module_env(&path, env)
            }
        },
    }
}

pub fn import_from(
    module: &ModuleAddress,
    values: &[(String, Position)],
    env: &mut Environment,
) -> Result<(), Error> {
    let mut temp_env = get_module_env(module, env)?;

    for (value, position) in values {
        match temp_env.get(value) {
            EnvResponse::Mutable((obj, addr)) => env.set_mutable(value, (obj.to_owned(), addr)),
            EnvResponse::Inmutable((obj, addr)) | EnvResponse::MutableOriginally((obj, addr)) => {
                env.set_inmutable(value, (obj.to_owned(), addr))
            }
            EnvResponse::NotFound => {
                let module = module.to_string();
                let symbol = value.to_string();
                return Err(Error::with_position(
                    ImportError::SymbolNotFound { module, symbol }.into(),
                    *position,
                    env.file_path(),
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
