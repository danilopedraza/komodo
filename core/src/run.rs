use std::{
    cell::RefCell,
    collections::HashMap,
    env::{self, VarError},
    fmt::{self, Debug},
    fs,
    path::{Path, PathBuf},
    rc::Rc,
};

use crate::{
    ast::ASTNode,
    builtin::standard_env,
    env::{EnvResponse, Environment, ExecContext},
    error::{Error, Position},
    exec::exec,
    lexer::{Lexer, Token},
    object::Object,
    parser::Parser,
    std::{json::komodo_json, math::komodo_math, time::komodo_time},
    typecheck::{self, elaborate},
    weeder::rewrite,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ImportError {
    SymbolNotFound { module: String, symbol: String },
}

pub fn collect_nodes<T: Iterator<Item = Result<Token, Error>>>(
    parser: Parser<T>,
) -> Result<Vec<ASTNode>, Error> {
    let path = parser.path();
    parser
        .map(|node| {
            rewrite(node?).map_err(|(err, pos)| Error::with_position(err.into(), pos, path.clone()))
        })
        .collect()
}

pub fn run(source: &str, env: &mut Environment) -> Result<(), Error> {
    let nodes = collect_nodes(Parser::from(Lexer::from((source, env.file_path()))))?;
    run_nodes(nodes, env)
}

fn run_nodes(
    nodes: impl IntoIterator<Item = impl std::borrow::Borrow<ASTNode>>,
    env: &mut Environment,
) -> Result<(), Error> {
    let mut typecheck_env = typecheck::Environment::std_env(env.ctx());
    // Lower each checked node immediately, releasing its syntax trees. Execute
    // only after the entire program has passed validation.
    let blocks = nodes
        .into_iter()
        .map(|node| {
            elaborate(std::borrow::Borrow::borrow(&node), &mut typecheck_env)
                .map(|typed| crate::ir::lower(&typed))
                .map_err(|(err, pos)| Error::with_position(err.into(), pos, env.file_path()))
        })
        .collect::<Result<Vec<_>, _>>()?;
    for block in blocks {
        exec(&block, env)?;
    }
    Ok(())
}

pub fn run_node(node: crate::typed_ast::ASTNode, env: &mut Environment) -> Result<Object, Error> {
    exec(&crate::ir::lower(&node), env)
}

pub static STDLIB_PATH: &str = "/usr/local/lib/komodo/";
pub static STDLIB_PATH_VAR: &str = "KOMODO_STD";

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

pub fn get_std_path(env_var: Result<String, VarError>) -> PathBuf {
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

// Bound retention independently of how many files a long-running REPL opens.
// Compare contents, not timestamps: same-size edits and dependency changes must
// remain visible. Only parsing is cached; inference and execution stay fresh.
struct ParsedModule {
    source: String,
    nodes: Rc<[ASTNode]>,
}
thread_local! {
    static MODULES: RefCell<HashMap<PathBuf, ParsedModule>> = RefCell::default();
}

pub(crate) fn module_nodes(path: &Path) -> Result<Rc<[ASTNode]>, Error> {
    let source = fs::read_to_string(path)?;
    MODULES.with(|cache| {
        let mut cache = cache.borrow_mut();
        if let Some(entry) = cache.get(path) {
            if entry.source == source {
                return Ok(entry.nodes.clone());
            }
        }
        let nodes: Rc<[ASTNode]> = collect_nodes(Parser::from(Lexer::from((
            source.as_str(),
            path.to_path_buf(),
        ))))?
        .into();
        if cache.len() >= 64 {
            cache.clear();
        }
        cache.insert(
            path.to_path_buf(),
            ParsedModule {
                source,
                nodes: nodes.clone(),
            },
        );
        Ok(nodes)
    })
}

fn module_env(path: &PathBuf, env: &Environment) -> Result<Environment, Error> {
    let reference_path = env.ctx().reference_path;
    let abs_path = reference_path.join(Path::new(path));
    let nodes = module_nodes(&abs_path)?;
    let mut temp_env = standard_env(ExecContext::new(abs_path, reference_path));
    run_nodes(nodes.iter(), &mut temp_env)?;

    Ok(temp_env)
}

fn get_module_env(module: &ModuleAddress, env: &Environment) -> Result<Environment, Error> {
    match module {
        ModuleAddress::LocalPath { path } => module_env(path, env),
        ModuleAddress::StandardLibrary { name } => match name.as_str() {
            "collections" => Ok(crate::std::collections::environment(env.ctx())),
            "json" => Ok(komodo_json(env.ctx())),
            "math" => Ok(komodo_math(env.ctx())),
            "time" => Ok(komodo_time(env.ctx())),
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
            EnvResponse::Mutable(obj) => env.set_mutable(value, obj.to_owned()),
            EnvResponse::Inmutable(obj) | EnvResponse::MutableOriginally(obj) => {
                env.set_inmutable(value, obj.to_owned())
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
    fn parsed_modules_share_and_invalidate_by_contents() {
        let path =
            std::env::temp_dir().join(format!("komodo-module-cache-{}.komodo", std::process::id()));
        fs::write(&path, "let value := 1").unwrap();
        let first = module_nodes(&path).unwrap();
        let second = module_nodes(&path).unwrap();
        assert!(Rc::ptr_eq(&first, &second));
        // Equal length edits must invalidate even on coarse timestamp filesystems.
        fs::write(&path, "let value := 2").unwrap();
        let changed = module_nodes(&path).unwrap();
        assert!(!Rc::ptr_eq(&first, &changed));
        assert_ne!(first, changed);
        fs::write(&path, "let =").unwrap();
        assert!(module_nodes(&path).is_err());
        fs::remove_file(&path).unwrap();
        assert!(module_nodes(&path).is_err());
    }

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
