use crate::parser::{
    ast::{ParseError, Parser, Statement},
    lexer::Lexer,
};
use std::{
    fmt, fs,
    io::{prelude::*, Error},
    path::Path,
};

#[derive(Debug)]
pub struct Module {
    pub identifier: String,
    pub body: Vec<Statement>,
    pub children: Vec<Module>,
}

impl Module {
    pub fn new(name: impl Into<String>) -> Module {
        Module {
            identifier: name.into(),
            body: Vec::new(),
            children: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub enum ModuleError {
    ParseError(ParseError),
    IoError(Error),
    Other(String),
}

impl fmt::Display for ModuleError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ModuleError::ParseError(err) => write!(f, "ParseError: {}", err),
            ModuleError::IoError(err) => write!(f, "IoError: {}", err),
            ModuleError::Other(err) => write!(f, "Other: {}", err),
        }
    }
}

impl std::error::Error for ModuleError {}

type ModuleResult<T> = Result<T, ModuleError>;

impl From<ParseError> for ModuleError {
    fn from(err: ParseError) -> ModuleError {
        ModuleError::ParseError(err)
    }
}

impl From<Error> for ModuleError {
    fn from(err: Error) -> ModuleError {
        ModuleError::IoError(err)
    }
}

const EXTENSION: &'static str = "rsc";

fn parse_source(source: impl AsRef<str>) -> ModuleResult<Vec<Statement>> {
    let lexer = Lexer::new(source.as_ref());
    let mut parser = Parser::new(lexer);
    let body = parser.parse_statements()?;
    Ok(body)
}

fn parse_body(path: impl AsRef<Path>) -> ModuleResult<Vec<Statement>> {
    let path = path.as_ref();
    let mut file = fs::File::open(path)?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    // Parse file
    parse_source(&buf)
}

pub struct ModuleInfo {
    pub name: String,
    pub body: String,
    pub children: Vec<ModuleInfo>,
}

impl ModuleInfo {
    pub fn new(
        name: impl Into<String>,
        body: impl Into<String>,
        children: Vec<ModuleInfo>,
    ) -> ModuleInfo {
        ModuleInfo {
            name: name.into(),
            body: body.into(),
            children,
        }
    }
}

pub fn parse_from_meta(meta: &ModuleInfo) -> ModuleResult<Module> {
    let mut base_module = Module::new(&meta.name);

    // Check if we have a folder or not
    if meta.children.len() > 0 {
        // Folder
        // Find its source in the `mod` child
        for child in &meta.children {
            if &child.name == "mod" {
                let body = parse_source(&child.body)?;
                base_module.body = body;
            } else {
                let child_module = parse_from_meta(child)?;
                base_module.children.push(child_module);
            }
        }
    } else {
        let body = parse_source(&meta.body)?;
        base_module.body = body;
    }

    Ok(base_module)
}

pub fn parse_module(entry_point: impl AsRef<Path>) -> ModuleResult<Module> {
    // Check if directory or file
    let path = entry_point.as_ref();
    let identifier = path
        .file_stem()
        .and_then(|s| s.to_str())
        .map(|s| s.to_string())
        .unwrap();

    if path.is_dir() {
        let mut base_module = Module::new(identifier);
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let child_path = entry.path();
            if let Some(extension) = child_path.extension() {
                if extension == EXTENSION {
                    let stem = child_path.file_stem().unwrap();
                    if stem == "mod" {
                        let body = parse_body(child_path)?;
                        base_module.body = body;
                    } else {
                        let child_module = parse_module(child_path)?;
                        base_module.children.push(child_module);
                    }
                }
            } else if child_path.is_dir() {
                let child_module = parse_module(child_path)?;
                base_module.children.push(child_module);
            }
        }
        Ok(base_module)
    } else {
        let body = parse_body(path)?;

        let sub_module = Module {
            identifier,
            body,
            children: Vec::new(),
        };
        Ok(sub_module)
    }
}
