use crate::parser::{
    ast::{BinaryOp, Expression, Parameter, Statement, UnaryOp, Visibility},
    resolver::{Module, ModuleError},
};
use std::collections::HashMap;

#[derive(Debug)]
pub enum CompileError {
    ModuleError(ModuleError),
    Message(String),
}
type CompileResult<T> = Result<T, CompileError>;

impl From<ModuleError> for CompileError {
    fn from(err: ModuleError) -> CompileError {
        CompileError::ModuleError(err)
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CompileError::ModuleError(err) => write!(f, "{}", err),
            CompileError::Message(s) => write!(f, "{}", s),
        }
    }
}

impl std::error::Error for CompileError {}

pub struct Compiler {
    cur_symbol: u32,
    cur_module: Vec<String>,
    symbols: HashMap<String, u32>,
    root: &'static str,
}

impl Compiler {
    pub fn new(root: &'static str) -> Compiler {
        Compiler {
            cur_symbol: 0,
            cur_module: Vec::new(),
            symbols: HashMap::new(),
            root
        }
    }

    fn join_paths(&self, prefix: &[String], suffix: &[String]) -> String {
        let full = [prefix, suffix].concat();
        format!("{}.{}", self.root, full.join("."))
    }

    fn compile_identifier(&self, identifier: &[String]) -> String {
        if identifier.len() == 1 {
            identifier[0].to_string()
        } else {
            self.join_paths(&self.cur_module, identifier)
        }
    }

    fn compile_binary_op(&self, op: &BinaryOp) -> CompileResult<String> {
        use BinaryOp::*;
        Ok(match op {
            Plus => "+",
            Minus => "-",
            Times => "*",
            Divide => "/",
            Mod => "%",
            Equal => "===",
            NotEqual => "!==",
            GT => ">",
            GTE => ">=",
            LT => "<",
            LTE => "<=",
            And => "&",
            Or => "|",
            DoubleAnd => "&&",
            DoubleOr => "||",
            LShift => "<<",
            RShift => ">>",
            Xor => "^",
        }
        .to_string())
    }

    fn compile_primative(&self, prim: &Expression) -> CompileResult<String> {
        use Expression::*;
        match prim {
            Int(i) => Ok(i.to_string()),
            Float(f) => Ok(f.to_string()),
            Boolean(b) => Ok(if *b {
                "true".to_string()
            } else {
                "false".to_string()
            }),
            String(s) => Ok(format!("`{}`", s)),
            other => Err(CompileError::Message(format!(
                "{:?} is not a primitive value",
                other
            ))),
        }
    }

    fn compile_expression(&mut self, expr: &Expression) -> CompileResult<String> {
        use Expression::*;
        match expr {
            Tuple(exprs) if exprs.is_empty() => Ok("undefined".to_string()),
            Tuple(exprs) => {
                let compiled: Vec<_> = exprs
                    .into_iter()
                    .map(|expr| self.compile_expression(expr))
                    .collect::<Result<_, _>>()?;

                let inner = compiled.join(",");

                Ok(format!("[{}]", inner))
            }
            List(exprs) => {
                let compiled: Vec<_> = exprs
                    .into_iter()
                    .map(|expr| self.compile_expression(expr))
                    .collect::<Result<_, _>>()?;

                let inner = compiled.join(",");

                Ok(format!("[{}]", inner))
            }
            Identifier(path) => {
                let symbol = self.compile_identifier(path);
                Ok(format!("eval({})", symbol))
            }
            Member(expr, identifier) => {
                let expr = self.compile_expression(expr)?;
                Ok(format!("({}[{}])", expr, identifier))
            }
            Call(func, args) => {
                let func = self.compile_expression(func.as_ref())?;
                let args: Result<Vec<_>, _> = args
                    .into_iter()
                    .map(|arg| self.compile_expression(arg))
                    .collect();
                let args = args?;
                let args = args.join(",");
                Ok(format!("{}({})", func, args))
            }
            Binary(op, lhs, rhs) => {
                let op = self.compile_binary_op(op)?;
                let lhs = self.compile_expression(lhs)?;
                let rhs = self.compile_expression(rhs)?;
                Ok(format!("({}{}{})", lhs, op, rhs))
            }
            Block(statements, value) => {
                // foo
                let statements: Result<Vec<_>, _> = statements
                    .into_iter()
                    .map(|stmt| self.compile_statement(stmt))
                    .collect();
                let statements = statements?;
                let statements = statements.join("");
                let value = self.compile_expression(value)?;
                Ok(format!("(()=>{{{}return {};}})()", statements, value))
            }
            If(cond, then, otherwise) => {
                let cond = self.compile_expression(cond.as_ref())?;
                let then = self.compile_expression(then)?;
                let otherwise = self.compile_expression(otherwise)?;
                Ok(format!("({}?{}:{})", cond, then, otherwise))
            }
            Index(expr, index) => {
                let expr = self.compile_expression(expr)?;
                let index = self.compile_expression(index)?;
                Ok(format!("({}[{}])", expr, index))
            }
            other => self.compile_primative(other),
        }
    }

    pub fn compile_statement(&mut self, stmt: &Statement) -> CompileResult<String> {
        match stmt {
            Statement::FunctionDeclaration {
                visibility,
                identifier,
                parameters,
                body,
                ..
            } => {
                let identifier = self.compile_identifier(&[identifier.to_string()]);
                let params: Vec<_> = parameters
                    .into_iter()
                    .map(|Parameter { identifier, .. }| identifier.clone())
                    .collect();
                let params = params.join(",");
                let body = self.compile_expression(body)?;
                let mut code = format!("const {}=({})=>{};", identifier, params, body);
                if let Visibility::Public = visibility {
                    let export = format!("this.{}={};", identifier, identifier);
                    code.push_str(&export);
                }
                Ok(code)
            }
            Statement::Expression(expr) => {
                let expr = self.compile_expression(expr)?;
                Ok(format!("{};", expr))
            }
            Statement::Assignment {
                visibility,
                mutable,
                parameter,
                value,
            } => {
                let value = self.compile_expression(value)?;
                let ident = &parameter.identifier;
                let mut code = if *mutable {
                    format!("let {}={};", ident, value)
                } else {
                    format!("const {}={};", ident, value)
                };
                let export = format!("this.{}={};", ident, ident);
                Ok(code)
            }
            Statement::Reassignment { location, value } => {
                let location = self.compile_expression(location)?;
                let value = self.compile_expression(value)?;
                Ok(format!("{}={};", location, value))
            }
            Statement::Import { path, alias } => Ok(format!(
                "const {}=\"{}\";",
                alias,
                self.compile_identifier(path)
            )),
            Statement::TypeAlias { .. } => Ok("".to_string()),
            // _ => Err(CompileError::Message(format!(
            //     "{:?} is not a valid statement",
            //     stmt
            // ))),
        }
    }

    pub fn compile_module(&mut self, module: &Module) -> CompileResult<String> {
        // Compile current module
        let statements: Result<Vec<_>, _> = module
            .body
            .iter()
            .map(|stmt| self.compile_statement(stmt))
            .collect();
        let statements = statements?;
        let mut statements = statements.join("");

        // Compile each child module
        for child in &module.children {
            let mod_name = &child.identifier;
            let child_module = self.compile_module(child)?;
            let code = format!("this.{}={}", mod_name, child_module);
            statements.push_str(&code);
        }

        Ok(format!("(new(function(){{{}}})());", statements))
    }
}
