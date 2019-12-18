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
    cur_module: Vec<String>,
    symbols: Vec<HashMap<String, String>>,
    root: &'static str,
}

struct Import {
    getter: String,
    js_value: String,
}

impl Compiler {
    pub fn new(root: &'static str) -> Compiler {
        Compiler {
            cur_module: Vec::new(),
            symbols: Vec::new(),
            root,
        }
    }

    fn lookup_symbol<'a, 'b>(&'a self, key: &'b str) -> Option<&'a String> {
        self.symbols
            .iter()
            .rev()
            .filter(|map| map.contains_key(key))
            .next()
            .and_then(|map| map.get(key))
    }

    fn insert_symbol(&mut self, key: String, value: String) {
        self.symbols
            .iter_mut()
            .rev()
            .next()
            .map(|map| map.insert(key, value));
    }

    fn process_imports(&mut self, statements: &[Statement]) -> CompileResult<Vec<Statement>> {
        let mut others = Vec::new();

        for statement in statements {
            match statement {
                Statement::Import { path, alias } => {
                    // foo
                    // Compile the object we will use
                    let identifier = self.compile_identifier(path);

                    // Insert into our symbol table
                    self.insert_symbol(alias.clone(), identifier);
                }
                other => others.push(other.clone()),
            }
        }
        Ok(others)
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

    fn compile_identifier_expression(&self, identifier: &[String]) -> String {
        let ident = self.compile_identifier(identifier);

        self.lookup_symbol(&ident).cloned().unwrap_or_else(|| ident)
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
            String(s) => {
                let mut buf = std::string::String::new();
                for ch in s.chars() {
                    match ch {
                        '`' => {
                            buf.push_str("\\`");
                        }
                        ch => buf.push(ch),
                    }
                }
                Ok(format!("`{}`", buf))
            }
            other => Err(CompileError::Message(format!(
                "{:?} is not a primitive value",
                other
            ))),
        }
    }

    fn compile_location(&mut self, location: &Expression) -> CompileResult<String> {
        use Expression::*;
        match location {
            Member(value, member) => {
                let value = self.compile_expression(value)?;
                Ok(format!("{}[\"{}\"]", value, member))
            }
            Index(value, index) => {
                let value = self.compile_expression(value)?;
                let index = self.compile_expression(index)?;
                Ok(format!("{}[{}]", value, index))
            }
            Identifier(path) => {
                let path = self.compile_identifier(path);
                Ok(path)
            }
            _ => unimplemented!(),
        }
    }

    fn compile_reassignment(
        &mut self,
        location: &Expression,
        value: &Expression,
    ) -> CompileResult<String> {
        let location = self.compile_location(location)?;
        let value = self.compile_expression(value)?;
        Ok(format!("{}={};", location, value))
    }

    fn compile_statements(&mut self, statements: &[Statement]) -> CompileResult<String> {
        // Add new level to symbols table
        self.symbols.push(HashMap::new());

        // Split statements into imports and non imports
        let statements = self.process_imports(statements)?;

        // Compile remaining statements
        let statements: Vec<_> = statements
            .iter()
            .map(|statement| self.compile_statement(statement))
            .collect::<Result<_, _>>()?;
        let statements_str = statements.join("");

        // Remove level from symbols table
        self.symbols.pop();

        Ok(statements_str)
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
            Identifier(path) => Ok(self.compile_identifier_expression(path)),
            Member(expr, identifier) => {
                let expr = self.compile_expression(expr)?;
                Ok(format!("({}[\"{}\"])", expr, identifier))
            }
            Call(func, args) => {
                let func = self.compile_expression(func.as_ref())?;

                // // Check if we are compiling a curried function
                // let mut curried_arg_indices = Vec::new();
                // for (index, arg) in args.into_iter().enumerate() {
                //     match arg {
                //         Expression::Identifier(path) => match &path[..] {
                //             [param] if param == "_" => {
                //                 curried_arg_indices.push(index);
                //             }
                //             _ => (),
                //         },
                //         _ => (),
                //     }
                // }

                // if !curried_arg_indices.is_empty() {
                //     let mut compiled_args: Vec<std::string::String> = Vec::new();
                //     for index in 0..args.len() {
                //         if curried_arg_indices.contains(&index) {
                //             compiled_args.push(format!("__arg_{}", index));
                //         } else {
                //             let compiled_arg = self.compile_expression(&args[index])?;
                //             compiled_args.push(compiled_arg);
                //         }
                //     }
                //     let compiled_args = compiled_args.join(",");

                //     let params: Vec<_> = curried_arg_indices
                //         .iter()
                //         .map(|index| format!("__arg_{}", index))
                //         .collect();
                //     let params = params.join(",");

                //     let compiled = format!("(({})=>{}({}))", params, func, compiled_args);
                //     Ok(compiled)
                // } else {
                let args: Vec<_> = args
                    .into_iter()
                    .map(|arg| self.compile_expression(arg))
                    .collect::<Result<_, _>>()?;
                let args = args.join(",");
                Ok(format!("({})({})", func, args))
                // }
            }
            Binary(op, lhs, rhs) => {
                let op = self.compile_binary_op(op)?;
                let lhs = self.compile_expression(lhs)?;
                let rhs = self.compile_expression(rhs)?;
                Ok(format!("({}{}{})", lhs, op, rhs))
            }
            Block(statements, value) => {
                // foo
                let statements = self.compile_statements(statements)?;
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
            Lambda(params, body) => {
                // Compile params
                let params: Vec<_> = params
                    .into_iter()
                    .map(|param| param.identifier.clone())
                    .collect();
                let params = params.join(",");
                let body = self.compile_expression(body)?;

                Ok(format!("({})=>{}", params, body))
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
                    .map(|identifier| identifier.clone())
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
                let ident = &parameter;
                let mut code = if *mutable {
                    format!("let {}={};", ident, value)
                } else {
                    format!("const {}={};", ident, value)
                };
                let export = format!("this.{}={};", ident, ident);
                Ok(code)
            }
            Statement::Reassignment { location, value } => {
                self.compile_reassignment(location, value)
            }
            Statement::Import { path, alias } => {
                // foo
                // Compile the object we will use
                let identifier = self.compile_identifier(path);
                let getter = format!("{{get {}(){{return eval({});}}}}", alias, identifier);
                Ok(format!("const __get_{}={};", alias, getter))
            }
            Statement::TypeAlias { .. } => Ok("".to_string()),
            // _ => Err(CompileError::Message(format!(
            //     "{:?} is not a valid statement",
            //     stmt
            // ))),
        }
    }

    pub fn compile_module(&mut self, module: &Module) -> CompileResult<String> {
        // Compile current module
        let mut statements = self.compile_statements(&module.body)?;

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
