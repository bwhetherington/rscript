use crate::parser::{
    ast::{BinaryOp, Expression, Parameter, Statement, UnaryOp, Visibility},
    resolver::{Module, ModuleError},
};

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

fn compile_binary_op(op: &BinaryOp) -> CompileResult<String> {
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

fn compile_primative(expr: &Expression) -> CompileResult<String> {
    use Expression::*;
    match expr {
        Int(i) => Ok(i.to_string()),
        Float(f) => Ok(f.to_string()),
        Boolean(b) => Ok(if *b {
            "true".to_string()
        } else {
            "false".to_string()
        }),
        Identifier(s) => Ok(s.to_string()),
        String(s) => Ok(format!("`{}`", s)),
        Tuple(values) => {
            let content: Vec<_> = values
                .iter()
                .map(compile_expression)
                .collect::<Result<_, _>>()?;
            let content = content.join(",");

            Ok(format!("[{}]", content))
        }
        List(values) => {
            let content: Vec<_> = values
                .iter()
                .map(compile_expression)
                .collect::<Result<_, _>>()?;
            let content = content.join(",");

            Ok(format!("[{}]", content))
        }
        other => Err(CompileError::Message(format!(
            "{:?} is not a primitive value",
            other
        ))),
    }
}

pub fn compile_expression(expr: &Expression) -> CompileResult<String> {
    use Expression::*;
    match expr {
        Member(expr, identifier) => {
            let expr = compile_expression(expr)?;
            Ok(format!("({}[{}])", expr, identifier))
        }
        Call(func, args) => {
            let func = compile_expression(func.as_ref())?;
            let args: Result<Vec<_>, _> = args
                .into_iter()
                .map(|arg| compile_expression(arg))
                .collect();
            let args = args?;
            let args = args.join(",");
            Ok(format!("{}({})", func, args))
        }
        Binary(op, lhs, rhs) => {
            let op = compile_binary_op(op)?;
            let lhs = compile_expression(lhs)?;
            let rhs = compile_expression(rhs)?;
            Ok(format!("({}{}{})", lhs, op, rhs))
        }
        Block(statements, value) => {
            // foo
            let statements: Result<Vec<_>, _> = statements
                .into_iter()
                .map(|stmt| compile_statement(stmt))
                .collect();
            let statements = statements?;
            let statements = statements.join("");
            let value = compile_expression(value)?;
            Ok(format!("(()=>{{{}return {};}})()", statements, value))
        }
        If(cond, then, otherwise) => {
            let cond = compile_expression(cond.as_ref())?;
            let then = compile_expression(then)?;
            let otherwise = compile_expression(otherwise)?;
            Ok(format!("({}?{}:{})", cond, then, otherwise))
        }
        Index(expr, index) => {
            let expr = compile_expression(expr)?;
            let index = compile_expression(index)?;
            Ok(format!("({}[{}])", expr, index))
        }
        other => compile_primative(other),
    }
}

pub fn compile_statement(stmt: &Statement) -> CompileResult<String> {
    match stmt {
        Statement::FunctionDeclaration {
            visibility,
            identifier,
            parameters,
            body,
            ..
        } => {
            let params: Vec<_> = parameters
                .into_iter()
                .map(|Parameter { identifier, .. }| identifier.clone())
                .collect();
            let params = params.join(",");
            let body = compile_expression(body)?;
            let mut code = format!("let {}=({})=>{};", identifier, params, body);
            if let Visibility::Public = visibility {
                let export = format!("this.{}={};", identifier, identifier);
                code.push_str(&export);
            }
            Ok(code)
        }
        Statement::Expression(expr) => {
            let expr = compile_expression(expr)?;
            Ok(format!("{};", expr))
        }
        Statement::Assignment {
            visibility,
            mutable,
            parameter,
            value,
        } => {
            let value = compile_expression(value)?;
            let ident = &parameter.identifier;
            let mut code = if *mutable {
                format!("let {}={};", ident, value)
            } else {
                format!("const {}={};", ident, value)
            };
            if let Visibility::Public = visibility {
                let export = format!("this.{}={};", ident, ident);
                code.push_str(&export);
            }
            Ok(code)
        }
        Statement::Reassignment { location, value } => {
            let location = compile_expression(location)?;
            let value = compile_expression(value)?;
            Ok(format!("{}={};", location, value))
        }
        Statement::TypeAlias { .. } => Ok("".to_string()),
        // _ => Err(CompileError::Message(format!(
        //     "{:?} is not a valid statement",
        //     stmt
        // ))),
    }
}

pub fn compile_module(module: &Module) -> CompileResult<String> {
    // Compile current module
    let statements: Result<Vec<_>, _> = module.body.iter().map(compile_statement).collect();
    let statements = statements?;
    let mut statements = statements.join("");

    // Compile each child module
    for child in &module.children {
        let mod_name = &child.identifier;
        let child_module = compile_module(child)?;
        let code = format!("this.{}={}", mod_name, child_module);
        statements.push_str(&code);
    }

    Ok(format!("(new(function(){{{}}})());", statements))
}
