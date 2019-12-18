use crate::parser::{
    ast::{Expression, InferParameter, Parameter, Statement, Type as AstType},
    resolver::Module,
};
use std::collections::HashMap;

#[derive(Clone)]
pub enum Type {
    Int,
    Float,
    Boolean,
    String,
    Function(Box<Function>),
    Tuple(Vec<Type>),
}

#[derive(Clone)]
struct TypeParameter {
    identifier: String,
    constraint: Option<Type>,
}

#[derive(Clone)]
struct TypeAlias {
    identifier: String,
    type_parameters: Vec<TypeParameter>,
    concrete_type: Type,
}

#[derive(Clone)]
struct Function {
    receiver: Option<Type>,
    type_parameters: Vec<TypeParameter>,
    parameters: Vec<Type>,
    output: Type,
}

pub struct TypedExpression {
    expression: Expression,
    type_annotation: Type,
}

type CheckResult<T> = Result<T, CheckError>;

enum CheckError {
    Arity(usize, usize),
    TypeError(String, String),
    TypeNotFound(String),
}

struct TypeChecker {
    aliases: HashMap<String, TypeAlias>,
    functions: Vec<Function>,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {
            aliases: HashMap::new(),
            functions: Vec::new(),
        }
    }

    fn transform_type(&self, ast_type: &AstType) -> CheckResult<Type> {
        match ast_type {
            AstType::Alias {
                alias,
                type_arguments,
            } => {
                // Look up the alias
                self.aliases
                    .get(alias)
                    .map(|alias| alias.concrete_type.clone())
                    .ok_or_else(|| CheckError::TypeNotFound(alias.to_string()))
            }
            _ => unimplemented!(),
        }
    }

    fn check_definition(&mut self, statement: &Statement) -> CheckResult<()> {
        match statement {
            Statement::TypeAlias {
                identifier,
                visibility,
                concrete_type,
                type_parameters,
            } => {
                let mapped_parameters: Vec<_> = type_parameters
                    .iter()
                    .map(|param| {
                        let identifier = param.identifier.clone();
                        let annotation = &param.type_annotation;
                        annotation
                            .as_ref()
                            .map(|annotation| {
                                self.transform_type(&annotation.label).map(|ty| Some(ty))
                            })
                            .unwrap_or_else(|| Ok(None))
                    })
                    .collect::<Result<_, _>>()?;
                let alias = TypeAlias {
                    identifier: identifier.clone(),
                    concrete_type: self.transform_type(concrete_type)?,
                    type_parameters: unimplemented!(),
                };
                self.aliases.insert(identifier.clone(), alias);
            },
            Statement::FunctionDeclaration {
                
            }
            _ => (),
        }
        Ok(())
    }

    fn gather_definitions(&mut self, module: &Module) -> CheckResult<()> {
        // Look through statements
        for statement in &module.body {
            self.check_definition(statement)?;
        }

        // Look through children
        for child in &module.children {
            self.gather_definitions(child)?;
        }
        Ok(())
    }
}

// fn determine_type(expr: &Expression) -> Type {
//     match expr {
//         Expression::Boolean(_) => Type::Boolean,
//         Expression::Int(_) => Type::Int,
//         Expression::Float(_) => Type::Float,
//         Expression::Call(func, args) => {
//         }
//     }
// }
