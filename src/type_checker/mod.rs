use crate::parser::ast::Expression;
use std::collections::HashMap;

pub enum Type {
    Int,
    Float,
    Boolean,
    String,
    Function(Vec<Type>, Type),
    Tuple(Vec<Type>),
}

struct TypeChecker {
    aliases: HashMap<String, Type>
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {
            aliases: HashMap::new()
        }
    }   
}

fn determine_type(expr: &Expression) -> Type {
    match expr {
        Expression::Boolean(_) => Type::Boolean,
        Expression::Int(_) => Type::Int,
        Expression::Float(_) => Type::Float,
        Expression::Call(func, args) => {
            
        }
    }
}