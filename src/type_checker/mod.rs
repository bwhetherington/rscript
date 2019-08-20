use crate::parser::ast::Expression;

enum BuiltIn {
    Int,
    Float,
}

pub enum Type {
    BuiltIn(BuiltIn),
    Function(Vec<Type>, Type),
    Tuple(Vec<Type>),
}

fn determine_type(expr: Expression)