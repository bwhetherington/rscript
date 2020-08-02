#![allow(unused)]

use crate::parser::{BinaryOp, Expression, Statement, UnaryOp, Visibility};
use std::rc::Rc;

type Str = Rc<str>;

#[derive(Clone, Debug)]
pub enum DExpression {
    Number(f64),
    Boolean(bool),
    Identifier(Str),
    Path(Vec<Str>),
    Character(char),
    String(Str),
    Block(Vec<DStatement>, Box<DExpression>),
    Function(Vec<Str>, Box<DExpression>),
    Apply(Box<DExpression>, Vec<DExpression>),
    Binary(BinaryOp, Box<DExpression>, Box<DExpression>),
    Unary(UnaryOp, Box<DExpression>),
    Member(Box<DExpression>, Str),
    TryMember(Box<DExpression>, Str),
    If(Box<DExpression>, Box<DExpression>, Box<DExpression>),
    List(Vec<DExpression>),
    None,
}

#[derive(Clone, Debug)]
pub enum DStatement {
    GlobImport(Vec<Str>),
    Assignment(Visibility, Str, DExpression),
    Reassignment(DExpression, DExpression),
    Expression(DExpression),
    Loop(Vec<DStatement>),
    Break,
}

impl DExpression {
    pub fn desugar_function(params: &[String], body: &Expression) -> DExpression {
        DExpression::Function(
            params.iter().map(|param| param.as_str().into()).collect(),
            Box::new(Self::desugar(body)),
        )
    }

    pub fn desugar_object(proto: &Expression, bindings: &[(String, Expression)]) -> DExpression {
        let mut body = Vec::with_capacity(bindings.len() + 1);

        // Create object
        let name: Str = "__name__0".into();
        body.push(DStatement::Assignment(
            Visibility::Private,
            name.clone(),
            DExpression::Apply(Box::new(Self::desugar(proto)), Vec::new()),
        ));

        // Bind fields to object
        let ident = DExpression::Identifier(name.clone());
        for (key, value) in bindings {
            let lhs = DExpression::Member(Box::new(ident.clone()), key.as_str().into());
            let rhs = Self::desugar(value);
            let stmt = DStatement::Reassignment(lhs, rhs);
            body.push(stmt);
        }

        DExpression::Block(body, Box::new(ident))
    }

    pub fn desugar_apply(func: &Expression, args: &[Expression]) -> DExpression {
        DExpression::Apply(
            Box::new(Self::desugar(func)),
            args.iter().map(|arg| Self::desugar(arg)).collect(),
        )
    }

    pub fn desugar(expr: &Expression) -> DExpression {
        match expr {
            Expression::Character(ch) => DExpression::Character(*ch),
            Expression::None => DExpression::None,
            Expression::Lambda(params, body) => Self::desugar_function(&params[..], body),
            Expression::Tuple(exprs) => {
                DExpression::List(exprs.iter().map(|expr| Self::desugar(expr)).collect())
            }
            Expression::List(exprs) => {
                DExpression::List(exprs.iter().map(|expr| Self::desugar(expr)).collect())
            }
            Expression::If(cond, then, otherwise) => DExpression::If(
                Box::new(Self::desugar(cond)),
                Box::new(Self::desugar(then)),
                Box::new(Self::desugar(otherwise)),
            ),
            Expression::Index(obj, index) => {
                let obj = Self::desugar(obj);
                let index = Self::desugar(index);
                let name: Str = "index_get".into();
                let func = DExpression::Member(Box::new(obj), name);
                DExpression::Apply(Box::new(func), vec![index])
            }
            Expression::Call(func, args) => Self::desugar_apply(func, args),
            Expression::Identifier(idents) if idents.len() == 1 => {
                DExpression::Identifier(idents[0].as_str().into())
            }
            Expression::Identifier(idents) => {
                DExpression::Path(idents.iter().map(|s| s.as_str().into()).collect())
            }
            Expression::String(s) => DExpression::String(s.as_str().into()),
            Expression::Member(expr, field) => {
                DExpression::Member(Box::new(Self::desugar(expr)), field.as_str().into())
            }
            Expression::TryMember(expr, field) => {
                DExpression::TryMember(Box::new(Self::desugar(expr)), field.as_str().into())
            }
            Expression::Binary(op, left, right) => DExpression::Binary(
                *op,
                Box::new(Self::desugar(left)),
                Box::new(Self::desugar(right)),
            ),
            Expression::Unary(op, val) => DExpression::Unary(*op, Box::new(Self::desugar(val))),
            Expression::Int(i) => DExpression::Number(*i as f64),
            Expression::Float(f) => DExpression::Number(*f),
            Expression::Boolean(b) => DExpression::Boolean(*b),
            Expression::Block(stmts, last) => {
                let mut body = Vec::new();
                for stmt in stmts {
                    DStatement::desugar(stmt, &mut body);
                }
                DExpression::Block(body, Box::new(Self::desugar(last)))
            }
            Expression::ObjectLiteral(proto, mappings) => Self::desugar_object(proto, mappings),
            Expression::Xml(_) => panic!("XML not yet supported"),
        }
    }
}

impl DStatement {
    pub fn desugar_while(cond: &Expression, body: &[Statement], buf: &mut Vec<DStatement>) {
        // while cond do {
        //   ...body
        // };
        // ->
        // loop {
        //   if cond then {
        //     ...body
        //   } else {
        //     break;
        //   };
        // };

        let mut then_body = Vec::with_capacity(body.len() + 1);
        for stmt in body {
            Self::desugar(stmt, &mut then_body);
        }
        let cond = DExpression::desugar(cond);
        let then_branch = DExpression::Block(then_body, Box::new(DExpression::None));
        let else_branch = DExpression::Block(vec![DStatement::Break], Box::new(DExpression::None));
        let loop_body =
            DExpression::If(Box::new(cond), Box::new(then_branch), Box::new(else_branch));
        let loop_body = vec![DStatement::Expression(loop_body)];
        let loop_stmt = DStatement::Loop(loop_body);
        buf.push(loop_stmt);
    }

    pub fn desugar_for(
        item: &str,
        iterator: &Expression,
        body: &[Statement],
        buf: &mut Vec<DStatement>,
    ) {
        // for item in iterator do {
        //   ...body
        // }
        // ->
        // {
        //   let item = None;
        //   loop {
        //     item = iterator.next();
        //     if item == None then {
        //       break;
        //     };
        //     ...body
        //   };
        // };
        //
        let mut new_body = Vec::with_capacity(body.len() + 2);

        // Poll iterator
        let iter_value = DExpression::desugar(iterator);
        let item_value = DExpression::Identifier(item.into());
        let call = DExpression::Apply(Box::new(iter_value), Vec::new());
        let poll = DStatement::Reassignment(item_value.clone(), call);
        new_body.push(poll);

        // Check if we should continue iterating
        let cond = DExpression::Binary(
            BinaryOp::Equal,
            Box::new(item_value.clone()),
            Box::new(DExpression::Boolean(false)),
        );
        let break_branch = DExpression::Block(vec![DStatement::Break], Box::new(DExpression::None));
        let check = DExpression::If(
            Box::new(cond),
            Box::new(break_branch),
            Box::new(DExpression::None),
        );
        let check = DStatement::Expression(check);
        new_body.push(check);

        // For body
        for stmt in body {
            Self::desugar(stmt, &mut new_body);
        }

        let top = DExpression::Block(new_body, Box::new(DExpression::None));
        let top = DStatement::Expression(top);
        buf.push(top);
    }

    pub fn desugar(stmt: &Statement, buf: &mut Vec<DStatement>) {
        match stmt {
            Statement::Reassignment { location, value } => {
                let loc = DExpression::desugar(location);
                let val = DExpression::desugar(value);
                let stmt = DStatement::Reassignment(loc, val);
                buf.push(stmt);
            }
            Statement::Expression(expr) => {
                let expr = DExpression::desugar(expr);
                let stmt = DStatement::Expression(expr);
                buf.push(stmt);
            }
            Statement::Assignment {
                visibility,
                parameter,
                value,
                ..
            } => {
                let new = DStatement::Assignment(
                    *visibility,
                    parameter.as_str().into(),
                    DExpression::desugar(value),
                );
                buf.push(new);
            }
            Statement::FunctionDeclaration {
                visibility,
                identifier,
                parameters,
                body,
            } => {
                let function = DExpression::desugar_function(parameters, body);
                let new = DStatement::Assignment(*visibility, identifier.as_str().into(), function);
                buf.push(new);
            }
            Statement::Loop { body } => {
                let mut new_body = Vec::with_capacity(body.len());
                for stmt in body {
                    Self::desugar(stmt, &mut new_body);
                }
                let new = DStatement::Loop(new_body);
                buf.push(new);
            }
            Statement::For {
                item,
                iterator,
                body,
            } => {
                Self::desugar_for(item, iterator, body, buf);
            }
            Statement::While { condition, body } => {
                Self::desugar_while(condition, body, buf);
            }
            Statement::Break => {
                buf.push(DStatement::Break);
            }
            Statement::Import { path, alias } => match (&path[..path.len() - 1], path.last()) {
                // Check for glob import
                (prefix, Some(last)) if last == "_" => {
                    let prefix: Vec<_> = prefix.iter().map(|item| item.as_str().into()).collect();
                    let glob = DStatement::GlobImport(prefix);
                    buf.push(glob);
                }
                // 
                _ => {
                    let value =
                        DExpression::Path(path.iter().map(|item| item.as_str().into()).collect());
                    let alias = alias.as_str().into();
                    let assignment = DStatement::Assignment(Visibility::Private, alias, value);
                    buf.push(assignment);
                }
            },
            Statement::ClassDeclaration {
                visibility,
                identifier,
                parent,
                body,
            } => {
                // class Foo ext Bar {
                //   fn foo() = bar;
                // };
                // ->
                // let Foo = Bar();
                // Foo.foo = fn() = bar;

                // Create initial object
                let parent = parent
                    .as_ref()
                    .map(|parent| Expression::Identifier(parent.clone()))
                    .map(|parent| DExpression::desugar(&parent))
                    .unwrap_or_else(|| DExpression::Identifier("__Object__".into()));
                let call = DExpression::Apply(Box::new(parent), vec![]);
                let identifier: Str = identifier.as_str().into();
                let assignment = DStatement::Assignment(*visibility, identifier.clone(), call);
                let identifier = DExpression::Identifier(identifier);
                buf.push(assignment);

                // Assign all fields
                let mut statements = Vec::with_capacity(body.len());
                for stmt in body {
                    Self::desugar(stmt, &mut statements);
                }
                for stmt in statements {
                    match stmt {
                        DStatement::Assignment(_, name, value) => {
                            // Create reassignment statement
                            let loc = DExpression::Member(Box::new(identifier.clone()), name);
                            let stmt = DStatement::Reassignment(loc, value);
                            buf.push(stmt);
                        }
                        other => panic!("Not supported"),
                    }
                }
            }
            Statement::TypeAlias { .. } => panic!("Type aliases not supported"),
        }
    }
}
