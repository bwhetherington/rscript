use crate::parser::{
    lexer::{LexError, Token, TokenIter, TokenKind},
    Span,
};
use std::{error, fmt};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
    Ref,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Times,
    Divide,
    Exponentiate,
    Mod,
    Equal,
    NotEqual,
    And,
    DoubleAnd,
    Or,
    DoubleOr,
    RShift,
    LShift,
    LT,
    LTE,
    GT,
    GTE,
    Xor,
}

impl BinaryOp {
    pub fn precedence(&self) -> usize {
        use BinaryOp::*;
        match self {
            Exponentiate => 10,
            Times | Divide | Mod => 9,
            Plus | Minus => 8,
            RShift | LShift => 7,
            LT | LTE | GT | GTE => 6,
            Equal | NotEqual => 5,
            And => 4,
            Xor => 3,
            Or => 2,
            DoubleAnd => 1,
            DoubleOr => 0,
        }
    }
}

// const UNIT: Expression = Expression::List(vec![]);

fn unit() -> Expression {
    Expression::Tuple(vec![])
}

#[derive(Debug, Clone)]
pub struct XmlNode {
    value: Box<Expression>,
    params: Vec<Expression>,
    children: Vec<XmlNode>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    ObjectLiteral(Box<Expression>, Vec<(String, Expression)>),
    Int(i32),
    Float(f64),
    Boolean(bool),
    Identifier(Vec<String>),
    String(String),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Index(Box<Expression>, Box<Expression>),
    Block(Vec<Statement>, Box<Expression>),
    Tuple(Vec<Expression>),
    List(Vec<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Lambda(Vec<String>, Box<Expression>),
    Xml(XmlNode),
    Member(Box<Expression>, String),
    None,
}

impl Expression {
    pub fn requires_semicolon(&self) -> bool {
        match self {
            Self::If(..) => true,
            Self::Block(..) => true,
            _ => false,
        }
    }
}

static OPERATOR_NAMES: [&'static str; 9] = [
    "new",
    "plus",
    "minus",
    "times",
    "divide",
    "to_string",
    "next",
    "index_get",
    "index_set",
];

#[derive(Debug, Clone)]
pub enum Type {
    Alias {
        alias: String,
        type_arguments: Vec<String>,
    },
    Tuple {
        types: Vec<Type>,
    },
    Interface {
        statements: Vec<InterfaceStatement>,
    },
    Enum {
        variants: Vec<EnumVariant>,
    },
}

#[derive(Debug, Clone)]
pub enum EnumVariant {
    Tuple(Vec<Type>),
    Interface(Vec<InterfaceStatement>),
}

#[derive(Debug, Clone)]
pub enum InterfaceStatement {
    TypeAlias(String),
    Member(String, TypeAnnotation),
    Function(String, Vec<Parameter>, TypeAnnotation),
}

#[derive(Debug, Clone)]
pub struct TypeAnnotation {
    pub label: Type,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub identifier: String,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, Clone)]
pub struct InferParameter {
    pub identifier: String,
    pub type_annotation: Option<TypeAnnotation>,
}

#[derive(Debug, Clone, Copy)]
pub enum Visibility {
    Private,
    Public,
}

impl Visibility {
    pub fn is_visible(&self) -> bool {
        match self {
            Visibility::Private => false,
            Visibility::Public => true,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Import {
        path: Vec<String>,
        alias: String,
    },
    ClassDeclaration {
        visibility: Visibility,
        identifier: String,
        parent: Option<Identifier>,
        body: Vec<Statement>,
    },
    Expression(Expression),
    Assignment {
        visibility: Visibility,
        mutable: bool,
        parameter: String,
        value: Expression,
    },
    Reassignment {
        location: Expression,
        value: Expression,
    },
    FunctionDeclaration {
        visibility: Visibility,
        identifier: String,
        parameters: Vec<String>,
        body: Expression,
    },
    TypeAlias {
        visibility: Visibility,
        identifier: String,
        concrete_type: Type,
        type_parameters: Vec<InferParameter>,
    },
    Loop {
        body: Vec<Statement>,
    },
    While {
        condition: Expression,
        body: Vec<Statement>,
    },
    Break,
    For {
        item: String,
        iterator: Expression,
        body: Vec<Statement>,
    },
}

impl Statement {
    pub fn is_top_level(&self) -> bool {
        use Statement::*;
        match self {
            TypeAlias { .. }
            | FunctionDeclaration { .. }
            | ClassDeclaration { .. }
            | Import { .. }
            | Assignment { .. }
            | Reassignment { .. } => true,
            _ => false,
        }
    }

    pub fn requires_semicolon(&self) -> bool {
        match self {
            Self::ClassDeclaration { .. } => false,
            Self::For { .. } => false,
            Self::Loop { .. } => false,
            Self::While { .. } => false,
            Self::Expression(expr) => expr.requires_semicolon(),
            _ => true,
        }
    }
}

pub struct Parser<I: TokenIter> {
    lexer: I,
    stack: Vec<Token>,
    last_span: Option<Span>,
}

#[derive(Debug)]
pub enum ParseError {
    LexError(LexError),
    EOF,
    Static(&'static str, Option<Span>),
    Custom(String, Option<Span>),
}

fn fmt_option(obj: &Option<impl fmt::Display>, f: &mut fmt::Formatter) -> fmt::Result {
    match obj {
        Some(obj) => write!(f, "{}", obj),
        None => write!(f, "<None>"),
    }
}

impl error::Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::LexError(e) => write!(f, "{}", e),
            ParseError::EOF => write!(f, "EOF"),
            ParseError::Static(s, span) => {
                write!(f, "{} at ", s)?;
                fmt_option(span, f)
            }
            ParseError::Custom(s, span) => {
                write!(f, "{} at ", s)?;
                fmt_option(span, f)
            }
        }
    }
}

impl Into<ParseError> for LexError {
    fn into(self) -> ParseError {
        ParseError::LexError(self)
    }
}

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
enum RpnToken {
    Expression(Expression),
    Operator(BinaryOp),
}

impl RpnToken {
    fn to_expression(self) -> ParseResult<Expression> {
        match self {
            RpnToken::Expression(expr) => Ok(expr),
            RpnToken::Operator(_) => Err(ParseError::Static(
                "expected expression, found operator",
                None,
            )),
        }
    }
}

fn convert_rpn_to_expression(tokens: Vec<RpnToken>) -> ParseResult<Expression> {
    let mut stack = Vec::new();

    for token in tokens {
        match token {
            token @ RpnToken::Expression(_) => stack.push(token),
            RpnToken::Operator(op) => {
                let rhs = stack
                    .pop()
                    .ok_or_else(|| ParseError::Static("binary requires 2 arguments", None))?
                    .to_expression()?;
                let lhs = stack
                    .pop()
                    .ok_or_else(|| ParseError::Static("binary requires 2 arguments", None))?
                    .to_expression()?;
                let call = Expression::Binary(op, Box::new(lhs), Box::new(rhs));
                stack.push(RpnToken::Expression(call));
            }
        }
    }

    stack
        .pop()
        .ok_or_else(|| ParseError::Static("failed to parse RPN", None))?
        .to_expression()
}

struct BlockData {
    statements: Vec<Statement>,
    last_expr: Option<Expression>,
}

type Identifier = Vec<String>;

enum CurryState {
    Curry,
    NoCurry,
}

impl CurryState {
    fn is_curried(&self) -> bool {
        match self {
            CurryState::Curry => true,
            CurryState::NoCurry => false,
        }
    }
}

fn transform_curry_function(call: Expression) -> Expression {
    match call {
        Expression::Call(func, args) => {
            let curry_states: Vec<_> = args
                .iter()
                .map(|arg| match arg {
                    // Check if any identifiers are `_`
                    Expression::Identifier(path) => match &path[..] {
                        [value] if value == "_" => CurryState::Curry,
                        _ => CurryState::NoCurry,
                    },
                    _ => CurryState::NoCurry,
                })
                .collect();

            // Check if we curry anything
            let any_curried = curry_states.iter().any(|state| state.is_curried());
            if any_curried {
                // Create the parameters for the lambda
                let mut params = Vec::new();
                let mut passed_args = Vec::new();

                let iter = args.into_iter().enumerate().zip(curry_states.iter());
                for ((index, arg), state) in iter {
                    if state.is_curried() {
                        let identifier = format!("__arg_{}", index);
                        params.push(identifier.clone());
                        let passed_arg = Expression::Identifier(vec![identifier]);
                        passed_args.push(passed_arg);
                    } else {
                        passed_args.push(arg);
                    }
                }

                let call = Expression::Call(func, passed_args);
                let lambda = Expression::Lambda(params, Box::new(call));

                lambda
            } else {
                Expression::Call(func, args)
            }
        }
        other => other,
    }
}

impl<I: TokenIter> Parser<I> {
    pub fn new(lexer: I) -> Parser<I> {
        Parser {
            lexer,
            stack: Vec::new(),
            last_span: None,
        }
    }

    fn unread(&mut self, token: Token) {
        self.stack.push(token);
    }

    fn last_span(&self) -> Option<Span> {
        self.last_span.as_ref().map(|span| span.clone())
    }

    pub fn next_token(&mut self) -> ParseResult<Option<Token>> {
        let token = self
            .stack
            .pop()
            .map(|tok| Ok(Some(tok)))
            .unwrap_or_else(|| self.lexer.next_token().map_err(|err| err.into()))?;
        self.last_span = token.as_ref().map(|tok| tok.span.clone());
        Ok(token)
    }

    fn next_operator(&mut self) -> ParseResult<Option<BinaryOp>> {
        let token = self.next_token()?;
        Ok(token.and_then(|token| match token.kind {
            TokenKind::DoubleAsterisk => Some(BinaryOp::Exponentiate),
            TokenKind::Plus => Some(BinaryOp::Plus),
            TokenKind::Minus => Some(BinaryOp::Minus),
            TokenKind::Times => Some(BinaryOp::Times),
            TokenKind::Divide => Some(BinaryOp::Divide),
            TokenKind::Mod => Some(BinaryOp::Mod),
            TokenKind::Or => Some(BinaryOp::Or),
            TokenKind::And => Some(BinaryOp::And),
            TokenKind::Xor => Some(BinaryOp::Xor),
            TokenKind::RShift => Some(BinaryOp::RShift),
            TokenKind::LShift => Some(BinaryOp::LShift),
            TokenKind::LT => Some(BinaryOp::LT),
            TokenKind::LTE => Some(BinaryOp::LTE),
            TokenKind::GT => Some(BinaryOp::GT),
            TokenKind::GTE => Some(BinaryOp::GTE),
            TokenKind::DoubleAnd => Some(BinaryOp::DoubleAnd),
            TokenKind::DoubleOr => Some(BinaryOp::DoubleOr),
            TokenKind::DoubleEqual => Some(BinaryOp::Equal),
            TokenKind::NotEqual => Some(BinaryOp::NotEqual),
            _ => {
                // Token isn't an operator
                self.unread(token);
                None
            }
        }))
    }

    fn parse_comma_separated<T>(
        &mut self,
        parser: impl Fn(&mut Self) -> ParseResult<T>,
        end: TokenKind,
    ) -> ParseResult<Vec<T>> {
        let mut buf = Vec::new();
        self.parse_comma_separated_to_buf(parser, end, &mut buf)?;
        Ok(buf)
    }

    fn parse_comma_separated_to_buf<T>(
        &mut self,
        parser: impl Fn(&mut Self) -> ParseResult<T>,
        end: TokenKind,
        buf: &mut Vec<T>,
    ) -> ParseResult<()> {
        let token = self.next_token()?.ok_or_else(|| ParseError::EOF)?;
        match &token.kind {
            data if data == &end => return Ok(()),
            _ => self.unread(token),
        }

        let item = parser(self)?;
        buf.push(item);

        let token = self.next_token()?.ok_or_else(|| ParseError::EOF)?;

        match &token.kind {
            data if data == &end => Ok(()),
            TokenKind::Comma => self.parse_comma_separated_to_buf(parser, end, buf),
            _ => Err(ParseError::Static(
                "expected comma or end of comma-separated list",
                Some(token.span.clone()),
            )),
        }
    }

    fn next_token_internal(&mut self) -> ParseResult<Token> {
        self.next_token()?.ok_or_else(|| ParseError::EOF)
    }

    fn parse_full_identifier(&mut self) -> ParseResult<Vec<String>> {
        let mut buf = Vec::new();

        loop {
            let i = self.parse_identifier()?;
            buf.push(i);
            let tok = self.next_token()?;
            if let Some(tok) = tok {
                if let TokenKind::DoubleColon = tok.kind {
                    continue;
                } else {
                    self.unread(tok);
                    break;
                }
            } else {
                break;
            }
        }

        Ok(buf)
    }

    fn parse_object_literal(&mut self, proto: Expression) -> ParseResult<Expression> {
        let pairs = self.parse_comma_separated(
            |parser| {
                // Parser each pair of key and value
                let key = parser.parse_identifier()?;
                parser.parse_token(TokenKind::Colon)?;
                let value = parser.parse_expr()?;
                Ok((key, value))
            },
            TokenKind::CloseBrace,
        )?;

        let mut body = Vec::new();

        let temp_name = "__value__";

        let initial_value = Expression::Call(Box::new(proto), Vec::new());
        let initial_assignment = Statement::Assignment {
            visibility: Visibility::Private,
            mutable: true,
            value: initial_value,
            parameter: temp_name.to_string(),
        };
        body.push(initial_assignment);

        let identifier = Expression::Identifier(vec![temp_name.to_string()]);

        for (key, value) in pairs {
            let location = Expression::Member(Box::new(identifier.clone()), key);
            let assign_key = Statement::Reassignment { location, value };
            body.push(assign_key);
        }

        let expr = Expression::Block(body, Box::new(identifier));
        Ok(expr)

        // Ok(Expression::ObjectLiteral(Box::new(proto), pairs))
    }

    fn parse_expr_internal(&mut self, base_expr: Expression) -> ParseResult<Expression> {
        // let expr = self.parse_primary_expr()?;
        let tok = self.next_token()?;
        if let Some(tok) = tok {
            match tok.kind {
                TokenKind::Dot => {
                    let member = self.parse_identifier()?;
                    let next_base = Expression::Member(Box::new(base_expr), member);
                    self.parse_expr_internal(next_base)
                }
                TokenKind::OpenParen => {
                    let params = self.parse_comma_separated(
                        |parser| parser.parse_expr(),
                        TokenKind::CloseParen,
                    )?;
                    let next_base = {
                        let first_call = Expression::Call(Box::new(base_expr), params);
                        transform_curry_function(first_call)
                    };
                    self.parse_expr_internal(next_base)
                }
                TokenKind::OpenBracket => {
                    let expr = self.parse_expr()?;
                    let token = self.next_token_internal()?;
                    let next_base = match token.kind {
                        TokenKind::CloseBracket => {
                            Expression::Index(Box::new(base_expr), Box::new(expr))
                        }
                        _ => return Err(ParseError::EOF),
                    };
                    self.parse_expr_internal(next_base)
                }
                TokenKind::OpenBrace => {
                    let next_base = self.parse_object_literal(base_expr)?;
                    self.parse_expr_internal(next_base)
                }
                _ => {
                    self.unread(tok);
                    Ok(base_expr)
                }
            }
        } else {
            Ok(base_expr)
        }
    }

    pub fn parse_expr(&mut self) -> ParseResult<Expression> {
        self.parse_tertiary_expr()
    }

    fn parse_tertiary_expr(&mut self) -> ParseResult<Expression> {
        let mut rpn_stack: Vec<RpnToken> = Vec::new();
        let mut op_stack: Vec<BinaryOp> = Vec::new();

        // Add secondary expressions to stack
        loop {
            let expr = self.parse_secondary_expr()?;
            let op = self.next_operator()?;

            rpn_stack.push(RpnToken::Expression(expr));

            if let Some(op) = op {
                let cur_prec = op.precedence();
                while let Some(stack_op) = op_stack.pop() {
                    let stack_prec = stack_op.precedence();
                    if cur_prec <= stack_prec {
                        rpn_stack.push(RpnToken::Operator(stack_op));
                    } else {
                        op_stack.push(stack_op);
                        break;
                    }
                }
                op_stack.push(op);
            } else {
                // Pop full op_stack
                while let Some(op) = op_stack.pop() {
                    rpn_stack.push(RpnToken::Operator(op));
                }
                break;
            }
        }
        convert_rpn_to_expression(rpn_stack)
    }

    fn parse_secondary_expr(&mut self) -> ParseResult<Expression> {
        let base_expr = self.parse_primary_expr()?;
        self.parse_expr_internal(base_expr)
    }

    fn parse_block(&mut self) -> ParseResult<Expression> {
        let mut block_data = BlockData {
            statements: Vec::new(),
            last_expr: None,
        };
        self.parse_block_internal(&mut block_data)?;
        block_data.statements = hoist_assignments(block_data.statements);

        let BlockData {
            statements,
            last_expr,
        } = block_data;

        let last_expr = last_expr.unwrap_or_else(unit);

        if statements.len() > 0 {
            Ok(Expression::Block(statements, Box::new(last_expr)))
        } else {
            Ok(last_expr)
        }
    }

    fn parse_lambda(&mut self) -> ParseResult<Expression> {
        self.parse_token(TokenKind::OpenParen)?;
        let params =
            self.parse_comma_separated(|parser| parser.parse_identifier(), TokenKind::CloseParen)?;
        self.parse_token(TokenKind::Equal)?;
        let body = self.parse_expr()?;
        Ok(Expression::Lambda(params, Box::new(body)))
    }

    fn parse_primary_expr(&mut self) -> ParseResult<Expression> {
        let token = self.next_token_internal()?;
        match token.kind {
            TokenKind::None => Ok(Expression::None),
            TokenKind::Int(i) => Ok(Expression::Int(i)),
            TokenKind::Float(f) => Ok(Expression::Float(f)),
            TokenKind::Boolean(b) => Ok(Expression::Boolean(b)),
            TokenKind::String(s) => Ok(Expression::String(s)),
            TokenKind::Identifier(_) => {
                self.unread(token);
                let ident = self.parse_full_identifier()?;
                Ok(Expression::Identifier(ident))
            }
            // !expr
            TokenKind::Not => {
                let expr = self.parse_expr()?;
                Ok(Expression::Unary(UnaryOp::Not, Box::new(expr)))
            }
            // +expr
            TokenKind::Plus => {
                let expr = self.parse_expr()?;
                Ok(Expression::Unary(UnaryOp::Plus, Box::new(expr)))
            }
            // -expr
            TokenKind::Minus => {
                let expr = self.parse_expr()?;
                Ok(Expression::Unary(UnaryOp::Minus, Box::new(expr)))
            }
            // &expr
            TokenKind::And => {
                let expr = self.parse_expr()?;
                Ok(Expression::Unary(UnaryOp::Ref, Box::new(expr)))
            }
            TokenKind::OpenBrace => self.parse_block(),
            // TokenKind::OpenBrace => self.parse_block(),
            TokenKind::OpenBracket => {
                let exprs = self
                    .parse_comma_separated(|parser| parser.parse_expr(), TokenKind::CloseBracket)?;
                Ok(Expression::List(exprs))
            }
            TokenKind::OpenParen => {
                // Check if next token is another parenthesis
                match self.next_token()? {
                    Some(tok) => match tok.kind {
                        TokenKind::CloseParen => return Ok(Expression::Tuple(Vec::new())),
                        _ => self.unread(tok),
                    },
                    None => return Err(ParseError::EOF),
                }

                // Start by trying to parse parenthesized expression
                let first_expr = self.parse_expr()?;

                // Check next token
                match self.next_token()? {
                    Some(tok) => match tok.kind {
                        TokenKind::CloseParen => Ok(first_expr),
                        TokenKind::Comma => {
                            let mut buf = Vec::new();
                            buf.push(first_expr);
                            self.parse_comma_separated_to_buf(
                                |parser| parser.parse_expr(),
                                TokenKind::CloseParen,
                                &mut buf,
                            )?;
                            Ok(Expression::Tuple(buf))
                        }
                        _ => Err(ParseError::Static(
                            "expected comma or close parenthesis",
                            Some(tok.span.clone()),
                        )),
                    },
                    None => Err(ParseError::EOF),
                }
            }
            TokenKind::If => {
                let condition = self.parse_expr()?;
                self.parse_token(TokenKind::Then)?;
                let then_expr = self.parse_expr()?;

                let token = self.next_token_internal()?;
                match token.kind {
                    TokenKind::Else => {
                        let else_expr = self.parse_expr()?;
                        Ok(Expression::If(
                            Box::new(condition),
                            Box::new(then_expr),
                            Box::new(else_expr),
                        ))
                    }
                    _ => {
                        self.unread(token);
                        Ok(Expression::If(
                            Box::new(condition),
                            Box::new(then_expr),
                            Box::new(Expression::None),
                        ))
                    }
                }
            }
            TokenKind::Function => self.parse_lambda(),
            // TokenKind::Or => self.parse_lambda(),
            // TokenKind::DoubleOr => {
            //     let params = Vec::new();
            //     let body = self.parse_expr()?;
            //     Ok(Expression::Lambda(params, Box::new(body)))
            // }
            _ => Err(ParseError::Custom(
                format!("unexpected token: {:?}", token.kind),
                Some(token.span.clone()),
            )),
        }
    }

    fn parse_token(&mut self, token_kind: TokenKind) -> ParseResult<bool> {
        let token = self.next_token_internal()?;
        match &token.kind {
            kind if kind == &token_kind => Ok(true),
            // TODO give better error
            _ => {
                println!("Token error: expected {:?}, found {:?}", token_kind, token);
                Err(ParseError::Custom(
                    format!("expected token {:?}, found {:?}", token_kind, token.kind),
                    Some(token.span.clone()),
                ))
            }
        }
    }

    fn parse_identifier(&mut self) -> ParseResult<String> {
        let token = self.next_token_internal()?;
        match token.kind {
            TokenKind::Identifier(s) => Ok(s),
            _ => {
                println!("warning: {:?}", token);
                Err(ParseError::Custom(
                    format!("unexpected token: {:?}", token.kind),
                    Some(token.span.clone()),
                ))
            }
        }
    }

    fn parse_mutable(&mut self) -> ParseResult<bool> {
        let token = self.next_token_internal()?;
        match token.kind {
            TokenKind::Mut => Ok(true),
            _ => {
                self.unread(token);
                Ok(false)
            }
        }
    }

    fn parse_type_parameters(&mut self) -> ParseResult<Vec<InferParameter>> {
        let token = self.next_token()?;
        match token {
            Some(token) => match token.kind {
                TokenKind::LT => self
                    .parse_comma_separated(|parser| parser.parse_infer_parameter(), TokenKind::GT),
                _ => {
                    self.unread(token);
                    Ok(Vec::new())
                }
            },
            _ => Ok(Vec::new()),
        }
    }

    fn parse_type_arguments(&mut self) -> ParseResult<Vec<String>> {
        let token = self.next_token()?;
        match token {
            Some(token) => match token.kind {
                TokenKind::LT => {
                    self.parse_comma_separated(|parser| parser.parse_identifier(), TokenKind::GT)
                }
                _ => {
                    self.unread(token);
                    Ok(Vec::new())
                }
            },
            _ => Ok(Vec::new()),
        }
    }

    fn parse_type(&mut self) -> ParseResult<Type> {
        let token = self.next_token_internal()?;
        match token.kind {
            TokenKind::Identifier(alias) => {
                let type_arguments = self.parse_type_arguments()?;

                Ok(Type::Alias {
                    alias,
                    type_arguments,
                })
            }
            TokenKind::OpenParen => {
                let types = self
                    .parse_comma_separated(|parser| parser.parse_type(), TokenKind::CloseParen)?;
                Ok(Type::Tuple { types })
            }
            TokenKind::OpenBrace => self.parse_interface(),
            _ => Err(ParseError::Static(
                "no type pattern starts with that token",
                Some(token.span.clone()),
            )),
        }
    }

    fn parse_type_annotation(&mut self) -> ParseResult<TypeAnnotation> {
        self.parse_token(TokenKind::Colon)?;
        let ty = self.parse_type()?;
        Ok(TypeAnnotation { label: ty })
    }

    fn parse_optional_type_annotation(&mut self) -> ParseResult<Option<TypeAnnotation>> {
        let token = self.next_token_internal()?;
        match token.kind {
            TokenKind::Colon => {
                self.unread(token);
                let param_type = self.parse_type_annotation()?;
                Ok(Some(param_type))
            }
            _ => {
                self.unread(token);
                Ok(None)
            }
        }
    }

    // fn parse_type_annotation(&mut self) -> ParseResult<TypeAnnotation> {

    // }

    fn parse_infer_parameter(&mut self) -> ParseResult<InferParameter> {
        let identifier = self.parse_identifier()?;
        let type_annotation = self.parse_optional_type_annotation()?;
        Ok(InferParameter {
            identifier,
            type_annotation,
        })
    }

    fn parse_parameter(&mut self) -> ParseResult<Parameter> {
        let identifier = self.parse_identifier()?;
        let type_annotation = self.parse_type_annotation()?;
        Ok(Parameter {
            identifier,
            type_annotation,
        })
    }
    fn parse_operator(&mut self, visibility: Visibility) -> ParseResult<Statement> {
        let name = self.parse_identifier()?;

        // Check name
        if OPERATOR_NAMES.iter().any(|el| *el == name) {
            self.parse_token(TokenKind::OpenParen)?;
            let parameters = self
                .parse_comma_separated(|parser| parser.parse_identifier(), TokenKind::CloseParen)?;

            self.parse_token(TokenKind::Equal)?;
            let body = self.parse_expr()?;
            Ok(Statement::FunctionDeclaration {
                visibility,
                identifier: name,
                parameters,
                body,
            })
        } else {
            let message = format!("`{}` is not a reserved operator name", name).into();
            Err(ParseError::Custom(message, self.last_span()))
        }
    }

    fn parse_function(&mut self, visibility: Visibility) -> ParseResult<Statement> {
        let name = self.parse_identifier()?;

        // Check name
        if OPERATOR_NAMES.iter().all(|el| *el != name) {
            self.parse_token(TokenKind::OpenParen)?;
            let parameters = self
                .parse_comma_separated(|parser| parser.parse_identifier(), TokenKind::CloseParen)?;

            self.parse_token(TokenKind::Equal)?;
            let body = self.parse_expr()?;
            Ok(Statement::FunctionDeclaration {
                visibility,
                identifier: name,
                parameters,
                body,
            })
        } else {
            let message = format!("`{}` is a reserved operator name", name).into();
            Err(ParseError::Custom(message, self.last_span()))
        }
    }

    fn parse_statement_visibility(&mut self, visibility: Visibility) -> ParseResult<Statement> {
        let token = self.next_token_internal()?;
        match token.kind {
            TokenKind::Mod => {
                let identifier = self.parse_identifier()?;
                todo!()
            }
            TokenKind::Class => {
                let identifier = self.parse_identifier()?;
                let tok = self.next_token_internal()?;
                let parent = match tok.kind {
                    TokenKind::Ext => {
                        // Get parent class
                        Some(self.parse_full_identifier()?)
                    }
                    _ => {
                        self.unread(tok);
                        None
                    }
                };

                let mut statements = Vec::new();

                self.parse_token(TokenKind::OpenBrace)?;

                while let Some(tok) = self.next_token()? {
                    match tok.kind {
                        TokenKind::CloseBrace => {
                            break;
                        }
                        _ => {
                            self.unread(tok);
                            let statement = self.parse_statement()?;
                            self.parse_semicolon()?;
                            statements.push(statement);
                        }
                    }
                }

                Ok(Statement::ClassDeclaration {
                    visibility,
                    identifier,
                    parent,
                    body: statements,
                })
            }
            TokenKind::Type => {
                let name = self.parse_identifier()?;

                let type_parameters = self.parse_type_parameters()?;

                self.parse_token(TokenKind::Equal)?;
                let ty = self.parse_type()?;
                Ok(Statement::TypeAlias {
                    visibility,
                    identifier: name,
                    concrete_type: ty,
                    type_parameters,
                })
            }
            TokenKind::Operator => self.parse_operator(visibility),
            TokenKind::Function => {
                // Check next token
                // If the next token is a parenthesis, parse as a lambda expression
                let next = self.next_token_internal()?;
                match next.kind {
                    TokenKind::OpenParen => {
                        self.unread(next);
                        let expr = self.parse_lambda()?;
                        Ok(Statement::Expression(expr))
                    }
                    _ => {
                        self.unread(next);
                        self.parse_function(visibility)
                    }
                }
            }
            TokenKind::Let => {
                let is_mut = self.parse_mutable()?;
                let ident = self.parse_identifier()?;
                self.parse_token(TokenKind::Equal)?;
                let body = self.parse_expr()?;
                Ok(Statement::Assignment {
                    visibility,
                    mutable: is_mut,
                    parameter: ident,
                    value: body,
                })
            }
            TokenKind::Import => {
                let path = self.parse_full_identifier()?;
                let default_alias = path.last().unwrap();

                let alias = if let Some(token) = self.next_token()? {
                    if let TokenKind::As = token.kind {
                        self.parse_identifier()?
                    } else {
                        self.unread(token);
                        default_alias.to_string()
                    }
                } else {
                    default_alias.to_string()
                };

                Ok(Statement::Import { path, alias })
            }
            // Parse break;
            TokenKind::Break => Ok(Statement::Break),

            TokenKind::Loop => {
                self.parse_token(TokenKind::OpenBrace)?;

                let mut statements = Vec::new();
                let mut finished = false;

                while let Some(tok) = self.next_token()? {
                    match tok.kind {
                        TokenKind::CloseBrace => {
                            finished = true;
                            break;
                        }
                        TokenKind::Semicolon => (),
                        _ => {
                            self.unread(tok);
                            let statement = self.read_statement_maybe_semicolon()?;
                            if statement.requires_semicolon() {
                                self.parse_semicolon()?;
                            }
                            statements.push(statement);
                        }
                    }
                }

                statements = hoist_assignments(statements);

                if finished {
                    Ok(Statement::Loop { body: statements })
                } else {
                    Err(ParseError::EOF)
                }
            }

            TokenKind::For => {
                let item = self.parse_identifier()?;
                self.parse_token(TokenKind::In)?;
                let iterator = self.parse_expr()?;
                self.parse_token(TokenKind::Do)?;
                self.parse_token(TokenKind::OpenBrace)?;

                let mut statements = Vec::new();
                let mut finished = false;

                while let Some(tok) = self.next_token()? {
                    match tok.kind {
                        TokenKind::CloseBrace => {
                            finished = true;
                            break;
                        }
                        TokenKind::Semicolon => (),
                        _ => {
                            self.unread(tok);
                            let statement = self.read_statement_maybe_semicolon()?;
                            if statement.requires_semicolon() {
                                self.parse_semicolon()?;
                            }
                            statements.push(statement);
                        }
                    }
                }

                statements = hoist_assignments(statements);

                if finished {
                    Ok(Statement::For {
                        item,
                        iterator,
                        body: statements,
                    })
                } else {
                    Err(ParseError::EOF)
                }
            }

            TokenKind::While => {
                let condition = self.parse_expr()?;
                self.parse_token(TokenKind::Do)?;
                self.parse_token(TokenKind::OpenBrace)?;

                let mut statements = Vec::new();
                let mut finished = false;

                while let Some(tok) = self.next_token()? {
                    match tok.kind {
                        TokenKind::CloseBrace => {
                            finished = true;
                            break;
                        }
                        TokenKind::Semicolon => (),
                        _ => {
                            self.unread(tok);
                            let statement = self.read_statement_maybe_semicolon()?;
                            if statement.requires_semicolon() {
                                self.parse_semicolon()?;
                            }
                            statements.push(statement);
                        }
                    }
                }

                statements = hoist_assignments(statements);

                if finished {
                    Ok(Statement::While {
                        condition,
                        body: statements,
                    })
                } else {
                    Err(ParseError::EOF)
                }
            }

            // Parse expression or assignment
            _ => {
                self.unread(token);
                let expr = self.parse_expr()?;

                let next_token = self.next_token_internal()?;
                match next_token.kind {
                    TokenKind::Equal => {
                        let value = self.parse_expr()?;
                        Ok(Statement::Reassignment {
                            location: expr,
                            value,
                        })
                    }
                    _ => {
                        self.unread(next_token);
                        Ok(Statement::Expression(expr))
                    }
                }
            }
        }
    }

    pub fn parse_statement(&mut self) -> ParseResult<Statement> {
        let token = self.next_token_internal()?;
        let visibility = match token.kind {
            TokenKind::Public => Visibility::Public,
            _ => {
                self.unread(token);
                Visibility::Private
            }
        };
        self.parse_statement_visibility(visibility)
    }

    fn read_statement_maybe_semicolon(&mut self) -> ParseResult<Statement> {
        let statement = self.parse_statement()?;
        // if !statement.requires_semicolon() {
        //     let semicolon = Token {
        //         kind: TokenKind::Semicolon,
        //         span: Span {
        //             col: 0,
        //             row: 0,
        //             len: 0,
        //             file: None,
        //         },
        //     };
        //     // Check next token
        //     self.unread(semicolon);
        // }
        Ok(statement)
    }

    // { statement ; statement ; [expression]? }

    fn parse_block_internal(&mut self, buf: &mut BlockData) -> ParseResult<()> {
        // Check if we're at the end
        let token = self.next_token_internal()?;
        match token.kind {
            TokenKind::CloseBrace => return Ok(()),
            TokenKind::Semicolon => {}
            _ => self.unread(token),
        }

        let statement = self.read_statement_maybe_semicolon()?;
        let token = self.next_token_internal()?;
        match &token.kind {
            TokenKind::CloseBrace => match statement {
                Statement::Expression(expr) => {
                    buf.last_expr = Some(expr);
                    Ok(())
                }
                _ => Err(ParseError::Static(
                    "final non-semicolon-suffixed statement of a block must be an expression",
                    self.last_span(),
                )),
            },
            TokenKind::Semicolon => {
                buf.statements.push(statement);
                self.parse_block_internal(buf)
            }
            _ => {
                buf.statements.push(statement);
                self.unread(token);
                Ok(())
            }
        }
    }

    fn parse_interface_alias_statement(&mut self) -> ParseResult<InterfaceStatement> {
        let ident = self.parse_identifier()?;
        Ok(InterfaceStatement::TypeAlias(ident))
    }

    fn parse_interface_member(&mut self) -> ParseResult<InterfaceStatement> {
        let ident = self.parse_identifier()?;
        let annotation = self.parse_type_annotation()?;
        Ok(InterfaceStatement::Member(ident, annotation))
    }

    fn parse_interface_func(&mut self) -> ParseResult<InterfaceStatement> {
        let ident = self.parse_identifier()?;
        self.parse_token(TokenKind::OpenParen)?;
        let params =
            self.parse_comma_separated(|parser| parser.parse_parameter(), TokenKind::CloseParen)?;
        let ret_ty = self.parse_type_annotation()?;
        Ok(InterfaceStatement::Function(ident, params, ret_ty))
    }

    fn parse_interface_statement(&mut self) -> ParseResult<InterfaceStatement> {
        let token = self.next_token_internal()?;
        match token.kind {
            TokenKind::Type => self.parse_interface_member(),
            TokenKind::Identifier(_) => {
                self.unread(token);
                self.parse_interface_member()
            }
            TokenKind::Function => self.parse_interface_func(),
            _ => Err(ParseError::Static(
                "unexpected token in interface statement",
                self.last_span(),
            )),
        }
    }

    fn parse_interface(&mut self) -> ParseResult<Type> {
        let mut statements = Vec::new();

        loop {
            let token = self.next_token_internal()?;
            match token.kind {
                TokenKind::CloseBrace => break,
                _ => {
                    self.unread(token);
                    let statement = self.parse_interface_statement()?;
                    self.parse_semicolon()?;
                    statements.push(statement);
                }
            }
        }

        Ok(Type::Interface { statements })
    }

    fn parse_semicolon(&mut self) -> ParseResult<()> {
        // match self.last_token {
        //     Some(TokenKind::CloseBrace) => {}
        //     _ => {
        //         self.parse_token(TokenKind::Semicolon)?;
        //     }
        // }
        // Ok(())
        self.parse_token(TokenKind::Semicolon)?;
        Ok(())
    }

    pub fn parse_statements(&mut self) -> ParseResult<Vec<Statement>> {
        let mut buf = Vec::new();
        while let Some(token) = self.next_token()? {
            self.unread(token);
            let statement = self.parse_statement()?;
            self.parse_semicolon()?;
            buf.push(statement);
        }
        buf = hoist_assignments(buf);
        // println!("====");
        // println!("{:#?}", buf);
        Ok(buf)
    }
}

/// Rearranges the list of statements such that all assignments appear first
pub fn hoist_assignments(statements: Vec<Statement>) -> Vec<Statement> {
    let mut imports = Vec::new();
    let mut class_forward_declarations = Vec::new();
    let mut assignments = Vec::new();
    let mut others = Vec::new();

    for statement in statements {
        match statement {
            import @ Statement::Import { .. } => {
                imports.push(import);
            }
            Statement::ClassDeclaration {
                identifier,
                visibility,
                parent,
                body,
            } => {
                let name = identifier;

                // Create forward declaration
                let proto = parent
                    .as_ref()
                    .map(|ident| ident.clone())
                    .unwrap_or_else(|| vec!["Object".into()]);

                let proto = Expression::Identifier(proto);
                let proto = Expression::Call(Box::new(proto), Vec::new());

                let forward = Statement::Assignment {
                    visibility,
                    parameter: name.clone(),
                    mutable: true,
                    value: proto,
                };

                class_forward_declarations.push(forward);

                for statement in body {
                    match statement {
                        Statement::FunctionDeclaration {
                            identifier,
                            parameters,
                            body,
                            ..
                        } => {
                            // Get location
                            let location = Expression::Member(
                                Box::new(Expression::Identifier(vec![name.clone()])),
                                identifier,
                            );

                            // Construct lambda
                            let lambda = Expression::Lambda(parameters, Box::new(body));
                            let assignment = Statement::Reassignment {
                                location,
                                value: lambda,
                            };
                            assignments.push(assignment);
                        }
                        Statement::Assignment {
                            parameter, value, ..
                        } => {
                            // Get location
                            let location = Expression::Member(
                                Box::new(Expression::Identifier(vec![name.clone()])),
                                parameter,
                            );
                            let assignment = Statement::Reassignment { location, value };
                            assignments.push(assignment);
                        }
                        _ => todo!(),
                    }
                }

                // let class = Statement::ClassDeclaration {
                //     identifier,
                //     visibility,
                //     parent,
                //     body,
                // };
                // assignments.push(class);
            }
            assignment @ Statement::Assignment { .. } => {
                assignments.push(assignment);
            }
            Statement::FunctionDeclaration {
                visibility,
                identifier,
                parameters,
                body,
            } => {
                let lambda = Expression::Lambda(parameters, Box::new(body));
                let assignment = Statement::Assignment {
                    visibility,
                    mutable: true,
                    parameter: identifier,
                    value: lambda,
                };
                assignments.push(assignment);
            }
            other => {
                assignments.push(other);
            }
        }
    }

    imports
        .into_iter()
        .chain(class_forward_declarations.into_iter())
        .chain(assignments.into_iter())
        .chain(others.into_iter())
        .collect()
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expression::*;
        match self {
            Boolean(b) if *b => write!(f, "True"),
            Boolean(b) => write!(f, "False"),
            None => write!(f, "None"),
            Int(n) => write!(f, "{}", n),
            Float(n) => write!(f, "{}", n),
            Block(statements, expr) => {
                write!(f, "{{ ")?;
                for statement in statements {}
                write!(f, " }}")
            }
            _ => Ok(()),
        }
    }
}

// impl fmt::Display for Statement {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         use Statement::*;
//         Ok(())
//     }
// }
