use crate::parser::lexer::{LexError, Token, TokenIter, TokenKind};
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
    Lambda(Vec<InferParameter>, Box<Expression>),
    Xml(XmlNode),
    Member(Box<Expression>, String),
}

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

#[derive(Debug, Clone)]
pub enum Visibility {
    Private,
    Public,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Import {
        path: Vec<String>,
        alias: String,
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
}

pub struct Parser<I: TokenIter> {
    lexer: I,
    stack: Vec<Token>,
}

#[derive(Debug)]
pub enum ParseError {
    LexError(LexError),
    EOF,
    Static(&'static str),
    Custom(String),
}

impl error::Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "ParseError: ")?;
        match self {
            ParseError::LexError(e) => write!(formatter, "{}", e),
            ParseError::EOF => write!(formatter, "EOF"),
            ParseError::Static(s) => write!(formatter, "{}", s),
            ParseError::Custom(s) => write!(formatter, "{}", s),
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
            RpnToken::Operator(_) => Err(ParseError::Static("expected expression, found operator")),
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
                    .ok_or_else(|| ParseError::Static("binary requires 2 arguments"))?
                    .to_expression()?;
                let lhs = stack
                    .pop()
                    .ok_or_else(|| ParseError::Static("binary requires 2 arguments"))?
                    .to_expression()?;
                let call = Expression::Binary(op, Box::new(lhs), Box::new(rhs));
                stack.push(RpnToken::Expression(call));
            }
        }
    }

    stack
        .pop()
        .ok_or_else(|| ParseError::Static("failed to parse RPN"))?
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
                        let param = InferParameter {
                            identifier: identifier.clone(),
                            type_annotation: None,
                        };
                        params.push(param);
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
        }
    }

    fn unread(&mut self, token: Token) {
        self.stack.push(token);
    }

    fn next_token(&mut self) -> ParseResult<Option<Token>> {
        let token = self
            .stack
            .pop()
            .map(|tok| Ok(Some(tok)))
            .unwrap_or_else(|| self.lexer.next_token().map_err(|err| err.into()));
        token
    }

    fn next_operator(&mut self) -> ParseResult<Option<BinaryOp>> {
        let token = self.next_token()?;
        Ok(token.and_then(|token| match token.kind {
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
        let params =
            self.parse_comma_separated(|parser| parser.parse_infer_parameter(), TokenKind::Or)?;
        let body = self.parse_expr()?;
        Ok(Expression::Lambda(params, Box::new(body)))
    }

    fn parse_primary_expr(&mut self) -> ParseResult<Expression> {
        let token = self.next_token_internal()?;
        match token.kind {
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
                        _ => Err(ParseError::Static("expected comma or close parenthesis")),
                    },
                    None => Err(ParseError::EOF),
                }
            }
            TokenKind::If => {
                let condition = self.parse_expr()?;
                self.parse_token(TokenKind::Then)?;
                let then_expr = self.parse_expr()?;
                self.parse_token(TokenKind::Else)?;
                let else_expr = self.parse_expr()?;
                Ok(Expression::If(
                    Box::new(condition),
                    Box::new(then_expr),
                    Box::new(else_expr),
                ))
            }
            TokenKind::Or => self.parse_lambda(),
            TokenKind::DoubleOr => {
                let params = Vec::new();
                let body = self.parse_expr()?;
                Ok(Expression::Lambda(params, Box::new(body)))
            }
            _ => {
                println!("{:?}", token);
                Err(ParseError::Custom(format!(
                    "unexpected token: {:?}",
                    token.kind
                )))
            }
        }
    }

    fn parse_token(&mut self, token_kind: TokenKind) -> ParseResult<bool> {
        let token = self.next_token_internal()?;
        match &token.kind {
            kind if kind == &token_kind => Ok(true),
            // TODO give better error
            _ => {
                println!("{:?}", token);
                Err(ParseError::Custom(format!(
                    "expected token {:?}, found {:?}",
                    token_kind, token.kind
                )))
            }
        }
    }

    fn parse_identifier(&mut self) -> ParseResult<String> {
        let token = self.next_token_internal()?;
        match token.kind {
            TokenKind::Identifier(s) => Ok(s),
            _ => {
                println!("warning: {:?}", token);
                Err(ParseError::Custom(format!(
                    "unexpected token: {:?}",
                    token.kind
                )))
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
            _ => Err(ParseError::Static("no type pattern starts with that token")),
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

    fn parse_function(&mut self, visibility: Visibility) -> ParseResult<Statement> {
        let name = self.parse_identifier()?;

        self.parse_token(TokenKind::OpenParen)?;
        let parameters =
            self.parse_comma_separated(|parser| parser.parse_identifier(), TokenKind::CloseParen)?;

        self.parse_token(TokenKind::Equal)?;
        let body = self.parse_expr()?;
        Ok(Statement::FunctionDeclaration {
            visibility,
            identifier: name,
            parameters,
            body,
        })
    }

    fn parse_statement_visibility(&mut self, visibility: Visibility) -> ParseResult<Statement> {
        let token = self.next_token_internal()?;
        match token.kind {
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
            TokenKind::Function => self.parse_function(visibility),
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
                        default_alias.to_string()
                    }
                } else {
                    default_alias.to_string()
                };

                Ok(Statement::Import { path, alias })
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

    // { statement ; statement ; [expression]? }

    fn parse_block_internal(&mut self, buf: &mut BlockData) -> ParseResult<()> {
        // Check if we're at the end
        let token = self.next_token_internal()?;
        match token.kind {
            TokenKind::CloseBrace => return Ok(()),
            _ => self.unread(token),
        }

        let statement = self.parse_statement()?;

        let token = self.next_token_internal()?;
        match token.kind {
            TokenKind::Semicolon => {
                buf.statements.push(statement);
                self.parse_block_internal(buf)
            }
            TokenKind::CloseBrace => match statement {
                Statement::Expression(expr) => {
                    buf.last_expr = Some(expr);
                    Ok(())
                }
                _ => Err(ParseError::Static(
                    "final non-semicolon-suffixed statement of a block must be an expression",
                )),
            },
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
                    self.parse_token(TokenKind::Semicolon)?;
                    statements.push(statement);
                }
            }
        }

        Ok(Type::Interface { statements })
    }

    pub fn parse_statements(&mut self) -> ParseResult<Vec<Statement>> {
        let mut buf = Vec::new();
        while let Some(token) = self.next_token()? {
            self.unread(token);
            let statement = self.parse_statement()?;
            self.parse_token(TokenKind::Semicolon)?;
            buf.push(statement);
        }
        Ok(buf)
    }
}
