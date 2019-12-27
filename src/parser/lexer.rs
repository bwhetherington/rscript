use crate::parser::Span;
use std::{error, fmt};

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Assign,
    Dot,
    DotDot,
    Function,
    Type,
    Let,
    Mut,
    If,
    Then,
    Else,
    Plus,
    PlusAssign,
    Minus,
    MinusAssign,
    Times,
    TimesAssign,
    Divide,
    DivideAssign,
    Mod,
    ModAssign,
    Equal,
    DoubleEqual,
    NotEqual,
    Not,
    BWNot,
    LT,
    LTE,
    GT,
    GTE,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Comma,
    Colon,
    DoubleColon,
    Arrow,
    And,
    Or,
    Xor,
    RShift,
    LShift,
    DoubleAnd,
    DoubleOr,
    Identifier(String),
    Int(i32),
    Float(f64),
    String(String),
    Boolean(bool),
    Public,
    Import,
    As,
    Class,
    While,
    Loop,
    Break,
    None,
    For,
    In,
}

const TOKENS: [(&'static str, TokenKind); 19] = [
    ("True", TokenKind::Boolean(true)),
    ("False", TokenKind::Boolean(false)),
    ("None", TokenKind::None),
    ("if", TokenKind::If),
    ("then", TokenKind::Then),
    ("else", TokenKind::Else),
    ("let", TokenKind::Let),
    ("mut", TokenKind::Mut),
    ("fn", TokenKind::Function),
    ("type", TokenKind::Type),
    ("pub", TokenKind::Public),
    ("import", TokenKind::Import),
    ("as", TokenKind::As),
    ("class", TokenKind::Class),
    ("while", TokenKind::While),
    ("loop", TokenKind::Loop),
    ("break", TokenKind::Break),
    ("for", TokenKind::For),
    ("in", TokenKind::In),
];

fn is_delimiter(ch: char) -> bool {
    match ch {
        '(' | ')' | '[' | ']' | '{' | '}' | '=' | '!' | '<' | '>' | ',' | ':' | ';' | '"'
        | '\'' | '+' | '-' | '*' | '/' | '%' | '|' | '.' => true,
        ch if ch.is_whitespace() => true,
        _ => false,
    }
}

fn is_identifier_char_first(ch: char) -> bool {
    match ch {
        '0'..='9' => false,
        ch => is_identifier_char(ch),
    }
}

fn is_identifier_char(ch: char) -> bool {
    match ch {
        'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => true,
        _ => false,
    }
}

fn is_identifier(s: &str) -> bool {
    let mut chars = s.chars();
    if let Some(ch) = chars.next() {
        if !is_identifier_char_first(ch) {
            false
        } else {
            chars.all(is_identifier_char)
        }
    } else {
        false
    }
}

pub type LexResult<T> = Result<T, LexError>;

#[derive(Debug)]
pub enum LexError {
    UnclosedString(Span),
    InvalidIdentifier(String, Span),
}

impl error::Error for LexError {}

impl fmt::Display for LexError {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "LexError: ")?;
        match self {
            LexError::UnclosedString(Span { col, row, len: _ }) => {
                write!(formatter, "unclosed string at {}, {}", col, row)
            }
            LexError::InvalidIdentifier(ident, Span { col, row, len: _ }) => write!(
                formatter,
                "invalid identifier \"{}\" at {}, {}",
                ident, row, col
            ),
        }
    }
}

pub struct Lexer<'a> {
    iter: Box<dyn Iterator<Item = char> + 'a>,
    read: Vec<char>,
    loc: (usize, usize),
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        Lexer {
            iter: Box::new(source.chars().peekable()),
            read: Vec::new(),
            loc: (1, 1),
        }
    }

    fn next_char(&mut self) -> Option<char> {
        let ch = self.read.pop().or_else(|| self.iter.next());
        match ch {
            Some('\n') => {
                self.loc.0 = 1;
                self.loc.1 += 1;
            }
            Some(_) => {
                self.loc.0 += 1;
            }
            _ => (),
        }
        ch
    }

    fn unread(&mut self, ch: char) {
        self.read.push(ch);
        match ch {
            '\n' => {
                self.loc.0 = 1;
                self.loc.1 -= 1;
            }
            _ => {
                self.loc.0 -= 1;
            }
        }
    }

    fn parse_string(&mut self) -> LexResult<(String, Span)> {
        let mut buf = String::new();
        let loc = (self.loc.0 - 1, self.loc.1);
        while let Some(ch) = self.next_char() {
            match ch {
                '$' => {
                    buf.push_str("\\$");
                }
                '\\' => {
                    self.next_char().map(|ch| buf.push(ch));
                }
                '"' => {
                    let len = buf.len() + 2;
                    return Ok((buf, Span::new(loc.0, loc.1, len)));
                }
                other => {
                    buf.push(other);
                }
            }
        }
        Err(LexError::UnclosedString(Span::new(
            loc.0,
            loc.1,
            buf.len() + 2,
        )))
    }

    fn parse_atom(&mut self) -> (String, Span) {
        let mut buf = String::new();
        let loc = self.loc;
        let mut prev = None;
        let mut is_numeric = true;
        while let Some(ch) = self.next_char() {
            // Check if everything thus far has been numeric
            is_numeric = is_numeric && prev.map(|ch: char| ch.is_numeric()).unwrap_or(true);
            match ch {
                ch if is_numeric && ch == '.' => buf.push(ch),
                ch if ch.is_whitespace() || is_delimiter(ch) => {
                    self.unread(ch);
                    let len = buf.len();
                    return (buf, Span::new(loc.0, loc.1, len));
                }
                ch => buf.push(ch),
            }
            prev = Some(ch);
        }
        let len = buf.len();
        (buf, Span::new(loc.0, loc.1, len))
    }

    fn parse_double(&mut self, default: TokenKind, seq: &[(char, TokenKind)]) -> LexResult<Token> {
        Ok(match self.next_char() {
            Some(ch) => seq
                .into_iter()
                .filter(|(next, _)| ch == *next)
                .map(|(_, token)| self.token(token.clone(), -2, 0, 2))
                .next()
                .unwrap_or_else(|| {
                    self.unread(ch);
                    self.token(default, -1, 0, 1)
                }),
            None => self.token(default, -1, 0, 1),
        })
    }

    fn token(&self, kind: TokenKind, col_offset: isize, row_offset: isize, len: usize) -> Token {
        let (col, row) = self.loc;
        let col = col as isize + col_offset;
        let row = row as isize + row_offset;
        Token {
            kind,
            span: Span::new(col as usize, row as usize, len),
        }
    }

    fn char_token(&self, token: TokenKind) -> LexResult<Token> {
        Ok(self.token(token, -1, 0, 1))
    }

    fn next_token_from_char(&mut self, ch: char) -> LexResult<Token> {
        use TokenKind::*;
        match ch {
            '(' => self.char_token(OpenParen),
            ')' => self.char_token(CloseParen),
            '[' => self.char_token(OpenBracket),
            ']' => self.char_token(CloseBracket),
            '{' => self.char_token(OpenBrace),
            '}' => self.char_token(CloseBrace),
            ',' => self.char_token(Comma),
            ';' => self.char_token(Semicolon),
            '^' => self.char_token(Xor),
            '~' => self.char_token(BWNot),
            '.' => self.parse_double(Dot, &[('.', DotDot)]),
            '&' => self.parse_double(And, &[('&', DoubleAnd)]),
            '|' => self.parse_double(Or, &[('|', DoubleOr)]),
            '!' => self.parse_double(Not, &[('=', NotEqual)]),
            '=' => self.parse_double(Equal, &[('=', DoubleEqual), ('>', Arrow)]),
            '+' => self.parse_double(Plus, &[('=', PlusAssign)]),
            '-' => self.parse_double(Minus, &[('=', MinusAssign)]),
            '*' => self.parse_double(Times, &[('=', TimesAssign)]),
            '/' => self.parse_double(Divide, &[('=', DivideAssign)]),
            '%' => self.parse_double(Mod, &[('=', ModAssign)]),
            '>' => self.parse_double(GT, &[('>', RShift), ('=', GTE)]),
            '<' => self.parse_double(LT, &[('<', LShift), ('=', LTE)]),
            ':' => self.parse_double(Colon, &[('=', Assign), (':', DoubleColon)]),
            '"' => self.parse_string().map(|(s, span)| Token {
                kind: String(s),
                span,
            }),
            other => {
                self.unread(other);
                let (atom, span) = self.parse_atom();
                if let Ok(num) = atom.parse::<f64>() {
                    return Ok(Token {
                        kind: Float(num),
                        span,
                    });
                }
                if let Ok(int) = atom.parse::<i32>() {
                    return Ok(Token {
                        kind: Int(int),
                        span,
                    });
                }
                for (key, value) in &TOKENS {
                    if &atom == key {
                        return Ok(Token {
                            kind: value.clone(),
                            span,
                        });
                    }
                }
                if is_identifier(&atom) {
                    Ok(Token {
                        kind: Identifier(atom),
                        span,
                    })
                } else {
                    Err(LexError::InvalidIdentifier(atom, span))
                }
            }
        }
    }

    pub fn tokens(&mut self) -> LexResult<Vec<Token>> {
        let mut tokens = Vec::new();
        while let Some(tok) = self.next_token()? {
            tokens.push(tok);
        }
        Ok(tokens)
    }
}

pub trait TokenIter {
    fn next_token(&mut self) -> LexResult<Option<Token>>;
}

impl<'a> TokenIter for Lexer<'a> {
    fn next_token(&mut self) -> LexResult<Option<Token>> {
        self.next_char()
            .map(|ch| match ch {
                ch if ch.is_whitespace() => self.next_token(),
                // Parse to the end of the line
                '#' => {
                    while let Some(ch) = self.next_char() {
                        if ch == '\n' {
                            break;
                        }
                    }
                    self.next_token()
                }
                ch => self.next_token_from_char(ch).map(|tok| Some(tok)),
            })
            .unwrap_or_else(|| Ok(None))
    }
}

impl<I> TokenIter for I
where
    I: Iterator<Item = Token>,
{
    fn next_token(&mut self) -> LexResult<Option<Token>> {
        Ok(self.next())
    }
}
