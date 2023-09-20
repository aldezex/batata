use std::fmt::{Display, Formatter, Result};

#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal,
    Eof,

    Ident(String),
    Int(String),
    Float(String),

    Assign,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Bang,

    Equal,
    StrictEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,

    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    Function,
    Let,
    Const,
    Var,
    If,
    Else,
    Return,
    Async,
    Await,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use Token::*;

        match self {
            Illegal => write!(f, "Illegal"),
            Eof => write!(f, "EOF"),

            Ident(s) => write!(f, "Ident({})", s),
            Int(i) => write!(f, "Int({})", i),
            Float(fl) => write!(f, "Float({})", fl),

            Assign => write!(f, "Assign"),
            Plus => write!(f, "Plus"),
            Minus => write!(f, "Minus"),
            Asterisk => write!(f, "Asterisk"),
            Slash => write!(f, "Slash"),

            Bang => write!(f, "Bang"),

            Equal => write!(f, "Equal"),
            StrictEqual => write!(f, "StrictEqual"),
            LessThan => write!(f, "LessThan"),
            GreaterThan => write!(f, "GreaterThan"),
            LessThanEqual => write!(f, "LessThanEqual"),
            GreaterThanEqual => write!(f, "GreaterThanEqual"),

            Comma => write!(f, "Comma"),
            Semicolon => write!(f, "Semicolon"),

            Lparen => write!(f, "Lparen"),
            Rparen => write!(f, "Rparen"),
            Lbrace => write!(f, "Lbrace"),
            Rbrace => write!(f, "Rbrace"),

            Function => write!(f, "Function"),
            Let => write!(f, "Let"),
            Const => write!(f, "Const"),
            Var => write!(f, "Var"),
            If => write!(f, "If"),
            Else => write!(f, "Else"),
            Return => write!(f, "Return"),
            Async => write!(f, "Async"),
            Await => write!(f, "Await"),
        }
    }
}
