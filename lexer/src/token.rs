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
        }
    }
}
