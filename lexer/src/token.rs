use std::fmt::{Display, Formatter, Result};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Illegal,
    Eof,

    Ident(String),
    Int(isize),
    Float(f32),
    Str(String),

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

    DoubleQuote,
    SingleQuote,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use Token::*;

        match self {
            Illegal => write!(f, "illegal"),
            Eof => write!(f, "eof"),

            Ident(s) => write!(f, "ident({})", s),
            Int(i) => write!(f, "int({})", i),
            Float(fl) => write!(f, "float({})", fl),
            Str(s) => write!(f, "str({})", s),

            Assign => write!(f, "assign"),
            Plus => write!(f, "plus"),
            Minus => write!(f, "minus"),
            Asterisk => write!(f, "asterisk"),
            Slash => write!(f, "slash"),

            Bang => write!(f, "bang"),

            Equal => write!(f, "equal"),
            StrictEqual => write!(f, "strictEqual"),
            LessThan => write!(f, "lessThan"),
            GreaterThan => write!(f, "greaterThan"),
            LessThanEqual => write!(f, "lessThanEqual"),
            GreaterThanEqual => write!(f, "greaterThanEqual"),

            Comma => write!(f, "comma"),
            Semicolon => write!(f, "semicolon"),

            Lparen => write!(f, "lparen"),
            Rparen => write!(f, "rparen"),
            Lbrace => write!(f, "lbrace"),
            Rbrace => write!(f, "rbrace"),

            Function => write!(f, "function"),
            Let => write!(f, "let"),
            Const => write!(f, "const"),
            Var => write!(f, "var"),
            If => write!(f, "if"),
            Else => write!(f, "else"),
            Return => write!(f, "return"),
            Async => write!(f, "async"),
            Await => write!(f, "await"),

            DoubleQuote => write!(f, "doubleQuote"),
            SingleQuote => write!(f, "singleQuote"),
        }
    }
}

impl Token {
    pub fn literal(&self) -> String {
        use Token::*;

        match self {
            Ident(s) => s.clone(),
            Int(i) => i.to_string(),
            Float(fl) => fl.to_string(),
            Return => "return".to_string(),
            Str(s) => s.clone(),
            _ => String::new(),
        }
    }
}
