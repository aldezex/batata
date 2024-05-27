use std::{fmt::Display, str::FromStr};

use super::error::LexicalError;

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier { name: String },
    DiscardIdentifier { name: String },
    String { value: String },
    Int { value: String },
    Float { value: String },

    // keywords
    Import,
    Let,
    Mut,
    Fn,
    Return,
    If,
    Else,

    // enclosures
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // separators
    Newline,
    Semicolon,
    Colon,
    Dot,

    // operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    And,
    Or,
    Bang,
    Assign,

    // terminals
    Eof,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ts = match self {
            Token::Identifier { name } => name,
            Token::DiscardIdentifier { name } => name,
            Token::String { value } => value,
            Token::Int { value } => value,
            Token::Float { value } => value,
            Token::Import => "import",
            Token::Let => "let",
            Token::Mut => "mut",
            Token::Fn => "fn",
            Token::Return => "return",
            Token::If => "if",
            Token::Else => "else",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBrace => "{",
            Token::RBrace => "}",
            Token::LBracket => "[",
            Token::RBracket => "]",
            Token::Newline => "newline",
            Token::Semicolon => ";",
            Token::Colon => ":",
            Token::Dot => ".",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Star => "*",
            Token::Slash => "/",
            Token::Percent => "%",
            Token::Equal => "==",
            Token::NotEqual => "!=",
            Token::LessThan => "<",
            Token::LessThanEqual => "<=",
            Token::GreaterThan => ">",
            Token::GreaterThanEqual => ">=",
            Token::And => "&&",
            Token::Or => "||",
            Token::Bang => "!",
            Token::Eof => "EOF",
            Token::Assign => "=",
        };

        write!(f, "{ts}")
    }
}

impl FromStr for Token {
    type Err = LexicalError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "import" => Ok(Token::Import),
            "let" => Ok(Token::Let),
            "mut" => Ok(Token::Mut),
            "fn" => Ok(Token::Fn),
            "return" => Ok(Token::Return),
            "if" => Ok(Token::If),
            "else" => Ok(Token::Else),
            _ => {
                if s.starts_with('_') {
                    Ok(Token::DiscardIdentifier {
                        name: s.to_string(),
                    })
                } else if s.starts_with('"') && s.ends_with('"') {
                    Ok(Token::String {
                        value: s[1..s.len() - 1].to_string(),
                    })
                } else {
                    Ok(Token::Identifier {
                        name: s.to_string(),
                    })
                }
            }
        }
    }
}

impl Token {
    pub fn get_precedence(&self) -> u8 {
        match self {
            Token::Assign => 1,
            Token::Or => 2,
            Token::And => 3,
            Token::Equal | Token::NotEqual => 4,
            Token::LessThan
            | Token::LessThanEqual
            | Token::GreaterThan
            | Token::GreaterThanEqual => 5,
            Token::Plus | Token::Minus => 6,
            Token::Star | Token::Slash | Token::Percent => 7,
            _ => 0,
        }
    }
}
