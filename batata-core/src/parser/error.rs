use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum LexicalError {
    #[error("Invalid token: {0}")]
    UnrecognizedToken(String),
    #[error("Invalid float number: {0}, detected more than one '.'")]
    InvalidFloatNumberMultipleDots(String),
    #[error("Invalid number: {0}, underscores not matched")]
    InvalidNumberUnderscores(String),
    #[error("Invalid number: {0}, underscores not allowed at the beginning or end")]
    InvalidNumberUnderscoresAtBeginningOrEnd(String),
    #[error("Invalid number: {0}, underscores not allowed after '.'")]
    InvalidNumberUnderscoresAfterDot(String),
    #[error("Invalid number: {0}, underscores not allowed before '.'")]
    InvalidNumberUnderscoresBeforeDot(String),
    #[error("Invalid identifier: {0}, can't start with a digit")]
    InvalidIdentifierStartsWithDigit(String),
}

#[derive(Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("Invalid token: {0}")]
    InvalidToken(String),
    #[error("Invalid let statement, expected {0}, found: {1}")]
    BadLetStatement(String, String),
    #[error("Expected semicolon")]
    ExpectedSemicolon,
    #[error("Expected token: {0}, found: {1}")]
    ExpectedToken(String, String),
    #[error("Expected Integer, found: {0}")]
    ExpectedInteger(String),
}

