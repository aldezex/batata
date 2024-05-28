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
    #[error("Unexpected token: {0}")]
    UnexpectedToken(String),
    #[error("Invalid let statement, expected {0}, found: {1}")]
    BadLetStatement(String, String),
    #[error("Expected semicolon")]
    ExpectedSemicolon,
    #[error("Expected token: {0}, found: {1}")]
    ExpectedToken(String, String),
    #[error("Expected Integer, found: {0}")]
    ExpectedInteger(String),
    #[error("Unexpected EOF")]
    UnexpectedEof,
    #[error("Error parsing function arguments: {0}")]
    ErrorParsingFunctionArguments(String),
    #[error("Function arguments should start enclosed in left parenthesis")]
    FunctionArgumentsStartWithLeftParenthesis,
    #[error("Function body should start enclosed in left brace")]
    FunctionBodyStartWithLeftBrace,
    #[error("Expected parameter name, found: {0}")]
    ExpectedParameterName(String),
    #[error("Let statements should have an identifier, which is the name of the variable being declared")]
    LetStatementsShouldHaveAnIdentifier,
    #[error("Let statements should be assigned with '=', found: {0}")]
    LetStatementsAreAssignedWithEqual(String),
    #[error("Let statements end with a semicolon")]
    LetStatementsEndWithSemicolon,
}
