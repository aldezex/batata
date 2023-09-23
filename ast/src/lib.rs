use lexer::token::Token;

pub struct Program {
    pub statements: Vec<Statement>,
}

pub enum Statement {
    LetStatement(LetStatement),
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Expression>,
}

pub struct Identifier {
    pub token: String,
    pub value: String,
}

pub enum Expression {
    Identifier(Identifier),
}
