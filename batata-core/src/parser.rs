mod error;
mod lexer;
mod tests;
mod token;

use crate::ast::{
    self,
    untyped::{Expression, ExpressionKind, Infix, Module, Parsed, Statement},
};

use self::{
    error::{LexicalError, ParseError},
    lexer::{make_tokenizer, LexerResult, Span},
    token::Token,
};

pub fn parse_module(input: &str) -> Result<Parsed, ParseError> {
    let lexer = make_tokenizer(input);
    let mut parser = Parser::new(lexer);
    let parsed = parser.parse_program()?;
    Ok(ast::untyped::Parsed { module: parsed })
}

pub struct Parser<T: Iterator<Item = LexerResult>> {
    tokens: T,
    lexer_errors: Vec<LexicalError>,
    parser_errors: Vec<ParseError>,
    tok0: Option<Span>,
    tok1: Option<Span>,
}

impl<T> Parser<T>
where
    T: Iterator<Item = LexerResult>,
{
    pub fn new(lexer: T) -> Self {
        let mut parser = Parser {
            tokens: lexer,
            lexer_errors: Vec::new(),
            parser_errors: Vec::new(),
            tok0: None,
            tok1: None,
        };

        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self) -> Option<Span> {
        let tok = self.tok0.take();
        let mut next;

        match self.tokens.next() {
            Some(Err(err)) => {
                self.lexer_errors.push(err);
                next = None;
            }

            Some(Ok(tok)) => {
                next = Some(tok);
            }

            None => next = None,
        }

        self.tok0 = self.tok1.take();
        self.tok1 = next.take();
        tok
    }

    fn parse_program(&mut self) -> Result<Module, ParseError> {
        let mut program = Module {
            statements: Vec::new(),
        };

        while let Some(tok) = self.tok0.take() {
            let statement = self.parse_statement(tok)?;
            program.statements.push(statement);
            self.next_token();
        }

        Ok(program)
    }

    fn parse_statement(&mut self, token: Span) -> Result<Statement, ParseError> {
        match token.1 {
            _ => self.parse_expression_statement(token),
        }
    }

    fn parse_expression_statement(&mut self, token: Span) -> Result<Statement, ParseError> {
        let expression = self.parse_expression(token, 0)?;

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Expression(expression))
    }

    fn parse_expression(&mut self, token: Span, precedence: u8) -> Result<Expression, ParseError> {
        let mut prefix = self.parse_prefix(token)?;

        while let Some(tok) = self.tok1.clone() {
            let next_precedence = self.peek_token_precedence();
            if precedence >= next_precedence {
                break;
            }

            self.next_token();
            prefix = self.parse_infix(tok, prefix)?;
        }

        Ok(prefix)
    }

    fn parse_prefix(&mut self, token: Span) -> Result<Expression, ParseError> {
        match token.1 {
            Token::Int { value } => Ok(Expression {
                kind: ExpressionKind::Integer(value),
            }),
            _ => Err(ParseError::InvalidToken(token.1.to_string())),
        }
    }

    fn parse_infix(&mut self, tok: Span, left: Expression) -> Result<Expression, ParseError> {
        let precedence = tok.1.get_precedence();
        self.next_token();

        let right_token = self.tok0.take().ok_or(ParseError::UnexpectedEof);
        let right_exp = self.parse_expression(right_token?, precedence)?;

        Ok(Expression {
            kind: ExpressionKind::Infix(Infix {
                left: Box::new(left),
                operator: tok.1.to_string(),
                right: Box::new(right_exp),
            }),
        })
    }

    fn peek_token_is(&self, token: Token) -> bool {
        self.tok1.as_ref().map_or(false, |tok| tok.1 == token)
    }

    fn current_precedence(&self) -> u8 {
        self.tok0.as_ref().map_or(0, |tok| tok.1.get_precedence())
    }

    fn peek_token_precedence(&self) -> u8 {
        self.tok1.as_ref().map_or(0, |tok| tok.1.get_precedence())
    }
}
