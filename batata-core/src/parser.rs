mod error;
mod lexer;
mod tests;
mod token;

use itertools::peek_nth;

use crate::ast::{
    self,
    untyped::{Block, Expression, ExpressionKind, Infix, Module, Parameter, Parsed, Statement},
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
    tok0: Option<Span>,
    tok1: Option<Span>,
    pub newlines_position: Vec<u32>,
}

impl<T> Parser<T>
where
    T: Iterator<Item = LexerResult>,
{
    pub fn new(lexer: T) -> Self {
        let mut parser = Parser {
            tokens: lexer,
            lexer_errors: Vec::new(),
            tok0: None,
            tok1: None,
            newlines_position: Vec::new(),
        };

        parser.next_token();
        parser.next_token();
        parser
    }

    fn next_token(&mut self) -> Option<Span> {
        let tok = self.tok0.take();
        let mut next;

        loop {
            match self.tokens.next() {
                Some(Ok((start, Token::Newline, _))) => {
                    self.newlines_position.push(start);
                }

                Some(Err(err)) => {
                    self.lexer_errors.push(err);
                    next = None;
                    break;
                }

                Some(Ok(tok)) => {
                    next = Some(tok);
                    break;
                }

                None => {
                    next = None;
                    break;
                }
            }
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
            if tok.1 == Token::Newline || tok.1 == Token::Semicolon {
                self.next_token();
                continue;
            }

            let statement = self.parse_statement(tok)?;
            program.statements.push(statement);
            self.next_token();
        }

        Ok(program)
    }

    fn parse_statement(&mut self, token: Span) -> Result<Statement, ParseError> {
        match token.1 {
            Token::Let => self.parse_let_statement(),
            Token::LBrace => {
                let block = self.parse_block_statement()?;
                Ok(Statement::Block(block))
            }
            Token::Fn => self.parse_function_statement(),
            _ => self.parse_expression_statement(token),
        }
    }

    fn parse_block_statement(&mut self) -> Result<Block, ParseError> {
        self.next_token();
        let mut statements = Vec::new();

        while let Some(tok) = self.tok0.take() {
            if tok.1 == Token::RBrace {
                break;
            }

            let statement = self.parse_statement(tok)?;
            statements.push(statement);
            self.next_token();
        }

        Ok(Block { statements })
    }

    fn parse_function_statement(&mut self) -> Result<Statement, ParseError> {
        let name = self.expect_identifier()?;

        if !self.peek_token_is(Token::LParen) {
            return Err(ParseError::FunctionArgumentsStartWithLeftParenthesis);
        }

        self.next_token();
        self.next_token();

        let parameters = self.parse_parameters()?;

        if !self.curr_token_is(Token::LBrace) {
            return Err(ParseError::FunctionBodyStartWithLeftBrace);
        }

        let body = self.parse_block_statement()?;

        Ok(Statement::Function(ast::untyped::Function {
            name,
            parameters,
            body,
        }))
    }

    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, ParseError> {
        let mut parameters = Vec::new();

        while self.peek_token_is(Token::Comma) || self.peek_token_is(Token::RParen) {
            let param = self.extract_parameter()?;
            parameters.push(param);

            self.next_token();
            self.next_token();
        }

        Ok(parameters)
    }

    fn extract_parameter(&mut self) -> Result<Parameter, ParseError> {
        match self.tok0.take() {
            Some(tok) => match tok.1 {
                Token::Identifier { name } | Token::DiscardIdentifier { name } => {
                    Ok(Parameter { name })
                }
                _ => Err(ParseError::ExpectedParameterName(tok.1.to_string())),
            },
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        let name = self.expect_identifier()?;

        if !self.peek_token_is(Token::Assign) {
            return Err(ParseError::LetStatementsAreAssignedWithEqual(
                self.tok0.as_ref().unwrap().1.to_string(),
            ));
        }

        // consume tokens and reset the stream
        self.next_token();
        self.next_token();

        if let Some(token) = self.tok0.take() {
            let value = self.parse_expression(token, 0)?;
            self.next_token();

            Ok(Statement::Definition(ast::untyped::Definition {
                name,
                value,
            }))
        } else {
            Err(ParseError::UnexpectedEof)
        }
    }

    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        self.next_token();

        let name = match self.tok0.take() {
            Some(tok) => match tok.1 {
                Token::Identifier { name } | Token::DiscardIdentifier { name } => Ok(name),
                _ => Err(ParseError::LetStatementsShouldHaveAnIdentifier),
            },
            None => Err(ParseError::UnexpectedEof),
        };

        name
    }

    fn parse_expression_statement(&mut self, token: Span) -> Result<Statement, ParseError> {
        let expression = self.parse_expression(token, 0)?;

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Expression(expression))
    }

    fn parse_expression(&mut self, token: Span, precedence: u8) -> Result<Expression, ParseError> {
        let mut left = self.parse_prefix(token)?;

        while let Some(tok) = self.tok1.take() {
            let next_precedence = tok.1.get_precedence();
            if precedence >= next_precedence {
                break;
            }

            self.next_token();
            left = self.parse_infix(tok, left)?;
        }

        Ok(left)
    }

    fn parse_prefix(&mut self, token: Span) -> Result<Expression, ParseError> {
        match token.1 {
            Token::Int { value } => Ok(Expression {
                kind: ExpressionKind::Integer(value),
            }),
            Token::Identifier { name } => Ok(Expression {
                kind: ExpressionKind::Identifier(name),
            }),
            Token::DiscardIdentifier { name } => Ok(Expression {
                kind: ExpressionKind::DiscardIdentifier(name),
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

    fn peek_token_is(&mut self, token: Token) -> bool {
        self.tok1.as_ref().map_or(false, |tok| tok.1 == token)
    }

    fn curr_token_is(&mut self, token: Token) -> bool {
        self.tok0.as_ref().map_or(false, |tok| tok.1 == token)
    }

    fn current_precedence(&self) -> u8 {
        self.tok0.as_ref().map_or(0, |tok| tok.1.get_precedence())
    }

    fn peek_token_precedence(&self) -> u8 {
        self.tok1.as_ref().map_or(0, |tok| tok.1.get_precedence())
    }
}
