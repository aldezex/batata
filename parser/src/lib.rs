use std::mem;

use anyhow::{Ok, Result};

use ast::{Expression, Identifier, LetStatement, Program, ReturnStatement, Statement};
use lexer::{token::Token, Lexer};

struct Parser {
    lexer: Lexer,
    current_token: Token,
    next_token: Token,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Result<Self> {
        let current_token = lexer.next_token()?;
        let next_token = lexer.next_token()?;

        Ok(Parser {
            lexer,
            current_token,
            next_token,
        })
    }

    fn parse_program(&mut self) -> Result<Program> {
        let mut program = Program {
            statements: Vec::new(),
        };

        while self.current_token != Token::Eof {
            let statement = self.parse_statement()?;
            program.statements.push(statement);
            self.step()?;
        }

        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => unimplemented!(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement> {
        let next_valid = matches!(
            (&self.next_token, Token::Ident("".to_string())),
            (Token::Ident(_), Token::Ident(_))
        );

        if !next_valid {
            return Err(anyhow::anyhow!("failed to parse let statement"));
        }

        self.step()?;

        let name = Identifier {
            token: self.current_token.to_string(),
            value: self.current_token.literal(),
        };

        let statement = Ok(Statement::LetStatement(LetStatement {
            token: Token::Let,
            name,
            value: None,
        }));

        while !self.current_token_is(Token::Semicolon) {
            self.step()?;
        }

        statement
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        let return_statement = Ok(Statement::ReturnStatement(ReturnStatement {
            token: self.next_token.clone(),
        }));

        while !self.current_token_is(Token::Semicolon) {
            self.step()?;
        }

        return_statement
    }

    fn step(&mut self) -> Result<()> {
        self.current_token = self.lexer.next_token()?;
        mem::swap(&mut self.current_token, &mut self.next_token);

        Ok(())
    }

    fn current_token_is(&self, token: Token) -> bool {
        self.current_token == token
    }

    // fn expect_peek(&mut self, token: Token) -> Result<bool> {
    //     if self.peek_token_is(token) {
    //         self.step()?;
    //         Ok(true)
    //     } else {
    //         Ok(false)
    //     }
    // }

    fn peek_token_is(&self, token: Token) -> bool {
        println!("peek_token_is: {:?} == {:?}", self.next_token, token);

        self.next_token == token
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_let_statement() -> Result<()> {
        let input = r#"
            let maki;
            let tates;
        "#;
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer)?;

        let program = parser.parse_program()?;
        assert_eq!(program.statements.len(), 2);

        let names = ["maki", "tates"];

        for (index, statement) in program.statements.iter().enumerate() {
            match statement {
                Statement::LetStatement(let_statement) => {
                    assert_eq!(let_statement.token, Token::Let);
                    assert!(let_statement.name.value == names[index]);
                }
                _ => panic!("expected let statement"),
            }
        }

        Ok(())
    }

    #[test]
    fn parse_return_statement() -> Result<()> {
        let input = r#"
            return;
            return 5;
        "#;
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer)?;

        let program = parser.parse_program()?;
        assert_eq!(program.statements.len(), 2);

        let returns = [
            ReturnStatement {
                token: Token::Semicolon,
            },
            ReturnStatement {
                token: Token::Int("5".into()),
            },
        ];

        for (index, statement) in program.statements.iter().enumerate() {
            match statement {
                Statement::ReturnStatement(return_statement) => {
                    assert_eq!(return_statement.token, returns[index].token);
                }
                _ => panic!("expected return statement"),
            }
        }

        Ok(())
    }
}
