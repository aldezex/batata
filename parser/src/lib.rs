use std::{collections::HashMap, mem};

use anyhow::{Ok, Result};

use ast::{
    Expression, Identifier, Infix, LetStatement, Prefix, Program, ReturnStatement, Statement,
};
use lexer::{token::Token, Lexer};

struct Parser {
    lexer: Lexer,
    current_token: Token,
    next_token: Token,
    precedences: HashMap<Token, Precedence>,
}

#[derive(PartialEq, PartialOrd, Clone, Debug)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Result<Self> {
        let current_token = lexer.next_token()?;
        let next_token = lexer.next_token()?;

        let mut precedences = HashMap::new();
        precedences.insert(Token::Equal, Precedence::Equals);
        precedences.insert(Token::NotEqual, Precedence::Equals);
        precedences.insert(Token::LessThan, Precedence::LessGreater);
        precedences.insert(Token::GreaterThan, Precedence::LessGreater);
        precedences.insert(Token::Plus, Precedence::Sum);
        precedences.insert(Token::Minus, Precedence::Sum);
        precedences.insert(Token::Slash, Precedence::Product);
        precedences.insert(Token::Asterisk, Precedence::Product);

        Ok(Parser {
            lexer,
            current_token,
            next_token,

            precedences,
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
            _ => self.parse_expression_statement(),
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

        // let mut statement = Ok(Statement::LetStatement(LetStatement {
        //     identifier: name.value,
        //     expression: None,
        // }));

        // while !self.current_token_is(Token::Assign) && !self.current_token_is(Token::Semicolon) {
        //     self.step()?;
        // }

        self.step()?;

        match self.current_token {
            Token::Assign => {
                self.step()?;
                let exp = self.parse_expression_statement()?;

                Ok(Statement::LetStatement(LetStatement {
                    identifier: name.value,
                    expression: match exp {
                        Statement::ExpressionStatement(expression) => Some(expression),
                        _ => None,
                    },
                }))
            }
            Token::Semicolon => Ok(Statement::LetStatement(LetStatement {
                identifier: name.value,
                expression: None,
            })),
            _ => Err(anyhow::anyhow!("failed to parse let statement")),
        }
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

    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(Token::Semicolon) {
            self.step()?;
        }

        Ok(Statement::ExpressionStatement(expression))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        let current_token = self.current_token.clone();
        let prefix = self.get_prefix_fn(current_token);

        match prefix {
            Some(prf) => {
                let mut left_exp = prf(self)?;

                while !self.peek_token_is(Token::Semicolon) && precedence < self.peek_precedence() {
                    let next_token = self.next_token.clone();
                    let infix = self.get_infix_fn(next_token);

                    match infix {
                        Some(inf) => {
                            self.step()?;
                            left_exp = inf(self, left_exp)?;
                        }
                        None => return Ok(prf(self)?),
                    }
                }

                Ok(left_exp)
            }
            None => Err(anyhow::anyhow!("failed to parse expression")),
        }
    }

    fn parse_integer_literal(&mut self) -> Result<Expression> {
        let exp = Expression::IntegerLiteral(self.current_token.literal().parse()?);
        Ok(exp)
    }

    fn parse_identifier(&mut self) -> Result<Expression> {
        let exp = Expression::Identifier(Identifier {
            token: self.current_token.to_string(),
            value: self.current_token.literal(),
        });

        Ok(exp)
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression> {
        let lit = self.current_token.literal();
        let curr = self.current_token.clone();

        self.step()?;

        let exp = Expression::Prefix(Prefix {
            token: curr,
            operator: lit,
            right: Box::new(self.parse_expression(Precedence::Prefix)?),
        });

        Ok(exp)
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression> {
        let curr = self.current_token.clone();
        let lit = self.current_token.literal();
        let precedence = self.curr_precedence();

        self.step()?;

        let exp = Expression::Infix(Infix {
            token: curr,
            left: Box::new(left),
            operator: lit,
            right: Box::new(self.parse_expression(precedence)?),
        });

        Ok(exp)
    }

    fn step(&mut self) -> Result<()> {
        self.current_token = self.lexer.next_token()?;
        mem::swap(&mut self.current_token, &mut self.next_token);

        Ok(())
    }

    fn current_token_is(&self, token: Token) -> bool {
        self.current_token == token
    }

    fn peek_token_is(&self, token: Token) -> bool {
        self.next_token == token
    }

    fn peek_precedence(&self) -> Precedence {
        if let Some(p) = self.precedences.get(&self.next_token) {
            p.clone()
        } else {
            Precedence::Lowest
        }
    }

    fn curr_precedence(&self) -> Precedence {
        if let Some(p) = self.precedences.get(&self.current_token) {
            p.clone()
        } else {
            Precedence::Lowest
        }
    }

    fn get_prefix_fn(&self, token: Token) -> Option<fn(&mut Self) -> Result<Expression>> {
        match token {
            Token::Int(_) => {
                return Some(Self::parse_integer_literal);
            }
            Token::Bang | Token::Minus => {
                return Some(Self::parse_prefix_expression);
            }
            Token::Ident(_) => {
                return Some(Self::parse_identifier);
            }
            _ => None,
        }
    }

    fn get_infix_fn(
        &self,
        token: Token,
    ) -> Option<fn(&mut Self, Expression) -> Result<Expression>> {
        match token {
            Token::Plus => Some(Self::parse_infix_expression),
            Token::Minus => Some(Self::parse_infix_expression),
            Token::Slash => Some(Self::parse_infix_expression),
            Token::Asterisk => Some(Self::parse_infix_expression),
            Token::Equal => Some(Self::parse_infix_expression),
            Token::NotEqual => Some(Self::parse_infix_expression),
            Token::LessThan => Some(Self::parse_infix_expression),
            Token::GreaterThan => Some(Self::parse_infix_expression),
            _ => None,
        }
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
                    assert_eq!(let_statement.identifier, names[index]);
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
            return "hello";
            return 'world';
            return 5 + 5;
        "#;
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer)?;

        let program = parser.parse_program()?;
        assert_eq!(program.statements.len(), 5);

        let returns = [
            ReturnStatement {
                token: Token::Semicolon,
            },
            ReturnStatement {
                token: Token::Int(5),
            },
            ReturnStatement {
                token: Token::Str("hello".to_string()),
            },
            ReturnStatement {
                token: Token::Str("world".to_string()),
            },
            ReturnStatement {
                token: Token::Int(5),
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

    #[test]
    fn test_integer_literals() -> Result<()> {
        let input = r#"
            5;
        "#;
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer)?;

        let program = parser.parse_program()?;
        assert_eq!(program.statements.len(), 1);

        let integer = Expression::IntegerLiteral(5);

        for statement in program.statements.iter() {
            match statement {
                Statement::ExpressionStatement(expression) => {
                    assert_eq!(expression, &integer);
                }
                _ => panic!("expected expression statement"),
            }
        }

        Ok(())
    }

    #[test]
    fn test_simple_let_integers() -> Result<()> {
        let input = r#"
            let uwu;
            let x = 5;
            let y = 10;
            let foobar = 838383;
        "#;

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer)?;

        let program = parser.parse_program()?;

        assert_eq!(program.statements.len(), 4);

        let names = ["uwu", "x", "y", "foobar"];
        let values = [None, Some(5), Some(10), Some(838383)];

        for (index, statement) in program.statements.iter().enumerate() {
            match statement {
                Statement::LetStatement(let_statement) => {
                    assert_eq!(let_statement.identifier, names[index]);
                    assert_eq!(
                        let_statement.expression,
                        values[index].map(Expression::IntegerLiteral)
                    );
                }
                _ => panic!("expected let statement"),
            }
        }

        Ok(())
    }

    #[test]
    fn test_prefix() -> Result<()> {
        let input = r#"
            !5;
            -15;
        "#;

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer)?;

        let program = parser.parse_program()?;
        assert_eq!(program.statements.len(), 2);

        let prefix = [
            Expression::Prefix(Prefix {
                token: Token::Bang,
                operator: "!".to_string(),
                right: Box::new(Expression::IntegerLiteral(5)),
            }),
            Expression::Prefix(Prefix {
                token: Token::Minus,
                operator: "-".to_string(),
                right: Box::new(Expression::IntegerLiteral(15)),
            }),
        ];

        for (index, statement) in program.statements.iter().enumerate() {
            match statement {
                Statement::ExpressionStatement(expression) => {
                    assert_eq!(expression, &prefix[index]);
                }
                _ => panic!("expected expression statement"),
            }
        }

        Ok(())
    }

    #[test]
    fn test_infix() -> Result<()> {
        let input = r#"
            5 + 5;
            5 - 5;
            5 * 5;
            5 / 5;
            5 > 5;
            5 < 5;
            5 == 5;
            5 != 5;
        "#;

        let lexer = Lexer::new(input.into());

        let mut parser = Parser::new(lexer)?;

        let program = parser.parse_program()?;

        assert_eq!(program.statements.len(), 8);

        let infix = [
            Expression::Infix(Infix {
                token: Token::Plus,
                left: Box::new(Expression::IntegerLiteral(5)),
                operator: "+".to_string(),
                right: Box::new(Expression::IntegerLiteral(5)),
            }),
            Expression::Infix(Infix {
                token: Token::Minus,
                left: Box::new(Expression::IntegerLiteral(5)),
                operator: "-".to_string(),
                right: Box::new(Expression::IntegerLiteral(5)),
            }),
            Expression::Infix(Infix {
                token: Token::Asterisk,
                left: Box::new(Expression::IntegerLiteral(5)),
                operator: "*".to_string(),
                right: Box::new(Expression::IntegerLiteral(5)),
            }),
            Expression::Infix(Infix {
                token: Token::Slash,
                left: Box::new(Expression::IntegerLiteral(5)),
                operator: "/".to_string(),
                right: Box::new(Expression::IntegerLiteral(5)),
            }),
            Expression::Infix(Infix {
                token: Token::GreaterThan,
                left: Box::new(Expression::IntegerLiteral(5)),
                operator: ">".to_string(),
                right: Box::new(Expression::IntegerLiteral(5)),
            }),
            Expression::Infix(Infix {
                token: Token::LessThan,
                left: Box::new(Expression::IntegerLiteral(5)),
                operator: "<".to_string(),
                right: Box::new(Expression::IntegerLiteral(5)),
            }),
            Expression::Infix(Infix {
                token: Token::Equal,
                left: Box::new(Expression::IntegerLiteral(5)),
                operator: "==".to_string(),
                right: Box::new(Expression::IntegerLiteral(5)),
            }),
            Expression::Infix(Infix {
                token: Token::NotEqual,
                left: Box::new(Expression::IntegerLiteral(5)),
                operator: "!=".to_string(),
                right: Box::new(Expression::IntegerLiteral(5)),
            }),
        ];

        for (index, statement) in program.statements.iter().enumerate() {
            match statement {
                Statement::ExpressionStatement(expression) => {
                    assert_eq!(expression, &infix[index]);
                }
                _ => panic!("expected expression statement"),
            }
        }

        Ok(())
    }

    #[test]
    fn test_operator_precedence_parsing() -> Result<()> {
        let tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            // ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            // ("(5 + 5) * 2", "((5 + 5) * 2)"),
            // ("2 / (5 + 5)", "(2 / (5 + 5))"),
            // ("-(5 + 5)", "(-(5 + 5))"),
            // ("!(true == true)", "(!(true == true))"),
            // ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
        ];

        for test in tests.iter() {
            let lexer = Lexer::new(test.0.into());
            let mut parser = Parser::new(lexer)?;

            let program = parser.parse_program()?;
            assert_eq!(program.to_string(), test.1);
        }

        Ok(())
    }
}
