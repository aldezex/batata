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
        Ok(Statement::Expression(expression))
    }

    fn parse_expression(&mut self, token: Span, precedence: u8) -> Result<Expression, ParseError> {
        let mut prefix = self.parse_prefix(token)?;

        while !self.peek_token_is(Token::Semicolon) && precedence < self.peek_token_precedence() {
            if let Some(tok) = self.tok1.take() {
                let infix = self.parse_infix(tok, prefix)?;
                prefix = infix;
            }
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
        let precedence = self.current_precedence();
        self.next_token();
        self.next_token();

        if let Some(token) = self.tok0.take() {
            let right = self.parse_expression(token, precedence)?;
            let expression = Expression {
                kind: ExpressionKind::Infix(Infix {
                    left: Box::new(left),
                    operator: tok.1.to_string(),
                    right: Box::new(right),
                }),
            };

            return Ok(expression);
        }

        Err(ParseError::InvalidToken(tok.1.to_string()))
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

/*
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
                expression: Expression::Empty,
            },
            ReturnStatement {
                expression: Expression::IntegerLiteral(5),
            },
            ReturnStatement {
                expression: Expression::StringLiteral("hello".to_string()),
            },
            ReturnStatement {
                expression: Expression::StringLiteral("world".to_string()),
            },
            ReturnStatement {
                expression: Expression::Infix(Infix {
                    token: Token::Plus,
                    left: Box::new(Expression::IntegerLiteral(5)),
                    operator: "+".to_string(),
                    right: Box::new(Expression::IntegerLiteral(5)),
                }),
            },
        ];

        for (index, statement) in program.statements.iter().enumerate() {
            match statement {
                Statement::ReturnStatement(return_statement) => {
                    assert_eq!(return_statement, &returns[index]);
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

        let names = ["uwu", "x", "y", "foobar", "maki", "bool"];
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
    fn test_more_let() -> Result<()> {
        let input = r#"
            let maki = 5;
            let tates = maki;
            let test = "hello world";
            let bool = true;
        "#;

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer)?;

        let program = parser.parse_program()?;

        assert_eq!(program.statements.len(), 4);

        let names = ["maki", "tates", "test", "bool"];
        let values = [
            Some(Expression::IntegerLiteral(5)),
            Some(Expression::Identifier(Identifier {
                token: "ident(maki)".to_string(),
                value: "maki".to_string(),
            })),
            Some(Expression::StringLiteral("hello world".to_string())),
            Some(Expression::Boolean(true)),
        ];

        for (index, statement) in program.statements.iter().enumerate() {
            match statement {
                Statement::LetStatement(let_statement) => {
                    assert_eq!(let_statement.identifier, names[index]);
                    assert_eq!(let_statement.expression, values[index]);
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
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
        ];

        for test in tests.iter() {
            let lexer = Lexer::new(test.0.into());
            let mut parser = Parser::new(lexer)?;

            let program = parser.parse_program()?;
            assert_eq!(program.to_string(), test.1);
        }

        Ok(())
    }

    #[test]
    fn parse_booleans() -> Result<()> {
        let input = r#"
            true;
            false;
        "#;

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer)?;

        let program = parser.parse_program()?;

        assert_eq!(program.statements.len(), 2);

        let booleans = [Expression::Boolean(true), Expression::Boolean(false)];

        for (index, statement) in program.statements.iter().enumerate() {
            match statement {
                Statement::ExpressionStatement(expression) => {
                    assert_eq!(expression, &booleans[index]);
                }
                _ => panic!("expected expression statement"),
            }
        }

        Ok(())
    }

    #[test]
    fn parse_if_else() -> Result<()> {
        let input = r#"
            if (x < y) { x; }
            else { y }
        "#;

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer)?;

        let program = parser.parse_program()?;

        assert_eq!(program.statements.len(), 1);

        let if_expression = Expression::IfExpression(IfExpression {
            token: Token::If,
            condition: Box::new(Expression::Infix(Infix {
                token: Token::LessThan,
                left: Box::new(Expression::Identifier(Identifier {
                    token: "ident(x)".to_string(),
                    value: "x".to_string(),
                })),
                operator: "<".to_string(),
                right: Box::new(Expression::Identifier(Identifier {
                    token: "ident(y)".to_string(),
                    value: "y".to_string(),
                })),
            })),
            consequence: BlockStatement {
                token: Token::Lbrace,
                statements: vec![Statement::ExpressionStatement(Expression::Identifier(
                    Identifier {
                        token: "ident(x)".to_string(),
                        value: "x".to_string(),
                    },
                ))],
            },
            alternative: Some(BlockStatement {
                token: Token::Lbrace,
                statements: vec![Statement::ExpressionStatement(Expression::Identifier(
                    Identifier {
                        token: "ident(y)".to_string(),
                        value: "y".to_string(),
                    },
                ))],
            }),
        });

        for statement in program.statements.iter() {
            match statement {
                Statement::ExpressionStatement(expression) => {
                    assert_eq!(expression, &if_expression);
                }
                _ => panic!("expected expression statement"),
            }
        }

        Ok(())
    }

    #[test]
    fn test_another_if() -> Result<()> {
        let input = r#"
            if (x < y) { x; }
        "#;

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer)?;

        let program = parser.parse_program()?;

        assert_eq!(program.statements.len(), 1);

        let if_expression = Expression::IfExpression(IfExpression {
            token: Token::If,
            condition: Box::new(Expression::Infix(Infix {
                token: Token::LessThan,
                left: Box::new(Expression::Identifier(Identifier {
                    token: "ident(x)".to_string(),
                    value: "x".to_string(),
                })),
                operator: "<".to_string(),
                right: Box::new(Expression::Identifier(Identifier {
                    token: "ident(y)".to_string(),
                    value: "y".to_string(),
                })),
            })),
            consequence: BlockStatement {
                token: Token::Lbrace,
                statements: vec![Statement::ExpressionStatement(Expression::Identifier(
                    Identifier {
                        token: "ident(x)".to_string(),
                        value: "x".to_string(),
                    },
                ))],
            },
            alternative: None,
        });

        for statement in program.statements.iter() {
            match statement {
                Statement::ExpressionStatement(expression) => {
                    assert_eq!(expression, &if_expression);
                }
                _ => panic!("expected expression statement"),
            }
        }

        Ok(())
    }

    #[test]
    fn test_last_if_complex() -> Result<()> {
        let input = r#"
            if (x < y) { x + y; }
            else { z + w; }
        "#;

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer)?;

        let program = parser.parse_program()?;
        assert_eq!(program.statements.len(), 1);

        let if_expression = Expression::IfExpression(IfExpression {
            token: Token::If,
            condition: Box::new(Expression::Infix(Infix {
                token: Token::LessThan,
                left: Box::new(Expression::Identifier(Identifier {
                    token: "ident(x)".to_string(),
                    value: "x".to_string(),
                })),
                operator: "<".to_string(),
                right: Box::new(Expression::Identifier(Identifier {
                    token: "ident(y)".to_string(),
                    value: "y".to_string(),
                })),
            })),
            consequence: BlockStatement {
                token: Token::Lbrace,
                statements: vec![Statement::ExpressionStatement(Expression::Infix(Infix {
                    token: Token::Plus,
                    left: Box::new(Expression::Identifier(Identifier {
                        token: "ident(x)".to_string(),
                        value: "x".to_string(),
                    })),
                    operator: "+".to_string(),
                    right: Box::new(Expression::Identifier(Identifier {
                        token: "ident(y)".to_string(),
                        value: "y".to_string(),
                    })),
                }))],
            },
            alternative: Some(BlockStatement {
                token: Token::Lbrace,
                statements: vec![Statement::ExpressionStatement(Expression::Infix(Infix {
                    token: Token::Plus,
                    left: Box::new(Expression::Identifier(Identifier {
                        token: "ident(z)".to_string(),
                        value: "z".to_string(),
                    })),
                    operator: "+".to_string(),
                    right: Box::new(Expression::Identifier(Identifier {
                        token: "ident(w)".to_string(),
                        value: "w".to_string(),
                    })),
                }))],
            }),
        });

        for statement in program.statements.iter() {
            match statement {
                Statement::ExpressionStatement(expression) => {
                    assert_eq!(expression, &if_expression);
                }
                _ => panic!("expected expression statement"),
            }
        }

        Ok(())
    }

    #[test]
    fn parse_function_literal() -> Result<()> {
        let input = r#"
            function(x, y, z) { x + y; }
        "#;

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer)?;

        let program = parser.parse_program()?;
        assert_eq!(program.statements.len(), 1);

        let function_literal = Expression::FunctionLiteral(FunctionLiteral {
            token: Token::Function,
            parameters: vec![
                Identifier {
                    token: "ident(x)".to_string(),
                    value: "x".to_string(),
                },
                Identifier {
                    token: "ident(y)".to_string(),
                    value: "y".to_string(),
                },
                Identifier {
                    token: "ident(z)".to_string(),
                    value: "z".to_string(),
                },
            ],
            body: BlockStatement {
                token: Token::Lbrace,
                statements: vec![Statement::ExpressionStatement(Expression::Infix(Infix {
                    token: Token::Plus,
                    left: Box::new(Expression::Identifier(Identifier {
                        token: "ident(x)".to_string(),
                        value: "x".to_string(),
                    })),
                    operator: "+".to_string(),
                    right: Box::new(Expression::Identifier(Identifier {
                        token: "ident(y)".to_string(),
                        value: "y".to_string(),
                    })),
                }))],
            },
        });

        for statement in program.statements.iter() {
            match statement {
                Statement::ExpressionStatement(expression) => {
                    assert_eq!(expression, &function_literal);
                }
                _ => panic!("expected expression statement"),
            }
        }

        Ok(())
    }

    #[test]
    fn parse_call_expression() -> Result<()> {
        let input = r#"
            a + add(b * c) + d;
            add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));
            add(a + b + c * d / f + g);
        "#;
        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer)?;

        let program = parser.parse_program()?;

        assert_eq!(program.statements.len(), 3);

        let call_expression = [
            Expression::Infix(Infix {
                token: Token::Plus,
                left: Box::new(Expression::Infix(Infix {
                    token: Token::Plus,
                    left: Box::new(Expression::Identifier(Identifier {
                        token: "ident(a)".to_string(),
                        value: "a".to_string(),
                    })),
                    operator: "+".to_string(),
                    right: Box::new(Expression::CallExpression(CallExpression {
                        token: Token::Lparen,
                        arguments: vec![Expression::Infix(Infix {
                            token: Token::Asterisk,
                            left: Box::new(Expression::Identifier(Identifier {
                                token: "ident(b)".to_string(),
                                value: "b".to_string(),
                            })),
                            operator: "*".to_string(),
                            right: Box::new(Expression::Identifier(Identifier {
                                token: "ident(c)".to_string(),
                                value: "c".to_string(),
                            })),
                        })],
                        function: Box::new(Expression::Identifier(Identifier {
                            token: "ident(add)".to_string(),
                            value: "add".to_string(),
                        })),
                    })),
                })),
                operator: "+".to_string(),
                right: Box::new(Expression::Identifier(Identifier {
                    token: "ident(d)".to_string(),
                    value: "d".to_string(),
                })),
            }),
            Expression::CallExpression(CallExpression {
                token: Token::Lparen,
                arguments: vec![
                    Expression::Identifier(Identifier {
                        token: "ident(a)".to_string(),
                        value: "a".to_string(),
                    }),
                    Expression::Identifier(Identifier {
                        token: "ident(b)".to_string(),
                        value: "b".to_string(),
                    }),
                    Expression::IntegerLiteral(1),
                    Expression::Infix(Infix {
                        token: Token::Asterisk,
                        left: Box::new(Expression::IntegerLiteral(2)),
                        operator: "*".to_string(),
                        right: Box::new(Expression::IntegerLiteral(3)),
                    }),
                    Expression::Infix(Infix {
                        token: Token::Plus,
                        left: Box::new(Expression::IntegerLiteral(4)),
                        operator: "+".to_string(),
                        right: Box::new(Expression::IntegerLiteral(5)),
                    }),
                    Expression::CallExpression(CallExpression {
                        token: Token::Lparen,
                        arguments: vec![
                            Expression::IntegerLiteral(6),
                            Expression::Infix(Infix {
                                token: Token::Asterisk,
                                left: Box::new(Expression::IntegerLiteral(7)),
                                operator: "*".to_string(),
                                right: Box::new(Expression::IntegerLiteral(8)),
                            }),
                        ],
                        function: Box::new(Expression::Identifier(Identifier {
                            token: "ident(add)".to_string(),
                            value: "add".to_string(),
                        })),
                    }),
                ],
                function: Box::new(Expression::Identifier(Identifier {
                    token: "ident(add)".to_string(),
                    value: "add".to_string(),
                })),
            }),
            Expression::CallExpression(CallExpression {
                token: Token::Lparen,
                arguments: vec![Expression::Infix(Infix {
                    token: Token::Plus,
                    left: Box::new(Expression::Infix(Infix {
                        token: Token::Plus,
                        left: Box::new(Expression::Infix(Infix {
                            token: Token::Plus,
                            left: Box::new(Expression::Identifier(Identifier {
                                token: "ident(a)".to_string(),
                                value: "a".to_string(),
                            })),
                            operator: "+".to_string(),
                            right: Box::new(Expression::Identifier(Identifier {
                                token: "ident(b)".to_string(),
                                value: "b".to_string(),
                            })),
                        })),
                        operator: "+".to_string(),
                        right: Box::new(Expression::Infix(Infix {
                            token: Token::Slash,
                            left: Box::new(Expression::Infix(Infix {
                                token: Token::Asterisk,
                                left: Box::new(Expression::Identifier(Identifier {
                                    token: "ident(c)".to_string(),
                                    value: "c".to_string(),
                                })),
                                operator: "*".to_string(),
                                right: Box::new(Expression::Identifier(Identifier {
                                    token: "ident(d)".to_string(),
                                    value: "d".to_string(),
                                })),
                            })),
                            operator: "/".to_string(),
                            right: Box::new(Expression::Identifier(Identifier {
                                token: "ident(f)".to_string(),
                                value: "f".to_string(),
                            })),
                        })),
                    })),
                    operator: "+".to_string(),
                    right: Box::new(Expression::Identifier(Identifier {
                        token: "ident(g)".to_string(),
                        value: "g".to_string(),
                    })),
                })],
                function: Box::new(Expression::Identifier(Identifier {
                    token: "ident(add)".to_string(),
                    value: "add".to_string(),
                })),
            }),
        ];

        for (index, statement) in program.statements.iter().enumerate() {
            match statement {
                Statement::ExpressionStatement(expression) => {
                    assert_eq!(expression, &call_expression[index]);
                }
                _ => panic!("expected expression statement"),
            }
        }

        Ok(())
    }
}
*/
