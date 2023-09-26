use std::{collections::HashMap, mem};

use anyhow::{Ok, Result};

use ast::{
    BlockStatement, CallExpression, Expression, FunctionLiteral, Identifier, IfExpression, Infix,
    LetStatement, Prefix, Program, ReturnStatement, Statement,
};
use lexer::{
    token::{self, Token},
    Lexer,
};

pub struct Parser {
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
    pub fn new(mut lexer: Lexer) -> Result<Self> {
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
        precedences.insert(Token::Lparen, Precedence::Call);

        Ok(Parser {
            lexer,
            current_token,
            next_token,

            precedences,
        })
    }

    pub fn parse_program(&mut self) -> Result<Program> {
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
        if self.peek_token_is(Token::Semicolon) {
            self.step()?;

            return Ok(Statement::ReturnStatement(ReturnStatement {
                expression: Expression::Empty,
            }));
        }

        self.step()?;

        let exp = self.parse_expression_statement()?;

        Ok(Statement::ReturnStatement(ReturnStatement {
            expression: match exp {
                Statement::ExpressionStatement(expression) => expression,
                _ => Expression::Empty,
            },
        }))
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

    fn parse_string_literal(&mut self) -> Result<Expression> {
        let exp = Expression::StringLiteral(self.current_token.literal());
        Ok(exp)
    }

    fn parse_identifier(&mut self) -> Result<Expression> {
        let exp = Expression::Identifier(Identifier {
            token: self.current_token.to_string(),
            value: self.current_token.literal(),
        });

        Ok(exp)
    }

    fn parse_boolean(&mut self) -> Result<Expression> {
        let exp = Expression::Boolean(self.current_token_is(Token::True));
        Ok(exp)
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression> {
        self.step()?;

        let exp = self.parse_expression(Precedence::Lowest)?;

        if !self.peek_token_is(Token::Rparen) {
            return Err(anyhow::anyhow!("failed to parse grouped expression"));
        }

        self.step()?;

        Ok(exp)
    }

    fn parse_if_expression(&mut self) -> Result<Expression> {
        let current_token = self.current_token.clone();

        if !self.peek_token_is(token::Token::Lparen) {
            return Err(anyhow::anyhow!("failed to parse if expression"));
        }

        self.step()?;

        let exp = self.parse_expression(Precedence::Lowest)?;

        if !self.current_token_is(Token::Rparen) {
            return Err(anyhow::anyhow!(
                "failed to parse if expression -> there is no right parenthesis"
            ));
        }

        if !self.peek_token_is(Token::Lbrace) {
            return Err(anyhow::anyhow!(
                "failed to parse if expression -> there is no left brace"
            ));
        }

        self.step()?;

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token_is(Token::Else) {
            self.step()?;

            if !self.peek_token_is(Token::Lbrace) {
                return Err(anyhow::anyhow!(
                    "failed to parse if expression -> there is no left brace"
                ));
            }

            self.step()?;

            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(Expression::IfExpression(IfExpression {
            token: current_token,
            condition: Box::new(exp),
            consequence,
            alternative,
        }))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement> {
        let current_token = self.current_token.clone();
        let mut statements = Vec::new();

        self.step()?;

        while !self.current_token_is(Token::Rbrace) && !self.current_token_is(Token::Eof) {
            let statement = self.parse_statement()?;

            statements.push(statement);
            self.step()?;
        }

        Ok(BlockStatement {
            token: current_token,
            statements,
        })
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

    fn parse_function_literal(&mut self) -> Result<Expression> {
        let curr = self.current_token.clone();

        if !self.peek_token_is(Token::Lparen) {
            return Err(anyhow::anyhow!("failed to parse function literal"));
        }

        self.step()?;

        let parameters = self.parse_function_parameters()?;

        if !self.peek_token_is(Token::Lbrace) {
            return Err(anyhow::anyhow!("failed to parse function literal"));
        }

        self.step()?;

        let body = self.parse_block_statement()?;

        Ok(Expression::FunctionLiteral(FunctionLiteral {
            token: curr,
            parameters,
            body,
        }))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(Token::Rparen) {
            self.step()?;
            self.step()?;

            return Ok(identifiers);
        }

        self.step()?;

        let identifier = Identifier {
            token: self.current_token.to_string(),
            value: self.current_token.literal(),
        };

        identifiers.push(identifier);

        while self.peek_token_is(Token::Comma) {
            self.step()?;
            self.step()?;

            let identifier = Identifier {
                token: self.current_token.to_string(),
                value: self.current_token.literal(),
            };

            identifiers.push(identifier);
        }

        if !self.peek_token_is(Token::Rparen) {
            return Err(anyhow::anyhow!("failed to parse function parameters"));
        }

        self.step()?;

        Ok(identifiers)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression> {
        Ok(Expression::CallExpression(CallExpression {
            token: self.current_token.clone(),
            arguments: self.parse_call_arguments()?,
            function: Box::new(function),
        }))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>> {
        let mut arguments = Vec::new();

        if self.peek_token_is(Token::Rparen) {
            self.step()?;
            self.step()?;

            return Ok(arguments);
        }

        self.step()?;

        arguments.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(token::Token::Comma) {
            self.step()?;
            self.step()?;

            arguments.push(self.parse_expression(Precedence::Lowest)?);
        }

        if !self.peek_token_is(Token::Rparen) {
            return Err(anyhow::anyhow!("failed to parse call arguments"));
        }

        self.step()?;

        Ok(arguments)
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
            Token::Int(_) => Some(Self::parse_integer_literal),
            Token::Bang | Token::Minus => Some(Self::parse_prefix_expression),
            Token::Ident(_) => Some(Self::parse_identifier),
            Token::Str(_) => Some(Self::parse_string_literal),
            Token::True | Token::False => Some(Self::parse_boolean),
            Token::Lparen => Some(Self::parse_grouped_expression),
            Token::If => Some(Self::parse_if_expression),
            Token::Function => Some(Self::parse_function_literal),
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
            Token::Lparen => Some(Self::parse_call_expression),
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
