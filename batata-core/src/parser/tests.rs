use crate::ast::untyped::{
    Block, Definition, Expression, ExpressionKind, Function, Infix, Module, Parameter, Statement,
};

use super::parse_module;

#[test]
fn test_parse_module() {
    let input = r#"let x = 1 + 2 * 3;
        let y = 4 / 5;
        let z = 6 - 7;
        let a = y + z * x;
        "#;
    let parsed = parse_module(input).unwrap();

    assert_eq!(parsed.module.statements.len(), 4);
    assert_eq!(
        parsed.module,
        Module {
            statements: vec![
                Statement::Definition(Definition {
                    name: "x".to_string(),
                    value: Expression {
                        kind: ExpressionKind::Infix(Infix {
                            left: Box::new(Expression {
                                kind: ExpressionKind::Integer("1".into())
                            }),
                            operator: "+".to_string(),
                            right: Box::new(Expression {
                                kind: ExpressionKind::Infix(Infix {
                                    left: Box::new(Expression {
                                        kind: ExpressionKind::Integer("2".into())
                                    }),
                                    operator: "*".to_string(),
                                    right: Box::new(Expression {
                                        kind: ExpressionKind::Integer("3".into())
                                    })
                                })
                            })
                        })
                    }
                }),
                Statement::Definition(Definition {
                    name: "y".to_string(),
                    value: Expression {
                        kind: ExpressionKind::Infix(Infix {
                            left: Box::new(Expression {
                                kind: ExpressionKind::Integer("4".into())
                            }),
                            operator: "/".to_string(),
                            right: Box::new(Expression {
                                kind: ExpressionKind::Integer("5".into())
                            })
                        })
                    }
                }),
                Statement::Definition(Definition {
                    name: "z".to_string(),
                    value: Expression {
                        kind: ExpressionKind::Infix(Infix {
                            left: Box::new(Expression {
                                kind: ExpressionKind::Integer("6".into())
                            }),
                            operator: "-".to_string(),
                            right: Box::new(Expression {
                                kind: ExpressionKind::Integer("7".into())
                            })
                        })
                    }
                }),
                Statement::Definition(Definition {
                    name: "a".to_string(),
                    value: Expression {
                        kind: ExpressionKind::Infix(Infix {
                            left: Box::new(Expression {
                                kind: ExpressionKind::Identifier("y".into())
                            }),
                            operator: "+".to_string(),
                            right: Box::new(Expression {
                                kind: ExpressionKind::Infix(Infix {
                                    left: Box::new(Expression {
                                        kind: ExpressionKind::Identifier("z".into())
                                    }),
                                    operator: "*".to_string(),
                                    right: Box::new(Expression {
                                        kind: ExpressionKind::Identifier("x".into())
                                    })
                                })
                            })
                        })
                    }
                })
            ]
        }
    );
    assert_eq!(
        parsed.module.to_string(),
        String::from(
            r#"let x = (1 + (2 * 3));let y = (4 / 5);let z = (6 - 7);let a = (y + (z * x));"#
        )
    );
}

#[test]
fn parse_block_statements() {
    let input = r#"
        {
            let x = 1 + 2 * 3;
            let y = 4 / 5;
            let z = 6 - 7;
            let a = y + z * x;
        }
        "#;
    let parsed = parse_module(input).unwrap();

    assert_eq!(parsed.module.statements.len(), 1);
    assert_eq!(
        parsed.module.to_string(),
        String::from(
            r#"{let x = (1 + (2 * 3));let y = (4 / 5);let z = (6 - 7);let a = (y + (z * x));}"#
        )
    );

    let input = r#"
        {
            let x = 1 + 2 * 3;
            let y = 4 / 5;
            {
                let z = 6 - 7;
                let a = y + z * x;
            }
        }
        "#;

    let parsed = parse_module(input).unwrap();
    assert_eq!(parsed.module.statements.len(), 1);
    assert_eq!(
        parsed.module.to_string(),
        String::from(
            r#"{let x = (1 + (2 * 3));let y = (4 / 5);{let z = (6 - 7);let a = (y + (z * x));}}"#
        )
    );

    let input = r#"
        {
            let x = 1 + 2 * 3;
            let y = 4 / 5;
            {
                let z = 6 - 7;
                let a = y + z * x;
            }
            let b = a + x;
            {
                let c = b + a;
            }
        }
        "#;

    let parsed = parse_module(input).unwrap();
    assert_eq!(parsed.module.statements.len(), 1);
    assert_eq!(
        parsed.module.to_string(),
        String::from(
            r#"{let x = (1 + (2 * 3));let y = (4 / 5);{let z = (6 - 7);let a = (y + (z * x));}let b = (a + x);{let c = (b + a);}}"#
        )
    );

    let input = r#"
            let x = 1 + 2 * 3;
            let y = 4 / 5;
            {
                let z = 6 - 7;
                let a = y + z * x;
            }
            let b = a + x;
            {
                let c = b + a;
            }
            let d = c + b;
        "#;

    let parsed = parse_module(input).unwrap();
    assert_eq!(parsed.module.statements.len(), 6);
    assert_eq!(
        parsed.module.to_string(),
        String::from(
            r#"let x = (1 + (2 * 3));let y = (4 / 5);{let z = (6 - 7);let a = (y + (z * x));}let b = (a + x);{let c = (b + a);}let d = (c + b);"#
        )
    );
}

#[test]
fn parse_function() {
    let input = r#"
        fn add(x, y) {
            x + y
        }
        "#;

    let parsed = parse_module(input).unwrap();

    assert_eq!(parsed.module.statements.len(), 1);
    assert_eq!(
        parsed.module.statements[0],
        Statement::Function(Function {
            name: "add".to_string(),
            parameters: vec![
                Parameter { name: "x".into() },
                Parameter { name: "y".into() }
            ],
            body: Block {
                statements: vec![Statement::Expression(Expression {
                    kind: ExpressionKind::Infix(Infix {
                        left: Box::new(Expression {
                            kind: ExpressionKind::Identifier("x".into())
                        }),
                        operator: "+".into(),
                        right: Box::new(Expression {
                            kind: ExpressionKind::Identifier("y".into())
                        })
                    })
                })]
            }
        })
    )
}
