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
