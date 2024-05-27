use super::parse_module;

#[test]
fn test_parse_module() {
    let input = "1 + 2 * 3";
    let parsed = parse_module(input).unwrap();
    println!("parsed: {}", parsed.module);
    assert_eq!(parsed.module.statements.len(), 1);
}
