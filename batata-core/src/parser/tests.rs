use super::parse_module;

#[test]
fn test_parse_module() {
    let input = "fn main() {}";
    let parsed = parse_module(input).unwrap();
    assert_eq!(parsed.module.statements.len(), 1);
}
