use anyhow::{Ok, Result};
use super::token::Token;

pub struct Lexer {
    input: Vec<u8>,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lexer = Lexer {
            input: input.into_bytes(),
            position: 0,
            read_position: 0,
            ch: 0,
        };

        lexer.read_char();

        lexer
    }

    pub fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Result<Token> {
        use Token::*;

        self.skip_whitespace();

        let token = match self.ch {
            0 => Eof,

            b'=' => {
                if self.peek() == b'=' {
                    self.read_char();

                    if self.peek() == b'=' {
                        self.read_char();
                        return Ok(StrictEqual);
                    }

                    self.read_char();

                    return Ok(Equal);
                }

                self.read_char();

                return Ok(Assign);
            }

            b'>' => {
                if self.peek() == b'=' {
                    self.read_char();
                    self.read_char();

                    return Ok(GreaterThanEqual);
                }

                self.read_char();

                return Ok(GreaterThan);
            }
            b'<' => {
                if self.peek() == b'=' {
                    self.read_char();
                    self.read_char();

                    return Ok(LessThanEqual);
                }

                self.read_char();

                return Ok(LessThan);
            }

            b'+' => Plus,
            b'-' => Minus,
            b'*' => Asterisk,
            b'/' => Slash,
            b'!' => {
                if self.peek() == b'=' {
                    self.read_char();
                    self.read_char();

                    return Ok(NotEqual);
                }

                self.read_char();
                return Ok(Bang);
            }

            b',' => Comma,
            b';' => Semicolon,

            b'(' => Lparen,
            b')' => Rparen,
            b'{' => Lbrace,
            b'}' => Rbrace,

            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let identifier = self.read_identifier();

                return Ok(match identifier.as_str() {
                    "let" => Let,
                    "var" => Var,
                    "const" => Const,
                    "function" => Function,
                    "if" => If,
                    "else" => Else,
                    "return" => Return,
                    "async" => Async,
                    "await" => Await,
                    "true" => True,
                    "false" => False,
                    _ => return Ok(Ident(identifier)),
                });
            }

            b'0'..=b'9' => {
                let number = self.read_number();
                return Ok(Token::Int(number.parse::<isize>().unwrap()));
            }

            b'"' => {
                self.read_char();

                let mut string = String::new();

                while self.ch != b'"' {
                    string.push(self.ch as char);
                    self.read_char();
                }

                self.read_char();

                return Ok(Token::Str(string));
            }

            b'\'' => {
                self.read_char();

                let mut string = String::new();

                while self.ch != b'\'' {
                    string.push(self.ch as char);
                    self.read_char();
                }

                self.read_char();

                return Ok(Token::Str(string));
            }

            _ => Illegal,
        };

        self.read_char();
        Ok(token)
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;

        while self.ch.is_ascii_alphabetic() || self.ch == b'_' {
            self.read_char();
        }

        String::from_utf8_lossy(&self.input[position..self.position]).to_string()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;

        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        String::from_utf8_lossy(&self.input[position..self.position]).to_string()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn peek(&self) -> u8 {
        if self.read_position >= self.input.len() {
            return 0;
        }

        self.input[self.read_position]
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use crate::parser::token::Token::*;
    use anyhow::Result;

    #[test]
    fn next_token() -> Result<()> {
        let input = "=+(){},;";

        let mut lexer = Lexer::new(input.into());

        let tokens = vec![
            Assign, Plus, Lparen, Rparen, Lbrace, Rbrace, Comma, Semicolon, Eof,
        ];

        for token in tokens {
            let tok = lexer.next_token()?;
            println!("Expected: {}, got: {}", token, tok);
            assert_eq!(tok, token);
        }

        Ok(())
    }

    #[test]
    fn input_with_function() -> Result<()> {
        let input = r#"let five = 5;
        let ten = 10;
        let add = function(x, y) {
            x + y;
        };"#;

        let mut lexer = Lexer::new(input.into());

        let tokens = vec![
            Let,
            Ident("five".into()),
            Assign,
            Int(5),
            Semicolon,
            Let,
            Ident("ten".into()),
            Assign,
            Int(10),
            Semicolon,
            Let,
            Ident("add".into()),
            Assign,
            Function,
            Lparen,
            Ident("x".into()),
            Comma,
            Ident("y".into()),
            Rparen,
            Lbrace,
            Ident("x".into()),
            Plus,
            Ident("y".into()),
            Semicolon,
            Rbrace,
            Semicolon,
            Eof,
        ];

        for token in tokens {
            let tok = lexer.next_token()?;
            println!("Expected: {}, got: {}", token, tok);
            assert_eq!(tok, token);
        }

        Ok(())
    }

    #[test]
    fn comparators() -> Result<()> {
        let input = "=";
        let mut lexer = Lexer::new(input.into());
        let tokens = vec![Assign];

        for token in tokens {
            let tok = lexer.next_token()?;
            println!("Expected: {}, got: {}", token, tok);
            assert_eq!(tok, token);
        }

        let input = "==";
        let mut lexer = Lexer::new(input.into());
        let tokens = vec![Equal];

        for token in tokens {
            let tok = lexer.next_token()?;
            println!("Expected: {}, got: {}", token, tok);
            assert_eq!(tok, token);
        }

        let input = "===";
        let mut lexer = Lexer::new(input.into());
        let tokens = vec![StrictEqual];

        for token in tokens {
            let tok = lexer.next_token()?;
            println!("Expected: {}, got: {}", token, tok);
            assert_eq!(tok, token);
        }

        let input = "<";
        let mut lexer = Lexer::new(input.into());
        let tokens = vec![LessThan];

        for token in tokens {
            let tok = lexer.next_token()?;
            println!("Expected: {}, got: {}", token, tok);
            assert_eq!(tok, token);
        }

        let input = ">";
        let mut lexer = Lexer::new(input.into());
        let tokens = vec![GreaterThan];

        for token in tokens {
            let tok = lexer.next_token()?;
            println!("Expected: {}, got: {}", token, tok);
            assert_eq!(tok, token);
        }

        let input = "<=";
        let mut lexer = Lexer::new(input.into());
        let tokens = vec![LessThanEqual];

        for token in tokens {
            let tok = lexer.next_token()?;
            println!("Expected: {}, got: {}", token, tok);
            assert_eq!(tok, token);
        }

        let input = ">=";
        let mut lexer = Lexer::new(input.into());
        let tokens = vec![GreaterThanEqual];

        for token in tokens {
            let tok = lexer.next_token()?;
            println!("Expected: {}, got: {}", token, tok);
            assert_eq!(tok, token);
        }

        Ok(())
    }

    #[test]
    fn keywords() -> Result<()> {
        let input = r#"
            const alvaro
            let tata
            var macarena;
            if else
            return
            async await
        "#;
        let mut lexer = Lexer::new(input.into());

        let tokens = vec![
            Const,
            Ident("alvaro".into()),
            Let,
            Ident("tata".into()),
            Var,
            Ident("macarena".into()),
            Semicolon,
            If,
            Else,
            Return,
            Async,
            Await,
        ];

        for token in tokens {
            let tok = lexer.next_token()?;
            println!("Expected: {}, got: {}", token, tok);
            assert_eq!(tok, token);
        }

        Ok(())
    }

    #[test]
    fn test_strings() -> Result<()> {
        let input = r#"
            "hello world"
            'hello world'
        "#;
        let mut lexer = Lexer::new(input.into());

        let tokens = vec![Str("hello world".into()), Str("hello world".into())];

        for token in tokens {
            let tok = lexer.next_token()?;
            println!("Expected: {}, got: {}", token, tok);
            assert_eq!(tok, token);
        }

        Ok(())
    }

    #[test]
    fn integer_literals_equal() -> Result<()> {
        let input = r#"
            5 == 5;
        "#;
        let mut lexer = Lexer::new(input.into());

        let tokens = vec![Int(5), Equal, Int(5), Semicolon, Eof];

        for token in tokens {
            let tok = lexer.next_token()?;
            println!("Expected: {}, got: {}", token, tok);
            assert_eq!(tok, token);
        }

        Ok(())
    }
}
