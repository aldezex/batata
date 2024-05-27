use super::{error::LexicalError, token::Token};
use std::str::FromStr;

pub fn make_tokenizer(source: &str) -> impl Iterator<Item = LexerResult> + '_ {
    let chars = source.char_indices().map(|(i, c)| (i as u32, c));
    let nlh = NewlineHandler::new(chars);
    Lexer::new(nlh)
}

struct Lexer<T: Iterator<Item = (u32, char)>> {
    source: T,

    ch0: Option<char>,
    ch1: Option<char>,
    loc0: u32,
    loc1: u32,
    loc: u32,

    queue: Vec<Span>,
}

#[derive(Debug)]
struct NewlineHandler<T: Iterator<Item = (u32, char)>> {
    source: T,
    ch0: Option<(u32, char)>,
    ch1: Option<(u32, char)>,
}

impl<T> NewlineHandler<T>
where
    T: Iterator<Item = (u32, char)>,
{
    fn new(source: T) -> Self {
        let mut nlh = NewlineHandler {
            source,
            ch0: None,
            ch1: None,
        };

        let _ = nlh.shift();
        let _ = nlh.shift();

        nlh
    }

    fn shift(&mut self) -> Option<(u32, char)> {
        let res = self.ch0;
        self.ch0 = self.ch1;
        self.ch1 = self.source.next();
        res
    }
}

impl<T> Iterator for NewlineHandler<T>
where
    T: Iterator<Item = (u32, char)>,
{
    type Item = (u32, char);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((loc, '\r')) = self.ch0 {
            if let Some((_, '\n')) = self.ch1 {
                let _ = self.shift();
                self.ch0 = Some((loc, '\n'));
            } else {
                self.ch0 = Some((loc, '\n'));
            }
        }

        self.shift()
    }
}

pub type Span = (u32, Token, u32);
pub type LexerResult = Result<Span, LexicalError>;

impl<T: Iterator<Item = (u32, char)>> Lexer<T> {
    pub fn new(source: T) -> Self {
        let mut lexer = Lexer {
            source,
            ch0: None,
            ch1: None,
            loc0: 0,
            loc1: 0,
            loc: 0,
            queue: Vec::new(),
        };

        let _ = lexer.read_char();
        let _ = lexer.read_char();
        lexer.loc = 0;

        lexer
    }

    fn inner_next(&mut self) -> LexerResult {
        while self.queue.is_empty() {
            self.read_token()?;
        }

        Ok(self.queue.remove(0))
    }

    fn read_token(&mut self) -> Result<(), LexicalError> {
        if let Some(c) = self.ch0 {
            // identifiers, keywords, etc...
            if c.is_ascii_alphabetic() || c == '_' {
                let t = self.read_identifier()?;
                self.add_token(t);
            }

            // strings
            if c == '"' {
                let t = self.read_string()?;
                self.add_token(t);
            }

            // numbers
            if c.is_ascii_digit() {
                let num = self.read_number()?;
                self.add_token(num);
            }

            // newlines
            if c == '\n' || c == ' ' || c == '\t' || c == '\x0C' {
                let token_position = self.get_position();
                let _ = self.read_char();
                let token_position_end = self.get_position();
                if c == '\n' {
                    self.add_token((token_position, Token::Newline, token_position_end));
                }
            }

            // enclosures
            if c == '(' {
                let t = self.read_single_char(Token::LParen)?;
                self.add_token(t);
            }

            if c == ')' {
                let t = self.read_single_char(Token::RParen)?;
                self.add_token(t);
            }

            if c == '{' {
                let t = self.read_single_char(Token::LBrace)?;
                self.add_token(t);
            }

            if c == '}' {
                let t = self.read_single_char(Token::RBrace)?;
                self.add_token(t);
            }

            if c == '[' {
                let t = self.read_single_char(Token::LBracket)?;
                self.add_token(t);
            }

            if c == ']' {
                let t = self.read_single_char(Token::RBracket)?;
                self.add_token(t);
            }

            // separators
            if c == ';' {
                let t = self.read_single_char(Token::Semicolon)?;
                self.add_token(t);
            }

            if c == ':' {
                let t = self.read_single_char(Token::Colon)?;
                self.add_token(t);
            }

            if c == '.' {
                let t = self.read_single_char(Token::Dot)?;
                self.add_token(t);
            }

            // operators
            if c == '+' {
                let t = self.read_single_char(Token::Plus)?;
                self.add_token(t);
            }

            if c == '-' {
                let t = self.read_single_char(Token::Minus)?;
                self.add_token(t);
            }

            if c == '*' {
                let t = self.read_single_char(Token::Star)?;
                self.add_token(t);
            }

            if c == '/' {
                let t = self.read_single_char(Token::Slash)?;
                self.add_token(t);
            }

            if c == '%' {
                let t = self.read_single_char(Token::Percent)?;
                self.add_token(t);
            }

            if c == '=' {
                if self.ch1 == Some('=') {
                    let token_position = self.get_position();
                    let _ = self.read_char();
                    let _ = self.read_char();
                    let token_position_end = self.get_position();
                    self.add_token((token_position, Token::Equal, token_position_end));
                } else {
                    let t = self.read_single_char(Token::Assign)?;
                    self.add_token(t);
                }
            }

            if c == '!' {
                if self.ch1 == Some('=') {
                    let token_position = self.get_position();
                    let _ = self.read_char();
                    let _ = self.read_char();
                    let token_position_end = self.get_position();
                    self.add_token((token_position, Token::NotEqual, token_position_end));
                } else {
                    let t = self.read_single_char(Token::Bang)?;
                    self.add_token(t);
                }
            }

            if c == '<' {
                if self.ch1 == Some('=') {
                    let token_position = self.get_position();
                    let _ = self.read_char();
                    let _ = self.read_char();
                    let token_position_end = self.get_position();
                    self.add_token((token_position, Token::LessThanEqual, token_position_end));
                } else {
                    let t = self.read_single_char(Token::LessThan)?;
                    self.add_token(t);
                }
            }

            if c == '>' {
                if self.ch1 == Some('=') {
                    let token_position = self.get_position();
                    let _ = self.read_char();
                    let _ = self.read_char();
                    let token_position_end = self.get_position();
                    self.add_token((token_position, Token::GreaterThanEqual, token_position_end));
                } else {
                    let t = self.read_single_char(Token::GreaterThan)?;
                    self.add_token(t);
                }
            }

            if c == '&' {
                if self.ch1 == Some('&') {
                    let token_position = self.get_position();
                    let _ = self.read_char();
                    let _ = self.read_char();
                    let token_position_end = self.get_position();
                    self.add_token((token_position, Token::And, token_position_end));
                } else {
                    return Err(LexicalError::UnrecognizedToken(String::from(c)));
                }
            }

            if c == '|' {
                if self.ch1 == Some('|') {
                    let token_position = self.get_position();
                    let _ = self.read_char();
                    let _ = self.read_char();
                    let token_position_end = self.get_position();
                    self.add_token((token_position, Token::Or, token_position_end));
                } else {
                    return Err(LexicalError::UnrecognizedToken(String::from(c)));
                }
            }
        } else {
            let token_position = self.get_position();
            self.add_token((token_position, Token::Eof, token_position));
        }

        Ok(())
    }

    fn read_identifier(&mut self) -> LexerResult {
        let mut ident = String::new();
        let token_position = self.get_position();

        while let Some(c) = self.ch0 {
            if c.is_ascii_alphabetic() || c == '_' {
                ident.push(c);
                let _ = self.read_char();
            } else {
                break;
            }
        }

        let token_position_end = self.get_position();
        let token = Token::from_str(&ident)?;

        Ok((token_position, token, token_position_end))
    }

    fn read_string(&mut self) -> LexerResult {
        let mut ident = String::new();
        let token_position = self.get_position();

        // add the opening quote
        ident.push('"');
        let _ = self.read_char();

        while let Some(c) = self.ch0 {
            if c != '"' {
                ident.push(c);
                let _ = self.read_char();
            } else {
                break;
            }
        }

        // add the closing quote
        ident.push('"');
        let _ = self.read_char();

        let token_position_end = self.get_position();
        let token = Token::from_str(&ident)?;

        Ok((token_position, token, token_position_end))
    }

    fn read_number(&mut self) -> LexerResult {
        let mut num = String::new();
        let token_position = self.get_position();

        while let Some(c) = self.ch0 {
            match c {
                '0'..='9' | '_' | '.' => {
                    num.push(c);
                    let _ = self.read_char();
                }
                _ => break,
            }
        }

        if has_more_than_one_occurences(&num, '.') {
            return Err(LexicalError::InvalidFloatNumberMultipleDots(num));
        }

        if num.contains('_') && !number_has_matching_underscores(&num) {
            return Err(LexicalError::InvalidNumberUnderscores(num));
        }

        let token_position_end = self.get_position();
        let token = if num.contains('.') {
            Token::Float { value: num }
        } else {
            Token::Int { value: num }
        };

        Ok((token_position, token, token_position_end))
    }

    fn read_single_char(&mut self, token: Token) -> LexerResult {
        let token_position = self.get_position();
        let _ = self.read_char();
        let token_position_end = self.get_position();
        Ok((token_position, token, token_position_end))
    }

    fn add_token(&mut self, token: Span) {
        self.queue.push(token);
    }

    fn get_position(&self) -> u32 {
        self.loc0
    }

    fn read_char(&mut self) -> Option<char> {
        let c = self.ch0;
        let next = match self.source.next() {
            Some((loc, c)) => {
                self.loc0 = self.loc1;
                self.loc1 = loc;
                Some(c)
            }
            None => {
                self.loc0 = self.loc1;
                self.loc1 += 1;
                None
            }
        };

        self.ch0 = self.ch1;
        self.ch1 = next;
        c
    }
}

impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = (u32, char)>,
{
    type Item = LexerResult;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.inner_next();

        match token {
            Ok((_, Token::Eof, _)) => None,
            r => Some(r),
        }
    }
}

pub fn has_more_than_one_occurences(source: &str, char: char) -> bool {
    let mut count = 0;
    for c in source.chars() {
        if c == char {
            count += 1;
        }
    }

    count > 1
}

pub fn number_has_matching_underscores(source: &str) -> bool {
    todo!()
}
