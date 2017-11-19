use std::num::ParseIntError;
use std::str::Chars;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier(String),
    Str(String),
    Int(i64),

    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ScanError {
    UnclosedStr,
    ParseInt(ParseIntError),
}

impl From<ParseIntError> for ScanError {
    fn from(err: ParseIntError) -> ScanError {
        ScanError::ParseInt(err)
    }
}

type ScanResult<T> = Result<T, ScanError>;

pub struct Scanner<'s> {
    src: Chars<'s>,
    peek: Option<char>,
}

impl<'s> Scanner<'s> {
    pub fn new(src: &'s str) -> Self {
        Self {
            src: src.chars(),
            peek: None,
        }
    }

    pub fn scan(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            match self.next_token() {
                Ok(token) => {
                    let stop = token == Token::Eof;
                    tokens.push(token);

                    if stop {
                        break;
                    }
                }
                Err(e) => panic!("Error scanning: {:?}", e),
            }
        }

        tokens
    }

    pub fn next_token(&mut self) -> ScanResult<Token> {
        self.eat_whitespace();

        match self.get() {
            Some('(') => Ok(Token::LeftParen),
            Some(')') => Ok(Token::RightParen),
            Some('{') => Ok(Token::LeftBrace),
            Some('}') => Ok(Token::RightBrace),
            Some(',') => Ok(Token::Comma),
            Some('.') => Ok(Token::Dot),
            Some('-') => Ok(Token::Minus),
            Some('+') => Ok(Token::Plus),
            Some(';') => Ok(Token::Semicolon),
            Some('*') => Ok(Token::Star),
            Some('!') => Ok(if self.next_is('=') {
                Token::BangEqual
            } else {
                Token::Bang
            }),
            Some('=') => Ok(if self.next_is('=') {
                Token::EqualEqual
            } else {
                Token::Equal
            }),
            Some('<') => Ok(if self.next_is('=') {
                Token::LessEqual
            } else {
                Token::Less
            }),
            Some('>') => Ok(if self.next_is('=') {
                Token::GreaterEqual
            } else {
                Token::Greater
            }),
            Some('/') => if self.next_is('/') {
                self.eat_line();
                self.next_token()
            } else {
                Ok(Token::Slash)
            },
            Some(c) => if c == '"' {
                Ok(Token::Str(self.eat_str()?))
            } else if is_digit(c) {
                Ok(Token::Int(self.eat_int(c)?))
            } else if is_alpha_or_underscore(c) {
                Ok(self.eat_identifier_or_keyword(c))
            } else {
                panic!("Unexpected character: {}", c);
            },
            None => Ok(Token::Eof),
        }
    }

    fn get(&mut self) -> Option<char> {
        self.peek.take().or_else(|| self.src.next())
    }

    fn next_is(&mut self, expected: char) -> bool {
        if let Some(c) = self.get() {
            if c == expected {
                return true;
            } else {
                self.peek = Some(c);
            }
        }
        false
    }

    fn eat_line(&mut self) {
        while let Some(c) = self.get() {
            if c == '\n' {
                break;
            }
        }
    }

    fn eat_whitespace(&mut self) {
        while let Some(c) = self.get() {
            if !c.is_whitespace() {
                self.peek = Some(c);
                break;
            }
        }
    }

    fn eat_str(&mut self) -> ScanResult<String> {
        let mut s = String::new();

        while let Some(c) = self.get() {
            if c == '"' {
                return Ok(s);
            }
            s.push(c);
        }

        Err(ScanError::UnclosedStr)
    }

    fn eat_int(&mut self, first: char) -> ScanResult<i64> {
        let mut s = first.to_string();

        while let Some(c) = self.get() {
            if is_digit(c) {
                s.push(c);
            } else {
                self.peek = Some(c);
                break;
            }
        }

        Ok(s.parse()?)
    }

    fn eat_identifier_or_keyword(&mut self, first: char) -> Token {
        let mut s = first.to_string();

        while let Some(c) = self.get() {
            if is_alpha_numeric_or_underscore(c) {
                s.push(c);
            } else {
                self.peek = Some(c);
                break;
            }
        }

        match s.as_ref() {
            "and" => return Token::And,
            "class" => return Token::Class,
            "else" => return Token::Else,
            "false" => return Token::False,
            "for" => return Token::For,
            "fun" => return Token::Fun,
            "if" => return Token::If,
            "nil" => return Token::Nil,
            "or" => return Token::Or,
            "print" => return Token::Print,
            "return" => return Token::Return,
            "super" => return Token::Super,
            "this" => return Token::This,
            "true" => return Token::True,
            "var" => return Token::Var,
            "while" => return Token::While,
            _ => {}
        }

        Token::Identifier(s)
    }
}

fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

fn is_alpha_or_underscore(c: char) -> bool {
    c == '_' || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
}

fn is_alpha_numeric_or_underscore(c: char) -> bool {
    is_alpha_or_underscore(c) || is_digit(c)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn print() {
        let mut scanner = Scanner::new(r#"print "hello, world""#);
        assert_eq!(scanner.next_token(), Ok(Token::Print));
        assert_eq!(scanner.next_token(), Ok(Token::Str("hello, world".into())));
        assert_eq!(scanner.next_token(), Ok(Token::Eof));
    }

    #[test]
    fn str() {
        let mut scanner = Scanner::new(r#""a string""#);
        assert_eq!(scanner.next_token(), Ok(Token::Str("a string".into())));
        assert_eq!(scanner.next_token(), Ok(Token::Eof));
    }

    #[test]
    fn int() {
        let mut scanner = Scanner::new("123");
        assert_eq!(scanner.next_token(), Ok(Token::Int(123)));
        assert_eq!(scanner.next_token(), Ok(Token::Eof));
    }

    #[test]
    fn identifier() {
        let mut scanner = Scanner::new("foo");
        assert_eq!(scanner.next_token(), Ok(Token::Identifier("foo".into())));
        assert_eq!(scanner.next_token(), Ok(Token::Eof));
    }

    #[test]
    fn conditional() {
        let mut scanner = Scanner::new("if (even) { total / 2; } else { total; }");
        assert_eq!(scanner.next_token(), Ok(Token::If));
        assert_eq!(scanner.next_token(), Ok(Token::LeftParen));
        assert_eq!(scanner.next_token(), Ok(Token::Identifier("even".into())));
        assert_eq!(scanner.next_token(), Ok(Token::RightParen));
        assert_eq!(scanner.next_token(), Ok(Token::LeftBrace));
        assert_eq!(scanner.next_token(), Ok(Token::Identifier("total".into())));
        assert_eq!(scanner.next_token(), Ok(Token::Slash));
        assert_eq!(scanner.next_token(), Ok(Token::Int(2)));
        assert_eq!(scanner.next_token(), Ok(Token::Semicolon));
        assert_eq!(scanner.next_token(), Ok(Token::RightBrace));
        assert_eq!(scanner.next_token(), Ok(Token::Else));
        assert_eq!(scanner.next_token(), Ok(Token::LeftBrace));
        assert_eq!(scanner.next_token(), Ok(Token::Identifier("total".into())));
        assert_eq!(scanner.next_token(), Ok(Token::Semicolon));
        assert_eq!(scanner.next_token(), Ok(Token::RightBrace));
        assert_eq!(scanner.next_token(), Ok(Token::Eof));
    }
}
