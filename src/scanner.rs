use std::num::ParseIntError;
use std::str::Chars;

#[derive(Debug, PartialEq, Eq)]
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

    pub fn next_token(&mut self) -> ScanResult<Token> {
        loop {
            self.eat_whitespace();

            match self.peek() {
                Some('(') => {
                    self.get().unwrap();
                    return Ok(Token::LeftParen);
                }
                Some(')') => {
                    self.get().unwrap();
                    return Ok(Token::RightParen);
                }
                Some('{') => {
                    self.get().unwrap();
                    return Ok(Token::LeftBrace);
                }
                Some('}') => {
                    self.get().unwrap();
                    return Ok(Token::RightBrace);
                }
                Some(',') => {
                    self.get().unwrap();
                    return Ok(Token::Comma);
                }
                Some('.') => {
                    self.get().unwrap();
                    return Ok(Token::Dot);
                }
                Some('-') => {
                    self.get().unwrap();
                    return Ok(Token::Minus);
                }
                Some('+') => {
                    self.get().unwrap();
                    return Ok(Token::Plus);
                }
                Some(';') => {
                    self.get().unwrap();
                    return Ok(Token::Semicolon);
                }
                Some('*') => {
                    self.get().unwrap();
                    return Ok(Token::Star);
                }
                Some('!') => {
                    self.get().unwrap();

                    return match self.peek() {
                        Some('=') => {
                            self.get().unwrap();
                            Ok(Token::BangEqual)
                        }
                        _ => Ok(Token::Bang),
                    };
                }
                Some('=') => {
                    self.get().unwrap();

                    return match self.peek() {
                        Some('=') => {
                            self.get().unwrap();
                            Ok(Token::EqualEqual)
                        }
                        _ => Ok(Token::Equal),
                    };
                }
                Some('<') => {
                    self.get().unwrap();

                    return match self.peek() {
                        Some('=') => {
                            self.get().unwrap();
                            Ok(Token::LessEqual)
                        }
                        _ => Ok(Token::Less),
                    };
                }
                Some('>') => {
                    self.get().unwrap();

                    return match self.peek() {
                        Some('=') => {
                            self.get().unwrap();
                            Ok(Token::GreaterEqual)
                        }
                        _ => Ok(Token::Greater),
                    };
                }
                Some('/') => {
                    self.get().unwrap();

                    match self.peek() {
                        Some('/') => {
                            self.get().unwrap();
                            self.eat_line();
                        }
                        _ => return Ok(Token::Slash),
                    }
                }
                Some('"') => return Ok(Token::Str(self.eat_str()?)),
                Some(c) if is_digit(c) => return Ok(Token::Int(self.eat_int()?)),
                Some(c) if is_alpha_or_underscore(c) => return self.eat_identifier_or_keyword(),
                Some(c) => panic!("unexpected char: {}", c),
                None => return Ok(Token::Eof),
            }
        }
    }

    fn peek(&mut self) -> Option<char> {
        if let Some(c) = self.peek {
            Some(c)
        } else if let Some(c) = self.src.next() {
            self.peek = Some(c);
            Some(c)
        } else {
            None
        }
    }

    fn get(&mut self) -> Option<char> {
        if let Some(c) = self.peek.take() {
            Some(c)
        } else {
            self.src.next()
        }
    }

    fn eat_line(&mut self) {
        while let Some(c) = self.get() {
            if c == '\n' {
                break;
            }
        }
    }

    fn eat_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c == ' ' || c == '\n' || c == '\t' || c == '\r' {
                self.get().unwrap();
            } else {
                break;
            }
        }
    }

    fn eat_str(&mut self) -> ScanResult<String> {
        assert_eq!(self.get().unwrap(), '"');

        let mut s = String::new();

        loop {
            match self.get() {
                Some(c) => {
                    if c == '"' {
                        return Ok(s);
                    }
                    s.push(c);
                }
                None => return Err(ScanError::UnclosedStr),
            }
        }
    }

    fn eat_int(&mut self) -> ScanResult<i64> {
        let mut s = String::new();
        s.push(self.get().unwrap());

        loop {
            match self.peek() {
                Some(c) if is_digit(c) => s.push(self.get().unwrap()),
                _ => break,
            }
        }

        Ok(s.parse()?)
    }

    fn eat_identifier_or_keyword(&mut self) -> ScanResult<Token> {
        let mut s = String::new();
        s.push(self.get().unwrap());

        loop {
            match self.peek() {
                Some(c) if is_alpha_numeric_or_underscore(c) => {
                    s.push(self.get().unwrap());
                }
                _ => break,
            }
        }

        match s.as_ref() {
            "and" => return Ok(Token::And),
            "class" => return Ok(Token::Class),
            "else" => return Ok(Token::Else),
            "false" => return Ok(Token::False),
            "for" => return Ok(Token::For),
            "fun" => return Ok(Token::Fun),
            "if" => return Ok(Token::If),
            "nil" => return Ok(Token::Nil),
            "or" => return Ok(Token::Or),
            "print" => return Ok(Token::Print),
            "return" => return Ok(Token::Return),
            "super" => return Ok(Token::Super),
            "this" => return Ok(Token::This),
            "true" => return Ok(Token::True),
            "var" => return Ok(Token::Var),
            "while" => return Ok(Token::While),
            _ => {}
        }

        Ok(Token::Identifier(s))
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
