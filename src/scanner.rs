use std::fmt;
use std::iter::Peekable;
use std::num::ParseFloatError;
use std::str::Chars;

#[derive(Clone, Debug, PartialEq)]
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
    Number(f64),

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

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Minus => write!(f, "-"),
            Token::Plus => write!(f, "+"),
            Token::Semicolon => write!(f, ";"),
            Token::Slash => write!(f, "/"),
            Token::Star => write!(f, "*"),

            Token::Bang => write!(f, "!"),
            Token::BangEqual => write!(f, "!="),
            Token::Equal => write!(f, "="),
            Token::EqualEqual => write!(f, "=="),
            Token::Greater => write!(f, ">"),
            Token::GreaterEqual => write!(f, ">="),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),

            Token::Identifier(ref s) | Token::Str(ref s) => write!(f, "{}", s),
            Token::Number(n) => write!(f, "{}", n),

            Token::And => write!(f, "and"),
            Token::Class => write!(f, "class"),
            Token::Else => write!(f, "else"),
            Token::False => write!(f, "false"),
            Token::Fun => write!(f, "fun"),
            Token::For => write!(f, "for"),
            Token::If => write!(f, "if"),
            Token::Nil => write!(f, "nil"),
            Token::Or => write!(f, "or"),
            Token::Print => write!(f, "print"),
            Token::Return => write!(f, "return"),
            Token::Super => write!(f, "super"),
            Token::This => write!(f, "this"),
            Token::True => write!(f, "true"),
            Token::Var => write!(f, "var"),
            Token::While => write!(f, "while"),

            Token::Eof => write!(f, "EOF"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Item {
    token: Token,
    line: usize,
}

impl Item {
    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn line(&self) -> usize {
        self.line
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ScanError {
    UnclosedStr,
    ParseFloat(ParseFloatError),
}

impl From<ParseFloatError> for ScanError {
    fn from(err: ParseFloatError) -> ScanError {
        ScanError::ParseFloat(err)
    }
}

type ScanResult<T> = Result<T, ScanError>;

pub struct Scanner<'s> {
    src: Peekable<Chars<'s>>,
    peek: Option<char>,
    line: usize,
}

impl<'s> Scanner<'s> {
    pub fn new(src: &'s str) -> Self {
        Self {
            src: src.chars().peekable(),
            peek: None,
            line: 1,
        }
    }

    pub fn scan(&mut self) -> Vec<Item> {
        let mut items = Vec::new();

        loop {
            match self.next_item() {
                Ok(item) => {
                    let stop = *item.token() == Token::Eof;
                    items.push(item);

                    if stop {
                        break;
                    }
                }
                Err(e) => panic!("Error scanning: {:?}", e),
            }
        }

        items
    }

    pub fn next_item(&mut self) -> ScanResult<Item> {
        self.eat_whitespace();

        match self.get() {
            Some('(') => Ok(self.item(Token::LeftParen)),
            Some(')') => Ok(self.item(Token::RightParen)),
            Some('{') => Ok(self.item(Token::LeftBrace)),
            Some('}') => Ok(self.item(Token::RightBrace)),
            Some(',') => Ok(self.item(Token::Comma)),
            Some('.') => Ok(self.item(Token::Dot)),
            Some('-') => Ok(self.item(Token::Minus)),
            Some('+') => Ok(self.item(Token::Plus)),
            Some(';') => Ok(self.item(Token::Semicolon)),
            Some('*') => Ok(self.item(Token::Star)),
            Some('!') => Ok(if self.next_is('=') {
                self.item(Token::BangEqual)
            } else {
                self.item(Token::Bang)
            }),
            Some('=') => Ok(if self.next_is('=') {
                self.item(Token::EqualEqual)
            } else {
                self.item(Token::Equal)
            }),
            Some('<') => Ok(if self.next_is('=') {
                self.item(Token::LessEqual)
            } else {
                self.item(Token::Less)
            }),
            Some('>') => Ok(if self.next_is('=') {
                self.item(Token::GreaterEqual)
            } else {
                self.item(Token::Greater)
            }),
            Some('/') => if self.next_is('/') {
                self.eat_line();
                self.next_item()
            } else {
                Ok(self.item(Token::Slash))
            },
            Some(c) => if c == '"' {
                let s = self.eat_str()?;
                Ok(self.item(Token::Str(s)))
            } else if is_digit(c) {
                let n = self.eat_number(c)?;
                Ok(self.item(Token::Number(n)))
            } else if is_alpha_or_underscore(c) {
                let token = self.eat_identifier_or_keyword(c);
                Ok(self.item(token))
            } else {
                panic!("Unexpected character: {}", c);
            },
            None => Ok(self.item(Token::Eof)),
        }
    }

    fn item(&self, token: Token) -> Item {
        Item {
            token,
            line: self.line,
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
                self.line += 1;
                break;
            }
        }
    }

    fn eat_whitespace(&mut self) {
        while let Some(c) = self.get() {
            if c == '\n' {
                self.line += 1;
            } else if !c.is_whitespace() {
                self.peek = Some(c);
                break;
            }
        }
    }

    fn eat_str(&mut self) -> ScanResult<String> {
        let mut s = String::new();

        while let Some(c) = self.get() {
            if c == '\n' {
                self.line += 1;
            } else if c == '"' {
                return Ok(s);
            }
            s.push(c);
        }

        Err(ScanError::UnclosedStr)
    }

    fn eat_number(&mut self, first: char) -> ScanResult<f64> {
        let mut s = first.to_string();
        let mut saw_decimal = false;

        while let Some(c) = self.get() {
            if is_digit(c) {
                s.push(c);
            } else if !saw_decimal && c == '.'
                && self.src.peek().map(|c| is_digit(*c)).unwrap_or(false)
            {
                saw_decimal = true;
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

    macro_rules! next {
        ($scanner:expr, $expect:expr) => (
            assert_eq!(*$scanner.next_item().unwrap().token(), $expect);
        )
    }

    #[test]
    fn print() {
        let mut scanner = Scanner::new(r#"print "hello, world""#);
        next!(scanner, Token::Print);
        next!(scanner, Token::Str("hello, world".into()));
        next!(scanner, Token::Eof);
    }

    #[test]
    fn str() {
        let mut scanner = Scanner::new(r#""a string""#);
        next!(scanner, Token::Str("a string".into()));
        next!(scanner, Token::Eof);
    }

    #[test]
    fn int() {
        let mut scanner = Scanner::new("123");
        next!(scanner, Token::Number(123.0));
        next!(scanner, Token::Eof);
    }

    #[test]
    fn identifier() {
        let mut scanner = Scanner::new("foo");
        next!(scanner, Token::Identifier("foo".into()));
        next!(scanner, Token::Eof);
    }

    #[test]
    fn conditional() {
        let mut scanner = Scanner::new("if (even) { total / 2; } else { total; }");
        next!(scanner, Token::If);
        next!(scanner, Token::LeftParen);
        next!(scanner, Token::Identifier("even".into()));
        next!(scanner, Token::RightParen);
        next!(scanner, Token::LeftBrace);
        next!(scanner, Token::Identifier("total".into()));
        next!(scanner, Token::Slash);
        next!(scanner, Token::Number(2.0));
        next!(scanner, Token::Semicolon);
        next!(scanner, Token::RightBrace);
        next!(scanner, Token::Else);
        next!(scanner, Token::LeftBrace);
        next!(scanner, Token::Identifier("total".into()));
        next!(scanner, Token::Semicolon);
        next!(scanner, Token::RightBrace);
        next!(scanner, Token::Eof);
    }
}
