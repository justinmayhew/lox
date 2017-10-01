use std::fmt;

use scanner::Token;

#[derive(Copy, Clone, Debug)]
pub enum BinOp {
    Plus,
    Minus,
    Star,
    Slash,

    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match *self {
            BinOp::Plus => "+",
            BinOp::Minus => "-",
            BinOp::Star => "*",
            BinOp::Slash => "/",

            BinOp::BangEqual => "!=",
            BinOp::EqualEqual => "==",
            BinOp::Greater => ">",
            BinOp::GreaterEqual => ">=",
            BinOp::Less => "<",
            BinOp::LessEqual => "<=",
        };
        write!(f, "{}", s)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum UnaryOp {
    Minus,
    Bang,
}
impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match *self {
            UnaryOp::Minus => "-",
            UnaryOp::Bang => "!",
        };
        write!(f, "{}", s)
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Str(String),
    Int(i64),
    Bool(bool),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Str(ref s) => write!(f, "{}", s),
            Value::Int(i) => write!(f, "{}", i),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    /// A binary expression, for example `1 + 2`.
    Binary(Box<Expr>, BinOp, Box<Expr>),
    /// A grouping expression, for example `(1)`.
    Grouping(Box<Expr>),
    /// A literal expression, for example `1` or `"foo"`.
    Literal(Value),
    /// A unary expression, for example `!true`.
    Unary(UnaryOp, Box<Expr>),
    /// A variable expression, for example `name`.
    Var(String),
    /// An assignment expression, for example `a = 5`.
    VarAssign(String, Box<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expr::Binary(ref left, op, ref right) => write!(f, "({} {} {})", op, left, right),
            Expr::Grouping(ref expr) => write!(f, "(group {})", expr),
            Expr::Literal(ref value) => write!(f, "{}", value),
            Expr::Unary(op, ref expr) => write!(f, "({} {})", op, expr),
            Expr::Var(ref name) => write!(f, "{}", name),
            Expr::VarAssign(ref name, ref expr) => write!(f, "{} = {}", name, expr),
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    /// An expression statement.
    Expr(Expr),
    /// A print statement.
    Print(Expr),
    /// A variable declaration.
    VarDecl(String, Option<Expr>),
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Token, String),
    MissingExpr(Token),
    ExpectedIdentifier(Token),
    InvalidAssignment(Expr),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParseError::UnexpectedToken(ref token, ref msg) => write!(f, "{}: Next token: {:?}", msg, token),
            ParseError::MissingExpr(ref token) => write!(f, "Expected expression, found {:?}", token),
            ParseError::ExpectedIdentifier(ref token) => write!(f, "Expected identifier, found {:?}", token),
            ParseError::InvalidAssignment(ref expr) => write!(f, "Invalid assignment target {}", expr),
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn parse(&mut self) -> ParseResult<Vec<Stmt>> {
        let mut stmts = Vec::new();

        while !self.is_at_end() {
            stmts.push(self.declaration()?);
        }

        Ok(stmts)
    }

    fn advance_if(&mut self, tokens: Vec<Token>) -> bool {
        for token in tokens {
            if self.check(&token) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn advance_if_str(&mut self) -> Option<String> {
        let rv = if let Token::Str(ref s) = *self.peek() {
            Some(s.clone())
        } else {
            None
        };

        if rv.is_some() {
            self.advance();
        }

        rv
    }

    fn advance_if_identifier(&mut self) -> Option<String> {
        let rv = if let Token::Identifier(ref s) = *self.peek() {
            Some(s.clone())
        } else {
            None
        };

        if rv.is_some() {
            self.advance();
        }

        rv
    }

    fn advance_if_int(&mut self) -> Option<i64> {
        if let Token::Int(i) = *self.peek() {
            self.advance();
            Some(i)
        } else {
            None
        }
    }

    fn check(&mut self, token: &Token) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek() == token
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.pos += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek() == &Token::Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.pos]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.pos - 1]
    }

    fn consume(&mut self, token: Token, message: &str) -> ParseResult<()> {
        if self.advance_if(vec![token]) {
            return Ok(());
        }

        Err(ParseError::UnexpectedToken(self.peek().clone(), message.into()))
    }

    fn consume_identifier(&mut self) -> ParseResult<String> {
        let result = if let Token::Identifier(ref name) = *self.peek() {
            Ok(name.clone())
        } else {
            Err(ParseError::ExpectedIdentifier(self.peek().clone()))
        };

        if result.is_ok() {
            self.advance();
        }

        result
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if *self.previous() == Token::Semicolon {
                return;
            }

            match *self.peek() {
                Token::Class |
                Token::Fun |
                Token::Var |
                Token::For |
                Token::If |
                Token::While |
                Token::Print |
                Token::Return => return,
                _ => {}
            }

            self.advance();
        }
    }

    fn declaration(&mut self) -> ParseResult<Stmt> {
        let result = if self.advance_if(vec![Token::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        };

        if result.is_err() {
            self.synchronize();
        }

        result
    }

    fn var_declaration(&mut self) -> ParseResult<Stmt> {
        let name = self.consume_identifier()?;

        let initializer = if self.advance_if(vec![Token::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(Token::Semicolon, "Expect ';' after variable declaration.")?;

        Ok(Stmt::VarDecl(name, initializer))
    }

    fn statement(&mut self) -> ParseResult<Stmt> {
        if self.advance_if(vec![Token::Print]) {
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> ParseResult<Stmt> {
        let expr = self.expression()?;
        self.consume(Token::Semicolon, "Expect ';' after print statement.")?;
        Ok(Stmt::Print(expr))
    }

    fn expression_statement(&mut self) -> ParseResult<Stmt> {
        let expr = self.expression()?;
        self.consume(Token::Semicolon, "Expect ';' after expression statement.")?;
        Ok(Stmt::Expr(expr))
    }

    pub fn expression(&mut self) -> ParseResult<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParseResult<Expr> {
        let expr = self.equality()?;

        if self.advance_if(vec![Token::Equal]) {
            let value = self.assignment()?;

            if let Expr::Var(ref name) = expr {
                return Ok(Expr::VarAssign(name.clone(), Box::new(value)));
            }

            return Err(ParseError::InvalidAssignment(expr));
        }

        Ok(expr)
    }

    fn equality(&mut self) -> ParseResult<Expr> {
        let mut expr = self.comparison()?;

        while self.advance_if(vec![Token::BangEqual, Token::EqualEqual]) {
            let op = match *self.previous() {
                Token::BangEqual => BinOp::BangEqual,
                Token::EqualEqual => BinOp::EqualEqual,
                ref token => panic!("unexpected token: {:?}", token),
            };
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ParseResult<Expr> {
        let mut expr = self.addition()?;

        while self.advance_if(vec![
            Token::Greater,
            Token::GreaterEqual,
            Token::Less,
            Token::LessEqual,
        ]) {
            let op = match *self.previous() {
                Token::Greater => BinOp::Greater,
                Token::GreaterEqual => BinOp::GreaterEqual,
                Token::Less => BinOp::Less,
                Token::LessEqual => BinOp::LessEqual,
                ref token => panic!("unexpected token: {:?}", token),
            };
            let right = self.addition()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn addition(&mut self) -> ParseResult<Expr> {
        let mut expr = self.multiplication()?;

        while self.advance_if(vec![Token::Minus, Token::Plus]) {
            let op = match *self.previous() {
                Token::Minus => BinOp::Minus,
                Token::Plus => BinOp::Plus,
                ref token => panic!("unexpected token: {:?}", token),
            };
            let right = self.multiplication()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> ParseResult<Expr> {
        let mut expr = self.unary()?;

        while self.advance_if(vec![Token::Slash, Token::Star]) {
            let op = match *self.previous() {
                Token::Slash => BinOp::Slash,
                Token::Star => BinOp::Star,
                ref token => panic!("unexpected token: {:?}", token),
            };
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ParseResult<Expr> {
        if self.advance_if(vec![Token::Minus, Token::Bang]) {
            let op = match *self.previous() {
                Token::Minus => UnaryOp::Minus,
                Token::Bang => UnaryOp::Bang,
                ref token => panic!("unexpected token: {:?}", token),
            };
            let right = self.unary()?;
            Ok(Expr::Unary(op, Box::new(right)))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> ParseResult<Expr> {
        if self.advance_if(vec![Token::False]) {
            return Ok(Expr::Literal(Value::Bool(false)));
        }
        if self.advance_if(vec![Token::True]) {
            return Ok(Expr::Literal(Value::Bool(true)));
        }
        if self.advance_if(vec![Token::Nil]) {
            return Ok(Expr::Literal(Value::Nil));
        }

        if let Some(i) = self.advance_if_int() {
            return Ok(Expr::Literal(Value::Int(i)));
        }
        if let Some(s) = self.advance_if_str() {
            return Ok(Expr::Literal(Value::Str(s)));
        }
        if let Some(s) = self.advance_if_identifier() {
            return Ok(Expr::Var(s));
        }

        if self.advance_if(vec![Token::LeftParen]) {
            let expr = self.expression()?;
            self.consume(Token::RightParen, "Expect ')' after expression.")?;
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        Err(ParseError::MissingExpr(self.peek().clone()))
    }
}
