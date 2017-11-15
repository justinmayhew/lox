use std::fmt;

use primitive::Value;
use scanner::Token;

const PARAM_LIMIT: usize = 8;

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

#[derive(Copy, Clone, Debug)]
pub enum LogicOp {
    And,
    Or,
}
impl fmt::Display for LogicOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match *self {
            LogicOp::And => "&&",
            LogicOp::Or => "||",
        };
        write!(f, "{}", s)
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    /// A binary expression, for example `1 + 2`.
    Binary(Box<Expr>, BinOp, Box<Expr>),
    /// A logical expression, for example `foo && bar`.
    Logical(Box<Expr>, LogicOp, Box<Expr>),
    /// A grouping expression, for example `(1)`.
    Grouping(Box<Expr>),
    /// A literal expression, for example `1` or `"foo"`.
    Literal(Value),
    /// A unary expression, for example `!true`.
    Unary(UnaryOp, Box<Expr>),
    /// A variable expression, for example `name`.
    Var(String, Option<usize>),
    /// An assignment expression, for example `a = 5`.
    VarAssign(String, Box<Expr>, Option<usize>),
    /// A function call, for example `f(1, 2)`.
    Call(Box<Expr>, Vec<Expr>),
    /// An anonymous function expression, for example `fun (a, b) { return a + b; }`.
    AnonymousFun(Function),
    /// A get expression, for example `point.x`.
    Get(Box<Expr>, String),
    /// A set expression, for example `point.x = 1`.
    Set(Box<Expr>, String, Box<Expr>),
    /// A this expression within a method.
    This(Option<usize>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expr::Binary(ref left, op, ref right) => write!(f, "({} {} {})", op, left, right),
            Expr::Logical(ref left, op, ref right) => write!(f, "({} {} {})", op, left, right),
            Expr::Grouping(ref expr) => write!(f, "(group {})", expr),
            Expr::Literal(ref value) => write!(f, "{}", value),
            Expr::Unary(op, ref expr) => write!(f, "({} {})", op, expr),
            Expr::Var(ref name, _) => write!(f, "{}", name),
            Expr::VarAssign(ref name, ref expr, _) => write!(f, "{} = {}", name, expr),
            Expr::Call(ref callee, ref arguments) => write!(f, "({} {:?})", callee, arguments),
            Expr::AnonymousFun(ref fun) => write!(f, "(anonymous {:?})", fun.parameters),
            Expr::Get(ref expr, ref name) => write!(f, "{}.{}", expr, name),
            Expr::Set(ref left, ref name, ref right) => write!(f, "{}.{} = {}", left, name, right),
            Expr::This(_) => write!(f, "this"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Vec<Stmt>,
}

#[derive(Clone, Debug)]
pub struct Class {
    pub name: String,
    pub methods: Vec<Function>,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    /// An expression statement.
    Expr(Expr),
    /// A print statement.
    Print(Expr),
    /// A variable declaration.
    VarDecl(String, Option<Expr>),
    /// A function declaration.
    Fun(Function),
    /// A class declaration.
    Class(Class),
    /// A return statement.
    Return(Option<Expr>),
    /// A block statement.
    Block(Vec<Stmt>),
    /// An if statement.
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    /// A while statement.
    While(Expr, Box<Stmt>),
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
            ParseError::UnexpectedToken(ref token, ref msg) => {
                write!(f, "{}: Next token: {:?}", msg, token)
            }
            ParseError::MissingExpr(ref token) => {
                write!(f, "Expected expression, found {:?}", token)
            }
            ParseError::ExpectedIdentifier(ref token) => {
                write!(f, "Expected identifier, found {:?}", token)
            }
            ParseError::InvalidAssignment(ref expr) => {
                write!(f, "Invalid assignment target {}", expr)
            }
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

    fn advance_if(&mut self, token: &Token) -> bool {
        if self.check(token) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn advance_if_any(&mut self, tokens: Vec<Token>) -> Option<Token> {
        for token in tokens {
            if self.advance_if(&token) {
                return Some(token);
            }
        }

        None
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

    fn consume(&mut self, token: &Token, message: &str) -> ParseResult<()> {
        if self.advance_if(token) {
            return Ok(());
        }

        Err(ParseError::UnexpectedToken(
            self.peek().clone(),
            message.into(),
        ))
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
        let result = if self.advance_if(&Token::Var) {
            self.var_declaration()
        } else if self.advance_if(&Token::Fun) {
            Ok(Stmt::Fun(self.fun_declaration()?))
        } else if self.advance_if(&Token::Class) {
            Ok(Stmt::Class(self.class_declaration()?))
        } else {
            self.statement()
        };

        if let Err(ref e) = result {
            eprintln!("Synchronizing due to {}...", e);
            self.synchronize();
        }

        result
    }

    fn var_declaration(&mut self) -> ParseResult<Stmt> {
        let name = self.consume_identifier()?;

        let initializer = if self.advance_if(&Token::Equal) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(&Token::Semicolon, "Expect ';' after variable declaration.")?;

        Ok(Stmt::VarDecl(name, initializer))
    }

    fn fun_declaration(&mut self) -> ParseResult<Function> {
        let name = self.consume_identifier()?;
        self.consume(&Token::LeftParen, "Expect '(' after fun name.")?;
        let (parameters, body) = self.fun_parameters_and_body()?;
        Ok(Function {
            name,
            parameters,
            body,
        })
    }

    fn fun_expression(&mut self) -> ParseResult<Function> {
        self.consume(&Token::LeftParen, "Expect '(' after fun keyword.")?;
        let (parameters, body) = self.fun_parameters_and_body()?;
        Ok(Function {
            name: "anonymous".into(),
            parameters,
            body,
        })
    }

    fn fun_parameters_and_body(&mut self) -> ParseResult<(Vec<String>, Vec<Stmt>)> {
        let mut parameters = Vec::new();
        if *self.peek() != Token::RightParen {
            loop {
                if parameters.len() >= PARAM_LIMIT {
                    panic!(
                        "Functions cannot have more than {} parameters.",
                        PARAM_LIMIT
                    );
                }
                parameters.push(self.consume_identifier()?);

                if !self.advance_if(&Token::Comma) {
                    break;
                }
            }
        }

        self.consume(&Token::RightParen, "Expect ')' after fun parameters.")?;
        self.consume(&Token::LeftBrace, "Expect '{' after fun parameter list.")?;

        let body = self.block_statement()?;

        Ok((parameters, body))
    }

    fn class_declaration(&mut self) -> ParseResult<Class> {
        let name = self.consume_identifier()?;
        self.consume(&Token::LeftBrace, "Expect '{' after class name.")?;

        let mut methods = Vec::new();
        while !self.check(&Token::RightBrace) && !self.is_at_end() {
            methods.push(self.fun_declaration()?);
        }

        self.consume(&Token::RightBrace, "Expect '}' after class declaration.")?;
        Ok(Class { name, methods })
    }

    fn statement(&mut self) -> ParseResult<Stmt> {
        if self.advance_if(&Token::Print) {
            self.print_statement()
        } else if self.advance_if(&Token::LeftBrace) {
            Ok(Stmt::Block(self.block_statement()?))
        } else if self.advance_if(&Token::If) {
            self.if_statement()
        } else if self.advance_if(&Token::While) {
            self.while_statement()
        } else if self.advance_if(&Token::For) {
            self.for_statement()
        } else if self.advance_if(&Token::Return) {
            self.return_statement()
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> ParseResult<Stmt> {
        let expr = self.expression()?;
        self.consume(&Token::Semicolon, "Expect ';' after print statement.")?;
        Ok(Stmt::Print(expr))
    }

    fn block_statement(&mut self) -> ParseResult<Vec<Stmt>> {
        let mut stmts = Vec::new();

        while !self.check(&Token::RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }

        self.consume(&Token::RightBrace, "Expect '}' after block.")?;
        Ok(stmts)
    }

    fn if_statement(&mut self) -> ParseResult<Stmt> {
        self.consume(&Token::LeftParen, "Expect '(' after if.")?;
        let condition = self.expression()?;
        self.consume(&Token::RightParen, "Expect ')' after if condition.")?;
        let then_branch = Box::new(self.statement()?);

        // Check for the optional else.
        let else_branch = if self.advance_if(&Token::Else) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If(condition, then_branch, else_branch))
    }

    fn while_statement(&mut self) -> ParseResult<Stmt> {
        self.consume(&Token::LeftParen, "Expect '(' after while.")?;
        let condition = self.expression()?;
        self.consume(&Token::RightParen, "Expect ')' after while condition.")?;
        let body = self.statement()?;
        Ok(Stmt::While(condition, Box::new(body)))
    }

    fn for_statement(&mut self) -> ParseResult<Stmt> {
        self.consume(&Token::LeftParen, "Expect '(' after for.")?;

        let initializer = if self.advance_if(&Token::Var) {
            Some(self.var_declaration()?)
        } else if self.advance_if(&Token::Semicolon) {
            None
        } else {
            // It has to be an expression statement.
            Some(self.expression_statement()?)
        };

        let condition = if self.advance_if(&Token::Semicolon) {
            Expr::Literal(Value::Bool(true))
        } else {
            let condition = self.expression()?;
            self.consume(&Token::Semicolon, "Expect ';' after for condition.")?;
            condition
        };

        let increment = if *self.peek() == Token::RightParen {
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(&Token::RightParen, "Expect ')' after for.")?;

        let mut body = self.statement()?;

        if let Some(increment) = increment {
            body = Stmt::Block(vec![body, Stmt::Expr(increment)]);
        }

        body = Stmt::While(condition, Box::new(body));

        if let Some(initializer) = initializer {
            body = Stmt::Block(vec![initializer, body]);
        }

        Ok(body)
    }

    fn return_statement(&mut self) -> ParseResult<Stmt> {
        let expr = if *self.peek() != Token::Semicolon {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(&Token::Semicolon, "Expect ';' after return statement.")?;

        Ok(Stmt::Return(expr))
    }

    fn expression_statement(&mut self) -> ParseResult<Stmt> {
        let expr = self.expression()?;
        self.consume(&Token::Semicolon, "Expect ';' after expression statement.")?;
        Ok(Stmt::Expr(expr))
    }

    pub fn expression(&mut self) -> ParseResult<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParseResult<Expr> {
        let expr = self.or()?;

        if self.advance_if(&Token::Equal) {
            let value = self.assignment()?;

            if let Expr::Var(ref name, hops) = expr {
                return Ok(Expr::VarAssign(name.clone(), Box::new(value), hops));
            }
            if let Expr::Get(expr, name) = expr {
                return Ok(Expr::Set(expr, name, Box::new(value)));
            }

            return Err(ParseError::InvalidAssignment(expr));
        }

        Ok(expr)
    }

    fn or(&mut self) -> ParseResult<Expr> {
        let mut expr = self.and()?;

        while self.advance_if(&Token::Or) {
            let right = self.and()?;
            expr = Expr::Logical(Box::new(expr), LogicOp::Or, Box::new(right));
        }

        Ok(expr)
    }

    fn and(&mut self) -> ParseResult<Expr> {
        let mut expr = self.equality()?;

        while self.advance_if(&Token::And) {
            let right = self.equality()?;
            expr = Expr::Logical(Box::new(expr), LogicOp::And, Box::new(right));
        }

        Ok(expr)
    }

    fn equality(&mut self) -> ParseResult<Expr> {
        let mut expr = self.comparison()?;

        while let Some(token) = self.advance_if_any(vec![Token::BangEqual, Token::EqualEqual]) {
            let op = match token {
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

        while let Some(token) = self.advance_if_any(vec![
            Token::Greater,
            Token::GreaterEqual,
            Token::Less,
            Token::LessEqual,
        ]) {
            let op = match token {
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

        while let Some(token) = self.advance_if_any(vec![Token::Minus, Token::Plus]) {
            let op = match token {
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

        while let Some(token) = self.advance_if_any(vec![Token::Slash, Token::Star]) {
            let op = match token {
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
        if let Some(token) = self.advance_if_any(vec![Token::Minus, Token::Bang]) {
            let op = match token {
                Token::Minus => UnaryOp::Minus,
                Token::Bang => UnaryOp::Bang,
                ref token => panic!("unexpected token: {:?}", token),
            };
            let right = self.unary()?;
            Ok(Expr::Unary(op, Box::new(right)))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> ParseResult<Expr> {
        let mut expr = self.primary()?;

        loop {
            if self.advance_if(&Token::LeftParen) {
                expr = self.finish_call(expr)?;
            } else if self.advance_if(&Token::Dot) {
                let name = self.consume_identifier()?;
                expr = Expr::Get(Box::new(expr), name);
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> ParseResult<Expr> {
        let mut arguments = Vec::new();

        if *self.peek() != Token::RightParen {
            if arguments.len() >= PARAM_LIMIT {
                panic!("More than {} arguments to function call.", PARAM_LIMIT);
            }
            loop {
                arguments.push(self.expression()?);
                if !self.advance_if(&Token::Comma) {
                    break;
                }
            }
        }

        self.consume(&Token::RightParen, "Expect ')' after function arguments.")?;

        Ok(Expr::Call(Box::new(callee), arguments))
    }

    fn primary(&mut self) -> ParseResult<Expr> {
        if self.advance_if(&Token::False) {
            return Ok(Expr::Literal(Value::Bool(false)));
        }
        if self.advance_if(&Token::True) {
            return Ok(Expr::Literal(Value::Bool(true)));
        }
        if self.advance_if(&Token::Nil) {
            return Ok(Expr::Literal(Value::Nil));
        }
        if self.advance_if(&Token::Fun) {
            return Ok(Expr::AnonymousFun(self.fun_expression()?));
        }
        if self.advance_if(&Token::This) {
            return Ok(Expr::This(None));
        }

        if let Some(i) = self.advance_if_int() {
            return Ok(Expr::Literal(Value::Int(i)));
        }
        if let Some(s) = self.advance_if_str() {
            return Ok(Expr::Literal(Value::Str(s)));
        }
        if let Some(s) = self.advance_if_identifier() {
            return Ok(Expr::Var(s, None));
        }

        if self.advance_if(&Token::LeftParen) {
            let expr = self.expression()?;
            self.consume(&Token::RightParen, "Expect ')' after expression.")?;
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        Err(ParseError::MissingExpr(self.peek().clone()))
    }
}
