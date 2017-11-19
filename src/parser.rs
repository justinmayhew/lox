use std::fmt;

use primitive::Value;
use scanner::{Item, Token};

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

#[derive(Copy, Clone, Debug)]
pub enum UnaryOp {
    Minus,
    Bang,
}

#[derive(Copy, Clone, Debug)]
pub enum LogicOp {
    And,
    Or,
}

#[derive(Clone, Debug)]
pub struct Var {
    pub name: String,
    pub hops: Option<usize>,
}

impl Var {
    fn new(name: String) -> Self {
        Self { name, hops: None }
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    /// A binary expression, for example `1 + 2`.
    Binary(Box<ExprNode>, BinOp, Box<ExprNode>),
    /// A logical expression, for example `foo && bar`.
    Logical(Box<ExprNode>, LogicOp, Box<ExprNode>),
    /// A grouping expression, for example `(1)`.
    Grouping(Box<ExprNode>),
    /// A literal expression, for example `1` or `"foo"`.
    Literal(Value),
    /// A unary expression, for example `!true`.
    Unary(UnaryOp, Box<ExprNode>),
    /// A variable expression, for example `name`.
    Var(Var),
    /// An assignment expression, for example `a = 5`.
    VarAssign(Var, Box<ExprNode>),
    /// A function call, for example `f(1, 2)`.
    Call(Box<ExprNode>, Vec<ExprNode>),
    /// An anonymous function expression, for example `fun (a, b) { return a + b; }`.
    AnonymousFun(Function),
    /// A get expression, for example `point.x`.
    Get(Box<ExprNode>, String),
    /// A set expression, for example `point.x = 1`.
    Set(Box<ExprNode>, String, Box<ExprNode>),
    /// A this expression within a method.
    This(Var),
}

#[derive(Clone, Debug)]
pub struct ExprNode {
    expr: Expr,
    line: usize,
}

impl ExprNode {
    fn new(expr: Expr, line: usize) -> Self {
        ExprNode { expr, line }
    }

    pub fn expr(&self) -> &Expr {
        &self.expr
    }

    pub fn expr_mut(&mut self) -> &mut Expr {
        &mut self.expr
    }

    pub fn line(&self) -> usize {
        self.line
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
    Expr(ExprNode),
    /// A print statement.
    Print(ExprNode),
    /// A variable declaration.
    VarDecl(String, Option<ExprNode>),
    /// A function declaration.
    Fun(Function),
    /// A class declaration.
    Class(Class),
    /// A return statement.
    Return(Option<ExprNode>),
    /// A block statement.
    Block(Vec<Stmt>),
    /// An if statement.
    If(ExprNode, Box<Stmt>, Option<Box<Stmt>>),
    /// A while statement.
    While(ExprNode, Box<Stmt>),
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Item, String),
    MissingExpr(Item),
    ExpectedIdentifier(Item, String),
    InvalidAssignment(Item),
    TooManyParams(Item, String),
}

impl ParseError {
    pub fn line(&self) -> usize {
        match *self {
            ParseError::UnexpectedToken(ref item, ..) |
            ParseError::MissingExpr(ref item) |
            ParseError::ExpectedIdentifier(ref item, ..) |
            ParseError::InvalidAssignment(ref item) |
            ParseError::TooManyParams(ref item, ..) => item.line(),
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParseError::UnexpectedToken(ref item, ref msg) |
            ParseError::TooManyParams(ref item, ref msg) |
            ParseError::ExpectedIdentifier(ref item, ref msg) => {
                let location = if *item.token() == Token::Eof {
                    "end".into()
                } else {
                    format!("'{}'", item.token())
                };
                write!(f, "Error at {}: {}", location, msg)
            }
            ParseError::MissingExpr(ref item) => {
                write!(f, "Error at '{}': Expect expression.", item.token())
            }
            ParseError::InvalidAssignment(ref item) => {
                write!(f, "Error at '{}': Invalid assignment target.", item.token())
            }
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser {
    tokens: Vec<Item>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Item>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn reset(&mut self) -> &mut Self {
        self.pos = 0;
        self
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<ParseError>> {
        let mut stmts = Vec::new();
        let mut errors = Vec::new();

        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => errors.push(err),
            }
        }

        if errors.is_empty() {
            Ok(stmts)
        } else {
            Err(errors)
        }
    }

    fn next_is(&mut self, token: &Token) -> Option<&Item> {
        if self.check(token) {
            Some(self.advance())
        } else {
            None
        }
    }

    fn next_is_clone(&mut self, token: &Token) -> Option<Item> {
        self.next_is(token).cloned()
    }

    fn next_is_any(&mut self, tokens: Vec<Token>) -> Option<&Item> {
        for token in tokens {
            if self.check(&token) {
                return Some(self.advance());
            }
        }
        None
    }

    fn next_is_any_clone(&mut self, tokens: Vec<Token>) -> Option<Item> {
        self.next_is_any(tokens).cloned()
    }

    fn advance_if(&mut self, token: &Token) -> bool {
        if self.check(token) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check(&mut self, token: &Token) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().token() == token
    }

    fn advance(&mut self) -> &Item {
        if !self.is_at_end() {
            self.pos += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token() == &Token::Eof
    }

    fn peek(&self) -> &Item {
        &self.tokens[self.pos]
    }

    fn previous(&self) -> &Item {
        &self.tokens[self.pos - 1]
    }

    fn consume(&mut self, token: &Token, message: &str) -> ParseResult<&Item> {
        if self.advance_if(token) {
            return Ok(self.previous());
        }

        Err(ParseError::UnexpectedToken(
            self.peek().clone(),
            message.into(),
        ))
    }

    fn consume_identifier(&mut self, message: &str) -> ParseResult<String> {
        let result = if let Token::Identifier(ref name) = *self.peek().token() {
            Ok(name.clone())
        } else {
            Err(ParseError::ExpectedIdentifier(
                self.peek().clone(),
                message.into(),
            ))
        };

        if result.is_ok() {
            self.advance();
        }

        result
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if *self.previous().token() == Token::Semicolon {
                return;
            }

            match *self.peek().token() {
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
            self.fun_declaration("function").map(Stmt::Fun)
        } else if self.advance_if(&Token::Class) {
            self.class_declaration().map(Stmt::Class)
        } else {
            self.statement()
        };

        if result.is_err() {
            self.synchronize();
        }

        result
    }

    fn var_declaration(&mut self) -> ParseResult<Stmt> {
        let name = self.consume_identifier("Expect variable name.")?;

        let initializer = if self.advance_if(&Token::Equal) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(&Token::Semicolon, "Expect ';' after variable declaration.")?;

        Ok(Stmt::VarDecl(name, initializer))
    }

    fn fun_declaration(&mut self, kind: &str) -> ParseResult<Function> {
        let name = self.consume_identifier(&format!("Expect {} name.", kind))?;
        self.consume(
            &Token::LeftParen,
            &format!("Expect '(' after {} name.", kind),
        )?;
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
        if *self.peek().token() != Token::RightParen {
            loop {
                if parameters.len() >= PARAM_LIMIT {
                    return Err(ParseError::TooManyParams(
                        self.peek().clone(),
                        format!("Cannot have more than {} parameters.", PARAM_LIMIT),
                    ));
                }

                let identifier = self.consume_identifier("Expect parameter name.")?;
                parameters.push(identifier);
                if !self.advance_if(&Token::Comma) {
                    break;
                }
            }
        }

        self.consume(&Token::RightParen, "Expect ')' after parameters.")?;
        self.consume(&Token::LeftBrace, "Expect '{' before function body.")?;

        let body = self.block_statement()?;

        Ok((parameters, body))
    }

    fn class_declaration(&mut self) -> ParseResult<Class> {
        let name = self.consume_identifier("Expect class name.")?;
        self.consume(&Token::LeftBrace, "Expect '{' after class name.")?;

        let mut methods = Vec::new();
        while !self.check(&Token::RightBrace) && !self.is_at_end() {
            methods.push(self.fun_declaration("method")?);
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

        let condition = if let Some(item) = self.next_is_clone(&Token::Semicolon) {
            ExprNode::new(Expr::Literal(Value::Bool(true)), item.line())
        } else {
            let condition = self.expression()?;
            self.consume(&Token::Semicolon, "Expect ';' after for condition.")?;
            condition
        };

        let increment = if *self.peek().token() == Token::RightParen {
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
        let expr = if *self.peek().token() != Token::Semicolon {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(&Token::Semicolon, "Expect ';' after return statement.")?;

        Ok(Stmt::Return(expr))
    }

    fn expression_statement(&mut self) -> ParseResult<Stmt> {
        let expr = self.expression()?;
        self.consume(&Token::Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::Expr(expr))
    }

    pub fn expression(&mut self) -> ParseResult<ExprNode> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParseResult<ExprNode> {
        let node = self.or()?;

        if self.advance_if(&Token::Equal) {
            let equals = self.previous().clone();
            let value = self.assignment()?;

            if let Expr::Var(ref var) = *node.expr() {
                return Ok(ExprNode::new(
                    Expr::VarAssign(var.clone(), Box::new(value)),
                    node.line(),
                ));
            }
            if let Expr::Get(ref node, ref name) = *node.expr() {
                return Ok(ExprNode::new(
                    Expr::Set(node.clone(), name.clone(), Box::new(value)),
                    node.line(),
                ));
            }

            return Err(ParseError::InvalidAssignment(equals));
        }

        Ok(node)
    }

    fn or(&mut self) -> ParseResult<ExprNode> {
        let mut node = self.and()?;

        while let Some(item) = self.next_is_clone(&Token::Or) {
            let right = self.and()?;
            node = ExprNode::new(
                Expr::Logical(Box::new(node), LogicOp::Or, Box::new(right)),
                item.line(),
            );
        }

        Ok(node)
    }

    fn and(&mut self) -> ParseResult<ExprNode> {
        let mut node = self.equality()?;

        while let Some(item) = self.next_is_clone(&Token::And) {
            let right = self.equality()?;
            node = ExprNode::new(
                Expr::Logical(Box::new(node), LogicOp::And, Box::new(right)),
                item.line(),
            );
        }

        Ok(node)
    }

    fn equality(&mut self) -> ParseResult<ExprNode> {
        let mut node = self.comparison()?;

        while let Some(item) = self.next_is_any_clone(vec![Token::BangEqual, Token::EqualEqual]) {
            let op = match *item.token() {
                Token::BangEqual => BinOp::BangEqual,
                Token::EqualEqual => BinOp::EqualEqual,
                ref token => panic!("unexpected token: {:?}", token),
            };
            let right = self.comparison()?;
            node = ExprNode::new(
                Expr::Binary(Box::new(node), op, Box::new(right)),
                item.line(),
            );
        }

        Ok(node)
    }

    fn comparison(&mut self) -> ParseResult<ExprNode> {
        let mut node = self.addition()?;

        while let Some(item) = self.next_is_any_clone(vec![
            Token::Greater,
            Token::GreaterEqual,
            Token::Less,
            Token::LessEqual,
        ]) {
            let op = match *item.token() {
                Token::Greater => BinOp::Greater,
                Token::GreaterEqual => BinOp::GreaterEqual,
                Token::Less => BinOp::Less,
                Token::LessEqual => BinOp::LessEqual,
                ref token => panic!("unexpected token: {:?}", token),
            };
            let right = self.addition()?;
            node = ExprNode::new(
                Expr::Binary(Box::new(node), op, Box::new(right)),
                item.line(),
            );
        }

        Ok(node)
    }

    fn addition(&mut self) -> ParseResult<ExprNode> {
        let mut node = self.multiplication()?;

        while let Some(item) = self.next_is_any_clone(vec![Token::Minus, Token::Plus]) {
            let op = match *item.token() {
                Token::Minus => BinOp::Minus,
                Token::Plus => BinOp::Plus,
                ref token => panic!("unexpected token: {:?}", token),
            };
            let right = self.multiplication()?;
            node = ExprNode::new(
                Expr::Binary(Box::new(node), op, Box::new(right)),
                item.line(),
            );
        }

        Ok(node)
    }

    fn multiplication(&mut self) -> ParseResult<ExprNode> {
        let mut node = self.unary()?;

        while let Some(item) = self.next_is_any_clone(vec![Token::Slash, Token::Star]) {
            let op = match *item.token() {
                Token::Slash => BinOp::Slash,
                Token::Star => BinOp::Star,
                ref token => panic!("unexpected token: {:?}", token),
            };
            let right = self.unary()?;
            node = ExprNode::new(
                Expr::Binary(Box::new(node), op, Box::new(right)),
                item.line(),
            );
        }

        Ok(node)
    }

    fn unary(&mut self) -> ParseResult<ExprNode> {
        if let Some(item) = self.next_is_any_clone(vec![Token::Minus, Token::Bang]) {
            let op = match *item.token() {
                Token::Minus => UnaryOp::Minus,
                Token::Bang => UnaryOp::Bang,
                ref token => panic!("unexpected token: {:?}", token),
            };
            let right = self.unary()?;
            Ok(ExprNode::new(Expr::Unary(op, Box::new(right)), item.line()))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> ParseResult<ExprNode> {
        let mut node = self.primary()?;

        loop {
            if self.advance_if(&Token::LeftParen) {
                node = self.finish_call(node)?;
            } else if let Some(item) = self.next_is_clone(&Token::Dot) {
                let name = self.consume_identifier("Expect property name after '.'.")?;
                node = ExprNode::new(Expr::Get(Box::new(node), name), item.line());
            } else {
                break;
            }
        }

        Ok(node)
    }

    fn finish_call(&mut self, callee: ExprNode) -> ParseResult<ExprNode> {
        let mut arguments = Vec::new();

        if *self.peek().token() != Token::RightParen {
            loop {
                if arguments.len() >= PARAM_LIMIT {
                    return Err(ParseError::TooManyParams(
                        self.peek().clone(),
                        format!("Cannot have more than {} arguments.", PARAM_LIMIT),
                    ));
                }
                arguments.push(self.expression()?);
                if !self.advance_if(&Token::Comma) {
                    break;
                }
            }
        }

        let paren = self.consume(&Token::RightParen, "Expect ')' after function arguments.")?;
        Ok(ExprNode::new(
            Expr::Call(Box::new(callee), arguments),
            paren.line(),
        ))
    }

    fn primary(&mut self) -> ParseResult<ExprNode> {
        if let Some(item) = self.next_is(&Token::False) {
            return Ok(ExprNode::new(
                Expr::Literal(Value::Bool(false)),
                item.line(),
            ));
        }
        if let Some(item) = self.next_is(&Token::True) {
            return Ok(ExprNode::new(Expr::Literal(Value::Bool(true)), item.line()));
        }
        if let Some(item) = self.next_is(&Token::Nil) {
            return Ok(ExprNode::new(Expr::Literal(Value::Nil), item.line()));
        }
        if let Some(item) = self.next_is_clone(&Token::Fun) {
            match self.fun_expression() {
                Ok(f) => return Ok(ExprNode::new(Expr::AnonymousFun(f), item.line())),
                Err(_) => {
                    // Revert position so that we display the same errors as
                    // the reference Lox implementation.
                    self.pos -= 1;
                }
            }
        }
        if let Some(item) = self.next_is(&Token::This) {
            return Ok(ExprNode::new(
                Expr::This(Var::new("this".into())),
                item.line(),
            ));
        }
        if let Some(item) = self.next_is_clone(&Token::LeftParen) {
            let expr = self.expression()?;
            self.consume(&Token::RightParen, "Expect ')' after expression.")?;
            return Ok(ExprNode::new(Expr::Grouping(Box::new(expr)), item.line()));
        }

        let next = self.peek().clone();
        match *next.token() {
            Token::Int(i) => {
                self.advance();
                Ok(ExprNode::new(Expr::Literal(Value::Int(i)), next.line()))
            }
            Token::Str(ref s) => {
                self.advance();
                Ok(ExprNode::new(
                    Expr::Literal(Value::Str(s.clone())),
                    next.line(),
                ))
            }
            Token::Identifier(ref name) => {
                self.advance();
                Ok(ExprNode::new(
                    Expr::Var(Var::new(name.clone())),
                    next.line(),
                ))
            }
            _ => Err(ParseError::MissingExpr(next.clone())),
        }
    }
}
