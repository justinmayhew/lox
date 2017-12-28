use std::borrow::Borrow;
use std::usize;

use runtime::Value;
use scanner::{Item, Token};

const PARAM_LIMIT: usize = 8;

trait OpToken {
    fn token(&self) -> Token;
}

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

impl OpToken for BinOp {
    fn token(&self) -> Token {
        match *self {
            BinOp::Plus => Token::Plus,
            BinOp::Minus => Token::Minus,
            BinOp::Star => Token::Star,
            BinOp::Slash => Token::Slash,
            BinOp::BangEqual => Token::BangEqual,
            BinOp::EqualEqual => Token::EqualEqual,
            BinOp::Greater => Token::Greater,
            BinOp::GreaterEqual => Token::GreaterEqual,
            BinOp::Less => Token::Less,
            BinOp::LessEqual => Token::LessEqual,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum UnaryOp {
    Minus,
    Bang,
}

impl OpToken for UnaryOp {
    fn token(&self) -> Token {
        match *self {
            UnaryOp::Minus => Token::Minus,
            UnaryOp::Bang => Token::Bang,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum LogicOp {
    And,
    Or,
}

#[derive(Clone, Debug)]
pub struct Var {
    pub identifier: Identifier,
    pub hops: usize,
}

impl Var {
    fn new(identifier: Identifier) -> Self {
        Self {
            identifier,
            hops: usize::MAX,
        }
    }

    pub fn name(&self) -> &str {
        &self.identifier.name
    }

    pub fn line(&self) -> usize {
        self.identifier.line
    }
}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub name: String,
    pub line: usize,
}

impl Identifier {
    fn new(name: String, line: usize) -> Self {
        Self { name, line }
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Binary(Box<ExprNode>, BinOp, Box<ExprNode>),
    Logical(Box<ExprNode>, LogicOp, Box<ExprNode>),
    Grouping(Box<ExprNode>),
    Literal(Value),
    Unary(UnaryOp, Box<ExprNode>),
    Var(Var),
    VarAssign(Var, Box<ExprNode>),
    Call(Box<ExprNode>, Vec<ExprNode>),
    Fun(FunctionExpr),
    Get(Box<ExprNode>, Identifier),
    Set(Box<ExprNode>, Identifier, Box<ExprNode>),
    Super(Var, Var),
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
pub struct FunctionDecl {
    pub identifier: Identifier,
    pub parameters: Vec<Identifier>,
    pub body: Vec<Stmt>,
}

impl FunctionDecl {
    pub fn name(&self) -> &str {
        &self.identifier.name
    }
}

#[derive(Clone, Debug)]
pub struct FunctionExpr {
    pub parameters: Vec<Identifier>,
    pub body: Vec<Stmt>,
}

#[derive(Clone, Debug)]
pub enum Function {
    Decl(FunctionDecl),
    Expr(FunctionExpr),
}

#[derive(Clone, Debug)]
pub struct Class {
    pub identifier: Identifier,
    pub superclass: Option<Var>,
    pub methods: Vec<FunctionDecl>,
}

impl Class {
    pub fn name(&self) -> &str {
        &self.identifier.name
    }
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Expr(ExprNode),
    Print(ExprNode),
    VarDecl(Identifier, Option<ExprNode>),
    Fun(FunctionDecl),
    Class(Class),
    Return(Option<ExprNode>, usize),
    Block(Vec<Stmt>),
    If(ExprNode, Box<Stmt>, Option<Box<Stmt>>),
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
    pub fn item(&self) -> &Item {
        match *self {
            ParseError::UnexpectedToken(ref item, ..)
            | ParseError::MissingExpr(ref item)
            | ParseError::ExpectedIdentifier(ref item, ..)
            | ParseError::InvalidAssignment(ref item)
            | ParseError::TooManyParams(ref item, ..) => item,
        }
    }

    fn print(&self) {
        let message = match *self {
            ParseError::UnexpectedToken(ref _item, ref msg)
            | ParseError::TooManyParams(ref _item, ref msg)
            | ParseError::ExpectedIdentifier(ref _item, ref msg) => msg,
            ParseError::MissingExpr(ref _item) => "Expect expression",
            ParseError::InvalidAssignment(ref _item) => "Invalid assignment target",
        };

        let item = self.item();
        let line = item.line();

        eprintln!("[line {}] Error at {}: {}.", line, item.location(), message);
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

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ()> {
        let mut stmts = Vec::new();
        let mut had_error = false;

        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    had_error = true;
                    err.print();
                }
            }
        }

        if had_error {
            Err(())
        } else {
            Ok(stmts)
        }
    }

    fn match_token<T: Borrow<Token>>(&mut self, token: T) -> bool {
        if self.check(token) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_token_line<T: Borrow<Token>>(&mut self, token: T) -> Option<usize> {
        if self.check(token) {
            let line = self.advance().line();
            Some(line)
        } else {
            None
        }
    }

    fn match_op<T: OpToken>(&mut self, ops: Vec<T>) -> Option<(T, usize)> {
        for op in ops {
            if self.check(&op.token()) {
                let item = self.advance();
                return Some((op, item.line()));
            }
        }
        None
    }

    fn check<T: Borrow<Token>>(&mut self, token: T) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().token() == token.borrow()
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

    fn consume(&mut self, token: Token, message: &str) -> ParseResult<&Item> {
        if self.match_token(token) {
            return Ok(self.previous());
        }

        Err(ParseError::UnexpectedToken(
            self.peek().clone(),
            message.into(),
        ))
    }

    fn consume_identifier(&mut self, message: &str) -> ParseResult<Identifier> {
        let line = self.peek().line();

        let result = if let Token::Identifier(ref name) = *self.peek().token() {
            Ok(Identifier::new(name.clone(), line))
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
                Token::Class
                | Token::Fun
                | Token::Var
                | Token::For
                | Token::If
                | Token::While
                | Token::Print
                | Token::Return => return,
                _ => {}
            }

            self.advance();
        }
    }

    fn declaration(&mut self) -> ParseResult<Stmt> {
        let result = if self.match_token(Token::Var) {
            self.var_declaration()
        } else if self.match_token(Token::Fun) {
            self.fun_declaration("function").map(Stmt::Fun)
        } else if self.match_token(Token::Class) {
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
        let name = self.consume_identifier("Expect variable name")?;

        let initializer = if self.match_token(Token::Equal) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(Token::Semicolon, "Expect ';' after variable declaration")?;

        Ok(Stmt::VarDecl(name, initializer))
    }

    fn fun_declaration(&mut self, kind: &str) -> ParseResult<FunctionDecl> {
        let identifier = self.consume_identifier(&format!("Expect {} name", kind))?;
        self.consume(Token::LeftParen, &format!("Expect '(' after {} name", kind))?;
        let (parameters, body) = self.fun_parameters_and_body()?;
        Ok(FunctionDecl {
            identifier,
            parameters,
            body,
        })
    }

    fn fun_expression(&mut self) -> ParseResult<FunctionExpr> {
        self.consume(Token::LeftParen, "Expect '(' after fun keyword")?;
        let (parameters, body) = self.fun_parameters_and_body()?;
        Ok(FunctionExpr { parameters, body })
    }

    fn fun_parameters_and_body(&mut self) -> ParseResult<(Vec<Identifier>, Vec<Stmt>)> {
        let mut parameters = Vec::new();
        if *self.peek().token() != Token::RightParen {
            loop {
                if parameters.len() >= PARAM_LIMIT {
                    return Err(ParseError::TooManyParams(
                        self.peek().clone(),
                        format!("Cannot have more than {} parameters", PARAM_LIMIT),
                    ));
                }

                let identifier = self.consume_identifier("Expect parameter name")?;
                parameters.push(identifier);
                if !self.match_token(Token::Comma) {
                    break;
                }
            }
        }

        self.consume(Token::RightParen, "Expect ')' after parameters")?;
        self.consume(Token::LeftBrace, "Expect '{' before function body")?;

        let body = self.block_statement()?;

        Ok((parameters, body))
    }

    fn class_declaration(&mut self) -> ParseResult<Class> {
        let identifier = self.consume_identifier("Expect class name")?;

        let superclass = if self.match_token(Token::Less) {
            let identifier = self.consume_identifier("Expect superclass name")?;
            Some(Var::new(identifier))
        } else {
            None
        };

        self.consume(Token::LeftBrace, "Expect '{' after class name")?;

        let mut methods = Vec::new();
        while !self.check(Token::RightBrace) && !self.is_at_end() {
            methods.push(self.fun_declaration("method")?);
        }

        self.consume(Token::RightBrace, "Expect '}' after class declaration")?;
        Ok(Class {
            identifier,
            superclass,
            methods,
        })
    }

    fn statement(&mut self) -> ParseResult<Stmt> {
        if self.match_token(Token::Print) {
            self.print_statement()
        } else if self.match_token(Token::LeftBrace) {
            Ok(Stmt::Block(self.block_statement()?))
        } else if self.match_token(Token::If) {
            self.if_statement()
        } else if self.match_token(Token::While) {
            self.while_statement()
        } else if self.match_token(Token::For) {
            self.for_statement()
        } else if let Some(line) = self.match_token_line(Token::Return) {
            self.return_statement(line)
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> ParseResult<Stmt> {
        let expr = self.expression()?;
        self.consume(Token::Semicolon, "Expect ';' after print statement")?;
        Ok(Stmt::Print(expr))
    }

    fn block_statement(&mut self) -> ParseResult<Vec<Stmt>> {
        let mut stmts = Vec::new();

        while !self.check(Token::RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }

        self.consume(Token::RightBrace, "Expect '}' after block")?;
        Ok(stmts)
    }

    fn if_statement(&mut self) -> ParseResult<Stmt> {
        self.consume(Token::LeftParen, "Expect '(' after if")?;
        let condition = self.expression()?;
        self.consume(Token::RightParen, "Expect ')' after if condition")?;
        let then_branch = Box::new(self.statement()?);

        // Check for the optional else.
        let else_branch = if self.match_token(Token::Else) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If(condition, then_branch, else_branch))
    }

    fn while_statement(&mut self) -> ParseResult<Stmt> {
        self.consume(Token::LeftParen, "Expect '(' after while")?;
        let condition = self.expression()?;
        self.consume(Token::RightParen, "Expect ')' after while condition")?;
        let body = self.statement()?;
        Ok(Stmt::While(condition, Box::new(body)))
    }

    fn for_statement(&mut self) -> ParseResult<Stmt> {
        self.consume(Token::LeftParen, "Expect '(' after for")?;

        let initializer = if self.match_token(Token::Var) {
            Some(self.var_declaration()?)
        } else if self.match_token(Token::Semicolon) {
            None
        } else {
            // It has to be an expression statement.
            Some(self.expression_statement()?)
        };

        let condition = if let Some(line) = self.match_token_line(Token::Semicolon) {
            ExprNode::new(Expr::Literal(Value::Bool(true)), line)
        } else {
            let condition = self.expression()?;
            self.consume(Token::Semicolon, "Expect ';' after for condition")?;
            condition
        };

        let increment = if *self.peek().token() == Token::RightParen {
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(Token::RightParen, "Expect ')' after for")?;

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

    fn return_statement(&mut self, line: usize) -> ParseResult<Stmt> {
        let expr = if !self.check(Token::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(Token::Semicolon, "Expect ';' after return statement")?;

        Ok(Stmt::Return(expr, line))
    }

    fn expression_statement(&mut self) -> ParseResult<Stmt> {
        let expr = self.expression()?;
        self.consume(Token::Semicolon, "Expect ';' after expression")?;
        Ok(Stmt::Expr(expr))
    }

    pub fn expression(&mut self) -> ParseResult<ExprNode> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParseResult<ExprNode> {
        let node = self.or()?;

        if self.match_token(Token::Equal) {
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

        while let Some(line) = self.match_token_line(Token::Or) {
            let right = self.and()?;
            node = ExprNode::new(
                Expr::Logical(Box::new(node), LogicOp::Or, Box::new(right)),
                line,
            );
        }

        Ok(node)
    }

    fn and(&mut self) -> ParseResult<ExprNode> {
        let mut node = self.equality()?;

        while let Some(line) = self.match_token_line(Token::And) {
            let right = self.equality()?;
            node = ExprNode::new(
                Expr::Logical(Box::new(node), LogicOp::And, Box::new(right)),
                line,
            );
        }

        Ok(node)
    }

    fn equality(&mut self) -> ParseResult<ExprNode> {
        let mut node = self.comparison()?;

        while let Some((op, line)) = self.match_op(vec![BinOp::BangEqual, BinOp::EqualEqual]) {
            let right = self.comparison()?;
            node = ExprNode::new(Expr::Binary(Box::new(node), op, Box::new(right)), line);
        }

        Ok(node)
    }

    fn comparison(&mut self) -> ParseResult<ExprNode> {
        let mut node = self.addition()?;

        while let Some((op, line)) = self.match_op(vec![
            BinOp::Greater,
            BinOp::GreaterEqual,
            BinOp::Less,
            BinOp::LessEqual,
        ]) {
            let right = self.addition()?;
            node = ExprNode::new(Expr::Binary(Box::new(node), op, Box::new(right)), line);
        }

        Ok(node)
    }

    fn addition(&mut self) -> ParseResult<ExprNode> {
        let mut node = self.multiplication()?;

        while let Some((op, line)) = self.match_op(vec![BinOp::Minus, BinOp::Plus]) {
            let right = self.multiplication()?;
            node = ExprNode::new(Expr::Binary(Box::new(node), op, Box::new(right)), line);
        }

        Ok(node)
    }

    fn multiplication(&mut self) -> ParseResult<ExprNode> {
        let mut node = self.unary()?;

        while let Some((op, line)) = self.match_op(vec![BinOp::Slash, BinOp::Star]) {
            let right = self.unary()?;
            node = ExprNode::new(Expr::Binary(Box::new(node), op, Box::new(right)), line);
        }

        Ok(node)
    }

    fn unary(&mut self) -> ParseResult<ExprNode> {
        if let Some((op, line)) = self.match_op(vec![UnaryOp::Minus, UnaryOp::Bang]) {
            let right = self.unary()?;
            Ok(ExprNode::new(Expr::Unary(op, Box::new(right)), line))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> ParseResult<ExprNode> {
        let mut node = self.primary()?;

        loop {
            if self.match_token(Token::LeftParen) {
                node = self.finish_call(node)?;
            } else if let Some(line) = self.match_token_line(Token::Dot) {
                let name = self.consume_identifier("Expect property name after '.'")?;
                node = ExprNode::new(Expr::Get(Box::new(node), name), line);
            } else {
                break;
            }
        }

        Ok(node)
    }

    fn finish_call(&mut self, callee: ExprNode) -> ParseResult<ExprNode> {
        let mut arguments = Vec::new();

        if !self.check(Token::RightParen) {
            loop {
                if arguments.len() >= PARAM_LIMIT {
                    return Err(ParseError::TooManyParams(
                        self.peek().clone(),
                        format!("Cannot have more than {} arguments", PARAM_LIMIT),
                    ));
                }
                arguments.push(self.expression()?);
                if !self.match_token(Token::Comma) {
                    break;
                }
            }
        }

        let paren = self.consume(Token::RightParen, "Expect ')' after arguments")?;
        Ok(ExprNode::new(
            Expr::Call(Box::new(callee), arguments),
            paren.line(),
        ))
    }

    fn primary(&mut self) -> ParseResult<ExprNode> {
        let next = self.advance().clone();
        let line = next.line();

        let expr = match *next.token() {
            Token::False => Expr::Literal(Value::Bool(false)),
            Token::True => Expr::Literal(Value::Bool(true)),
            Token::Nil => Expr::Literal(Value::Nil),
            Token::Super => {
                self.consume(Token::Dot, "Expect '.' after 'super'")?;
                let keyword = Var::new(Identifier::new("super".into(), line));
                let method = self.consume_identifier("Expect superclass method name")?;
                Expr::Super(keyword, Var::new(method))
            }
            Token::This => Expr::This(Var::new(Identifier::new("this".into(), line))),
            Token::Number(n) => Expr::Literal(Value::Number(n)),
            Token::String(ref s) => Expr::Literal(Value::String(s.clone())),
            Token::Identifier(ref name) => Expr::Var(Var::new(Identifier::new(name.clone(), line))),
            Token::LeftParen => {
                let expr = self.expression()?;
                self.consume(Token::RightParen, "Expect ')' after expression")?;
                Expr::Grouping(Box::new(expr))
            }
            Token::Fun => {
                match self.fun_expression() {
                    Ok(f) => Expr::Fun(f),
                    Err(_) => {
                        // Revert position so that we display the same errors as
                        // the reference Lox implementation which does not have
                        // anonymous functions.
                        self.pos -= 1;
                        return Err(ParseError::MissingExpr(next.clone()));
                    }
                }
            }
            _ => {
                self.pos -= 1;
                return Err(ParseError::MissingExpr(next.clone()));
            }
        };

        Ok(ExprNode::new(expr, line))
    }
}
