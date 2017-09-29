use std::fmt;

#[derive(Copy, Clone)]
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

#[derive(Copy, Clone)]
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

pub enum Expr {
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Value),
    Unary(UnaryOp, Box<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expr::Binary(ref left, op, ref right) => write!(f, "({} {} {})", op, left, right),
            Expr::Grouping(ref expr) => write!(f, "(group {})", expr),
            Expr::Literal(ref value) => write!(f, "{}", value),
            Expr::Unary(op, ref expr) => write!(f, "({} {})", op, expr),
        }
    }
}
