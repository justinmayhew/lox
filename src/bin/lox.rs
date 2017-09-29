extern crate lox;

use lox::parser::*;

fn main() {
    let expr = Expr::Binary(
        Box::new(Expr::Unary(
            UnaryOp::Minus,
            Box::new(Expr::Literal(Value::Int(123))),
        )),
        BinOp::Star,
        Box::new(Expr::Grouping(Box::new(Expr::Literal(Value::Int(456))))),
    );

    println!("{}", expr);
}
