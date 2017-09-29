extern crate lox;

use std::io::{self, Write};

use lox::parser::*;
use lox::scanner::*;

fn main() {
    // let expr = Expr::Binary(
    //     Box::new(Expr::Unary(
    //         UnaryOp::Minus,
    //         Box::new(Expr::Literal(Value::Int(123))),
    //     )),
    //     BinOp::Star,
    //     Box::new(Expr::Grouping(Box::new(Expr::Literal(Value::Int(456))))),
    // );

    // println!("{}", expr);

    loop {
        print!("> ");
        io::stdout().flush().expect("error flushing stdout");

        let mut line = String::new();
        io::stdin().read_line(&mut line).unwrap();

        if line.is_empty() {
            break;
        }

        let mut scanner = Scanner::new(&line);
        let mut tokens = Vec::new();

        loop {
            match scanner.next_token() {
                Ok(token) => {
                    let mut stop = false;

                    if token == Token::Eof {
                        stop = true;
                    }
                    tokens.push(token);

                    if stop {
                        break;
                    }
                }
                Err(e) => panic!("Error scanning: {:?}", e),
            }
        }

        println!("Tokens: {:?}", tokens);

        let mut parser = Parser::new(tokens);
        match parser.parse() {
            Ok(expr) => println!("Expr: {}", expr),
            Err(e) => eprintln!("Parse error: {:?}", e),
        }

        println!("");
    }
}
