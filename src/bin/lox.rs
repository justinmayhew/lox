extern crate lox;

use std::io::{self, Write};

use lox::parser::*;
use lox::scanner::*;
use lox::interpreter::*;

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
                    let stop = token == Token::Eof;
                    tokens.push(token);

                    if stop {
                        break;
                    }
                }
                Err(e) => panic!("Error scanning: {:?}", e),
            }
        }

        // println!("Tokens: {:?}", tokens);

        let ntokens = tokens.len();
        if ntokens >= 2 && tokens[ntokens - 2] != Token::Semicolon {
            // Evaluate a single expr
            let mut parser = Parser::new(tokens);
            match parser.expression() {
                Ok(expr) => {
                    let interpreter = Interpreter::new();
                    match interpreter.evaluate(expr) {
                        Ok(value) => println!("{}", value),
                        Err(e) => eprintln!("Error evaluating expression: {:?}", e),
                    }
                }
                Err(e) => eprintln!("Parse error: {}", e),
            }
        } else {
            // Evaluate a list of stmts
            let mut parser = Parser::new(tokens);
            match parser.parse() {
                Ok(stmts) => {
                    let mut interpreter = Interpreter::new();
                    if let Err(e) = interpreter.execute(stmts) {
                        eprintln!("Interpreter error: {:?}", e);
                    }
                }
                Err(e) => eprintln!("Parse error: {}", e),
            }
        }
    }
}
