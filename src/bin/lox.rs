extern crate lox;
extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use lox::parser::*;
use lox::scanner::*;
use lox::interpreter::*;

static HISTORY_FILE: &'static str = ".lox_history";

fn main() {
    let mut interpreter = Interpreter::new();

    let mut rl = Editor::<()>::new();
    if let Err(_) = rl.load_history(HISTORY_FILE) {
        println!("No previous history.");
    }

    loop {
        match rl.readline("> ") {
            Ok(line) => {
                rl.add_history_entry(&line);
                run(&mut interpreter, line);
            }
            Err(ReadlineError::Interrupted) |
            Err(ReadlineError::Eof) => {
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }

    rl.save_history(HISTORY_FILE).unwrap();
}

fn run(interpreter: &mut Interpreter, line: String) {
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

    let ntokens = tokens.len();
    if ntokens >= 2 && tokens[ntokens - 2] != Token::Semicolon {
        // Evaluate a single expr
        let mut parser = Parser::new(tokens);
        match parser.expression() {
            Ok(expr) => {
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
                if let Err(e) = interpreter.execute(stmts) {
                    eprintln!("Interpreter error: {:?}", e);
                }
            }
            Err(e) => eprintln!("Parse error: {}", e),
        }
    }
}
