extern crate lox;
extern crate rustyline;

use std::env;
use std::fs::File;
use std::io::Read;

use rustyline::Editor;
use rustyline::error::ReadlineError;

use lox::interpreter::Interpreter;
use lox::parser::Parser;
use lox::resolver::Resolver;
use lox::scanner::{Scanner, Token};

fn main() {
    if let Some(filename) = env::args().nth(1) {
        execute_file(&filename);
    } else {
        repl();
    }
}

fn execute_file(filename: &str) {
    // Get the source code.
    let mut file = File::open(filename).expect("file not found");
    let mut src = String::new();
    file.read_to_string(&mut src).expect("error reading file");

    // Lex the tokens.
    let mut scanner = Scanner::new(&src);
    let tokens = scanner.scan_all();

    // Parse the list of statements.
    let mut parser = Parser::new(tokens);
    let mut stmts = parser.parse().expect("error parsing file");

    let mut resolver = Resolver::default();
    for stmt in &mut stmts {
        resolver.resolve_stmt(stmt);
    }

    Interpreter::default()
        .execute(&stmts)
        .expect("error executing program");
}

fn repl() {
    let mut rl = Editor::<()>::new();

    let history_path = format!("{}/.lox_history", env::var("HOME").unwrap());
    if rl.load_history(&history_path).is_err() {
        // No previous history, ignoring.
    }

    let mut interpreter = Interpreter::default();
    loop {
        match rl.readline("> ") {
            Ok(line) => {
                rl.add_history_entry(&line);
                execute_line(&mut interpreter, &line);
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    rl.save_history(&history_path).unwrap();
}

fn execute_line(interpreter: &mut Interpreter, line: &str) {
    let tokens = Scanner::new(line).scan_all();
    let len = tokens.len();
    let is_expression = len >= 2 && tokens[len - 2] != Token::Semicolon;
    let mut parser = Parser::new(tokens);

    if is_expression {
        // Evaluate and print the result of a single expression.
        match parser.expression() {
            Ok(expr) => match interpreter.evaluate(&expr) {
                Ok(value) => println!("{}", value),
                Err(e) => eprintln!("Error evaluating expression: {:?}", e),
            },
            Err(e) => eprintln!("Parse error: {}", e),
        }
    } else {
        // Evaluate a list of statements.
        match parser.parse() {
            Ok(stmts) => if let Err(e) = interpreter.execute(&stmts) {
                eprintln!("Interpreter error: {:?}", e);
            },
            Err(e) => eprintln!("Parse error: {}", e),
        }
    }
}
