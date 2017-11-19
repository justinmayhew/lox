extern crate lox;
extern crate rustyline;

use std::env;
use std::fs::File;
use std::io::Read;

use rustyline::Editor;
use rustyline::error::ReadlineError;

use lox::interpreter::Interpreter;
use lox::parser::{Parser, Stmt};
use lox::resolver::resolve;
use lox::scanner::Scanner;

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
    let tokens = scanner.scan();

    // Parse the list of statements.
    let mut parser = Parser::new(tokens);
    let mut stmts = parser.parse().expect("error parsing file");

    resolve(&mut stmts);

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
    let mut parser = Parser::new(Scanner::new(line).scan());

    let mut stmts = if let Ok(expr) = parser.expression() {
        vec![Stmt::Print(expr)]
    } else {
        match parser.reset().parse() {
            Ok(stmts) => stmts,
            Err(err) => {
                eprintln!("Parse error: {}", err);
                return;
            }
        }
    };

    resolve(&mut stmts);
    if let Err(err) = interpreter.execute(&stmts) {
        eprintln!("{}", err);
    }
}
