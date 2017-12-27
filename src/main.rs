extern crate env_logger;
#[macro_use]
extern crate log;
extern crate rustyline;
extern crate time;

mod callable;
mod class;
mod environment;
mod instance;
mod interpreter;
mod parser;
mod primitive;
mod resolver;
mod scanner;

use std::env;
use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::process;
use std::result;

use rustyline::Editor;
use rustyline::error::ReadlineError;

use interpreter::Interpreter;
use parser::{Parser, Stmt};
use resolver::resolve;
use scanner::Scanner;

type Result<T> = result::Result<T, Box<Error>>;

const ERROR_CODE: i32 = 65;
const RUNTIME_ERROR_CODE: i32 = 70;

fn main() {
    env_logger::init().unwrap();

    if let Some(filename) = env::args().nth(1) {
        execute_file(&filename);
    } else {
        repl();
    }
}

fn execute_file(filename: &str) {
    let mut file = File::open(filename).expect("file not found");
    let mut src = String::new();
    file.read_to_string(&mut src).expect("error reading file");

    let mut scanner = Scanner::new(&src);
    let (tokens, scan_err) = scanner.scan();

    let mut parser = Parser::new(tokens);
    let mut stmts = match parser.parse() {
        Ok(stmts) => stmts,
        Err(()) => process::exit(ERROR_CODE),
    };

    if scan_err || resolve(&mut stmts).is_err() {
        process::exit(ERROR_CODE)
    }

    if let Err(err) = Interpreter::default().execute(&stmts) {
        eprintln!("{}\n[line {}]", err, err.line());
        process::exit(RUNTIME_ERROR_CODE);
    }
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
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }

    rl.save_history(&history_path).unwrap();
}

fn execute_line(interpreter: &mut Interpreter, line: &str) {
    let mut scanner = Scanner::new(line);
    let (tokens, scan_err) = scanner.scan();

    let mut parser = Parser::new(tokens);

    let mut stmts = if let Ok(expr) = parser.expression() {
        vec![Stmt::Print(expr)]
    } else {
        match parser.reset().parse() {
            Ok(stmts) => stmts,
            Err(()) => return,
        }
    };

    if scan_err || resolve(&mut stmts).is_err() {
        return;
    }

    if let Err(err) = interpreter.execute(&stmts) {
        eprintln!("{}", err);
    }
}
