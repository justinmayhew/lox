extern crate lox;
extern crate rustyline;

use std::env;
use std::fs::File;
use std::io::Read;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use lox::parser::*;
use lox::scanner::*;
use lox::interpreter::*;

static HISTORY_FILE: &'static str = ".lox_history";

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
    let stmts = parser.parse().expect("error parsing file");

    let mut interpreter = Interpreter::new();
    interpreter.execute(&stmts).expect("error executing program");
}

fn repl() {
    let mut interpreter = Interpreter::new();

    let mut rl = Editor::<()>::new();
    if rl.load_history(HISTORY_FILE).is_err() {
        println!("No previous history.");
    }

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

    rl.save_history(HISTORY_FILE).unwrap();
}

fn execute_line(interpreter: &mut Interpreter, line: &str) {
    let mut scanner = Scanner::new(line);
    let tokens = scanner.scan_all();

    let ntokens = tokens.len();
    if ntokens >= 2 && tokens[ntokens - 2] != Token::Semicolon {
        // Evaluate a single expr
        let mut parser = Parser::new(tokens);
        match parser.expression() {
            Ok(expr) => match interpreter.evaluate(&expr) {
                Ok(value) => println!("{}", value),
                Err(e) => eprintln!("Error evaluating expression: {:?}", e),
            },
            Err(e) => eprintln!("Parse error: {}", e),
        }
    } else {
        // Evaluate a list of stmts
        let mut parser = Parser::new(tokens);
        match parser.parse() {
            Ok(stmts) => if let Err(e) = interpreter.execute(&stmts) {
                eprintln!("Interpreter error: {:?}", e);
            },
            Err(e) => eprintln!("Parse error: {}", e),
        }
    }
}
