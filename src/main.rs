use treewalk::Interpreter;

use crate::treewalk::token::SpannedToken;
use crate::treewalk::Lexer;
use crate::treewalk::Parser;
use std::io::Write;
use std::{env, fs, io, process};

mod treewalk;

type RunResult = Result<(), String>;

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => {
            eprintln!("Usage: rlox [script]");
            process::exit(64);
        }
    }
}

fn run_prompt() {
    let mut interpreter = Interpreter::new();

    loop {
        let mut input = String::new();

        print!("> ");
        io::stdout().flush().unwrap();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line.");

        match run(&mut interpreter, &input) {
            Ok(_) => {}
            Err(e) => report_error(&e),
        }
    }
}

fn run_file(filename: &str) {
    let contents = fs::read_to_string(filename).expect("Failed to read file.");
    let mut interpreter = Interpreter::new();

    match run(&mut interpreter, &contents) {
        Ok(_) => {}
        Err(e) => report_error(&e),
    }
}

fn run(interpreter: &mut Interpreter, source: &str) -> RunResult {
    let lexer = Lexer::new(source);
    let tokens: Result<Vec<_>, _> = lexer.iter().collect();

    let tokens = match tokens {
        Ok(tokens) => tokens,
        Err(e) => return Err(e),
    };

    let parser = Parser::new(tokens.into_iter());
    let statements: Result<Vec<_>, _> = parser.parse().into_iter().collect();

    let statements = match statements {
        Ok(statements) => statements,
        Err(e) => return Err(format!("{:?}", e)),
    };

    // Evaluate expression.
    match interpreter.eval_statements(statements) {
        Ok(_) => Ok(()),
        Err(e) => Err(format!("{:?}", e)),
    }
}

fn report_error(error_message: &str) {
    eprintln!("An error: {}", error_message);
}
