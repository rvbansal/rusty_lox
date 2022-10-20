use crate::bytecode_vm::{Chunk, Compiler, VM};
use crate::lox_frontend::{Lexer, Parser};
use crate::treewalk_interpreter::{Interpreter, Resolver};

mod bytecode_vm;
mod lox_frontend;
mod treewalk_interpreter;

use std::io::Write;
use std::{env, fs, io, process};

type RunResult = Result<(), String>;

const USE_BYTECODE_VM: bool = true;

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

/// Run prompt from REPL.
fn run_prompt() {
    let mut interpreter = Interpreter::new();

    loop {
        let mut input = String::new();
        print!("> ");
        loop {
            io::stdout().flush().unwrap();
            let bytes_read = io::stdin()
                .read_line(&mut input)
                .expect("Failed to read line.");

            if bytes_read <= 1 {
                break;
            }

            print!("  ");
        }

        match run(&mut interpreter, &input) {
            Ok(_) => {}
            Err(e) => report_error(&e),
        }
    }
}

/// Run .lox source code file.
fn run_file(filename: &str) {
    let contents = fs::read_to_string(filename).expect("Failed to read file.");
    let mut interpreter = Interpreter::new();

    match run(&mut interpreter, &contents) {
        Ok(_) => {}
        Err(e) => report_error(&e),
    }
}

fn run(interpreter: &mut Interpreter, source: &str) -> RunResult {
    // Run lexer.
    let lexer = Lexer::new(source);
    let tokens: Result<Vec<_>, _> = lexer.iter().collect();
    let tokens = match tokens {
        Ok(tokens) => tokens,
        Err(e) => return Err(e),
    };

    // Run parser.
    let parser = Parser::new(tokens.into_iter());
    let stmts: Vec<_> = parser.parse();
    let mut stmts: Vec<_> = match stmts.into_iter().collect() {
        Ok(stmts) => stmts,
        Err(e) => return Err(format!("{:?}", e)),
    };

    if USE_BYTECODE_VM {
        let mut vm = VM::new();
        let mut compiler = Compiler::new(&mut vm);

        let mut bytecode = Chunk::new();
        compiler.compile(&stmts[0], &mut bytecode);

        match vm.interpret(&bytecode) {
            Ok(_) => Ok(()),
            Err(e) => Err(format!("{:?}", e)),
        }
    } else {
        // Run resolver.
        let mut resolver = Resolver::new();
        for stmt in stmts.iter_mut() {
            match resolver.resolve_root(stmt) {
                Ok(_) => (),
                Err(e) => return Err(format!("{:?}", e)),
            }
        }

        // Run interpreter.
        match interpreter.eval_statements(stmts) {
            Ok(_) => Ok(()),
            Err(e) => Err(format!("{:?}", e)),
        }
    }
}

fn report_error(error_message: &str) {
    eprintln!("An error: {}", error_message);
}
