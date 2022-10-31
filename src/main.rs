use crate::bytecode_vm::{Compiler, VM};
use crate::lox_frontend::grammar::Stmt;
use crate::lox_frontend::Parser;
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

    let interpreter: Box<dyn Run> = if USE_BYTECODE_VM {
        Box::new(VM::new())
    } else {
        Box::new(Interpreter::new())
    };

    match args.len() {
        1 => run_prompt(interpreter),
        2 => run_file(interpreter, &args[1]),
        _ => {
            eprintln!("Usage: rlox [script]");
            process::exit(64);
        }
    }
}

/// Run prompt from REPL.
fn run_prompt(mut interpreter: Box<dyn Run>) {
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

        match run(interpreter.as_mut(), &input) {
            Ok(_) => {}
            Err(e) => report_error(&e),
        }
    }
}

/// Run .lox source code file.
fn run_file(mut interpreter: Box<dyn Run>, filename: &str) {
    let contents = fs::read_to_string(filename).expect("Failed to read file.");

    match run(interpreter.as_mut(), &contents) {
        Ok(_) => {}
        Err(e) => report_error(&e),
    }
}

fn run(interpreter: &mut dyn Run, source: &str) -> RunResult {
    // Run parser.
    let parser = Parser::new(source);
    let stmts: Vec<_> = parser.parse();
    let stmts: Vec<_> = match stmts.into_iter().collect() {
        Ok(stmts) => stmts,
        Err(e) => return Err(format!("{:?}", e)),
    };

    interpreter.consume_statements(stmts)
}

trait Run {
    fn consume_statements(&mut self, stmts: Vec<Stmt>) -> RunResult;
}

impl Run for VM {
    fn consume_statements(&mut self, stmts: Vec<Stmt>) -> RunResult {
        let mut compiler = Compiler::new(self.borrow_string_table());

        let main_fn = match compiler.compile(&stmts) {
            Ok(main_fn) => main_fn,
            Err(e) => return Err(format!("{:?}", e)),
        };

        match self.interpret(main_fn) {
            Ok(_) => Ok(()),
            Err(e) => Err(format!("{:?}", e)),
        }
    }
}

impl Run for Interpreter {
    fn consume_statements(&mut self, mut stmts: Vec<Stmt>) -> RunResult {
        let mut resolver = Resolver::new();
        for stmt in stmts.iter_mut() {
            match resolver.resolve_root(stmt) {
                Ok(_) => (),
                Err(e) => return Err(format!("{:?}", e)),
            }
        }

        match self.eval_statements(stmts) {
            Ok(_) => Ok(()),
            Err(e) => Err(format!("{:?}", e)),
        }
    }
}

fn report_error(error_message: &str) {
    eprintln!("An error: {}", error_message);
}
