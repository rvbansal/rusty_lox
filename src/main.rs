use crate::bytecode_vm::{Compiler, VM};

use crate::lox_frontend::grammar::Tree;
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
    };
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

        run(interpreter.as_mut(), input);
    }
}

/// Run .lox source code file.
fn run_file(mut interpreter: Box<dyn Run>, filename: &str) {
    let contents = fs::read_to_string(filename).expect("Failed to read file.");
    run(interpreter.as_mut(), contents);
}

fn run(interpreter: &mut dyn Run, source: String) {
    // Run parser.
    let parser = Parser::new(&source);

    match parser.parse() {
        Ok(tree) => match interpreter.consume(tree) {
            Ok(_) => {}
            Err(e) => eprintln!("{:?}", e),
        },
        Err(errors) => {
            eprintln!("Parser errors:");
            for e in errors {
                eprintln!("{}", e)
            }
        }
    }
}

trait Run {
    fn consume(&mut self, tree: Tree) -> RunResult;
}

impl<S: std::io::Write> Run for VM<S> {
    fn consume(&mut self, tree: Tree) -> RunResult {
        let mut compiler = Compiler::new(self.borrow_string_table());

        let main_fn = match compiler.compile(&tree.stmts) {
            Ok(main_fn) => main_fn,
            Err(e) => return Err(format!("{:?}", e)),
        };

        match self.interpret(main_fn) {
            Ok(_) => Ok(()),
            Err(e) => Err(format!("{:?}", e)),
        }
    }
}

impl<S: std::io::Write> Run for Interpreter<S> {
    fn consume(&mut self, tree: Tree) -> RunResult {
        let resolver = Resolver::new();
        let tree = match resolver.resolve(tree) {
            Ok(t) => t,
            Err(e) => return Err(format!("{:?}", e)),
        };

        match self.eval_statements(tree.stmts) {
            Ok(_) => Ok(()),
            Err(e) => Err(format!("{:?}", e)),
        }
    }
}
