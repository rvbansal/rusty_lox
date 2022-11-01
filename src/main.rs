use crate::lox_frontend::Parser;

use crate::bytecode_vm::{Compiler, VM};
use crate::treewalk_interpreter::{Interpreter, Resolver};

use clap::{Parser as ClapParser, Subcommand};
use std::io::{Stdout, Write};
use std::{fs, io};

mod bytecode_vm;
mod lox_frontend;
mod treewalk_interpreter;

#[derive(ClapParser)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    filename: Option<String>,
    #[clap(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Parse,
    Treewalk,
    Bytecode,
}

enum Runner {
    Parser,
    Treewalk(Interpreter<Stdout>),
    Bytecode(Box<VM<Stdout>>),
}

fn main() {
    let cli = Cli::parse();

    let runner = match cli.command {
        Commands::Parse => Runner::Parser,
        Commands::Bytecode => Runner::Bytecode(Box::new(VM::new())),
        Commands::Treewalk => Runner::Treewalk(Interpreter::new()),
    };

    match cli.filename {
        Some(filename) => run_file(runner, &filename),
        None => run_repl(runner),
    }
}

fn run_repl(mut runner: Runner) {
    loop {
        let mut input = String::new();
        print!("> ");

        loop {
            io::stdout().flush().unwrap();
            let mut new_line = String::new();
            io::stdin()
                .read_line(&mut new_line)
                .expect("Failed to read line");

            if new_line.trim().is_empty() {
                break;
            }
            input += &new_line;

            print!("  ");
        }

        match runner.consume(&input) {
            Ok(()) => {}
            Err(errors) => {
                for error in errors {
                    eprintln!("{}", error);
                }
            }
        }
    }
}

fn run_file(mut runner: Runner, filename: &str) {
    let contents = fs::read_to_string(filename).expect("Failed to read file.");

    match runner.consume(&contents) {
        Ok(()) => {}
        Err(errors) => {
            for error in errors {
                eprintln!("{}", error);
            }
        }
    }
}

impl Runner {
    fn consume(&mut self, source: &str) -> Result<(), Vec<String>> {
        let parser = Parser::new(source);
        let tree = parser.parse().map_err(|errors| {
            errors
                .into_iter()
                .map(|e| e.render(source))
                .collect::<Vec<_>>()
        })?;

        match self {
            Runner::Parser => {
                println!("{:?}", tree);
            }
            Runner::Treewalk(interpreter) => {
                let resolver = Resolver::new();
                let tree = resolver
                    .resolve(tree)
                    .map_err(|e| vec![format!("{:?}", e)])?;

                interpreter
                    .eval_statements(&tree.stmts)
                    .map_err(|e| vec![format!("{:?}", e)])?;
            }
            Runner::Bytecode(vm) => {
                let mut compiler = Compiler::new(vm.borrow_string_table());
                let main_fn = compiler
                    .compile(&tree.stmts)
                    .map_err(|e| vec![format!("{:?}", e)])?;

                vm.interpret(main_fn)
                    .map_err(|e| vec![format!("{:?}", e)])?;
            }
        }

        Ok(())
    }
}
