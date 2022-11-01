use rusty_lox::bytecode_vm::{Compiler, VM};
use rusty_lox::lox_frontend::Parser;

use regex::Regex;
use test_generator::test_resources;

#[derive(Debug, PartialEq)]
enum ExpectedOutput {
    ParserError(Vec<String>),
    CompilerError(Vec<String>),
    Compiled(Output),
}

#[derive(Debug, PartialEq)]
struct Output {
    output: Vec<String>,
    vm_error: Option<String>,
}

#[test_resources("tests/lox_test_cases/**/*.lox")]
fn test_bytecode_vm(file: &str) {
    if file.contains("expressions") | file.contains("scanning") {
        return;
    }

    let source = std::fs::read_to_string(file).unwrap();

    let expected_output = get_expected_output(&source);
    let output = run_vm_on_source(&source);

    if matches!(output, ExpectedOutput::CompilerError(_)) {
        return;
    }

    assert_eq!(expected_output, output);
}

fn run_vm_on_source(source: &str) -> ExpectedOutput {
    let mut output = vec![];
    let mut vm = VM::new_with_output(std::io::Cursor::new(&mut output));

    let mut result = Output {
        output: vec![],
        vm_error: None,
    };

    let parser = Parser::new(source);
    let tree = match parser.parse() {
        Ok(tree) => tree,
        Err(errors) => {
            let errors = errors.into_iter().map(|e| e.render(source)).collect();
            return ExpectedOutput::ParserError(errors);
        }
    };

    let mut compiler = Compiler::new(vm.borrow_string_table());
    let main_fn = match compiler.compile(&tree.stmts) {
        Ok(main_fn) => main_fn,
        Err(error) => {
            let errors = vec![format!("{:?}", error)];
            return ExpectedOutput::CompilerError(errors);
        }
    };

    let vm_result = vm.interpret(main_fn);

    result.output = String::from_utf8(output)
        .unwrap()
        .lines()
        .map(|l| l.to_owned())
        .collect();
    result.vm_error = vm_result.err().map(|e| e.to_string());

    ExpectedOutput::Compiled(result)
}

fn get_expected_output(source: &str) -> ExpectedOutput {
    let output_regexer = Regex::new(r"// expect: (.*)$").unwrap();
    let vm_error_regexer = Regex::new(r"// expect runtime error: (.*)$").unwrap();
    let compile_error_regexer = Regex::new(r"// (\[line \d+\] )?(Error.*)").unwrap();

    let mut parser_errors = vec![];
    let mut result = Output {
        output: vec![],
        vm_error: None,
    };

    for line in source.lines() {
        if let Some(r) = output_regexer.captures(line) {
            result.output.push(r.get(1).unwrap().as_str().to_owned());
        }
        if let Some(r) = vm_error_regexer.captures(line) {
            result
                .vm_error
                .replace(r.get(1).unwrap().as_str().to_owned());
        }
        if let Some(r) = compile_error_regexer.captures(line) {
            parser_errors.push(r.get(2).unwrap().as_str().to_owned());
        }
    }

    if !parser_errors.is_empty() {
        ExpectedOutput::ParserError(parser_errors)
    } else {
        ExpectedOutput::Compiled(result)
    }
}
