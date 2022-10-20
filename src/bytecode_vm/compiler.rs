use crate::lox_frontend::grammar::{Expr, ExprType, Literal, Stmt, StmtType};
use crate::lox_frontend::operator::{InfixOperator, PrefixOperator};

use super::chunk::Chunk;
use super::opcode::OpCode;
use super::value::Value;
use super::vm::VM;

const DEBUG_COMPILE_CODE: bool = false;

pub struct Compiler<'vm_lifetime> {
    // Need to reference vm to handle string interning
    vm_ref: &'vm_lifetime mut VM,
}

impl<'vm_lifetime> Compiler<'vm_lifetime> {
    pub fn new(vm_ref: &'vm_lifetime mut VM) -> Self {
        Compiler { vm_ref }
    }

    pub fn compile(&mut self, stmt: &Stmt, chunk: &mut Chunk) {
        let expr = match &stmt.stmt {
            StmtType::Expression(expr) => expr,
            _ => panic!("Bytecode vm can only handle expressions right now."),
        };

        self.compile_expression(expr, chunk);
        chunk.write_instruction(OpCode::Return, 99);

        if DEBUG_COMPILE_CODE {
            chunk.disassemble("Compiler code.")
        }
    }

    fn compile_expression(&mut self, expr: &Expr, chunk: &mut Chunk) {
        let line = expr.span.start_pos.line_no;

        match &expr.expr {
            ExprType::Literal(l) => self.compile_literal(l, line, chunk),
            ExprType::Infix(op, lhs, rhs) => self.compile_infix(*op, lhs, rhs, chunk),
            ExprType::Prefix(op, expr) => self.compile_prefix(*op, expr, chunk),
            _ => panic!("Bytecode vm cannot compile this expression right now."),
        }
    }

    fn compile_literal(&mut self, l: &Literal, line: usize, chunk: &mut Chunk) {
        match l {
            Literal::Number(n) => {
                let value = Value::Number(*n);
                let index = chunk.add_constant(value);
                chunk.write_instruction(OpCode::Constant, line);
                chunk.write_byte(index, line);
            }
            Literal::Boolean(b) => {
                let opcode = match *b {
                    true => OpCode::True,
                    false => OpCode::False,
                };
                chunk.write_instruction(opcode, line);
            }
            Literal::Nil => {
                chunk.write_instruction(OpCode::Nil, line);
            }
            Literal::Str(s) => {
                let string_intern = Value::String(self.vm_ref.get_string_intern(s));
                let index = chunk.add_constant(string_intern);
                chunk.write_instruction(OpCode::Constant, line);
                chunk.write_byte(index, line);
            }
        }
    }

    fn compile_infix(&mut self, op: InfixOperator, lhs: &Expr, rhs: &Expr, chunk: &mut Chunk) {
        let line = lhs.span.end_pos.line_no;

        self.compile_expression(lhs, chunk);
        self.compile_expression(rhs, chunk);

        match op {
            InfixOperator::Add => chunk.write_instruction(OpCode::Add, line),
            InfixOperator::Subtract => chunk.write_instruction(OpCode::Subtract, line),
            InfixOperator::Multiply => chunk.write_instruction(OpCode::Multiply, line),
            InfixOperator::Divide => chunk.write_instruction(OpCode::Divide, line),
            InfixOperator::EqualTo => chunk.write_instruction(OpCode::Equal, line),
            InfixOperator::NotEqualTo => {
                chunk.write_instruction(OpCode::Equal, line);
                chunk.write_instruction(OpCode::Not, line)
            }
            InfixOperator::GreaterThan => chunk.write_instruction(OpCode::GreaterThan, line),
            InfixOperator::GreaterEq => {
                chunk.write_instruction(OpCode::LessThan, line);
                chunk.write_instruction(OpCode::Not, line)
            }
            InfixOperator::LessThan => chunk.write_instruction(OpCode::LessThan, line),
            InfixOperator::LessEq => {
                chunk.write_instruction(OpCode::GreaterThan, line);
                chunk.write_instruction(OpCode::Not, line)
            }
        };
    }

    fn compile_prefix(&mut self, op: PrefixOperator, expr: &Expr, chunk: &mut Chunk) {
        let line = expr.span.start_pos.line_no;

        self.compile_expression(expr, chunk);

        let opcode = match op {
            PrefixOperator::Negate => OpCode::Negate,
            PrefixOperator::LogicalNot => OpCode::Not,
        };

        chunk.write_instruction(opcode, line);
    }
}
