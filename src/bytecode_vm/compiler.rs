use crate::lox_frontend::grammar::{Expr, ExprType, Literal, Stmt, StmtType};
use crate::lox_frontend::operator::{InfixOperator, PrefixOperator};

use super::chunk::Chunk;
use super::opcode::OpCode;
use super::value::Value;

const DEBUG_COMPILE_CODE: bool = false;

pub struct Compiler {}

impl Compiler {
    pub fn compile(stmt: &Stmt, chunk: &mut Chunk) {
        let expr = match &stmt.stmt {
            StmtType::Expression(expr) => expr,
            _ => panic!("Bytecode vm can only handle expressions right now."),
        };

        println!("{:?}", expr);

        Self::compile_expression(expr, chunk);
        chunk.write_instruction(OpCode::Return, 9999999);

        if DEBUG_COMPILE_CODE {
            chunk.disassemble("Compiler code.")
        }
    }

    fn compile_expression(expr: &Expr, chunk: &mut Chunk) {
        let line = expr.span.start_pos.line_no;

        match &expr.expr {
            ExprType::Literal(l) => Self::compile_literal(l, line, chunk),
            ExprType::Infix(op, lhs, rhs) => Self::compile_infix(*op, lhs, rhs, chunk),
            ExprType::Prefix(op, expr) => Self::compile_prefix(*op, expr, chunk),
            _ => panic!("Bytecode vm cannot compile this expression right now."),
        }
    }

    fn compile_literal(l: &Literal, line: usize, chunk: &mut Chunk) {
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
            _ => panic!("Bytecode vm cannot compile this type of literal right now."),
        }
    }

    fn compile_infix(op: InfixOperator, lhs: &Expr, rhs: &Expr, chunk: &mut Chunk) {
        let line = lhs.span.end_pos.line_no;

        Self::compile_expression(lhs, chunk);
        Self::compile_expression(rhs, chunk);

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

    fn compile_prefix(op: PrefixOperator, expr: &Expr, chunk: &mut Chunk) {
        let line = expr.span.start_pos.line_no;

        Self::compile_expression(expr, chunk);

        let opcode = match op {
            PrefixOperator::Negate => OpCode::Negate,
            PrefixOperator::LogicalNot => OpCode::Not,
        };

        chunk.write_instruction(opcode, line);
    }
}
