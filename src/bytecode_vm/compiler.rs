use crate::lox_frontend::grammar::{Expr, ExprType, Literal, Stmt, StmtType};
use crate::lox_frontend::operator::{InfixOperator, PrefixOperator};

use super::chunk::Chunk;
use super::opcode::OpCode;

const DEBUG_COMPILE_CODE: bool = false;

pub struct Compiler {}

impl Compiler {
    pub fn compile(stmt: &Stmt, chunk: &mut Chunk) {
        let expr = match &stmt.stmt {
            StmtType::Expression(expr) => expr,
            _ => panic!("Bytecode vm can only handle expressions right now."),
        };

        Self::compile_expression(expr, chunk);
        chunk.write_instruction(OpCode::Return, 817);

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
                let index = chunk.add_constant(*n);
                chunk.write_instruction(OpCode::Constant, line);
                chunk.write_byte(index, line);
            }
            _ => panic!("Bytecode vm cannot compile this type of literal right now."),
        }
    }

    fn compile_infix(op: InfixOperator, lhs: &Expr, rhs: &Expr, chunk: &mut Chunk) {
        let line = lhs.span.end_pos.line_no;

        Self::compile_expression(lhs, chunk);
        Self::compile_expression(rhs, chunk);

        let opcode = match op {
            InfixOperator::Add => OpCode::Add,
            InfixOperator::Subtract => OpCode::Subtract,
            InfixOperator::Multiply => OpCode::Multiply,
            InfixOperator::Divide => OpCode::Divide,
            _ => panic!("Bytecode vm cannot compile this type of infix right now."),
        };

        chunk.write_instruction(opcode, line);
    }

    fn compile_prefix(op: PrefixOperator, expr: &Expr, chunk: &mut Chunk) {
        let line = expr.span.start_pos.line_no;

        let opcode = match op {
            PrefixOperator::Negate => OpCode::Negate,
            _ => panic!("Bytecode vm cannot compile this type of prefix right now."),
        };

        chunk.write_instruction(opcode, line);
    }
}
