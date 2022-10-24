use crate::lox_frontend::grammar::{Expr, ExprType, Literal, Stmt, StmtType, VariableInfo};
use crate::lox_frontend::operator::{InfixOperator, PrefixOperator};

use super::chunk::{Chunk, ConstantIndex};
use super::errors::{CompilerError, CompilerResult};
use super::opcode::OpCode;
use super::value::Value;
use super::vm::VM;

const DEBUG_COMPILE_CODE: bool = false;
// Index of local var in stack is stored in u8 for GET_LOCAL instruction.
const MAX_LOCAL_VARS: usize = 256;

type LocalIndex = u8;

struct Local {
    name: String,
    scope_depth: u32,
    initialized: bool,
}

pub struct Compiler<'vm_lifetime> {
    // Need to reference vm to handle string interning
    vm_ref: &'vm_lifetime mut VM,
    locals: Vec<Local>,
    current_scope_depth: u32,
}

impl<'vm_lifetime> Compiler<'vm_lifetime> {
    pub fn new(vm_ref: &'vm_lifetime mut VM) -> Self {
        Compiler {
            vm_ref,
            locals: vec![],
            current_scope_depth: 0,
        }
    }

    pub fn compile(&mut self, stmts: &Vec<Stmt>, chunk: &mut Chunk) -> CompilerResult<()> {
        for stmt in stmts.iter() {
            self.compile_statement(stmt, chunk)?;
        }

        chunk.write_instruction(OpCode::Return, 99);

        if DEBUG_COMPILE_CODE {
            chunk.disassemble("Compiler code.")
        }

        Ok(())
    }

    pub fn compile_statement(&mut self, stmt: &Stmt, chunk: &mut Chunk) -> CompilerResult<()> {
        let line = stmt.span.start_pos.line_no;
        match &stmt.stmt {
            StmtType::Expression(expr) => {
                self.compile_expression(expr, chunk)?;
                chunk.write_instruction(OpCode::Pop, line);
            }
            StmtType::Print(expr) => {
                self.compile_expression(expr, chunk)?;
                chunk.write_instruction(OpCode::Print, line);
            }
            StmtType::VariableDecl(name, expr) => {
                self.define_variable(name, expr, chunk, line)?;
            }
            StmtType::Block(stmts) => {
                self.begin_scope();
                for stmt in stmts.iter() {
                    self.compile_statement(stmt, chunk)?;
                }
                self.end_scope(chunk);
            }
            _ => panic!("Bytecode vm cannot compile this statement right now."),
        };

        Ok(())
    }

    fn compile_expression(&mut self, expr: &Expr, chunk: &mut Chunk) -> CompilerResult<()> {
        let line = expr.span.start_pos.line_no;

        match &expr.expr {
            ExprType::Literal(l) => self.compile_literal(l, line, chunk),
            ExprType::Infix(op, lhs, rhs) => self.compile_infix(*op, lhs, rhs, chunk)?,
            ExprType::Prefix(op, expr) => self.compile_prefix(*op, expr, chunk)?,
            ExprType::Variable(var) => self.get_variable(var, chunk, line)?,
            ExprType::Assignment(var, expr) => self.set_variable(var, expr, chunk, line)?,
            _ => panic!("Bytecode vm cannot compile this expression right now."),
        }

        Ok(())
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
                let index = self.add_constant_string(s, chunk);
                chunk.write_instruction(OpCode::Constant, line);
                chunk.write_byte(index, line);
            }
        }
    }

    fn compile_infix(
        &mut self,
        op: InfixOperator,
        lhs: &Expr,
        rhs: &Expr,
        chunk: &mut Chunk,
    ) -> CompilerResult<()> {
        let line = lhs.span.end_pos.line_no;

        self.compile_expression(lhs, chunk)?;
        self.compile_expression(rhs, chunk)?;

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

        Ok(())
    }

    fn compile_prefix(
        &mut self,
        op: PrefixOperator,
        expr: &Expr,
        chunk: &mut Chunk,
    ) -> CompilerResult<()> {
        let line = expr.span.start_pos.line_no;

        self.compile_expression(expr, chunk)?;

        let opcode = match op {
            PrefixOperator::Negate => OpCode::Negate,
            PrefixOperator::LogicalNot => OpCode::Not,
        };

        chunk.write_instruction(opcode, line);

        Ok(())
    }

    fn add_constant_string(&mut self, name: &str, chunk: &mut Chunk) -> ConstantIndex {
        let string_intern = Value::String(self.vm_ref.get_string_intern(name));
        chunk.add_constant(string_intern)
    }

    fn define_variable(
        &mut self,
        name: &str,
        expr: &Expr,
        chunk: &mut Chunk,
        line: usize,
    ) -> CompilerResult<()> {
        // Global scope.
        if self.current_scope_depth == 0 {
            // Store the global var name as a string constant, so VM can
            // refer to it.
            let global_var_idx = self.add_constant_string(name, chunk);
            self.compile_expression(expr, chunk)?;
            chunk.write_instruction(OpCode::DefineGlobal, line);
            chunk.write_byte(global_var_idx, line);
        } else {
            self.add_local(name)?;
            self.compile_expression(expr, chunk)?;
            self.locals.last_mut().unwrap().initialized = true;
        }

        Ok(())
    }

    fn get_variable(
        &mut self,
        var: &VariableInfo,
        chunk: &mut Chunk,
        line: usize,
    ) -> CompilerResult<()> {
        match self.find_local(&var.name)? {
            None => {
                // Global variable.
                let global_var_idx = self.add_constant_string(&var.name, chunk);
                chunk.write_instruction(OpCode::GetGlobal, line);
                chunk.write_byte(global_var_idx, line);
            }
            Some(index) => {
                chunk.write_instruction(OpCode::GetLocal, line);
                chunk.write_byte(index, line);
            }
        }

        Ok(())
    }

    fn set_variable(
        &mut self,
        var: &VariableInfo,
        expr: &Expr,
        chunk: &mut Chunk,
        line: usize,
    ) -> CompilerResult<()> {
        self.compile_expression(expr, chunk)?;

        match self.find_local(&var.name)? {
            None => {
                // Global variable.
                let global_var_idx = self.add_constant_string(&var.name, chunk);
                chunk.write_instruction(OpCode::SetGlobal, line);
                chunk.write_byte(global_var_idx, line);
            }
            Some(index) => {
                chunk.write_instruction(OpCode::SetLocal, line);
                chunk.write_byte(index, line);
            }
        }

        Ok(())
    }

    fn add_local(&mut self, name: &str) -> CompilerResult<()> {
        if self.locals.len() == MAX_LOCAL_VARS {
            return Err(CompilerError::TooManyLocalVars);
        }

        for local in self.locals.iter().rev() {
            if local.scope_depth == self.current_scope_depth && local.name == name {
                return Err(CompilerError::LocalVarDefinedAlready(name.to_owned()));
            }
        }

        let local = Local {
            name: name.to_owned(),
            scope_depth: self.current_scope_depth,
            initialized: false,
        };
        self.locals.push(local);

        Ok(())
    }

    fn find_local(&self, name: &str) -> CompilerResult<Option<LocalIndex>> {
        for (index, local) in self.locals.iter().enumerate().rev() {
            if local.name == name {
                if !local.initialized {
                    return Err(CompilerError::UseVarInInitialization(name.to_owned()));
                }
                return Ok(Some(index as LocalIndex));
            }
        }

        return Ok(None);
    }

    fn begin_scope(&mut self) {
        self.current_scope_depth += 1;
    }

    fn end_scope(&mut self, chunk: &mut Chunk) {
        self.current_scope_depth -= 1;

        // Pop off local variables. Have to pop them off VM stack.
        while let Some(local) = self.locals.last() {
            if local.scope_depth > self.current_scope_depth {
                chunk.write_instruction(OpCode::Pop, 99);
                self.locals.pop();
            } else {
                break;
            }
        }
    }
}
