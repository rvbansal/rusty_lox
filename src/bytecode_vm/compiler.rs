use crate::lox_frontend::grammar::{Expr, ExprType, Literal, Stmt, StmtType, VariableInfo};
use crate::lox_frontend::operator::{InfixOperator, LogicalOperator, PrefixOperator};

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

        chunk.write_op(OpCode::Return, 99);

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
                chunk.write_op(OpCode::Pop, line);
            }
            StmtType::Print(expr) => {
                self.compile_expression(expr, chunk)?;
                chunk.write_op(OpCode::Print, line);
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
            StmtType::IfElse(condition, if_body, else_body) => {
                self.compile_if_else(condition, if_body.as_ref(), else_body.as_deref(), chunk)?;
            }
            StmtType::While(condition, body) => {
                self.compile_while(condition, body.as_ref(), chunk)?;
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
            ExprType::Logical(op, lhs, rhs) => match op {
                LogicalOperator::And => self.compile_and(lhs, rhs, chunk)?,
                LogicalOperator::Or => self.compile_or(lhs, rhs, chunk)?,
            },
            _ => panic!("Bytecode vm cannot compile this expression right now."),
        }

        Ok(())
    }

    fn compile_and(&mut self, lhs: &Expr, rhs: &Expr, chunk: &mut Chunk) -> CompilerResult<()> {
        let lhs_line = lhs.span.start_pos.line_no;
        let rhs_line = rhs.span.start_pos.line_no;

        self.compile_expression(lhs, chunk)?;
        let jump_short_circuit = chunk.emit_jump(OpCode::JumpIfFalse, lhs_line);
        chunk.write_op(OpCode::Pop, rhs_line);
        self.compile_expression(rhs, chunk)?;
        chunk.patch_jump(jump_short_circuit);

        Ok(())
    }

    fn compile_or(&mut self, lhs: &Expr, rhs: &Expr, chunk: &mut Chunk) -> CompilerResult<()> {
        let lhs_line = lhs.span.start_pos.line_no;
        let rhs_line = rhs.span.start_pos.line_no;

        self.compile_expression(lhs, chunk)?;
        let jump_lhs_false = chunk.emit_jump(OpCode::JumpIfFalse, lhs_line);
        let jump_rhs_true = chunk.emit_jump(OpCode::Jump, lhs_line);
        chunk.patch_jump(jump_lhs_false);
        chunk.write_op(OpCode::Pop, rhs_line);
        self.compile_expression(rhs, chunk)?;
        chunk.patch_jump(jump_rhs_true);

        Ok(())
    }

    fn compile_literal(&mut self, l: &Literal, line: usize, chunk: &mut Chunk) {
        match l {
            Literal::Number(n) => {
                let value = Value::Number(*n);
                let index = chunk.add_constant(value);
                chunk.write_op_with_byte(OpCode::Constant, index, line);
            }
            Literal::Boolean(b) => {
                let opcode = match *b {
                    true => OpCode::True,
                    false => OpCode::False,
                };
                chunk.write_op(opcode, line);
            }
            Literal::Nil => {
                chunk.write_op(OpCode::Nil, line);
            }
            Literal::Str(s) => {
                let index = self.add_constant_string(s, chunk);
                chunk.write_op_with_byte(OpCode::Constant, index, line);
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
            InfixOperator::Add => chunk.write_op(OpCode::Add, line),
            InfixOperator::Subtract => chunk.write_op(OpCode::Subtract, line),
            InfixOperator::Multiply => chunk.write_op(OpCode::Multiply, line),
            InfixOperator::Divide => chunk.write_op(OpCode::Divide, line),
            InfixOperator::EqualTo => chunk.write_op(OpCode::Equal, line),
            InfixOperator::NotEqualTo => {
                chunk.write_op(OpCode::Equal, line);
                chunk.write_op(OpCode::Not, line)
            }
            InfixOperator::GreaterThan => chunk.write_op(OpCode::GreaterThan, line),
            InfixOperator::GreaterEq => {
                chunk.write_op(OpCode::LessThan, line);
                chunk.write_op(OpCode::Not, line)
            }
            InfixOperator::LessThan => chunk.write_op(OpCode::LessThan, line),
            InfixOperator::LessEq => {
                chunk.write_op(OpCode::GreaterThan, line);
                chunk.write_op(OpCode::Not, line)
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

        chunk.write_op(opcode, line);

        Ok(())
    }

    fn compile_if_else(
        &mut self,
        condition: &Expr,
        if_body: &Stmt,
        else_body: Option<&Stmt>,
        chunk: &mut Chunk,
    ) -> CompilerResult<()> {
        let line = condition.span.start_pos.line_no;

        self.compile_expression(condition, chunk)?;

        // If condition is true, pop condition from stack, run if body,
        // and then jump past else body. Else, jump past if body, pop
        // condition from stack and run else body.
        let jump_to_else_location = chunk.emit_jump(OpCode::JumpIfFalse, line);
        chunk.write_op(OpCode::Pop, line);
        self.compile_statement(if_body, chunk)?;
        let jump_over_else_location = chunk.emit_jump(OpCode::Jump, line);
        chunk.write_op(OpCode::Pop, line);
        
        chunk.patch_jump(jump_to_else_location);
        if let Some(else_body) = else_body {
            self.compile_statement(else_body, chunk)?;
        }
        chunk.patch_jump(jump_over_else_location);

        Ok(())
    }

    fn compile_while(
        &mut self,
        condition: &Expr,
        body: &Stmt,
        chunk: &mut Chunk,
    ) -> CompilerResult<()> {
        let line = condition.span.start_pos.line_no;
        let loop_start = chunk.len();

        self.compile_expression(condition, chunk)?;

        let jump_to_exit = chunk.emit_jump(OpCode::JumpIfFalse, line);
        chunk.write_op(OpCode::Pop, line);
        self.compile_statement(body, chunk)?;
        chunk.emit_loop(loop_start, line);
        chunk.patch_jump(jump_to_exit);
        chunk.write_op(OpCode::Pop, line);

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
            chunk.write_op_with_byte(OpCode::DefineGlobal, global_var_idx, line);
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
                chunk.write_op_with_byte(OpCode::GetGlobal, global_var_idx, line);
            }
            Some(index) => {
                // Local variable.
                chunk.write_op_with_byte(OpCode::GetLocal, index, line);
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
                chunk.write_op_with_byte(OpCode::SetGlobal, global_var_idx, line);
            }
            Some(index) => {
                // Local variable.
                chunk.write_op_with_byte(OpCode::SetLocal, index, line);
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
                chunk.write_op(OpCode::Pop, 99);
                self.locals.pop();
            } else {
                break;
            }
        }
    }
}
