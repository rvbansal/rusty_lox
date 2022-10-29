use crate::lox_frontend::grammar::{
    Expr, ExprType, FuncInfo, Literal, Stmt, StmtType,
};
use crate::lox_frontend::operator::{InfixOperator, LogicalOperator, PrefixOperator};

use super::chunk::{Chunk, ChunkConstant, ConstantIndex};
use super::errors::{CompilerError, CompilerResult};
use super::opcode::OpCode;
use super::string_interner::StringInterner;

use std::convert::{TryFrom, TryInto};
use std::rc::Rc;

// Index of local var in stack is stored in u8 for GET_LOCAL instruction.
const MAX_LOCAL_VARS: usize = 256;
type LocalIndex = u8;

const MAX_UPVALUES: usize = 256;
type UpvalueIndex = u8;

#[derive(Clone, PartialEq, Eq)]
pub enum Upvalue {
    Immediate(LocalIndex),
    Recursive(UpvalueIndex),
}

enum VariableLocator {
    Local(LocalIndex),
    Upvalue(UpvalueIndex),
    Global,
}

struct Local {
    name: String,
    scope_depth: u32,
    initialized: bool,
    captured: bool,
}

struct CompilerContext {
    chunk: Chunk,
    locals: Vec<Local>,
    upvalues: Vec<Upvalue>,
    scope_depth: u32,
}

pub struct Compiler<'s> {
    string_table: &'s mut StringInterner,
    context_stack: Vec<CompilerContext>,
}

impl CompilerContext {
    fn new(reserved_name: &str) -> Self {
        let reserved_local = Local {
            name: reserved_name.to_owned(),
            scope_depth: 0,
            initialized: false,
            captured: false,
        };
        CompilerContext {
            chunk: Chunk::new(),
            locals: vec![reserved_local],
            upvalues: vec![],
            scope_depth: 0,
        }
    }

    fn find_local(&self, name: &str) -> CompilerResult<Option<LocalIndex>> {
        for (index, local) in self.locals.iter().enumerate().rev() {
            if local.name == name {
                if !local.initialized {
                    return Err(CompilerError::UseVarInInitialization(name.to_owned()));
                }

                return Ok(Some(index.try_into().unwrap()));
            }
        }

        Ok(None)
    }

    fn add_local(&mut self, name: &str) -> CompilerResult<()> {
        if self.locals.len() == MAX_LOCAL_VARS {
            return Err(CompilerError::TooManyLocalVars);
        }

        for local in self.locals.iter().rev() {
            if local.scope_depth == self.scope_depth && local.name == name {
                return Err(CompilerError::LocalVarDefinedAlready(name.to_owned()));
            }
        }

        let local = Local {
            name: name.to_owned(),
            scope_depth: self.scope_depth,
            initialized: false,
            captured: false,
        };
        self.locals.push(local);

        Ok(())
    }

    fn add_upvalue(&mut self, key: Upvalue) -> CompilerResult<UpvalueIndex> {
        for (i, upvalue) in self.upvalues.iter().enumerate() {
            if key == *upvalue {
                return Ok(i.try_into().unwrap());
            }
        }

        if self.upvalues.len() == MAX_UPVALUES {
            return Err(CompilerError::TooManyUpvalues);
        }

        self.upvalues.push(key);
        let index = self.upvalues.len() - 1;

        Ok(index.try_into().unwrap())
    }

    fn mark_last_local_initialized(&mut self) {
        self.locals.last_mut().unwrap().initialized = true;
    }
}

impl<'s> Compiler<'s> {
    pub fn new(string_table: &'s mut StringInterner) -> Self {
        Compiler {
            string_table,
            context_stack: vec![],
        }
    }

    pub fn compile(&mut self, stmts: &[Stmt]) -> CompilerResult<Chunk> {
        // Push a new compiler context and reserve first slot.
        self.context_stack.push(CompilerContext::new(""));

        // Compile statements.
        for stmt in stmts.iter() {
            self.compile_statement(stmt)?;
        }
        self.chunk().write_op(OpCode::Return, 99);

        let main_context = self.context_stack.pop().expect("Empty context stack.");
        self.print_chunk("<main>", &main_context.chunk);

        Ok(main_context.chunk)
    }

    pub fn compile_statement(&mut self, stmt: &Stmt) -> CompilerResult<()> {
        let line = stmt.span.start_pos.line_no;
        match &stmt.stmt {
            StmtType::Expression(expr) => {
                self.compile_expression(expr)?;
                self.chunk().write_op(OpCode::Pop, line);
            }
            StmtType::Print(expr) => {
                self.compile_expression(expr)?;
                self.chunk().write_op(OpCode::Print, line);
            }
            StmtType::VariableDecl(name, expr) => {
                self.compile_expression(expr)?;
                self.declare_variable(name)?;
                self.define_variable(name, line)?;
            }
            StmtType::Block(stmts) => {
                self.begin_scope();
                for stmt in stmts.iter() {
                    self.compile_statement(stmt)?;
                }
                self.end_scope();
            }
            StmtType::IfElse(condition, if_body, else_body) => {
                self.compile_if_else(condition, if_body.as_ref(), else_body.as_deref())?;
            }
            StmtType::While(condition, body) => {
                self.compile_while(condition, body.as_ref())?;
            }
            StmtType::FuncDecl(fn_decl) => {
                // Recursive functions can refer to themselves, so declare a local
                // func with the correct name before compiling it.
                self.declare_variable(&fn_decl.name)?;
                if self.get_context().scope_depth != 0 {
                    self.get_context_mut().mark_last_local_initialized();
                }
                self.compile_func_decl(fn_decl, line)?;
                self.define_variable(&fn_decl.name, line)?;
            }
            StmtType::Return(expr) => {
                match expr {
                    Some(expr) => {
                        self.compile_expression(expr)?;
                    }
                    None => self.chunk().write_op(OpCode::Nil, line),
                }
                self.chunk().write_op(OpCode::Return, line);
            }
            StmtType::ClassDecl(name, _superclass, _methods) => {
                self.declare_variable(name)?;
                let index = self.add_string_constant(name);
                self.chunk()
                    .write_op_with_byte(OpCode::MakeClass, index, line);
                self.define_variable(name, line)?;
            }
        };

        Ok(())
    }

    fn compile_expression(&mut self, expr: &Expr) -> CompilerResult<()> {
        let line = expr.span.start_pos.line_no;

        match &expr.expr {
            ExprType::Literal(l) => self.compile_literal(l, line),
            ExprType::Infix(op, lhs, rhs) => self.compile_infix(*op, lhs, rhs)?,
            ExprType::Prefix(op, expr) => self.compile_prefix(*op, expr)?,
            ExprType::Variable(var) => self.get_variable(&var.name, line)?,
            ExprType::Assignment(var, expr) => self.set_variable(&var.name, expr, line)?,
            ExprType::Logical(op, lhs, rhs) => match op {
                LogicalOperator::And => self.compile_and(lhs, rhs)?,
                LogicalOperator::Or => self.compile_or(lhs, rhs)?,
            },
            ExprType::Call(callee, args) => {
                self.compile_expression(callee.as_ref())?;
                for arg in args.iter() {
                    self.compile_expression(arg)?;
                }
                self.chunk().write_op_with_byte(
                    OpCode::Call,
                    u8::try_from(args.len()).expect("Too many function arguments."),
                    line,
                );
            }
            ExprType::Get(expr, name) => {
                let index = self.add_string_constant(name);
                self.compile_expression(expr)?;
                self.chunk()
                    .write_op_with_byte(OpCode::GetProperty, index, line);
            }
            ExprType::Set(expr, name, value_expr) => {
                let index = self.add_string_constant(name);
                self.compile_expression(expr)?;
                self.compile_expression(value_expr)?;
                self.chunk()
                    .write_op_with_byte(OpCode::SetProperty, index, line);
            }
            _ => panic!("Bytecode vm cannot compile this expression right now."),
        }

        Ok(())
    }

    fn compile_func_decl(&mut self, fn_decl: &FuncInfo, line: usize) -> CompilerResult<()> {
        // New compiler context for the function.
        self.context_stack.push(CompilerContext::new(""));
        self.begin_scope();

        // Declare and define arguments.
        for param in fn_decl.params.iter() {
            self.declare_variable(param)?;
            self.define_variable(param, line)?;
        }

        // Compile function.
        self.compile_statement(fn_decl.body.as_ref())?;
        self.chunk()
            .write_op(OpCode::Nil, fn_decl.body.span.end_pos.line_no);
        self.chunk()
            .write_op(OpCode::Return, fn_decl.body.span.end_pos.line_no);

        // No need to end scope b/c we will pop off function context all together.
        // Build function data.
        let fn_context = self.context_stack.pop().expect("Empty context stack.");
        let fn_name = self.string_table.get_string_intern(&fn_decl.name);

        self.print_chunk(&fn_decl.name, &fn_context.chunk);

        let fn_template = ChunkConstant::FnTemplate {
            name: fn_name,
            arity: fn_decl.params.len(),
            chunk: Rc::new(fn_context.chunk),
            upvalue_count: fn_context.upvalues.len(),
        };
        let index = self.add_constant(fn_template);

        self.chunk()
            .write_op_with_byte(OpCode::MakeClosure, index, line);
        for upvalue in fn_context.upvalues.iter().cloned() {
            let bytes = match upvalue {
                Upvalue::Immediate(index) => [1, index],
                Upvalue::Recursive(index) => [0, index],
            };

            self.chunk().write_byte(bytes[0], line);
            self.chunk().write_byte(bytes[1], line);
        }

        Ok(())
    }

    fn compile_literal(&mut self, l: &Literal, line: usize) {
        match l {
            Literal::Number(n) => {
                let value = ChunkConstant::Number(*n);
                let index = self.add_constant(value);
                self.chunk()
                    .write_op_with_byte(OpCode::Constant, index, line);
            }
            Literal::Boolean(b) => {
                let opcode = match *b {
                    true => OpCode::True,
                    false => OpCode::False,
                };
                self.chunk().write_op(opcode, line);
            }
            Literal::Nil => {
                self.chunk().write_op(OpCode::Nil, line);
            }
            Literal::Str(s) => {
                let value = ChunkConstant::String(self.string_table.get_string_intern(s));
                let index = self.add_constant(value);
                self.chunk()
                    .write_op_with_byte(OpCode::Constant, index, line);
            }
        }
    }

    fn compile_infix(&mut self, op: InfixOperator, lhs: &Expr, rhs: &Expr) -> CompilerResult<()> {
        let line = lhs.span.end_pos.line_no;

        self.compile_expression(lhs)?;
        self.compile_expression(rhs)?;

        let chunk = self.chunk();
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

    fn compile_prefix(&mut self, op: PrefixOperator, expr: &Expr) -> CompilerResult<()> {
        let line = expr.span.start_pos.line_no;

        self.compile_expression(expr)?;

        let opcode = match op {
            PrefixOperator::Negate => OpCode::Negate,
            PrefixOperator::LogicalNot => OpCode::Not,
        };

        self.chunk().write_op(opcode, line);

        Ok(())
    }

    fn compile_and(&mut self, lhs: &Expr, rhs: &Expr) -> CompilerResult<()> {
        let lhs_line = lhs.span.start_pos.line_no;
        let rhs_line = rhs.span.start_pos.line_no;

        self.compile_expression(lhs)?;
        let jump_short_circuit = self.chunk().emit_jump(OpCode::JumpIfFalse, lhs_line);
        self.chunk().write_op(OpCode::Pop, rhs_line);
        self.compile_expression(rhs)?;
        self.chunk().patch_jump(jump_short_circuit);

        Ok(())
    }

    fn compile_or(&mut self, lhs: &Expr, rhs: &Expr) -> CompilerResult<()> {
        let lhs_line = lhs.span.start_pos.line_no;
        let rhs_line = rhs.span.start_pos.line_no;

        self.compile_expression(lhs)?;
        let jump_lhs_false = self.chunk().emit_jump(OpCode::JumpIfFalse, lhs_line);
        let jump_rhs_true = self.chunk().emit_jump(OpCode::Jump, lhs_line);
        self.chunk().patch_jump(jump_lhs_false);
        self.chunk().write_op(OpCode::Pop, rhs_line);
        self.compile_expression(rhs)?;
        self.chunk().patch_jump(jump_rhs_true);

        Ok(())
    }

    fn compile_if_else(
        &mut self,
        condition: &Expr,
        if_body: &Stmt,
        else_body: Option<&Stmt>,
    ) -> CompilerResult<()> {
        let line = condition.span.start_pos.line_no;

        self.compile_expression(condition)?;

        // If condition is true, pop condition from stack, run if body,
        // and then jump past else body. Else, jump past if body, pop
        // condition from stack and run else body.
        let jump_to_else_location = self.chunk().emit_jump(OpCode::JumpIfFalse, line);
        self.chunk().write_op(OpCode::Pop, line);
        self.compile_statement(if_body)?;
        let jump_over_else_location = self.chunk().emit_jump(OpCode::Jump, line);
        self.chunk().write_op(OpCode::Pop, line);

        self.chunk().patch_jump(jump_to_else_location);
        if let Some(else_body) = else_body {
            self.compile_statement(else_body)?;
        }
        self.chunk().patch_jump(jump_over_else_location);

        Ok(())
    }

    fn compile_while(&mut self, condition: &Expr, body: &Stmt) -> CompilerResult<()> {
        let line = condition.span.start_pos.line_no;
        let loop_start = self.chunk().len();

        self.compile_expression(condition)?;
        let jump_to_exit = self.chunk().emit_jump(OpCode::JumpIfFalse, line);
        self.chunk().write_op(OpCode::Pop, line);
        self.compile_statement(body)?;
        self.chunk().emit_loop(loop_start, line);
        self.chunk().patch_jump(jump_to_exit);
        self.chunk().write_op(OpCode::Pop, line);

        Ok(())
    }

    fn declare_variable(&mut self, name: &str) -> CompilerResult<()> {
        if self.get_context().scope_depth == 0 {
        } else {
            self.get_context_mut().add_local(name)?;
        }

        Ok(())
    }

    fn define_variable(&mut self, name: &str, line: usize) -> CompilerResult<()> {
        // Global scope.
        if self.get_context().scope_depth == 0 {
            // Store the global var name as a string constant, so VM can
            // refer to it.
            let global_var_idx = self.add_string_constant(name);
            self.chunk()
                .write_op_with_byte(OpCode::DefineGlobal, global_var_idx, line);
        } else {
            self.get_context_mut().mark_last_local_initialized();
        }

        Ok(())
    }

    fn get_variable(&mut self, var_name: &str, line: usize) -> CompilerResult<()> {
        match self.resolve_variable(var_name)? {
            VariableLocator::Global => {
                // Global variable.
                let global_var_idx = self.add_string_constant(var_name);
                self.chunk()
                    .write_op_with_byte(OpCode::GetGlobal, global_var_idx, line);
            }
            VariableLocator::Local(index) => {
                // Local variable.
                self.chunk()
                    .write_op_with_byte(OpCode::GetLocal, index, line);
            }
            VariableLocator::Upvalue(index) => {
                self.chunk()
                    .write_op_with_byte(OpCode::GetUpvalue, index, line);
            }
        }

        Ok(())
    }

    fn set_variable(&mut self, var_name: &str, expr: &Expr, line: usize) -> CompilerResult<()> {
        self.compile_expression(expr)?;

        match self.resolve_variable(var_name)? {
            VariableLocator::Global => {
                let global_var_idx = self.add_string_constant(var_name);
                self.chunk()
                    .write_op_with_byte(OpCode::SetGlobal, global_var_idx, line);
            }
            VariableLocator::Local(index) => {
                self.chunk()
                    .write_op_with_byte(OpCode::SetLocal, index, line);
            }
            VariableLocator::Upvalue(index) => {
                self.chunk()
                    .write_op_with_byte(OpCode::SetUpvalue, index, line);
            }
        }

        Ok(())
    }

    fn add_constant(&mut self, constant: ChunkConstant) -> ConstantIndex {
        self.chunk().add_constant(constant)
    }

    fn add_string_constant(&mut self, name: &str) -> ConstantIndex {
        let constant = ChunkConstant::String(self.string_table.get_string_intern(name));
        self.chunk().add_constant(constant)
    }

    fn get_context(&self) -> &CompilerContext {
        match self.context_stack.last() {
            Some(context) => context,
            None => panic!("Context stack empty."),
        }
    }

    fn get_context_mut(&mut self) -> &mut CompilerContext {
        match self.context_stack.last_mut() {
            Some(context) => context,
            None => panic!("Context stack empty."),
        }
    }

    fn chunk(&mut self) -> &mut Chunk {
        &mut self.get_context_mut().chunk
    }

    fn begin_scope(&mut self) {
        self.get_context_mut().scope_depth += 1;
    }

    fn end_scope(&mut self) {
        let context = self.get_context_mut();
        context.scope_depth -= 1;

        // Pop off local variables. Have to pop them off VM stack so put pop opcodes.
        while let Some(local) = context.locals.last() {
            if local.scope_depth <= context.scope_depth {
                break;
            }
            if local.captured {
                context.chunk.write_op(OpCode::CloseUpvalue, 99);
            } else {
                context.chunk.write_op(OpCode::Pop, 99);
            }
            context.locals.pop();
        }
    }

    fn resolve_variable(&mut self, name: &str) -> CompilerResult<VariableLocator> {
        let mut found_at = None;
        for (stack_index, context) in self.context_stack.iter().enumerate().rev() {
            if let Some(local_index) = context.find_local(name)? {
                found_at = Some((stack_index, local_index));
                break;
            }
        }

        let (stack_index, local_index) = match found_at {
            Some(t) => t,
            None => return Ok(VariableLocator::Global),
        };

        if stack_index == self.context_stack.len() - 1 {
            return Ok(VariableLocator::Local(local_index));
        }

        self.context_stack[stack_index].locals[local_index as usize].captured = true;

        let mut upvalue_index =
            self.context_stack[stack_index + 1].add_upvalue(Upvalue::Immediate(local_index))?;
        for context in self.context_stack[stack_index + 2..].iter_mut() {
            upvalue_index = context.add_upvalue(Upvalue::Recursive(upvalue_index))?;
        }

        Ok(VariableLocator::Upvalue(upvalue_index))
    }

    fn print_chunk(&self, _name: &str, _chunk: &Chunk) {
        #[cfg(feature = "print-chunks")]
        _chunk.disassemble(_name);
    }
}
