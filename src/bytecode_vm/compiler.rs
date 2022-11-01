use crate::lox_frontend::grammar::{Expr, ExprType, FuncInfo, Literal, Stmt, StmtType};
use crate::lox_frontend::grammar::{InfixOperator, LogicalOperator, PrefixOperator};

use super::chunk::{Chunk, ChunkConstant};
use super::errors::{CompilerError, CompilerResult};
use super::opcode::{
    ConstantIndex, LocalIndex, StructOpCode, UpvalueIndex, UpvalueLocation, MAX_LOCAL_VARS,
    MAX_UPVALUES,
};
use super::string_interner::StringInterner;

use std::convert::{TryFrom, TryInto};
use std::rc::Rc;

pub const THIS_STR: &str = "this";
pub const INIT_STR: &str = "init";
pub const SUPER_STR: &str = "super";

enum VariableLocator {
    Local(LocalIndex),
    Upvalue(UpvalueIndex),
    Global,
}

#[derive(PartialEq, Eq)]
enum FunctionType {
    Function,
    Method,
    Initializer,
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
    upvalues: Vec<UpvalueLocation>,
    scope_depth: u32,
    is_initializer: bool,
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
            initialized: true,
            captured: false,
        };
        CompilerContext {
            chunk: Chunk::new(),
            locals: vec![reserved_local],
            upvalues: vec![],
            scope_depth: 0,
            is_initializer: false,
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

    fn add_upvalue(&mut self, key: UpvalueLocation) -> CompilerResult<UpvalueIndex> {
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
        self.emit_op(StructOpCode::Return, 99);

        let main_context = self.context_stack.pop().expect("Empty context stack.");
        self.print_chunk("<main>", &main_context.chunk);

        Ok(main_context.chunk)
    }

    pub fn compile_statement(&mut self, stmt: &Stmt) -> CompilerResult<()> {
        let line = stmt.span.start_pos.line_no;
        match &stmt.stmt {
            StmtType::Expression(expr) => {
                self.compile_expression(expr)?;
                self.emit_op(StructOpCode::Pop, line);
            }
            StmtType::Print(expr) => {
                self.compile_expression(expr)?;
                self.emit_op(StructOpCode::Print, line);
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
                self.compile_func_decl(fn_decl, line, FunctionType::Function)?;
                self.define_variable(&fn_decl.name, line)?;
            }
            StmtType::Return(expr) => {
                match expr {
                    Some(expr) => {
                        self.compile_expression(expr)?;
                    }
                    None => {
                        self.emit_default_return(line);
                    }
                }
                self.emit_op(StructOpCode::Return, line);
            }
            StmtType::ClassDecl(name, superclass, methods) => {
                self.declare_variable(name)?;
                let index = self.add_string_constant(name);
                self.emit_op(StructOpCode::MakeClass(index), line);
                self.define_variable(name, line)?;
                // Put class on stack
                if let Some(superclass) = superclass {
                    if superclass == name {
                        panic!("Superclass is same as subclass.");
                    }

                    self.begin_scope();
                    self.declare_variable(SUPER_STR)?;
                    self.define_variable(SUPER_STR, line)?;

                    self.get_variable(superclass, line)?;
                    self.get_variable(name, line)?;
                    self.emit_op(StructOpCode::Inherit, line);
                } else {
                    self.get_variable(name, line)?;
                }

                // Put methods on stack
                for method in methods.iter() {
                    let method_type = if method.name == INIT_STR {
                        FunctionType::Initializer
                    } else {
                        FunctionType::Method
                    };

                    self.compile_func_decl(
                        method,
                        method.body.span.start_pos.line_no,
                        method_type,
                    )?;
                    let index = self.add_string_constant(&method.name);
                    self.emit_op(
                        StructOpCode::MakeMethod(index),
                        method.body.span.end_pos.line_no,
                    );
                }

                // Pop class of stack
                self.emit_op(StructOpCode::Pop, line);

                if superclass.is_some() {
                    self.end_scope();
                }
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
            ExprType::Variable(var) => self.get_variable(var, line)?,
            ExprType::Assignment(var, expr) => self.set_variable(var, expr, line)?,
            ExprType::Logical(op, lhs, rhs) => match op {
                LogicalOperator::And => self.compile_and(lhs, rhs)?,
                LogicalOperator::Or => self.compile_or(lhs, rhs)?,
            },
            ExprType::Call(callee, args) => self.compile_call(callee, args)?,
            ExprType::Get(expr, name) => {
                let index = self.add_string_constant(name);
                self.compile_expression(expr)?;
                self.emit_op(StructOpCode::GetProperty(index), line);
            }
            ExprType::Set(expr, name, value_expr) => {
                let index = self.add_string_constant(name);
                self.compile_expression(expr)?;
                self.compile_expression(value_expr)?;
                self.emit_op(StructOpCode::SetProperty(index), line);
            }
            ExprType::This => {
                self.get_variable(THIS_STR, line)?;
            }
            ExprType::Super(method_name) => {
                self.get_variable(THIS_STR, line)?;
                self.get_variable(SUPER_STR, line)?;

                let index = self.add_string_constant(method_name);
                self.emit_op(StructOpCode::GetSuper(index), line);
            }
        }

        Ok(())
    }

    fn compile_func_decl(
        &mut self,
        fn_decl: &FuncInfo,
        line: usize,
        func_type: FunctionType,
    ) -> CompilerResult<()> {
        // New compiler context for the function.
        let mut func_context = CompilerContext::new(match func_type {
            FunctionType::Function => "",
            FunctionType::Method | FunctionType::Initializer => THIS_STR,
        });
        if func_type == FunctionType::Initializer {
            func_context.is_initializer = true;
        }

        self.context_stack.push(func_context);
        self.begin_scope();

        // Declare and define arguments.
        for param in fn_decl.params.iter() {
            self.declare_variable(param)?;
            self.define_variable(param, line)?;
        }

        // Compile function.
        self.compile_statement(fn_decl.body.as_ref())?;
        let last_line = fn_decl.body.span.end_pos.line_no;
        self.emit_default_return(last_line);
        self.emit_op(StructOpCode::Return, last_line);

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

        self.emit_op(StructOpCode::MakeClosure(index), line);
        for upvalue in fn_context.upvalues.iter().cloned() {
            self.chunk().write_upvalue(upvalue, line);
        }

        Ok(())
    }

    fn compile_literal(&mut self, l: &Literal, line: usize) {
        match l {
            Literal::Number(n) => {
                let value = ChunkConstant::Number(*n);
                let index = self.add_constant(value);
                self.emit_op(StructOpCode::Constant(index), line);
            }
            Literal::Boolean(b) => {
                let opcode = match *b {
                    true => StructOpCode::True,
                    false => StructOpCode::False,
                };
                self.emit_op(opcode, line);
            }
            Literal::Nil => {
                self.emit_op(StructOpCode::Nil, line);
            }
            Literal::Str(s) => {
                let value = ChunkConstant::String(self.string_table.get_string_intern(s));
                let index = self.add_constant(value);
                self.emit_op(StructOpCode::Constant(index), line);
            }
        }
    }

    fn compile_infix(&mut self, op: InfixOperator, lhs: &Expr, rhs: &Expr) -> CompilerResult<()> {
        let line = lhs.span.end_pos.line_no;

        self.compile_expression(lhs)?;
        self.compile_expression(rhs)?;

        let _chunk = self.chunk();
        match op {
            InfixOperator::Add => self.emit_op(StructOpCode::Add, line),
            InfixOperator::Subtract => self.emit_op(StructOpCode::Subtract, line),
            InfixOperator::Multiply => self.emit_op(StructOpCode::Multiply, line),
            InfixOperator::Divide => self.emit_op(StructOpCode::Divide, line),
            InfixOperator::EqualTo => self.emit_op(StructOpCode::Equal, line),
            InfixOperator::NotEqualTo => {
                self.emit_op(StructOpCode::Equal, line);
                self.emit_op(StructOpCode::Not, line)
            }
            InfixOperator::GreaterThan => self.emit_op(StructOpCode::GreaterThan, line),
            InfixOperator::GreaterEq => {
                self.emit_op(StructOpCode::LessThan, line);
                self.emit_op(StructOpCode::Not, line)
            }
            InfixOperator::LessThan => self.emit_op(StructOpCode::LessThan, line),
            InfixOperator::LessEq => {
                self.emit_op(StructOpCode::GreaterThan, line);
                self.emit_op(StructOpCode::Not, line)
            }
        };

        Ok(())
    }

    fn compile_prefix(&mut self, op: PrefixOperator, expr: &Expr) -> CompilerResult<()> {
        let line = expr.span.start_pos.line_no;

        self.compile_expression(expr)?;

        let opcode = match op {
            PrefixOperator::Negate => StructOpCode::Negate,
            PrefixOperator::LogicalNot => StructOpCode::Not,
        };

        self.emit_op(opcode, line);

        Ok(())
    }

    fn compile_and(&mut self, lhs: &Expr, rhs: &Expr) -> CompilerResult<()> {
        let lhs_line = lhs.span.start_pos.line_no;
        let rhs_line = rhs.span.start_pos.line_no;

        self.compile_expression(lhs)?;
        let jump_short_circuit = self.emit_jump(StructOpCode::JumpIfFalse(0), lhs_line);
        self.emit_op(StructOpCode::Pop, rhs_line);
        self.compile_expression(rhs)?;
        self.patch_jump(jump_short_circuit);

        Ok(())
    }

    fn compile_or(&mut self, lhs: &Expr, rhs: &Expr) -> CompilerResult<()> {
        let lhs_line = lhs.span.start_pos.line_no;
        let rhs_line = rhs.span.start_pos.line_no;

        self.compile_expression(lhs)?;
        let jump_lhs_false = self.emit_jump(StructOpCode::JumpIfFalse(0), lhs_line);
        let jump_rhs_true = self.emit_jump(StructOpCode::Jump(0), lhs_line);
        self.patch_jump(jump_lhs_false);
        self.emit_op(StructOpCode::Pop, rhs_line);
        self.compile_expression(rhs)?;
        self.patch_jump(jump_rhs_true);

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
        let jump_to_else_location = self.emit_jump(StructOpCode::JumpIfFalse(0), line);
        self.emit_op(StructOpCode::Pop, line);
        self.compile_statement(if_body)?;
        let jump_over_else_location = self.emit_jump(StructOpCode::Jump(0), line);
        self.emit_op(StructOpCode::Pop, line);
        self.patch_jump(jump_to_else_location);
        if let Some(else_body) = else_body {
            self.compile_statement(else_body)?;
        }
        self.patch_jump(jump_over_else_location);

        Ok(())
    }

    fn compile_while(&mut self, condition: &Expr, body: &Stmt) -> CompilerResult<()> {
        let line = condition.span.start_pos.line_no;
        let loop_start = self.chunk().len();

        self.compile_expression(condition)?;
        let jump_to_exit = self.emit_jump(StructOpCode::JumpIfFalse(0), line);
        self.emit_op(StructOpCode::Pop, line);
        self.compile_statement(body)?;
        self.emit_loop(loop_start, line);
        self.patch_jump(jump_to_exit);
        self.emit_op(StructOpCode::Pop, line);

        Ok(())
    }

    fn compile_call(&mut self, callee: &Expr, args: &Vec<Expr>) -> CompilerResult<()> {
        let num_args = u8::try_from(args.len()).expect("Too many function arguments.");

        match &callee.expr {
            ExprType::Get(instance_expr, method_name) => {
                // Method on an instance, we use invoke.
                let line = callee.span.end_pos.line_no;
                self.compile_expression(instance_expr)?;
                for arg in args.iter() {
                    self.compile_expression(arg)?;
                }
                let index = self.add_string_constant(method_name);

                self.emit_op(StructOpCode::Invoke(index, num_args), line);
            }
            ExprType::Super(method_name) => {
                // Method on super of instance.
                let line = callee.span.start_pos.line_no;
                self.get_variable(THIS_STR, line)?;
                for arg in args.iter() {
                    self.compile_expression(arg)?;
                }
                self.get_variable(SUPER_STR, line)?;

                let line = callee.span.end_pos.line_no;
                let index = self.add_string_constant(method_name);

                self.emit_op(StructOpCode::InvokeSuper(index, num_args), line);
            }
            _ => {
                // Usual call.
                let line = callee.span.end_pos.line_no;
                self.compile_expression(callee)?;
                for arg in args.iter() {
                    self.compile_expression(arg)?;
                }
                self.emit_op(StructOpCode::Call(num_args), line);
            }
        }

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
            self.emit_op(StructOpCode::DefineGlobal(global_var_idx), line);
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
                self.emit_op(StructOpCode::GetGlobal(global_var_idx), line);
            }
            VariableLocator::Local(index) => {
                // Local variable.
                self.emit_op(StructOpCode::GetLocal(index), line);
            }
            VariableLocator::Upvalue(index) => {
                self.emit_op(StructOpCode::GetUpvalue(index), line);
            }
        }

        Ok(())
    }

    fn set_variable(&mut self, var_name: &str, expr: &Expr, line: usize) -> CompilerResult<()> {
        self.compile_expression(expr)?;

        match self.resolve_variable(var_name)? {
            VariableLocator::Global => {
                let global_var_idx = self.add_string_constant(var_name);
                self.emit_op(StructOpCode::SetGlobal(global_var_idx), line);
            }
            VariableLocator::Local(index) => {
                self.emit_op(StructOpCode::SetLocal(index), line);
            }
            VariableLocator::Upvalue(index) => {
                self.emit_op(StructOpCode::SetUpvalue(index), line);
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
                context.chunk.write_op(StructOpCode::CloseUpvalue, 99);
            } else {
                context.chunk.write_op(StructOpCode::Pop, 99);
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

        let mut upvalue_index = self.context_stack[stack_index + 1]
            .add_upvalue(UpvalueLocation::Immediate(local_index))?;
        for context in self.context_stack[stack_index + 2..].iter_mut() {
            upvalue_index = context.add_upvalue(UpvalueLocation::Recursive(upvalue_index))?;
        }

        Ok(VariableLocator::Upvalue(upvalue_index))
    }

    fn emit_default_return(&mut self, line: usize) {
        if self.get_context().is_initializer {
            self.emit_op(StructOpCode::GetLocal(0), line);
        } else {
            self.emit_op(StructOpCode::Nil, line);
        }
    }

    fn emit_op(&mut self, op: StructOpCode, line: usize) {
        self.chunk().write_op(op, line);
    }

    fn emit_jump(&mut self, op: StructOpCode, line: usize) -> usize {
        let jump_index = self.chunk().len();
        self.emit_op(op, line);
        jump_index
    }

    fn patch_jump(&mut self, jump_index: usize) {
        let distance = self.chunk().len() - (jump_index + 3);

        let distance = u16::try_from(distance).expect("Jump distance too large.");
        self.chunk().patch_short(jump_index + 1, distance).unwrap();
    }

    fn emit_loop(&mut self, loop_start_index: usize, line: usize) {
        let distance = (self.chunk().len() + 3) - loop_start_index;
        let distance = u16::try_from(distance).expect("Jump distance too large.");

        self.emit_op(StructOpCode::Loop(distance), line);
    }

    fn print_chunk(&self, _name: &str, _chunk: &Chunk) {
        #[cfg(feature = "print-chunks")]
        _chunk.disassemble(_name);
    }
}
