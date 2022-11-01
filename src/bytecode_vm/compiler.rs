use crate::lox_frontend::grammar::{
    Identifier, InfixOperator, LogicalOperator, PrefixOperator,
    Expr, ExprType, FuncInfo, Literal, Stmt, StmtType
};
use crate::lox_frontend::span::Span;

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
    Root,
    Function,
    Method,
    Initializer,
}

#[derive(Debug)]
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
    fn_type: FunctionType,
}

struct ClassContext {
    contains_superclass: bool,
}

pub struct Compiler<'s> {
    string_table: &'s mut StringInterner,
    context_stack: Vec<CompilerContext>,
    class_stack: Vec<ClassContext>,
}

impl FunctionType {
    fn inside_class(&self) -> bool {
        match self {
            FunctionType::Root | FunctionType::Function => false,
            FunctionType::Method | FunctionType::Initializer => true,
        }
    }
}

impl CompilerContext {
    fn new(fn_type: FunctionType) -> Self {
        let reserved_name = if fn_type.inside_class() { THIS_STR } else { "" };

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
            fn_type,
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
            class_stack: vec![],
        }
    }

    pub fn compile(&mut self, stmts: &[Stmt]) -> CompilerResult<Chunk> {
        // Push a new compiler context and reserve first slot.
        self.context_stack
            .push(CompilerContext::new(FunctionType::Root));

        // Compile statements.
        for stmt in stmts.iter() {
            self.compile_statement(stmt)?;
        }
        self.emit_op(StructOpCode::Return, Span::default());

        let main_context = self.context_stack.pop().expect("Empty context stack.");
        self.print_chunk("<main>", &main_context.chunk);

        Ok(main_context.chunk)
    }

    pub fn compile_statement(&mut self, stmt: &Stmt) -> CompilerResult<()> {
        match &stmt.stmt {
            StmtType::Expression(expr) => {
                self.compile_expression(expr)?;
                self.emit_op(StructOpCode::Pop, stmt.span);
            }
            StmtType::Print(expr) => {
                self.compile_expression(expr)?;
                self.emit_op(StructOpCode::Print, stmt.span);
            }
            StmtType::VariableDecl(ident, expr) => {
                self.compile_expression(expr)?;
                self.declare_variable(&ident.name)?;
                self.define_variable(ident)?;
            }
            StmtType::Block(stmts) => {
                self.begin_scope();
                for stmt in stmts.iter() {
                    self.compile_statement(stmt)?;
                }
                self.end_scope();
            }
            StmtType::IfElse(condition, if_body, else_body) => {
                self.compile_if_else(condition, if_body.as_ref(), else_body.as_deref(), stmt.span)?;
            }
            StmtType::While(condition, body) => {
                self.compile_while(condition, body.as_ref(), stmt.span)?;
            }
            StmtType::For(initializer, condition, increment, body) => {
                self.compile_for(
                    initializer.as_deref(),
                    condition.as_deref(),
                    increment.as_deref(),
                    body.as_ref(),
                    stmt.span,
                )?;
            }
            StmtType::FuncDecl(fn_decl) => {
                // Recursive functions can refer to themselves, so declare a local
                // func with the correct name before compiling it.
                self.declare_variable(&fn_decl.ident.name)?;
                if self.get_context().scope_depth != 0 {
                    self.get_context_mut().mark_last_local_initialized();
                }
                self.compile_func_decl(fn_decl, FunctionType::Function)?;
                self.define_variable(&fn_decl.ident)?;
            }
            StmtType::Return(expr) => {
                self.compile_return(expr.as_ref(), stmt.span)?;
            }
            StmtType::ClassDecl(ident, superclass, methods) => {
                self.declare_variable(&ident.name)?;
                let index = self.add_string_constant(&ident.name)?;
                self.emit_op(StructOpCode::MakeClass(index), ident.span);
                self.define_variable(ident)?;
                self.class_stack.push(ClassContext {
                    contains_superclass: superclass.is_some(),
                });

                // Put class on stack
                if let Some(superclass) = superclass {
                    if superclass.name == ident.name {
                        return Err(CompilerError::SelfInherit(ident.name.clone()));
                    }
                    let synthetic_super = self.synthetic_id(SUPER_STR, stmt.span);

                    self.begin_scope();
                    self.declare_variable(&synthetic_super.name)?;
                    self.define_variable(&synthetic_super)?;

                    self.get_variable(superclass)?;
                    self.get_variable(ident)?;
                    self.emit_op(StructOpCode::Inherit, superclass.span);
                } else {
                    self.get_variable(ident)?;
                }

                // Put methods on stack
                for method in methods.iter() {
                    let method_type = if method.ident.name == INIT_STR {
                        FunctionType::Initializer
                    } else {
                        FunctionType::Method
                    };

                    self.compile_func_decl(method, method_type)?;
                    let index = self.add_string_constant(&method.ident.name)?;
                    self.emit_op(StructOpCode::MakeMethod(index), method.span);
                }

                // Pop class of stack
                self.emit_op(StructOpCode::Pop, ident.span);

                if superclass.is_some() {
                    self.end_scope();
                }
                self.class_stack.pop();
            }
        };

        Ok(())
    }

    fn compile_expression(&mut self, expr: &Expr) -> CompilerResult<()> {
        let _line = expr.span.start_pos.line_no;

        match &expr.expr {
            ExprType::Literal(l) => self.compile_literal(l, expr.span)?,
            ExprType::Infix(op, lhs, rhs) => self.compile_infix(*op, lhs, rhs, expr.span)?,
            ExprType::Prefix(op, expr) => self.compile_prefix(*op, expr, expr.span)?,
            ExprType::Variable(var) => self.get_variable(var)?,
            ExprType::Assignment(var, expr) => self.set_variable(var, expr)?,
            ExprType::Logical(op, lhs, rhs) => match op {
                LogicalOperator::And => self.compile_and(lhs, rhs, expr.span)?,
                LogicalOperator::Or => self.compile_or(lhs, rhs, expr.span)?,
            },
            ExprType::Call(callee, args) => self.compile_call(callee, args)?,
            ExprType::Get(expr, property) => {
                let index = self.add_string_constant(&property.name)?;
                self.compile_expression(expr)?;
                self.emit_op(StructOpCode::GetProperty(index), property.span);
            }
            ExprType::Set(expr, property, value_expr) => {
                let index = self.add_string_constant(&property.name)?;
                self.compile_expression(expr)?;
                self.compile_expression(value_expr)?;
                self.emit_op(StructOpCode::SetProperty(index), property.span);
            }
            ExprType::This => {
                if self.class_stack.is_empty() {
                    return Err(CompilerError::ThisOutsideClass);
                }
                let synthetic_this = self.synthetic_id(THIS_STR, expr.span);
                self.get_variable(&synthetic_this)?;
            }
            ExprType::Super(method) => {
                self.check_super_validity()?;
                let synthetic_this = self.synthetic_id(THIS_STR, expr.span);
                let synthetic_super = self.synthetic_id(SUPER_STR, expr.span);
                self.get_variable(&synthetic_this)?;
                self.get_variable(&synthetic_super)?;
                let index = self.add_string_constant(&method.name)?;
                self.emit_op(StructOpCode::GetSuper(index), expr.span);
            }
        }

        Ok(())
    }

    fn compile_func_decl(
        &mut self,
        fn_decl: &FuncInfo,
        fn_type: FunctionType,
    ) -> CompilerResult<()> {
        // New compiler context for the function.
        let new_context = CompilerContext::new(fn_type);

        self.context_stack.push(new_context);
        self.begin_scope();

        // Declare and define arguments.
        for param in fn_decl.params.iter() {
            self.declare_variable(&param.name)?;
            self.define_variable(param)?;
        }

        // Compile function.
        for stmt in fn_decl.body.iter() {
            self.compile_statement(stmt)?;
        }
        self.compile_return(None, fn_decl.span)?;
        self.emit_op(StructOpCode::Return, fn_decl.span);

        // No need to end scope b/c we will pop off function context all together.
        // Build function data.
        let fn_context = self.context_stack.pop().expect("Empty context stack.");
        let fn_name = self.string_table.get_string_intern(&fn_decl.ident.name);

        self.print_chunk(&fn_decl.ident.name, &fn_context.chunk);

        let fn_template = ChunkConstant::FnTemplate {
            name: fn_name,
            arity: fn_decl.params.len(),
            chunk: Rc::new(fn_context.chunk),
            upvalue_count: fn_context.upvalues.len(),
        };
        let index = self.add_constant(fn_template)?;

        self.emit_op(StructOpCode::MakeClosure(index), fn_decl.span);
        for upvalue in fn_context.upvalues.iter().cloned() {
            self.chunk()
                .write_upvalue(upvalue, fn_decl.span.start_pos.line_no);
        }

        Ok(())
    }

    fn compile_literal(&mut self, l: &Literal, span: Span) -> CompilerResult<()> {
        match l {
            Literal::Number(n) => {
                let value = ChunkConstant::Number(*n);
                let index = self.add_constant(value)?;
                self.emit_op(StructOpCode::Constant(index), span);
            }
            Literal::Boolean(b) => {
                let opcode = match *b {
                    true => StructOpCode::True,
                    false => StructOpCode::False,
                };
                self.emit_op(opcode, span);
            }
            Literal::Nil => {
                self.emit_op(StructOpCode::Nil, span);
            }
            Literal::Str(s) => {
                let value = ChunkConstant::String(self.string_table.get_string_intern(s));
                let index = self.add_constant(value)?;
                self.emit_op(StructOpCode::Constant(index), span);
            }
        }

        Ok(())
    }

    fn compile_infix(
        &mut self,
        op: InfixOperator,
        lhs: &Expr,
        rhs: &Expr,
        span: Span,
    ) -> CompilerResult<()> {
        self.compile_expression(lhs)?;
        self.compile_expression(rhs)?;

        match op {
            InfixOperator::Add => self.emit_op(StructOpCode::Add, span),
            InfixOperator::Subtract => self.emit_op(StructOpCode::Subtract, span),
            InfixOperator::Multiply => self.emit_op(StructOpCode::Multiply, span),
            InfixOperator::Divide => self.emit_op(StructOpCode::Divide, span),
            InfixOperator::EqualTo => self.emit_op(StructOpCode::Equal, span),
            InfixOperator::NotEqualTo => {
                self.emit_op(StructOpCode::Equal, span);
                self.emit_op(StructOpCode::Not, span)
            }
            InfixOperator::GreaterThan => self.emit_op(StructOpCode::GreaterThan, span),
            InfixOperator::GreaterEq => {
                self.emit_op(StructOpCode::LessThan, span);
                self.emit_op(StructOpCode::Not, span)
            }
            InfixOperator::LessThan => self.emit_op(StructOpCode::LessThan, span),
            InfixOperator::LessEq => {
                self.emit_op(StructOpCode::GreaterThan, span);
                self.emit_op(StructOpCode::Not, span)
            }
        };

        Ok(())
    }

    fn compile_prefix(
        &mut self,
        op: PrefixOperator,
        expr: &Expr,
        span: Span,
    ) -> CompilerResult<()> {
        self.compile_expression(expr)?;

        let opcode = match op {
            PrefixOperator::Negate => StructOpCode::Negate,
            PrefixOperator::LogicalNot => StructOpCode::Not,
        };

        self.emit_op(opcode, span);

        Ok(())
    }

    fn compile_and(&mut self, lhs: &Expr, rhs: &Expr, span: Span) -> CompilerResult<()> {
        self.compile_expression(lhs)?;
        let jump_short_circuit = self.emit_jump(StructOpCode::JumpIfFalse(0), span);
        self.emit_op(StructOpCode::Pop, span);
        self.compile_expression(rhs)?;
        self.patch_jump(jump_short_circuit)?;

        Ok(())
    }

    fn compile_or(&mut self, lhs: &Expr, rhs: &Expr, span: Span) -> CompilerResult<()> {
        self.compile_expression(lhs)?;
        let jump_lhs_false = self.emit_jump(StructOpCode::JumpIfFalse(0), span);
        let jump_rhs_true = self.emit_jump(StructOpCode::Jump(0), span);
        self.patch_jump(jump_lhs_false)?;
        self.emit_op(StructOpCode::Pop, span);
        self.compile_expression(rhs)?;
        self.patch_jump(jump_rhs_true)?;

        Ok(())
    }

    fn compile_if_else(
        &mut self,
        condition: &Expr,
        if_body: &Stmt,
        else_body: Option<&Stmt>,
        whole_span: Span,
    ) -> CompilerResult<()> {
        self.compile_expression(condition)?;

        // If condition is true, pop condition from stack, run if body,
        // and then jump past else body. Else, jump past if body, pop
        // condition from stack and run else body.
        let jump_to_else_location = self.emit_jump(StructOpCode::JumpIfFalse(0), whole_span);
        self.emit_op(StructOpCode::Pop, whole_span);
        self.compile_statement(if_body)?;

        let jump_over_else_location = self.emit_jump(StructOpCode::Jump(0), whole_span);
        self.patch_jump(jump_to_else_location)?;
        self.emit_op(StructOpCode::Pop, whole_span);

        if let Some(else_body) = else_body {
            self.compile_statement(else_body)?;
        }
        self.patch_jump(jump_over_else_location)?;

        Ok(())
    }

    fn compile_while(
        &mut self,
        condition: &Expr,
        body: &Stmt,
        whole_span: Span,
    ) -> CompilerResult<()> {
        let loop_start = self.chunk().len();
        self.compile_expression(condition)?;
        let jump_to_exit = self.emit_jump(StructOpCode::JumpIfFalse(0), whole_span);
        self.emit_op(StructOpCode::Pop, whole_span);
        self.compile_statement(body)?;
        self.emit_loop(loop_start, whole_span)?;
        self.patch_jump(jump_to_exit)?;
        self.emit_op(StructOpCode::Pop, whole_span);
        Ok(())
    }

    fn compile_for(
        &mut self,
        initializer: Option<&Stmt>,
        condition: Option<&Expr>,
        increment: Option<&Expr>,
        body: &Stmt,
        whole_span: Span,
    ) -> CompilerResult<()> {
        self.begin_scope();

        if let Some(initializer) = initializer {
            self.compile_statement(initializer)?;
        }

        let loop_start = self.chunk().len();
        let exit_jump = match condition {
            Some(condition) => {
                self.compile_expression(condition)?;
                let exit_jump = self.emit_jump(StructOpCode::JumpIfFalse(0), whole_span);
                self.emit_op(StructOpCode::Pop, whole_span);
                Some(exit_jump)
            }
            None => None,
        };

        self.begin_scope();
        self.compile_statement(body)?;
        self.end_scope();

        if let Some(increment) = increment {
            self.compile_expression(increment)?;
            self.emit_op(StructOpCode::Pop, whole_span);
        }

        self.emit_loop(loop_start, whole_span)?;
        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump)?;
        }
        self.emit_op(StructOpCode::Pop, whole_span);

        self.end_scope();

        Ok(())
    }

    fn compile_call(&mut self, callee: &Expr, args: &Vec<Expr>) -> CompilerResult<()> {
        let num_args = u8::try_from(args.len()).expect("Too many function arguments.");

        match &callee.expr {
            ExprType::Get(instance_expr, method) => {
                // Method on an instance, we use invoke.
                self.compile_expression(instance_expr)?;
                for arg in args.iter() {
                    self.compile_expression(arg)?;
                }
                let index = self.add_string_constant(&method.name)?;

                self.emit_op(StructOpCode::Invoke(index, num_args), callee.span);
            }
            ExprType::Super(method) => {
                self.check_super_validity()?;
                // Method on super of instance.
                let synthetic_this = self.synthetic_id(THIS_STR, callee.span);
                let synthetic_super = self.synthetic_id(SUPER_STR, callee.span);

                self.get_variable(&synthetic_this)?;
                for arg in args.iter() {
                    self.compile_expression(arg)?;
                }
                self.get_variable(&synthetic_super)?;

                let index = self.add_string_constant(&method.name)?;

                self.emit_op(StructOpCode::InvokeSuper(index, num_args), callee.span);
            }
            _ => {
                // Usual call.
                let _line = callee.span.end_pos.line_no;
                self.compile_expression(callee)?;
                for arg in args.iter() {
                    self.compile_expression(arg)?;
                }
                self.emit_op(StructOpCode::Call(num_args), callee.span);
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

    fn define_variable(&mut self, id: &Identifier) -> CompilerResult<()> {
        // Global scope.
        if self.get_context().scope_depth == 0 {
            // Store the global var name as a string constant, so VM can
            // refer to it.
            let global_var_idx = self.add_string_constant(&id.name)?;
            self.emit_op(StructOpCode::DefineGlobal(global_var_idx), id.span);
        } else {
            self.get_context_mut().mark_last_local_initialized();
        }

        Ok(())
    }

    fn get_variable(&mut self, id: &Identifier) -> CompilerResult<()> {
        match self.resolve_variable(&id.name)? {
            VariableLocator::Global => {
                // Global variable.
                let global_var_idx = self.add_string_constant(&id.name)?;
                self.emit_op(StructOpCode::GetGlobal(global_var_idx), id.span);
            }
            VariableLocator::Local(index) => {
                // Local variable.
                self.emit_op(StructOpCode::GetLocal(index), id.span);
            }
            VariableLocator::Upvalue(index) => {
                self.emit_op(StructOpCode::GetUpvalue(index), id.span);
            }
        }

        Ok(())
    }

    fn set_variable(&mut self, id: &Identifier, expr: &Expr) -> CompilerResult<()> {
        self.compile_expression(expr)?;

        match self.resolve_variable(&id.name)? {
            VariableLocator::Global => {
                let global_var_idx = self.add_string_constant(&id.name)?;
                self.emit_op(StructOpCode::SetGlobal(global_var_idx), id.span);
            }
            VariableLocator::Local(index) => {
                self.emit_op(StructOpCode::SetLocal(index), id.span);
            }
            VariableLocator::Upvalue(index) => {
                self.emit_op(StructOpCode::SetUpvalue(index), id.span);
            }
        }

        Ok(())
    }

    fn synthetic_id(&self, name: &str, span: Span) -> Identifier {
        Identifier::new(name.to_owned(), span)
    }

    fn add_constant(&mut self, constant: ChunkConstant) -> CompilerResult<ConstantIndex> {
        self.chunk().add_constant(constant)
    }

    fn add_string_constant(&mut self, name: &str) -> CompilerResult<ConstantIndex> {
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
                context.chunk.write_op(StructOpCode::CloseUpvalue, 0);
            } else {
                context.chunk.write_op(StructOpCode::Pop, 0);
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

    fn compile_return(&mut self, expr: Option<&Expr>, whole_span: Span) -> CompilerResult<()> {
        match self.get_context().fn_type {
            FunctionType::Root => return Err(CompilerError::ReturnAtTopLevel),
            FunctionType::Function | FunctionType::Method => match expr {
                Some(expr) => self.compile_expression(expr)?,
                None => self.emit_op(StructOpCode::Nil, whole_span),
            },
            FunctionType::Initializer => {
                if expr.is_some() {
                    return Err(CompilerError::ReturnInInitializer);
                } else {
                    self.emit_op(StructOpCode::GetLocal(0), whole_span);
                }
            }
        };

        self.emit_op(StructOpCode::Return, whole_span);
        Ok(())
    }

    fn emit_op(&mut self, op: StructOpCode, span: Span) {
        self.chunk().write_op(op, span.start_pos.line_no);
    }

    fn emit_jump(&mut self, op: StructOpCode, span: Span) -> usize {
        let jump_index = self.chunk().len();
        self.emit_op(op, span);
        jump_index
    }

    fn patch_jump(&mut self, jump_index: usize) -> CompilerResult<()> {
        let distance = self.chunk().len() - (jump_index + 3);

        let distance = u16::try_from(distance).map_err(|_| CompilerError::JumpTooLong)?;
        self.chunk().patch_short(jump_index + 1, distance).unwrap();

        Ok(())
    }

    fn emit_loop(&mut self, loop_start_index: usize, span: Span) -> CompilerResult<()> {
        let distance = (self.chunk().len() + 3) - loop_start_index;
        let distance = u16::try_from(distance).map_err(|_| CompilerError::JumpTooLong)?;

        self.emit_op(StructOpCode::Loop(distance), span);

        Ok(())
    }

    fn check_super_validity(&self) -> CompilerResult<()> {
        match self.class_stack.last() {
            Some(class) => {
                if class.contains_superclass {
                    Ok(())
                } else {
                    Err(CompilerError::SuperWithoutSuperclass)
                }
            }
            None => Err(CompilerError::SuperOutsideClass),
        }
    }

    fn print_chunk(&self, _name: &str, _chunk: &Chunk) {
        #[cfg(feature = "print-chunks")]
        _chunk.disassemble(_name);
    }
}
