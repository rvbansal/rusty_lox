use crate::lox_frontend::constants::{INIT_STR, SUPER_STR, THIS_STR};
use crate::lox_frontend::grammar::{Expr, ExprType, FuncInfo, Stmt, StmtType, VariableInfo};
use std::collections::HashMap;

type Scope = HashMap<String, VariableState>;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum VariableState {
    Initialized,
    Uninitialized,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum FuncContext {
    Global,
    Function,
    Method,
    Initializer,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ClassContext {
    Global,
    Class,
    Subclass,
}

pub struct Resolver {
    scopes: Vec<Scope>,
    func_context: FuncContext,
    class_context: ClassContext,
}

#[derive(Debug)]
pub enum ResolverError {
    ReturnStatementInGlobal,
    UseVarInInitialization(String),
    LocalVarDefinedAlready(String),
    ReferencetoThisOutsideOfClass,
    ReturnInInitializer,
    SuperClassIsSameAsClass,
    SuperStatementOutsideClass,
}

pub type ResolverResult<T> = Result<T, ResolverError>;

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            scopes: vec![],
            func_context: FuncContext::Global,
            class_context: ClassContext::Global,
        }
    }

    pub fn resolve_root(&mut self, stmt: &mut Stmt) -> ResolverResult<()> {
        self.scopes = vec![];
        self.resolve_statement(stmt)
    }

    fn resolve_statement(&mut self, stmt: &mut Stmt) -> ResolverResult<()> {
        match &mut stmt.stmt {
            StmtType::Expression(expr) => self.resolve_expression(expr)?,
            StmtType::Print(expr) => self.resolve_expression(expr)?,
            StmtType::VariableDecl(name, expr) => {
                if self.is_var_already_defined(name) {
                    return Err(ResolverError::LocalVarDefinedAlready(name.to_owned()));
                }

                self.declare_variable(name);
                self.resolve_expression(expr)?;
                self.define_variable(name);
            }
            StmtType::Block(stmts) => {
                self.push_scope();
                for stmt in stmts.iter_mut() {
                    self.resolve_statement(stmt)?;
                }
                self.pop_scope();
            }
            StmtType::IfElse(cond, if_body, else_body) => {
                self.resolve_expression(cond)?;
                self.resolve_statement(if_body)?;

                if let Some(else_body) = else_body {
                    self.resolve_statement(else_body)?;
                }
            }
            StmtType::While(cond, body) => {
                self.resolve_expression(cond)?;
                self.resolve_statement(body)?;
            }
            StmtType::FuncDecl(func_info) => {
                self.resolve_function(func_info, FuncContext::Function)?
            }
            StmtType::Return(expr) => {
                if self.func_context == FuncContext::Global {
                    return Err(ResolverError::ReturnStatementInGlobal);
                }
                if self.func_context == FuncContext::Initializer && expr.is_some() {
                    return Err(ResolverError::ReturnInInitializer);
                }
                if let Some(expr) = expr {
                    self.resolve_expression(expr)?;
                }
            }
            StmtType::ClassDecl(name, superclass, methods) => {
                self.define_variable(name);

                if let Some(superclass) = superclass {
                    if superclass.name == **name {
                        return Err(ResolverError::SuperClassIsSameAsClass);
                    }
                    self.resolve_local_variable(superclass);
                }

                if superclass.is_some() {
                    self.push_scope();
                    self.define_variable(SUPER_STR);
                }
                self.push_scope();
                self.define_variable(THIS_STR);

                let prev_class_context = self.class_context;
                self.class_context = if superclass.is_some() {
                    ClassContext::Subclass
                } else {
                    ClassContext::Class
                };

                for method in methods.iter_mut() {
                    let func_context = if method.name == INIT_STR {
                        FuncContext::Initializer
                    } else {
                        FuncContext::Method
                    };
                    self.resolve_function(method, func_context)?;
                }

                self.class_context = prev_class_context;
                self.pop_scope();
                if superclass.is_some() {
                    self.pop_scope();
                }
            }
        }

        Ok(())
    }

    fn resolve_function(
        &mut self,
        func_info: &mut FuncInfo,
        context: FuncContext,
    ) -> ResolverResult<()> {
        // Handle the case where the function is used recursively.
        // We need to define it eagerly.
        self.define_variable(&func_info.name);

        self.push_scope();
        let prev_func_context = self.func_context;
        self.func_context = context;

        for name in func_info.params.iter() {
            self.define_variable(name);
        }

        self.resolve_statement(&mut func_info.body)?;

        self.func_context = prev_func_context;
        self.pop_scope();

        Ok(())
    }

    fn resolve_expression(&mut self, expr: &mut Expr) -> ResolverResult<()> {
        match &mut expr.expr {
            ExprType::Literal(_) => (),
            ExprType::Infix(_, lhs, rhs) => {
                self.resolve_expression(lhs)?;
                self.resolve_expression(rhs)?;
            }
            ExprType::Prefix(_, expr) => self.resolve_expression(expr)?,
            ExprType::Logical(_, lhs, rhs) => {
                self.resolve_expression(lhs)?;
                self.resolve_expression(rhs)?;
            }
            ExprType::Variable(var_info) => {
                if self.is_during_var_initialization(&var_info.name) {
                    return Err(ResolverError::UseVarInInitialization(
                        var_info.name.to_owned(),
                    ));
                }
                self.resolve_local_variable(var_info);
            }
            ExprType::Assignment(var_info, expr) => {
                self.resolve_expression(expr)?;
                self.resolve_local_variable(var_info);
            }
            ExprType::Call(callee, args) => {
                self.resolve_expression(callee)?;
                for arg in args.iter_mut() {
                    self.resolve_expression(arg)?;
                }
            }
            ExprType::Get(expr_obj, _property) => {
                self.resolve_expression(expr_obj)?;
            }
            ExprType::Set(expr_lhs, _property, expr_rhs) => {
                self.resolve_expression(expr_lhs)?;
                self.resolve_expression(expr_rhs)?;
            }
            ExprType::This(var) => {
                if self.class_context == ClassContext::Global {
                    return Err(ResolverError::ReferencetoThisOutsideOfClass);
                }
                self.resolve_local_variable(var);
            }
            ExprType::Super(var, _) => {
                if self.class_context != ClassContext::Subclass {
                    return Err(ResolverError::SuperStatementOutsideClass);
                }
                self.resolve_local_variable(var);
            }
        }
        Ok(())
    }

    /// Returns true if we are trying to re-define a local variable.
    fn is_var_already_defined(&self, name: &str) -> bool {
        if let Some(scope) = self.scopes.last() {
            return scope.contains_key(name);
        }
        false
    }

    /// Returns true if we are trying to resolve the same variable
    /// we are initializing.
    fn is_during_var_initialization(&self, name: &str) -> bool {
        if let Some(scope) = self.scopes.last() {
            if let Some(VariableState::Uninitialized) = scope.get(name) {
                return true;
            }
        }
        false
    }

    fn resolve_local_variable(&self, var_info: &mut VariableInfo) {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(&var_info.name) {
                var_info.env_hops = Some(i);
                return;
            }
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare_variable(&mut self, name: &str) {
        self.set_variable_state(name, VariableState::Uninitialized);
    }

    fn define_variable(&mut self, name: &str) {
        self.set_variable_state(name, VariableState::Initialized);
    }

    fn set_variable_state(&mut self, name: &str, var_state: VariableState) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_owned(), var_state);
        }
    }
}
