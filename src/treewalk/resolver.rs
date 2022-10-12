use super::ast::{Expr, Stmt, VariableInfo};
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
    InsideFunction,
}

pub struct Resolver {
    scopes: Vec<Scope>,
    func_context: FuncContext,
}

#[derive(Debug)]
pub enum ResolverError {
    ReturnStatementInGlobal,
    UseVarInInitialization(String),
    LocalVarDefinedAlready(String),
}

pub type ResolverResult<T> = Result<T, ResolverError>;

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            scopes: vec![],
            func_context: FuncContext::Global,
        }
    }

    pub fn resolve_root(&mut self, stmt: &mut Stmt) -> ResolverResult<()> {
        self.scopes = vec![];
        self.resolve_statement(stmt)
    }

    fn resolve_statement(&mut self, stmt: &mut Stmt) -> ResolverResult<()> {
        match stmt {
            Stmt::Expression(expr) => self.resolve_expression(expr)?,
            Stmt::Print(expr) => self.resolve_expression(expr)?,
            Stmt::VariableDecl(name, expr) => {
                if self.is_var_already_defined(name) {
                    return Err(ResolverError::LocalVarDefinedAlready(name.to_owned()));
                }

                self.declare_variable(name);
                self.resolve_expression(expr)?;
                self.define_variable(name);
            }
            Stmt::Block(stmts) => {
                self.push_scope();
                for stmt in stmts.iter_mut() {
                    self.resolve_statement(stmt)?;
                }
                self.pop_scope();
            }
            Stmt::IfElse(cond, if_body, else_body) => {
                self.resolve_expression(cond)?;
                self.resolve_statement(if_body)?;

                if let Some(ref mut else_body) = **else_body {
                    self.resolve_statement(else_body)?;
                }
            }
            Stmt::While(cond, body) => {
                self.resolve_expression(cond)?;
                self.resolve_statement(body)?;
            }
            Stmt::FuncDecl(func_info) => {
                // Handle the case where the function is used recursively.
                // We need to define it eagerly.
                self.define_variable(&func_info.name);

                self.push_scope();
                let prev_func_context = self.func_context;
                self.func_context = FuncContext::InsideFunction;

                for name in func_info.params.iter() {
                    self.define_variable(name);
                }

                self.resolve_statement(&mut func_info.body)?;

                self.func_context = prev_func_context;
                self.pop_scope();
            }
            Stmt::Return(expr) => {
                if self.func_context == FuncContext::Global {
                    return Err(ResolverError::ReturnStatementInGlobal);
                }
                self.resolve_expression(expr)?;
            },
            Stmt::ClassDecl(name, methods) => {
                self.define_variable(name);
            }
        }

        Ok(())
    }

    fn resolve_expression(&mut self, expr: &mut Expr) -> ResolverResult<()> {
        match expr {
            Expr::NumberLiteral(_)
            | Expr::BooleanLiteral(_)
            | Expr::StringLiteral(_)
            | Expr::NilLiteral => (),
            Expr::Infix(_, lhs, rhs) => {
                self.resolve_expression(lhs)?;
                self.resolve_expression(rhs)?;
            }
            Expr::Prefix(_, expr) => self.resolve_expression(expr)?,
            Expr::Logical(_, lhs, rhs) => {
                self.resolve_expression(lhs)?;
                self.resolve_expression(rhs)?;
            }
            Expr::Variable(var_info) => {
                if self.is_during_var_initialization(&var_info.name) {
                    return Err(ResolverError::UseVarInInitialization(
                        var_info.name.to_owned(),
                    ));
                }
                self.resolve_local_variable(var_info);
            }
            Expr::Assignment(var_info, expr) => {
                self.resolve_expression(expr)?;
                self.resolve_local_variable(var_info);
            }
            Expr::Call(callee, args) => {
                self.resolve_expression(callee)?;
                for arg in args.iter_mut() {
                    self.resolve_expression(arg)?;
                }
            },
            Expr::Get(expr_obj, property) => {
                self.resolve_expression(expr_obj)?;
            },
            Expr::Set(expr_lhs, property, expr_rhs) => {
                self.resolve_expression(expr_lhs)?;
                self.resolve_expression(expr_rhs)?;
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
