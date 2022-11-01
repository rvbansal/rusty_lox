use super::grammar::{
    Expr, ExprType, FuncInfo, Literal, Stmt, StmtType, Tree, VHops, VariableInfo,
};
use crate::lox_frontend::constants::{INIT_STR, SUPER_STR, THIS_STR};
use crate::lox_frontend::grammar as frontend_grammar;
use crate::lox_frontend::span::Span;
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

    pub fn resolve(mut self, tree: frontend_grammar::Tree) -> ResolverResult<Tree> {
        let mut stmts = vec![];

        for stmt in tree.stmts.iter() {
            stmts.push(self.resolve_statement(stmt)?);
        }

        Ok(Tree { stmts })
    }

    fn resolve_statement(&mut self, stmt: &frontend_grammar::Stmt) -> ResolverResult<Stmt> {
        let stmt_type = match &stmt.stmt {
            frontend_grammar::StmtType::Expression(expr) => {
                StmtType::Expression(self.resolve_expression(expr)?)
            }
            frontend_grammar::StmtType::Print(expr) => {
                StmtType::Print(self.resolve_expression(expr)?)
            }
            frontend_grammar::StmtType::VariableDecl(name, expr) => {
                if self.is_var_already_defined(name) {
                    return Err(ResolverError::LocalVarDefinedAlready(name.to_owned()));
                }

                self.declare_variable(name);
                let expr = self.resolve_expression(expr)?;
                self.define_variable(name);
                StmtType::VariableDecl(name.clone(), expr)
            }
            frontend_grammar::StmtType::Block(stmts) => {
                self.push_scope();
                let mut resolved_stmts = Vec::with_capacity(stmts.len());

                for stmt in stmts.iter() {
                    resolved_stmts.push(self.resolve_statement(stmt)?);
                }
                self.pop_scope();
                StmtType::Block(resolved_stmts)
            }
            frontend_grammar::StmtType::IfElse(cond, if_body, else_body) => {
                let cond = self.resolve_expression(cond)?;
                let body = Box::new(self.resolve_statement(if_body)?);
                let else_body = match else_body {
                    Some(s) => Some(Box::new(self.resolve_statement(s)?)),
                    None => None,
                };
                StmtType::IfElse(cond, body, else_body)
            }
            frontend_grammar::StmtType::While(cond, body) => {
                let cond = self.resolve_expression(cond)?;
                let body = Box::new(self.resolve_statement(body)?);
                StmtType::While(cond, body)
            }
            frontend_grammar::StmtType::FuncDecl(func_info) => {
                let fn_data = self.resolve_function(func_info, FuncContext::Function)?;
                StmtType::FuncDecl(fn_data)
            }
            frontend_grammar::StmtType::Return(expr) => {
                let return_expr = match self.func_context {
                    FuncContext::Global => return Err(ResolverError::ReturnStatementInGlobal),
                    FuncContext::Initializer => match expr {
                        Some(_) => return Err(ResolverError::ReturnInInitializer),
                        None => Expr {
                            expr: ExprType::This(VariableInfo {
                                name: THIS_STR.to_owned(),
                                env_hops: self.lookup_variable(THIS_STR),
                            }),
                            span: Span::default(),
                        },
                    },
                    _ => match expr {
                        Some(expr) => self.resolve_expression(expr)?,
                        None => Expr {
                            expr: ExprType::Literal(Literal::Nil),
                            span: Span::default(),
                        },
                    },
                };
                StmtType::Return(return_expr)
            }
            frontend_grammar::StmtType::ClassDecl(name, superclass, methods) => {
                self.define_variable(name);

                let resolved_superclass = match superclass {
                    Some(superclass) => {
                        if superclass == name {
                            return Err(ResolverError::SuperClassIsSameAsClass);
                        }
                        Some(self.resolve_variable(superclass))
                    }
                    None => None,
                };

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

                let mut resolved_methods = Vec::with_capacity(methods.len());
                for method in methods.iter() {
                    let func_context = if method.name == INIT_STR {
                        FuncContext::Initializer
                    } else {
                        FuncContext::Method
                    };
                    resolved_methods.push(self.resolve_function(method, func_context)?);
                }

                self.class_context = prev_class_context;
                self.pop_scope();
                if superclass.is_some() {
                    self.pop_scope();
                }

                StmtType::ClassDecl(name.clone(), resolved_superclass, resolved_methods)
            }
        };

        Ok(Stmt {
            stmt: stmt_type,
            span: stmt.span,
        })
    }

    fn resolve_function(
        &mut self,
        func_info: &frontend_grammar::FuncInfo,
        context: FuncContext,
    ) -> ResolverResult<FuncInfo> {
        // Handle the case where the function is used recursively.
        // We need to define it eagerly.
        self.define_variable(&func_info.name);

        self.push_scope();
        let prev_func_context = self.func_context;
        self.func_context = context;

        for name in func_info.params.iter() {
            self.define_variable(name);
        }

        let resolved_body = Box::new(self.resolve_statement(&func_info.body)?);

        self.func_context = prev_func_context;
        self.pop_scope();

        Ok(FuncInfo {
            name: func_info.name.clone(),
            params: func_info.params.clone(),
            body: resolved_body,
        })
    }

    fn resolve_expression(&mut self, expr: &frontend_grammar::Expr) -> ResolverResult<Expr> {
        let expr_type = match &expr.expr {
            frontend_grammar::ExprType::Literal(l) => ExprType::Literal(l.clone()),
            frontend_grammar::ExprType::Infix(op, lhs, rhs) => {
                let lhs = Box::new(self.resolve_expression(lhs)?);
                let rhs = Box::new(self.resolve_expression(rhs)?);
                ExprType::Infix(*op, lhs, rhs)
            }
            frontend_grammar::ExprType::Prefix(op, expr) => {
                let expr = Box::new(self.resolve_expression(expr)?);
                ExprType::Prefix(*op, expr)
            }
            frontend_grammar::ExprType::Logical(op, lhs, rhs) => {
                let lhs = Box::new(self.resolve_expression(lhs)?);
                let rhs = Box::new(self.resolve_expression(rhs)?);
                ExprType::Logical(*op, lhs, rhs)
            }
            frontend_grammar::ExprType::Variable(var) => {
                if self.is_during_var_initialization(var) {
                    return Err(ResolverError::UseVarInInitialization(var.to_owned()));
                }
                ExprType::Variable(self.resolve_variable(var))
            }
            frontend_grammar::ExprType::Assignment(var, expr) => {
                let var = self.resolve_variable(var);
                let expr = Box::new(self.resolve_expression(expr)?);
                ExprType::Assignment(var, expr)
            }
            frontend_grammar::ExprType::Call(callee, args) => {
                let callee = Box::new(self.resolve_expression(callee)?);
                let args: Result<Vec<_>, _> =
                    args.iter().map(|e| self.resolve_expression(e)).collect();
                ExprType::Call(callee, args?)
            }
            frontend_grammar::ExprType::Get(expr, property) => {
                let expr = Box::new(self.resolve_expression(expr)?);
                ExprType::Get(expr, property.clone())
            }
            frontend_grammar::ExprType::Set(expr, property, value) => {
                let expr = Box::new(self.resolve_expression(expr)?);
                let value = Box::new(self.resolve_expression(value)?);
                ExprType::Set(expr, property.clone(), value)
            }
            frontend_grammar::ExprType::This => {
                if self.class_context == ClassContext::Global {
                    return Err(ResolverError::ReferencetoThisOutsideOfClass);
                }
                ExprType::This(self.resolve_variable(THIS_STR))
            }
            frontend_grammar::ExprType::Super(method_name) => {
                if self.class_context != ClassContext::Subclass {
                    return Err(ResolverError::SuperStatementOutsideClass);
                }
                ExprType::Super(self.resolve_variable(SUPER_STR), method_name.clone())
            }
        };

        Ok(Expr {
            expr: expr_type,
            span: expr.span,
        })
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

    fn resolve_variable(&self, name: &str) -> VariableInfo {
        VariableInfo {
            name: name.to_owned(),
            env_hops: self.lookup_variable(name),
        }
    }

    fn lookup_variable(&self, name: &str) -> VHops {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name) {
                return VHops::Local(i);
            }
        }

        VHops::Global
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
