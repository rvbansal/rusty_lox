use super::class::LoxClassDataPtr;
use super::environment::Environment;
use super::errors::{InterpreterError, RuntimeResult};
use super::function::LoxFn;
use super::native_function::get_native_funcs;
use super::object::Object;
use crate::lox_frontend::constants::{INIT_STR, SUPER_STR, THIS_STR};
use crate::lox_frontend::grammar::{
    Expr, ExprType, FuncInfo, Literal, Stmt, StmtType, VariableInfo,
};
use crate::lox_frontend::operator::{InfixOperator, LogicalOperator, PrefixOperator};

use std::collections::HashMap;

pub struct Interpreter {
    pub env: Environment,
    pub globals: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        let env = Environment::new();
        for native_func in get_native_funcs().into_iter() {
            let name = native_func.name().to_owned();
            let func = Object::NativeFunc(native_func);
            env.define(name, func);
        }
        let globals = env.clone();

        Interpreter { env, globals }
    }

    pub fn swap_env(&mut self, mut env: Environment) -> Environment {
        std::mem::swap(&mut self.env, &mut env);
        // Return old original interpreter env
        env
    }

    pub fn eval_statements(&mut self, stmts: Vec<Stmt>) -> RuntimeResult<()> {
        for stmt in stmts.iter() {
            self.eval_statement(stmt)?;
        }
        Ok(())
    }

    pub fn eval_statement(&mut self, stmt: &Stmt) -> RuntimeResult<()> {
        match &stmt.stmt {
            StmtType::Expression(expr) => {
                self.eval_expression(expr)?;
            }
            StmtType::Print(expr) => {
                println!("[out] {:?}", self.eval_expression(expr)?);
            }
            StmtType::IfElse(if_condition, if_body, else_body) => {
                self.eval_if_else(if_condition, if_body, else_body.as_deref())?
            }
            StmtType::While(condition, body) => self.eval_while(condition, body)?,
            StmtType::VariableDecl(name, expr) => {
                let value = self.eval_expression(expr)?;
                self.env.define(name.clone(), value);
            }
            StmtType::Block(stmts) => self.eval_block(stmts)?,
            StmtType::FuncDecl(func_info) => {
                self.env.define(
                    func_info.name.clone(),
                    Object::LoxFunc(self.make_fn(func_info, false)),
                );
            }
            StmtType::Return(expr) => {
                let value = match expr {
                    Some(expr) => self.eval_expression(expr)?,
                    None => Object::Nil,
                };
                return Err(InterpreterError::Return(value));
            }
            StmtType::ClassDecl(name, superclass_name, methods_info) => {
                let superclass = match superclass_name {
                    None => None,
                    Some(class) => match self.env_var_lookup(class)? {
                        Object::LoxClass(class) => Some(class),
                        obj => return Err(InterpreterError::NotAClass(obj)),
                    },
                };

                let curr_env = self.env.clone();
                if let Some(superclass) = &superclass {
                    let superclass_copy = Object::LoxClass(superclass.clone());
                    self.env = Environment::with_enclosing(&self.env);
                    self.env.define(SUPER_STR.to_string(), superclass_copy);
                }

                let mut methods = HashMap::new();
                for method in methods_info.iter() {
                    methods.insert(method.name.clone(), self.make_fn(method, true));
                }

                self.env = curr_env;
                let class = LoxClassDataPtr::new(name.clone(), superclass, methods);
                self.env.define(name.clone(), Object::LoxClass(class));
            }
        }

        Ok(())
    }

    fn make_fn(&self, func_info: &FuncInfo, is_method: bool) -> LoxFn {
        let is_initializer = is_method && func_info.name == INIT_STR;
        LoxFn::new(func_info.clone(), is_initializer, self.env.clone())
    }

    pub fn eval_if_else(
        &mut self,
        if_condition: &Expr,
        if_body: &Stmt,
        else_body: Option<&Stmt>,
    ) -> RuntimeResult<()> {
        if self.eval_expression(if_condition)?.is_truthy() {
            return self.eval_statement(if_body);
        }
        if let Some(else_body) = else_body {
            return self.eval_statement(else_body);
        }

        Ok(())
    }

    pub fn eval_while(&mut self, condition: &Expr, body: &Stmt) -> RuntimeResult<()> {
        while self.eval_expression(condition)?.is_truthy() {
            self.eval_statement(body)?;
        }

        Ok(())
    }

    pub fn eval_block(&mut self, stmts: &[Stmt]) -> RuntimeResult<()> {
        let prev_env = self.env.clone();
        self.env = Environment::with_enclosing(&prev_env);

        for stmt in stmts.iter() {
            match self.eval_statement(stmt) {
                Ok(_) => {}
                Err(e) => {
                    self.env = prev_env;
                    return Err(e);
                }
            }
        }

        // Reset to enclosing environment.
        self.env = prev_env;
        Ok(())
    }

    pub fn eval_expression(&mut self, expr: &Expr) -> RuntimeResult<Object> {
        match &expr.expr {
            ExprType::Literal(l) => Ok(self.eval_literal(l)),
            ExprType::Infix(op, lhs, rhs) => self.eval_infix_operator(*op, lhs, rhs),
            ExprType::Prefix(op, expr) => self.eval_prefix_operator(*op, expr),
            ExprType::Logical(op, lhs, rhs) => self.eval_logical_operator(*op, lhs, rhs),
            ExprType::Variable(var_info) => self.env_var_lookup(var_info),
            ExprType::Assignment(var_info, expr) => {
                let value = self.eval_expression(expr)?;
                match var_info.env_hops {
                    Some(env_hops) => self.env.set_at(env_hops, &var_info.name, value.clone())?,
                    None => self.globals.set(&var_info.name, value.clone())?,
                }
                Ok(value)
            }
            ExprType::Call(callee, args) => self.eval_func_call(callee, args),
            ExprType::Get(obj_expr, property) => self.eval_property_get(obj_expr, property),
            ExprType::Set(expr_lhs, property, expr_rhs) => {
                self.eval_property_set(expr_lhs, property, expr_rhs)
            }
            ExprType::This(var) => self.env_var_lookup(var),
            ExprType::Super(var, method) => {
                let superclass = match self.env_var_lookup(var)? {
                    Object::LoxClass(cls) => cls,
                    _ => panic!("super is not a class"),
                };

                let err_msg = "Cannot find super environment";
                let this_depth = var.env_hops.expect(err_msg).checked_sub(1).expect(err_msg);
                let this = self.env.get_at(this_depth, THIS_STR).expect(err_msg);
                match superclass.find_method(method) {
                    Some(method) => Ok(Object::LoxFunc(method.bind(this))),
                    None => Err(InterpreterError::MissingProperty(this, method.to_owned())),
                }
            }
        }
    }

    fn eval_literal(&self, l: &Literal) -> Object {
        match l {
            Literal::Number(n) => Object::Number(*n as f64),
            Literal::Boolean(b) => Object::Boolean(*b),
            Literal::Str(s) => Object::String(s.clone()),
            Literal::Nil => Object::Nil,
        }
    }

    fn env_var_lookup(&self, var_info: &VariableInfo) -> RuntimeResult<Object> {
        match var_info.env_hops {
            Some(env_hops) => self.env.get_at(env_hops, &var_info.name),
            None => self.globals.get(&var_info.name),
        }
    }

    fn eval_property_get(&mut self, expr: &Expr, property: &str) -> RuntimeResult<Object> {
        match self.eval_expression(expr)? {
            Object::LoxInstance(instance) => Ok(instance.get(property)?),
            other => Err(InterpreterError::NotAnInstance(other)),
        }
    }

    fn eval_property_set(
        &mut self,
        expr_lhs: &Expr,
        property: &str,
        expr_rhs: &Expr,
    ) -> RuntimeResult<Object> {
        let instance = match self.eval_expression(expr_lhs)? {
            Object::LoxInstance(instance) => instance,
            other => return Err(InterpreterError::NotAnInstance(other)),
        };

        let value = self.eval_expression(expr_rhs)?;
        instance.set(property, value.clone());

        Ok(value)
    }

    pub fn eval_logical_operator(
        &mut self,
        op: LogicalOperator,
        lhs: &Expr,
        rhs: &Expr,
    ) -> RuntimeResult<Object> {
        let lhs = self.eval_expression(lhs)?;

        // Handle short circuiting.
        match op {
            LogicalOperator::And if !lhs.is_truthy() => Ok(lhs),
            LogicalOperator::Or if lhs.is_truthy() => Ok(lhs),
            _ => self.eval_expression(rhs),
        }
    }

    pub fn eval_infix_operator(
        &mut self,
        op: InfixOperator,
        lhs: &Expr,
        rhs: &Expr,
    ) -> RuntimeResult<Object> {
        let lhs = self.eval_expression(lhs)?;
        let rhs = self.eval_expression(rhs)?;
        Object::apply_infix_op(op, lhs, rhs)
    }

    pub fn eval_prefix_operator(
        &mut self,
        op: PrefixOperator,
        expr: &Expr,
    ) -> RuntimeResult<Object> {
        let value = self.eval_expression(expr)?;
        Object::apply_prefix_op(op, value)
    }

    pub fn eval_func_call(&mut self, callee: &Expr, raw_args: &Vec<Expr>) -> RuntimeResult<Object> {
        let callee = self.eval_expression(callee)?;
        let mut args = Vec::with_capacity(raw_args.len());
        for raw_arg in raw_args.iter() {
            args.push(self.eval_expression(raw_arg)?);
        }

        callee.execute(args, self)
    }
}
