use super::ast::{Expr, Stmt};
use super::environment::Environment;
use super::errors::{InterpreterError, RuntimeResult};
use super::native_funcs::{self, get_native_funcs};
use super::object::Object;
use super::operator::{InfixOperator, LogicalOperator, PrefixOperator};
use std::rc::Rc;

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut env = Environment::new();
        for native_func in get_native_funcs().into_iter() {
            let name = native_func.name.clone();
            let func = Object::NativeFunc(Rc::new(native_func));
            env.define(name, func);
        }

        Interpreter { env }
    }

    pub fn eval_statements(&mut self, stmts: Vec<Stmt>) -> RuntimeResult<()> {
        for stmt in stmts.iter() {
            self.eval_statement(stmt)?;
        }
        Ok(())
    }

    fn eval_statement(&mut self, stmt: &Stmt) -> RuntimeResult<()> {
        match stmt {
            Stmt::Expression(expr) => {
                self.eval_expression(expr)?;
                Ok(())
            }
            Stmt::Print(expr) => {
                println!("[out] {:?}", self.eval_expression(expr)?);
                Ok(())
            }
            Stmt::IfElse(if_condition, if_body, else_body) => {
                self.eval_if_else(if_condition, if_body, else_body)
            }
            Stmt::While(condition, body) => self.eval_while(condition, body),
            Stmt::VariableDecl(name, expr) => {
                let value = self.eval_expression(expr)?;
                self.env.define(name.clone(), value);
                Ok(())
            }
            Stmt::Block(stmts) => self.eval_block(stmts),
        }
    }

    pub fn eval_if_else(
        &mut self,
        if_condition: &Expr,
        if_body: &Stmt,
        else_body: &Option<Stmt>,
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

    pub fn eval_block(&mut self, stmts: &Vec<Stmt>) -> RuntimeResult<()> {
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
        match expr {
            Expr::NumberLiteral(n) => Ok(Object::Number(*n as f64)),
            Expr::BooleanLiteral(b) => Ok(Object::Boolean(*b)),
            Expr::StringLiteral(s) => Ok(Object::String(s.clone())),
            Expr::NilLiteral => Ok(Object::Nil),
            Expr::Infix(op, lhs, rhs) => self.eval_infix_operator(op, lhs, rhs),
            Expr::Prefix(op, expr) => self.eval_prefix_operator(op, expr),
            Expr::Logical(op, lhs, rhs) => self.eval_logical_operator(op, lhs, rhs),
            Expr::Variable(name) => self.env.get(name),
            Expr::Assignment(name, expr) => {
                let value = self.eval_expression(expr)?;
                self.env.set(name.clone(), value.clone())?;
                Ok(value)
            }
            Expr::Call(callee, args) => self.eval_func_call(callee, args),
        }
    }

    pub fn eval_logical_operator(
        &mut self,
        op: &LogicalOperator,
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
        op: &InfixOperator,
        lhs: &Expr,
        rhs: &Expr,
    ) -> RuntimeResult<Object> {
        let lhs = self.eval_expression(lhs)?;
        let rhs = self.eval_expression(rhs)?;

        match op {
            InfixOperator::Add => match (lhs, rhs) {
                (Object::Number(a), Object::Number(b)) => Ok(Object::Number(a + b)),
                (Object::String(a), Object::String(b)) => Ok(Object::String(a + &b)),
                (a, b) => Err(InterpreterError::IllegalInfixOperation(*op, a, b)),
            },
            InfixOperator::Subtract => numerical_binop(op, lhs, rhs, |a, b| Object::Number(a - b)),
            InfixOperator::Multiply => numerical_binop(op, lhs, rhs, |a, b| Object::Number(a * b)),
            InfixOperator::Divide => numerical_binop(op, lhs, rhs, |a, b| Object::Number(a / b)),
            InfixOperator::EqualTo => Ok(Object::Boolean(lhs == rhs)),
            InfixOperator::NotEqualTo => Ok(Object::Boolean(lhs != rhs)),
            InfixOperator::GreaterEq => {
                numerical_binop(op, lhs, rhs, |a, b| Object::Boolean(a >= b))
            }
            InfixOperator::GreaterThan => {
                numerical_binop(op, lhs, rhs, |a, b| Object::Boolean(a > b))
            }
            InfixOperator::LessEq => numerical_binop(op, lhs, rhs, |a, b| Object::Boolean(a <= b)),
            InfixOperator::LessThan => numerical_binop(op, lhs, rhs, |a, b| Object::Boolean(a < b)),
        }
    }

    pub fn eval_prefix_operator(
        &mut self,
        op: &PrefixOperator,
        expr: &Expr,
    ) -> RuntimeResult<Object> {
        let value = self.eval_expression(expr)?;

        match op {
            PrefixOperator::Negate => match value {
                Object::Number(n) => Ok(Object::Number(-n)),
                _ => Err(InterpreterError::IllegalPrefixOperation(*op, value)),
            },
            PrefixOperator::LogicalNot => Ok(Object::Boolean(!value.is_truthy())),
        }
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

fn numerical_binop<F>(
    op: &InfixOperator,
    lhs: Object,
    rhs: Object,
    func: F,
) -> RuntimeResult<Object>
where
    F: Fn(f64, f64) -> Object,
{
    match (lhs, rhs) {
        (Object::Number(a), Object::Number(b)) => Ok(func(a, b)),
        (a, b) => Err(InterpreterError::IllegalInfixOperation(*op, a, b)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_eval_func(expr: &Expr) -> RuntimeResult<Object> {
        let mut interpreter = Interpreter::new();
        return interpreter.eval_expression(expr);
    }

    #[test]
    fn test_interpreter_infix_expr() {
        let expr1 = Expr::Infix(
            InfixOperator::Add,
            Box::new(Expr::NumberLiteral(1.0)),
            Box::new(Expr::NumberLiteral(2.0)),
        );

        let expr2 = Expr::Infix(
            InfixOperator::Add,
            Box::new(Expr::StringLiteral("test1".to_string())),
            Box::new(Expr::StringLiteral("test2".to_string())),
        );

        let expr3 = Expr::Infix(
            InfixOperator::LessEq,
            Box::new(Expr::NumberLiteral(1.0)),
            Box::new(Expr::NumberLiteral(0.5)),
        );

        assert_eq!(test_eval_func(&expr1), Ok(Object::Number(3.0)));
        assert_eq!(
            test_eval_func(&expr2),
            Ok(Object::String("test1test2".to_string()))
        );
        assert_eq!(test_eval_func(&expr3), Ok(Object::Boolean(false)));
    }

    #[test]
    fn test_interpreter_prefix_expr() {
        let expr1 = Expr::Prefix(PrefixOperator::Negate, Box::new(Expr::NumberLiteral(2.0)));

        let expr2 = Expr::Prefix(
            PrefixOperator::LogicalNot,
            Box::new(Expr::BooleanLiteral(false)),
        );

        assert_eq!(test_eval_func(&expr1), Ok(Object::Number(-2.0)));
        assert_eq!(test_eval_func(&expr2), Ok(Object::Boolean(true)));
    }
}
