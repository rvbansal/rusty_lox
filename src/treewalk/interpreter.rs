use super::ast::{Expr, Stmt};
use super::environment::Environment;
use super::errors::{InterpreterError, RuntimeResult};
use super::object::Object;
use super::operator::{InfixOperator, PrefixOperator};

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Environment::new(),
        }
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
            }
            Stmt::Print(expr) => {
                println!("[out] {:?}", self.eval_expression(expr)?);
            }
            Stmt::VariableDecl(name, expr) => {
                let value = self.eval_expression(expr)?;
                self.env.define(name.clone(), value);
            }
        }

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
            Expr::Variable(name) => self.env.get(name),
            Expr::Assignment(name, expr) => {
                let value = self.eval_expression(expr)?;
                self.env.set(name.clone(), value.clone())?;
                Ok(value)
            }
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
