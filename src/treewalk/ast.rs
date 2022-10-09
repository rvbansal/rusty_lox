use super::operator::{InfixOperator, PrefixOperator};

#[derive(Debug, PartialEq)]
pub enum Expr {
    NumberLiteral(f64),
    BooleanLiteral(bool),
    StringLiteral(String),
    NilLiteral,
    Infix(InfixOperator, Box<Expr>, Box<Expr>),
    Prefix(PrefixOperator, Box<Expr>),
    Variable(String),
    Assignment(String, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    VariableDecl(String, Expr),
}

impl Expr {
    pub fn ast_string(&self) -> String {
        match self {
            Expr::NumberLiteral(n) => n.to_string(),
            Expr::BooleanLiteral(b) => b.to_string(),
            Expr::StringLiteral(s) => format!("\"{}\"", s),
            Expr::NilLiteral => "nil".to_owned(),
            Expr::Infix(op, lhs, rhs) => format!(
                "({} {} {})",
                op.symbol(),
                lhs.ast_string(),
                rhs.ast_string()
            ),
            Expr::Prefix(op, expr) => format!("({} {})", op.symbol(), expr.ast_string()),
            Expr::Variable(s) => s.clone(),
            Expr::Assignment(s, expr) => format!("(set {} {})", s, expr.ast_string()),
        }
    }
}
