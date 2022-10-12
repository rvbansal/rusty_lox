use super::operator::{InfixOperator, LogicalOperator, PrefixOperator};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VariableInfo {
    pub name: String,
    pub env_hops: Option<usize>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncInfo {
    pub name: String,
    pub params: Vec<String>,
    pub body: Box<Stmt>
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    NumberLiteral(f64),
    BooleanLiteral(bool),
    StringLiteral(String),
    NilLiteral,
    Infix(InfixOperator, Box<Expr>, Box<Expr>),
    Prefix(PrefixOperator, Box<Expr>),
    Logical(LogicalOperator, Box<Expr>, Box<Expr>),
    Variable(VariableInfo),
    Assignment(VariableInfo, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Get(Box<Expr>, String),
    Set(Box<Expr>, String, Box<Expr>)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    VariableDecl(String, Expr),
    Block(Vec<Stmt>),
    IfElse(Expr, Box<Stmt>, Box<Option<Stmt>>),
    While(Expr, Box<Stmt>),
    FuncDecl(FuncInfo),
    Return(Expr),
    ClassDecl(String, Vec<FuncInfo>)
}

impl VariableInfo {
    pub fn new(name: String) -> Self {
        VariableInfo {
            name,
            env_hops: None,
        }
    }
}

impl FuncInfo {
    pub fn new(name: String, params: Vec<String>, body: Stmt) -> Self {
        FuncInfo { name, params, body: Box::new(body) }
    }
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
            Expr::Logical(op, lhs, rhs) => format!(
                "({} {} {})",
                op.symbol(),
                lhs.ast_string(),
                rhs.ast_string()
            ),
            Expr::Variable(var) => var.name.clone(),
            Expr::Assignment(var, expr) => format!("(set {} {})", var.name, expr.ast_string()),
            Expr::Call(callee, args) => {
                let exprs: Vec<_> = args.iter().map(|a| a.ast_string()).collect();
                format!("(call {} {})", callee.ast_string(), exprs.join(" "))
            },
            Expr::Get(expr, property) => format!("(get {} {})", expr.ast_string(), property),
            Expr::Set(expr_lhs, property, expr_rhs) => format!(
                "(set {} {} {})",
                expr_lhs.ast_string(),
                property,
                expr_rhs.ast_string()
            )
        }
    }
}
