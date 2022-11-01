use super::span::Span;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LogicalOperator {
    And,
    Or,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PrefixOperator {
    Negate,
    LogicalNot,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum InfixOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    EqualTo,
    NotEqualTo,
    GreaterThan,
    GreaterEq,
    LessThan,
    LessEq,
}

#[derive(Debug)]
pub struct Tree {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Stmt {
    pub stmt: StmtType,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StmtType {
    Expression(Expr),
    Print(Expr),
    VariableDecl(Identifier, Expr),
    Block(Vec<Stmt>),
    IfElse(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    For(
        Option<Box<Stmt>>,
        Option<Box<Expr>>,
        Option<Box<Expr>>,
        Box<Stmt>,
    ),
    FuncDecl(FuncInfo),
    Return(Option<Expr>),
    ClassDecl(Identifier, Option<Identifier>, Vec<FuncInfo>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub expr: ExprType,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprType {
    Literal(Literal),
    Infix(InfixOperator, Box<Expr>, Box<Expr>),
    Prefix(PrefixOperator, Box<Expr>),
    Logical(LogicalOperator, Box<Expr>, Box<Expr>),
    Variable(Identifier),
    Assignment(Identifier, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Get(Box<Expr>, Identifier),
    Set(Box<Expr>, Identifier, Box<Expr>),
    This,
    Super(Identifier),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncInfo {
    pub ident: Identifier,
    pub params: Vec<Identifier>,
    pub body: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Number(f64),
    Boolean(bool),
    Str(String),
    Nil,
}

impl LogicalOperator {
    pub fn symbol(&self) -> &str {
        match self {
            LogicalOperator::And => "and",
            LogicalOperator::Or => "or",
        }
    }
}

impl PrefixOperator {
    pub fn symbol(&self) -> &str {
        match self {
            PrefixOperator::LogicalNot => "!",
            PrefixOperator::Negate => "-",
        }
    }
}

impl InfixOperator {
    pub fn symbol(&self) -> &str {
        match self {
            InfixOperator::Add => "+",
            InfixOperator::Subtract => "-",
            InfixOperator::Multiply => "*",
            InfixOperator::Divide => "/",
            InfixOperator::EqualTo => "==",
            InfixOperator::NotEqualTo => "!=",
            InfixOperator::GreaterThan => ">",
            InfixOperator::GreaterEq => ">=",
            InfixOperator::LessThan => "<",
            InfixOperator::LessEq => "<=",
        }
    }
}

impl Identifier {
    pub fn new(name: String, span: Span) -> Self {
        Self { name, span }
    }
}

impl Stmt {
    pub fn new(stmt: StmtType, span: Span) -> Self {
        Stmt { stmt, span }
    }
}

impl FuncInfo {
    pub fn new(ident: Identifier, params: Vec<Identifier>, body: Vec<Stmt>, span: Span) -> Self {
        FuncInfo {
            ident,
            params,
            body,
            span,
        }
    }
}

impl Expr {
    pub fn new(expr: ExprType, span: Span) -> Self {
        Expr { expr, span }
    }

    pub fn ast_string(&self) -> String {
        match &self.expr {
            ExprType::Literal(l) => match l {
                Literal::Number(n) => n.to_string(),
                Literal::Boolean(b) => b.to_string(),
                Literal::Str(s) => format!("\"{}\"", s),
                Literal::Nil => "nil".to_owned(),
            },
            ExprType::Infix(op, lhs, rhs) => format!(
                "({} {} {})",
                op.symbol(),
                lhs.ast_string(),
                rhs.ast_string()
            ),
            ExprType::Prefix(op, expr) => format!("({} {})", op.symbol(), expr.ast_string()),
            ExprType::Logical(op, lhs, rhs) => format!(
                "({} {} {})",
                op.symbol(),
                lhs.ast_string(),
                rhs.ast_string()
            ),
            ExprType::Variable(var) => var.name.clone(),
            ExprType::Assignment(var, expr) => format!("(set {} {})", var.name, expr.ast_string()),
            ExprType::Call(callee, args) => {
                let exprs: Vec<_> = args.iter().map(|a| a.ast_string()).collect();
                format!("(call {} {})", callee.ast_string(), exprs.join(" "))
            }
            ExprType::Get(expr, property) => {
                format!("(get {} {})", expr.ast_string(), property.name)
            }
            ExprType::Set(expr_lhs, property, expr_rhs) => format!(
                "(set {} {} {})",
                expr_lhs.ast_string(),
                property.name,
                expr_rhs.ast_string()
            ),
            ExprType::This => String::from("this"),
            ExprType::Super(method) => format!("(super {})", method.name),
        }
    }
}
