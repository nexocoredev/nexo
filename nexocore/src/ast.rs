#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

impl Span {
    pub fn merge(a: Span, b: Span) -> Span {
        Span {
            start_line: a.start_line,
            start_col: a.start_col,
            end_line: b.end_line,
            end_col: b.end_col,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Number(f64),
    String(String),
    Bool(bool),
    Null,
    Var(String),

    ArrayLiteral(Vec<Expr>),
    MapLiteral(Vec<(Expr, Expr)>),

    Index {
        target: Box<Expr>,
        index: Box<Expr>,
    },

    Binary {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },

    Unary {
        op: UnOp,
        expr: Box<Expr>,
    },

    Call {
        name: String,
        args: Vec<Expr>,
    },
    CallExpr {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },

    Pipe {
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Not,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Mul,

    Greater,
    GreaterEq,
    Less,
    LessEq,
    Equal,
    NotEqual,

    And,
    Or,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    ExprStmt(Expr),
    Assign {
        name: String,
        value: Expr,
    },
    IndexAssign {
        target: Expr,
        index: Expr,
        value: Expr,
    },

    Import(Expr),
    ImportModule {
        name: String,
        alias: String,
    },
    Export {
        name: String,
    },
    TryCatch {
        try_body: Vec<Stmt>,
        catch_name: String,
        catch_body: Vec<Stmt>,
    },

    If {
        condition: Expr,
        then_branch: Vec<Stmt>,
        else_branch: Option<Vec<Stmt>>,
    },

    While {
        condition: Expr,
        body: Vec<Stmt>,
    },

    FuncDef {
        name: String,
        params: Vec<String>,
        body: Vec<Stmt>,
    },

    Return(Expr),

    Break,
    Continue,
}
