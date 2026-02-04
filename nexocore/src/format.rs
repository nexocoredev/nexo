use crate::ast::{BinOp, Expr, ExprKind, Stmt, StmtKind, UnOp};
use crate::lexer::Lexer;
use crate::parser::Parser;

const INDENT_SIZE: usize = 4;

pub fn format_source(src: &str) -> Result<String, String> {
    let tokens = Lexer::new(src).tokenize().map_err(|e| e)?;
    let ast = catch_parse(|| Parser::new_with_source(tokens, src).parse())?;

    let mut out = String::new();
    let lines = format_block(&ast, 0);
    for (i, line) in lines.iter().enumerate() {
        if i > 0 {
            out.push('\n');
        }
        out.push_str(line);
    }
    out.push('\n');
    Ok(out)
}

fn panic_message(err: Box<dyn std::any::Any + Send>) -> String {
    if let Some(s) = err.downcast_ref::<&str>() {
        return s.to_string();
    }
    if let Some(s) = err.downcast_ref::<String>() {
        return s.clone();
    }
    "formatter error".to_string()
}

fn catch_parse<T, F>(f: F) -> Result<T, String>
where
    F: FnOnce() -> T + std::panic::UnwindSafe,
{
    let hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));
    let result = std::panic::catch_unwind(f);
    std::panic::set_hook(hook);
    match result {
        Ok(v) => Ok(v),
        Err(e) => Err(panic_message(e)),
    }
}

fn format_block(stmts: &[Stmt], indent: usize) -> Vec<String> {
    if stmts.is_empty() {
        return vec![format!("{}null", indent_str(indent))];
    }

    let mut lines = Vec::new();
    for stmt in stmts {
        lines.extend(format_stmt(stmt, indent));
    }
    lines
}

fn format_stmt(stmt: &Stmt, indent: usize) -> Vec<String> {
    let pad = indent_str(indent);
    match &stmt.kind {
        StmtKind::ExprStmt(expr) => vec![format!("{}{}", pad, format_expr(expr))],
        StmtKind::Assign { name, value } => {
            vec![format!("{}{} = {}", pad, name, format_expr(value))]
        }
        StmtKind::IndexAssign {
            target,
            index,
            value,
        } => vec![format!(
            "{}{}[{}] = {}",
            pad,
            format_expr(target),
            format_expr(index),
            format_expr(value)
        )],
        StmtKind::Import(expr) => vec![format!("{}import {}", pad, format_expr(expr))],
        StmtKind::ImportModule { name, alias } => {
            if alias == name {
                vec![format!("{}import {}", pad, name)]
            } else {
                vec![format!("{}import {} as {}", pad, name, alias)]
            }
        }
        StmtKind::Export { name } => vec![format!("{}export {}", pad, name)],
        StmtKind::TryCatch {
            try_body,
            catch_name,
            catch_body,
        } => {
            let mut lines = Vec::new();
            lines.push(format!("{}try:", pad));
            lines.extend(format_block(try_body, indent + 1));
            lines.push(format!("{}catch {}:", pad, catch_name));
            lines.extend(format_block(catch_body, indent + 1));
            lines
        }
        StmtKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let mut lines = Vec::new();
            lines.push(format!("{}if {}:", pad, format_expr(condition)));
            lines.extend(format_block(then_branch, indent + 1));
            if let Some(else_branch) = else_branch {
                lines.push(format!("{}else:", pad));
                lines.extend(format_block(else_branch, indent + 1));
            }
            lines
        }
        StmtKind::While { condition, body } => {
            let mut lines = Vec::new();
            lines.push(format!("{}while {}:", pad, format_expr(condition)));
            lines.extend(format_block(body, indent + 1));
            lines
        }
        StmtKind::FuncDef { name, params, body } => {
            let params = params.join(", ");
            let mut lines = Vec::new();
            lines.push(format!("{}def {}({}):", pad, name, params));
            lines.extend(format_block(body, indent + 1));
            lines
        }
        StmtKind::Return(expr) => vec![format!("{}return {}", pad, format_expr(expr))],
        StmtKind::Break => vec![format!("{}break", pad)],
        StmtKind::Continue => vec![format!("{}continue", pad)],
    }
}

fn format_expr(expr: &Expr) -> String {
    format_expr_prec(expr, Prec::Pipe)
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Prec {
    Pipe = 1,
    Or = 2,
    And = 3,
    Equality = 4,
    Comparison = 5,
    Add = 6,
    Mul = 7,
    Unary = 8,
    Call = 9,
}

fn format_expr_prec(expr: &Expr, parent: Prec) -> String {
    let (text, prec) = match &expr.kind {
        ExprKind::Number(n) => (format_number(*n), Prec::Call),
        ExprKind::String(s) => (format_string_literal(s), Prec::Call),
        ExprKind::Bool(b) => (b.to_string(), Prec::Call),
        ExprKind::Null => ("null".to_string(), Prec::Call),
        ExprKind::Var(name) => (name.clone(), Prec::Call),
        ExprKind::ArrayLiteral(items) => {
            let parts: Vec<String> = items.iter().map(format_expr).collect();
            (format!("[{}]", parts.join(", ")), Prec::Call)
        }
        ExprKind::MapLiteral(items) => {
            let parts: Vec<String> = items
                .iter()
                .map(|(k, v)| format!("{}: {}", format_expr(k), format_expr(v)))
                .collect();
            (format!("{{{}}}", parts.join(", ")), Prec::Call)
        }
        ExprKind::Index { target, index } => {
            let t = format_expr_prec(target, Prec::Call);
            let i = format_expr(index);
            (format!("{}[{}]", t, i), Prec::Call)
        }
        ExprKind::Unary { op, expr } => {
            let inner = format_expr_prec(expr, Prec::Unary);
            let op = match op {
                UnOp::Not => "not ",
            };
            (format!("{}{}", op, inner), Prec::Unary)
        }
        ExprKind::Binary { left, op, right } => {
            let (prec, op_text) = match op {
                BinOp::Or => (Prec::Or, "or"),
                BinOp::And => (Prec::And, "and"),
                BinOp::Equal => (Prec::Equality, "=="),
                BinOp::NotEqual => (Prec::Equality, "!="),
                BinOp::Greater => (Prec::Comparison, ">"),
                BinOp::GreaterEq => (Prec::Comparison, ">="),
                BinOp::Less => (Prec::Comparison, "<"),
                BinOp::LessEq => (Prec::Comparison, "<="),
                BinOp::Add => (Prec::Add, "+"),
                BinOp::Mul => (Prec::Mul, "*"),
            };
            let l = format_expr_prec(left, prec);
            let r = format_expr_prec(right, prec);
            (format!("{} {} {}", l, op_text, r), prec)
        }
        ExprKind::Call { name, args } => {
            let args = format_args(args);
            (format!("{}({})", name, args), Prec::Call)
        }
        ExprKind::CallExpr { callee, args } => {
            let c = format_expr_prec(callee, Prec::Call);
            let args = format_args(args);
            (format!("{}({})", c, args), Prec::Call)
        }
        ExprKind::Pipe { .. } => {
            let stages = flatten_pipe(expr);
            let parts: Vec<String> = stages
                .iter()
                .map(|e| format_expr_prec(e, Prec::Pipe))
                .collect();
            (parts.join(" |> "), Prec::Pipe)
        }
    };

    if prec < parent {
        format!("({})", text)
    } else {
        text
    }
}

fn format_args(args: &[Expr]) -> String {
    let parts: Vec<String> = args.iter().map(format_expr).collect();
    parts.join(", ")
}

fn flatten_pipe(expr: &Expr) -> Vec<&Expr> {
    let mut out = Vec::new();
    let mut current = expr;
    loop {
        if let ExprKind::Pipe { left, right } = &current.kind {
            out.push(right.as_ref());
            current = left.as_ref();
        } else {
            out.push(current);
            break;
        }
    }
    out.reverse();
    out
}

fn indent_str(level: usize) -> String {
    " ".repeat(level * INDENT_SIZE)
}

fn format_number(n: f64) -> String {
    if n.fract() == 0.0 {
        if n <= i64::MAX as f64 && n >= i64::MIN as f64 {
            return format!("{}", n as i64);
        }
    } else {
        return n.to_string();
    }
    n.to_string()
}

fn format_string_literal(s: &str) -> String {
    let mut out = String::new();
    out.push('"');
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            other => out.push(other),
        }
    }
    out.push('"');
    out
}
