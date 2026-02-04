use std::collections::HashSet;

use crate::ast::{Expr, ExprKind, Stmt, StmtKind};
use crate::lexer::Lexer;
use crate::parser::Parser;

#[derive(Debug, Clone, Copy)]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Debug)]
pub struct Lint {
    pub severity: Severity,
    pub message: String,
}

pub fn lint_source(src: &str, strict: bool) -> Result<Vec<Lint>, String> {
    let tokens = Lexer::new(src).tokenize().map_err(|e| e)?;
    let ast = catch_parse(|| Parser::new_with_source(tokens, src).parse())?;
    let mut lints = Vec::new();
    let mut scope = Scope::new();
    collect_function_names(&ast, &mut scope);
    let mut ctx = Context::new();
    lint_block(&ast, &mut scope, &mut lints, strict, &mut ctx);
    scope.emit_unused(&mut lints);
    Ok(lints)
}

fn panic_message(err: Box<dyn std::any::Any + Send>) -> String {
    if let Some(s) = err.downcast_ref::<&str>() {
        return s.to_string();
    }
    if let Some(s) = err.downcast_ref::<String>() {
        return s.clone();
    }
    "parse error".to_string()
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

struct Scope {
    assigned: HashSet<String>,
    used: HashSet<String>,
    imports: HashSet<String>,
    exported: HashSet<String>,
    functions: HashSet<String>,
    functions_seen: HashSet<String>,
}

impl Scope {
    fn new() -> Self {
        Self {
            assigned: HashSet::new(),
            used: HashSet::new(),
            imports: HashSet::new(),
            exported: HashSet::new(),
            functions: HashSet::new(),
            functions_seen: HashSet::new(),
        }
    }

    fn emit_unused(&self, lints: &mut Vec<Lint>) {
        for name in &self.imports {
            if !self.used.contains(name) && !is_ignored(name) {
                lints.push(Lint {
                    severity: Severity::Warning,
                    message: format!("unused import '{}'", name),
                });
            }
        }

        for name in &self.assigned {
            if !self.used.contains(name) && !self.exported.contains(name) && !is_ignored(name) {
                lints.push(Lint {
                    severity: Severity::Warning,
                    message: format!("unused variable '{}'", name),
                });
            }
        }
    }
}

struct Context {
    in_loop: usize,
    in_function: usize,
}

impl Context {
    fn new() -> Self {
        Self {
            in_loop: 0,
            in_function: 0,
        }
    }
}

fn lint_block(
    stmts: &[Stmt],
    scope: &mut Scope,
    lints: &mut Vec<Lint>,
    strict: bool,
    ctx: &mut Context,
) -> bool {
    let mut terminated = false;
    for stmt in stmts {
        if terminated {
            lints.push(Lint {
                severity: Severity::Warning,
                message: "unreachable code".to_string(),
            });
            continue;
        }
        terminated = lint_stmt(stmt, scope, lints, strict, ctx) || terminated;
    }
    terminated
}

fn lint_stmt(
    stmt: &Stmt,
    scope: &mut Scope,
    lints: &mut Vec<Lint>,
    strict: bool,
    ctx: &mut Context,
) -> bool {
    match &stmt.kind {
        StmtKind::ExprStmt(expr) => {
            lint_expr(expr, scope, lints, strict);
            false
        }
        StmtKind::Assign { name, value } => {
            if scope.assigned.contains(name) {
                lints.push(Lint {
                    severity: if strict {
                        Severity::Error
                    } else {
                        Severity::Warning
                    },
                    message: format!("shadowing '{}'", name),
                });
            }
            scope.assigned.insert(name.clone());
            lint_expr(value, scope, lints, strict);
            false
        }
        StmtKind::IndexAssign {
            target,
            index,
            value,
        } => {
            if strict {
                if let ExprKind::Var(name) = &target.kind {
                    if !scope.assigned.contains(name) {
                        lints.push(Lint {
                            severity: Severity::Error,
                            message: format!("undefined variable '{}'", name),
                        });
                    }
                }
            }
            lint_expr(target, scope, lints, strict);
            lint_expr(index, scope, lints, strict);
            lint_expr(value, scope, lints, strict);
            false
        }
        StmtKind::Import(expr) => {
            lint_expr(expr, scope, lints, strict);
            false
        }
        StmtKind::ImportModule { name: _, alias } => {
            if scope.assigned.contains(alias) || scope.imports.contains(alias) {
                lints.push(Lint {
                    severity: if strict {
                        Severity::Error
                    } else {
                        Severity::Warning
                    },
                    message: format!("import name collision '{}'", alias),
                });
            }
            scope.assigned.insert(alias.clone());
            scope.imports.insert(alias.clone());
            false
        }
        StmtKind::Export { name } => {
            scope.exported.insert(name.clone());
            false
        }
        StmtKind::TryCatch {
            try_body,
            catch_name,
            catch_body,
        } => {
            if scope.assigned.contains(catch_name) {
                lints.push(Lint {
                    severity: if strict {
                        Severity::Error
                    } else {
                        Severity::Warning
                    },
                    message: format!("shadowing '{}'", catch_name),
                });
            }
            scope.assigned.insert(catch_name.clone());
            let try_term = lint_block(try_body, scope, lints, strict, ctx);
            let catch_term = lint_block(catch_body, scope, lints, strict, ctx);
            try_term && catch_term
        }
        StmtKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            lint_expr(condition, scope, lints, strict);
            let then_term = lint_block(then_branch, scope, lints, strict, ctx);
            let else_term = else_branch
                .as_ref()
                .map(|b| lint_block(b, scope, lints, strict, ctx))
                .unwrap_or(false);
            then_term && else_term
        }
        StmtKind::While { condition, body } => {
            lint_expr(condition, scope, lints, strict);
            ctx.in_loop += 1;
            lint_block(body, scope, lints, strict, ctx);
            ctx.in_loop -= 1;
            false
        }
        StmtKind::FuncDef { name, params, body } => {
            if scope.functions_seen.contains(name) {
                lints.push(Lint {
                    severity: Severity::Error,
                    message: format!("duplicate function '{}'", name),
                });
            }
            if scope.assigned.contains(name) && !scope.functions.contains(name) {
                lints.push(Lint {
                    severity: if strict {
                        Severity::Error
                    } else {
                        Severity::Warning
                    },
                    message: format!("shadowing '{}'", name),
                });
            }
            scope.assigned.insert(name.clone());
            scope.functions.insert(name.clone());
            scope.functions_seen.insert(name.clone());
            let free = lint_function(params, body, lints, strict, ctx);
            for name in free {
                if !is_builtin(&name) {
                    scope.used.insert(name);
                }
            }
            false
        }
        StmtKind::Return(expr) => {
            if ctx.in_function == 0 {
                lints.push(Lint {
                    severity: Severity::Error,
                    message: "return outside function".to_string(),
                });
            }
            lint_expr(expr, scope, lints, strict);
            true
        }
        StmtKind::Break | StmtKind::Continue => {
            if ctx.in_loop == 0 {
                lints.push(Lint {
                    severity: Severity::Error,
                    message: "break/continue outside loop".to_string(),
                });
            }
            true
        }
    }
}

fn lint_function(
    params: &[String],
    body: &[Stmt],
    lints: &mut Vec<Lint>,
    strict: bool,
    ctx: &mut Context,
) -> HashSet<String> {
    let mut scope = Scope::new();
    collect_function_names(body, &mut scope);
    let mut seen = HashSet::new();
    for p in params {
        if !seen.insert(p.clone()) {
            lints.push(Lint {
                severity: Severity::Error,
                message: format!("duplicate parameter '{}'", p),
            });
        }
        scope.assigned.insert(p.clone());
    }
    ctx.in_function += 1;
    lint_block(body, &mut scope, lints, strict, ctx);
    ctx.in_function -= 1;
    scope.emit_unused(lints);

    let mut free = HashSet::new();
    for name in &scope.used {
        if !scope.assigned.contains(name) && !is_builtin(name) {
            free.insert(name.clone());
        }
    }
    free
}

fn lint_expr(expr: &Expr, scope: &mut Scope, lints: &mut Vec<Lint>, strict: bool) {
    match &expr.kind {
        ExprKind::Var(name) => {
            if strict && !scope.assigned.contains(name) && !is_builtin(name) {
                lints.push(Lint {
                    severity: Severity::Error,
                    message: format!("undefined variable '{}'", name),
                });
            }
            scope.used.insert(name.clone());
        }
        ExprKind::Binary { left, right, .. } => {
            lint_expr(left, scope, lints, strict);
            lint_expr(right, scope, lints, strict);
        }
        ExprKind::Unary { expr, .. } => lint_expr(expr, scope, lints, strict),
        ExprKind::Call { name, args } => {
            if strict
                && !scope.assigned.contains(name)
                && !scope.imports.contains(name)
                && !is_builtin(name)
            {
                lints.push(Lint {
                    severity: Severity::Error,
                    message: format!("undefined function '{}'", name),
                });
            }
            scope.used.insert(name.clone());
            for arg in args {
                lint_expr(arg, scope, lints, strict);
            }
        }
        ExprKind::CallExpr { callee, args } => {
            lint_expr(callee, scope, lints, strict);
            for arg in args {
                lint_expr(arg, scope, lints, strict);
            }
        }
        ExprKind::ArrayLiteral(items) => {
            for item in items {
                lint_expr(item, scope, lints, strict);
            }
        }
        ExprKind::MapLiteral(items) => {
            for (key, value) in items {
                lint_expr(key, scope, lints, strict);
                lint_expr(value, scope, lints, strict);
            }
        }
        ExprKind::Index { target, index } => {
            lint_expr(target, scope, lints, strict);
            lint_expr(index, scope, lints, strict);
        }
        ExprKind::Pipe { left, right } => {
            lint_expr(left, scope, lints, strict);
            lint_expr(right, scope, lints, strict);
        }
        ExprKind::Number(_) | ExprKind::String(_) | ExprKind::Bool(_) | ExprKind::Null => {}
    }
}

fn is_ignored(name: &str) -> bool {
    name.starts_with('_')
}

fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "print"
            | "len"
            | "str"
            | "int"
            | "round"
            | "floor"
            | "ceil"
            | "input"
            | "append"
            | "log_info"
            | "log_warn"
            | "log_error"
            | "fs_read"
            | "fs_write"
            | "fs_exists"
            | "fs_list"
            | "proc_run"
            | "env_get"
            | "args"
            | "arg_has"
            | "arg_get"
            | "time_sleep"
            | "time_now"
            | "ffi_load"
            | "ffi_call"
            | "py_run"
            | "http_get"
            | "http_post"
            | "re_match"
            | "re_findall"
            | "re_replace"
            | "path_join"
            | "path_dirname"
            | "glob"
            | "import"
            | "import_module"
            | "export"
            | "json_parse"
            | "json_stringify"
    )
}

fn collect_function_names(stmts: &[Stmt], scope: &mut Scope) {
    for stmt in stmts {
        if let StmtKind::FuncDef { name, .. } = &stmt.kind {
            scope.assigned.insert(name.clone());
            scope.functions.insert(name.clone());
        }
    }
}
