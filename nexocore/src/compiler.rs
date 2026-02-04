// src/compiler.rs
use std::collections::HashMap;

use crate::{
    ast::{BinOp, Expr, ExprKind, Span, Stmt, StmtKind, UnOp},
    model::{ChunkDef, ConstValue, FunctionDef, Instruction, Program},
    opcode::OpCode,
};

pub struct Compiler {
    str_consts: HashMap<String, usize>,
    constants: Vec<ConstValue>,
    loop_stack: Vec<LoopContext>,
    locals: Option<LocalState>,
    upvalues: Option<UpvalueState>,
}

struct LoopContext {
    start: usize,
    break_jumps: Vec<usize>,
}

struct LocalState {
    map: HashMap<String, usize>,
    count: usize,
}

struct UpvalueState {
    map: HashMap<String, usize>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            str_consts: HashMap::new(),
            constants: Vec::new(),
            loop_stack: Vec::new(),
            locals: None,
            upvalues: None,
        }
    }

    fn new_function(params: &[String], upvalue_names: Vec<String>) -> Self {
        let mut compiler = Self::new();
        let mut map = HashMap::new();
        for (i, p) in params.iter().enumerate() {
            map.insert(p.clone(), i);
        }
        compiler.locals = Some(LocalState {
            map,
            count: params.len(),
        });
        let mut upvalue_map = HashMap::new();
        for (i, name) in upvalue_names.iter().enumerate() {
            upvalue_map.insert(name.clone(), i);
        }
        compiler.upvalues = Some(UpvalueState { map: upvalue_map });
        compiler
    }

    pub fn compile(&mut self, stmts: Vec<Stmt>) -> Program {
        let mut code: Vec<Instruction> = Vec::new();

        for stmt in &stmts {
            self.compile_stmt(stmt, &mut code);
        }

        code.push(Instruction {
            op: OpCode::PushConst,
            a: self.add_const(ConstValue::Null) as i32,
            b: 0,
            span: None,
        });
        code.push(Instruction {
            op: OpCode::Return,
            a: 0,
            b: 0,
            span: None,
        });

        Program {
            entry: FunctionDef {
                name: "__main__".into(),
                arity: 0,
                local_count: 0,
                chunk: ChunkDef {
                    constants: self.constants.clone(),
                    code,
                },
            },
        }
    }

    fn emit(code: &mut Vec<Instruction>, span: Option<Span>, op: OpCode, a: i32, b: i32) {
        code.push(Instruction { op, a, b, span });
    }

    fn compile_stmt(&mut self, stmt: &Stmt, code: &mut Vec<Instruction>) {
        let span = stmt.span;
        match &stmt.kind {
            StmtKind::ExprStmt(expr) => {
                self.compile_expr(expr, code);
                Self::emit(code, Some(span), OpCode::Pop, 0, 0);
            }

            StmtKind::Assign { name, value } => {
                self.compile_expr(value, code);
                self.store_name(name, code, Some(span));
            }

            StmtKind::IndexAssign {
                target,
                index,
                value,
            } => {
                let name = match &target.kind {
                    ExprKind::Var(name) => name,
                    _ => panic!("Index assignment target must be a variable"),
                };

                self.load_name(name, code, Some(span));
                self.compile_expr(index, code);
                self.compile_expr(value, code);
                Self::emit(code, Some(span), OpCode::SetIndex, 0, 0);
                self.store_name(name, code, Some(span));
            }

            StmtKind::Return(expr) => {
                self.compile_expr(expr, code);
                Self::emit(code, Some(span), OpCode::Return, 0, 0);
            }

            StmtKind::Import(expr) => {
                self.compile_expr(expr, code);
                let idx = self.add_string_const("import");
                Self::emit(code, Some(span), OpCode::CallBuiltin, idx as i32, 1);
                Self::emit(code, Some(span), OpCode::Pop, 0, 0);
            }

            StmtKind::ImportModule { name, alias } => {
                let idx = self.add_string_const(name);
                Self::emit(code, Some(span), OpCode::PushConst, idx as i32, 0);
                let import_idx = self.add_string_const("import_module");
                Self::emit(code, Some(span), OpCode::CallBuiltin, import_idx as i32, 1);
                self.store_name(alias, code, Some(span));
            }

            StmtKind::Export { name } => {
                let idx = self.add_string_const(name);
                Self::emit(code, Some(span), OpCode::PushConst, idx as i32, 0);
                self.load_name(name, code, Some(span));
                let export_idx = self.add_string_const("export");
                Self::emit(code, Some(span), OpCode::CallBuiltin, export_idx as i32, 2);
                Self::emit(code, Some(span), OpCode::Pop, 0, 0);
            }

            StmtKind::TryCatch {
                try_body,
                catch_name,
                catch_body,
            } => {
                let try_pos = code.len();
                Self::emit(code, Some(span), OpCode::TryStart, -1, 0);

                for stmt in try_body {
                    self.compile_stmt(stmt, code);
                }

                Self::emit(code, Some(span), OpCode::TryEnd, 0, 0);

                let jmp_pos = code.len();
                Self::emit(code, Some(span), OpCode::Jump, -1, 0);

                let catch_start = code.len();
                code[try_pos].a = catch_start as i32;

                self.store_name(catch_name, code, Some(span));

                for stmt in catch_body {
                    self.compile_stmt(stmt, code);
                }

                code[jmp_pos].a = code.len() as i32;
            }

            StmtKind::Break => {
                let Some(loop_ctx) = self.loop_stack.last_mut() else {
                    panic!("break used outside of loop");
                };
                let jmp_pos = code.len();
                Self::emit(code, Some(span), OpCode::Jump, -1, 0);
                loop_ctx.break_jumps.push(jmp_pos);
            }

            StmtKind::Continue => {
                let Some(loop_ctx) = self.loop_stack.last() else {
                    panic!("continue used outside of loop");
                };
                Self::emit(code, Some(span), OpCode::Jump, loop_ctx.start as i32, 0);
            }

            StmtKind::FuncDef { .. } => {
                self.compile_func_def(stmt, code);
            }

            StmtKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.compile_expr(condition, code);

                let jif_pos = code.len();
                Self::emit(code, Some(span), OpCode::JumpIfFalse, -1, 0);

                for stmt in then_branch {
                    self.compile_stmt(stmt, code);
                }

                if let Some(else_branch) = else_branch {
                    let jmp_pos = code.len();
                    Self::emit(code, Some(span), OpCode::Jump, -1, 0);

                    code[jif_pos].a = code.len() as i32;

                    for stmt in else_branch {
                        self.compile_stmt(stmt, code);
                    }

                    code[jmp_pos].a = code.len() as i32;
                } else {
                    code[jif_pos].a = code.len() as i32;
                }
            }

            // 🔹 NUEVO: SOPORTE PARA WHILE
            StmtKind::While { condition, body } => {
                let loop_start = code.len();
                self.loop_stack.push(LoopContext {
                    start: loop_start,
                    break_jumps: Vec::new(),
                });

                self.compile_expr(condition, code);

                let exit_jmp = code.len();
                Self::emit(code, Some(span), OpCode::JumpIfFalse, -1, 0);

                for stmt in body {
                    self.compile_stmt(stmt, code);
                }

                Self::emit(code, Some(span), OpCode::Jump, loop_start as i32, 0);

                let loop_end = code.len();
                code[exit_jmp].a = loop_end as i32;

                if let Some(loop_ctx) = self.loop_stack.pop() {
                    for jmp_pos in loop_ctx.break_jumps {
                        code[jmp_pos].a = loop_end as i32;
                    }
                }
            }
        }
    }

    fn compile_expr(&mut self, expr: &Expr, code: &mut Vec<Instruction>) {
        let span = expr.span;
        match &expr.kind {
            ExprKind::Number(n) => {
                let idx = self.add_const(ConstValue::Number(*n));
                Self::emit(code, Some(span), OpCode::PushConst, idx as i32, 0);
            }

            ExprKind::String(s) => {
                let idx = self.add_const(ConstValue::String(s.clone()));
                Self::emit(code, Some(span), OpCode::PushConst, idx as i32, 0);
            }

            ExprKind::Bool(b) => {
                let idx = self.add_const(ConstValue::Bool(*b));
                Self::emit(code, Some(span), OpCode::PushConst, idx as i32, 0);
            }

            ExprKind::Null => {
                let idx = self.add_const(ConstValue::Null);
                Self::emit(code, Some(span), OpCode::PushConst, idx as i32, 0);
            }

            ExprKind::ArrayLiteral(items) => {
                for item in items {
                    self.compile_expr(item, code);
                }
                Self::emit(code, Some(span), OpCode::BuildArray, items.len() as i32, 0);
            }

            ExprKind::MapLiteral(items) => {
                for (key, value) in items {
                    self.compile_expr(key, code);
                    self.compile_expr(value, code);
                }
                Self::emit(code, Some(span), OpCode::BuildMap, items.len() as i32, 0);
            }

            ExprKind::Var(name) => {
                if let Some(idx) = self.resolve_local(name) {
                    Self::emit(code, Some(span), OpCode::LoadLocal, idx as i32, 0);
                } else if let Some(idx) = self.resolve_upvalue(name) {
                    Self::emit(code, Some(span), OpCode::LoadUpvalue, idx as i32, 0);
                } else {
                    let idx = self.add_string_const(name);
                    Self::emit(code, Some(span), OpCode::LoadGlobal, idx as i32, 0);
                }
            }

            ExprKind::Unary { op, expr } => {
                self.compile_expr(expr, code);
                match op {
                    UnOp::Not => Self::emit(code, Some(span), OpCode::Not, 0, 0),
                }
            }

            ExprKind::Binary { left, op, right } => {
                self.compile_expr(left, code);
                self.compile_expr(right, code);

                let opcode = match op {
                    BinOp::Add => OpCode::Add,
                    BinOp::Mul => OpCode::Mul,

                    BinOp::Greater => OpCode::Greater,
                    BinOp::GreaterEq => OpCode::GreaterEq,
                    BinOp::Less => OpCode::Less,
                    BinOp::LessEq => OpCode::LessEq,
                    BinOp::Equal => OpCode::Equal,
                    BinOp::NotEqual => OpCode::NotEqual,

                    BinOp::And => OpCode::And,
                    BinOp::Or => OpCode::Or,
                };

                Self::emit(code, Some(span), opcode, 0, 0);
            }

            ExprKind::Index { target, index } => {
                self.compile_expr(target, code);
                self.compile_expr(index, code);
                Self::emit(code, Some(span), OpCode::Index, 0, 0);
            }

            ExprKind::Call { name, args } => {
                let callee = Expr {
                    kind: ExprKind::Var(name.clone()),
                    span: expr.span,
                };
                self.compile_call(&callee, args, code);
            }

            ExprKind::CallExpr { callee, args } => {
                self.compile_call(callee, args, code);
            }

            ExprKind::Pipe { left, right } => match &right.kind {
                ExprKind::CallExpr { callee, args } => {
                    let mut new_args = Vec::with_capacity(args.len() + 1);
                    new_args.push(left.as_ref().clone());
                    new_args.extend_from_slice(args);
                    self.compile_call(callee, &new_args, code);
                }
                ExprKind::Call { name, args } => {
                    let mut new_args = Vec::with_capacity(args.len() + 1);
                    new_args.push(left.as_ref().clone());
                    new_args.extend_from_slice(args);
                    let callee = Expr {
                        kind: ExprKind::Var(name.clone()),
                        span: right.span,
                    };
                    self.compile_call(&callee, &new_args, code);
                }
                _ => {
                    let args = vec![left.as_ref().clone()];
                    self.compile_call(right, &args, code);
                }
            },
        }
    }

    fn compile_call(&mut self, callee: &Expr, args: &[Expr], code: &mut Vec<Instruction>) {
        if let ExprKind::Var(name) = &callee.kind {
            if is_builtin(name) {
                for arg in args {
                    self.compile_expr(arg, code);
                }
                let idx = self.add_string_const(name);
                Self::emit(
                    code,
                    Some(callee.span),
                    OpCode::CallBuiltin,
                    idx as i32,
                    args.len() as i32,
                );
                return;
            }
        }

        self.compile_expr(callee, code);
        for arg in args {
            self.compile_expr(arg, code);
        }
        Self::emit(
            code,
            Some(callee.span),
            OpCode::CallFunction,
            args.len() as i32,
            0,
        );
    }

    fn compile_func_def(&mut self, stmt: &Stmt, code: &mut Vec<Instruction>) {
        let StmtKind::FuncDef { name, params, body } = &stmt.kind else {
            return;
        };
        let span = stmt.span;

        let upvalue_names = self.collect_upvalues(params, body);
        let mut func_compiler = Compiler::new_function(params, upvalue_names.clone());
        let mut func_code: Vec<Instruction> = Vec::new();

        for stmt in body {
            func_compiler.compile_stmt(stmt, &mut func_code);
        }

        func_code.push(Instruction {
            op: OpCode::PushConst,
            a: func_compiler.add_const(ConstValue::Null) as i32,
            b: 0,
            span: Some(span),
        });
        func_code.push(Instruction {
            op: OpCode::Return,
            a: 0,
            b: 0,
            span: Some(span),
        });

        let func_def = FunctionDef {
            name: name.clone(),
            arity: params.len(),
            local_count: func_compiler.local_count(),
            chunk: ChunkDef {
                constants: func_compiler.constants,
                code: func_code,
            },
        };

        let const_idx = self.add_const(ConstValue::Function(func_def));
        for upvalue in &upvalue_names {
            self.load_name(upvalue, code, Some(span));
        }
        Self::emit(
            code,
            Some(span),
            OpCode::MakeClosure,
            const_idx as i32,
            upvalue_names.len() as i32,
        );
        self.store_name(name, code, Some(span));
    }

    fn resolve_local(&self, name: &str) -> Option<usize> {
        self.locals
            .as_ref()
            .and_then(|locals| locals.map.get(name).copied())
    }

    fn resolve_upvalue(&self, name: &str) -> Option<usize> {
        self.upvalues
            .as_ref()
            .and_then(|upvalues| upvalues.map.get(name).copied())
    }

    fn define_local(&mut self, name: &str) -> usize {
        let locals = self.locals.as_mut().expect("No locals in this context");
        if let Some(&idx) = locals.map.get(name) {
            return idx;
        }
        let idx = locals.count;
        locals.count += 1;
        locals.map.insert(name.to_string(), idx);
        idx
    }

    fn local_count(&self) -> usize {
        self.locals.as_ref().map(|l| l.count).unwrap_or(0)
    }

    fn store_name(&mut self, name: &str, code: &mut Vec<Instruction>, span: Option<Span>) {
        if self.locals.is_some() {
            let idx = self.define_local(name);
            code.push(Instruction {
                op: OpCode::StoreLocal,
                a: idx as i32,
                b: 0,
                span,
            });
        } else {
            let idx = self.add_string_const(name);
            code.push(Instruction {
                op: OpCode::StoreGlobal,
                a: idx as i32,
                b: 0,
                span,
            });
        }
    }

    fn load_name(&mut self, name: &str, code: &mut Vec<Instruction>, span: Option<Span>) {
        if let Some(idx) = self.resolve_local(name) {
            code.push(Instruction {
                op: OpCode::LoadLocal,
                a: idx as i32,
                b: 0,
                span,
            });
        } else if let Some(idx) = self.resolve_upvalue(name) {
            code.push(Instruction {
                op: OpCode::LoadUpvalue,
                a: idx as i32,
                b: 0,
                span,
            });
        } else {
            let idx = self.add_string_const(name);
            code.push(Instruction {
                op: OpCode::LoadGlobal,
                a: idx as i32,
                b: 0,
                span,
            });
        }
    }

    fn collect_upvalues(&self, params: &[String], body: &[Stmt]) -> Vec<String> {
        let info = collect_vars_in_stmts(body);
        let mut locals = HashMap::new();
        for p in params {
            locals.insert(p.clone(), ());
        }
        for name in info.assigned.keys() {
            locals.insert(name.clone(), ());
        }

        let mut upvalues = Vec::new();
        for name in info.used {
            if locals.contains_key(&name) {
                continue;
            }
            if is_builtin(&name) {
                continue;
            }
            if self.resolve_local(&name).is_some() || self.resolve_upvalue(&name).is_some() {
                upvalues.push(name);
            }
        }
        upvalues
    }

    fn add_string_const(&mut self, s: &str) -> usize {
        if let Some(&idx) = self.str_consts.get(s) {
            return idx;
        }
        let idx = self.constants.len();
        self.constants.push(ConstValue::String(s.to_string()));
        self.str_consts.insert(s.to_string(), idx);
        idx
    }

    fn add_const(&mut self, c: ConstValue) -> usize {
        let idx = self.constants.len();
        self.constants.push(c);
        idx
    }
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

struct VarInfo {
    used: Vec<String>,
    used_set: HashMap<String, ()>,
    assigned: HashMap<String, ()>,
}

fn collect_vars_in_stmts(stmts: &[Stmt]) -> VarInfo {
    let mut info = VarInfo {
        used: Vec::new(),
        used_set: HashMap::new(),
        assigned: HashMap::new(),
    };

    for stmt in stmts {
        collect_vars_in_stmt(stmt, &mut info);
    }

    info
}

fn collect_vars_in_stmt(stmt: &Stmt, info: &mut VarInfo) {
    match &stmt.kind {
        StmtKind::ExprStmt(expr) => collect_vars_in_expr(expr, info),
        StmtKind::Assign { name, value } => {
            info.assigned.insert(name.clone(), ());
            collect_vars_in_expr(value, info);
        }
        StmtKind::IndexAssign {
            target,
            index,
            value,
        } => {
            collect_vars_in_expr(target, info);
            collect_vars_in_expr(index, info);
            collect_vars_in_expr(value, info);
        }
        StmtKind::Import(expr) => collect_vars_in_expr(expr, info),
        StmtKind::ImportModule { name: _, alias } => {
            info.assigned.insert(alias.clone(), ());
        }
        StmtKind::Export { name } => {
            add_used(name, info);
        }
        StmtKind::TryCatch {
            try_body,
            catch_name,
            catch_body,
        } => {
            info.assigned.insert(catch_name.clone(), ());
            for stmt in try_body {
                collect_vars_in_stmt(stmt, info);
            }
            for stmt in catch_body {
                collect_vars_in_stmt(stmt, info);
            }
        }
        StmtKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_vars_in_expr(condition, info);
            for stmt in then_branch {
                collect_vars_in_stmt(stmt, info);
            }
            if let Some(else_branch) = else_branch {
                for stmt in else_branch {
                    collect_vars_in_stmt(stmt, info);
                }
            }
        }
        StmtKind::While { condition, body } => {
            collect_vars_in_expr(condition, info);
            for stmt in body {
                collect_vars_in_stmt(stmt, info);
            }
        }
        StmtKind::FuncDef { name, .. } => {
            info.assigned.insert(name.clone(), ());
        }
        StmtKind::Return(expr) => collect_vars_in_expr(expr, info),
        StmtKind::Break | StmtKind::Continue => {}
    }
}

fn collect_vars_in_expr(expr: &Expr, info: &mut VarInfo) {
    match &expr.kind {
        ExprKind::Var(name) => add_used(name, info),
        ExprKind::Binary { left, right, .. } => {
            collect_vars_in_expr(left, info);
            collect_vars_in_expr(right, info);
        }
        ExprKind::Unary { expr, .. } => collect_vars_in_expr(expr, info),
        ExprKind::Call { name, args } => {
            add_used(name, info);
            for arg in args {
                collect_vars_in_expr(arg, info);
            }
        }
        ExprKind::CallExpr { callee, args } => {
            collect_vars_in_expr(callee, info);
            for arg in args {
                collect_vars_in_expr(arg, info);
            }
        }
        ExprKind::ArrayLiteral(items) => {
            for item in items {
                collect_vars_in_expr(item, info);
            }
        }
        ExprKind::MapLiteral(items) => {
            for (key, value) in items {
                collect_vars_in_expr(key, info);
                collect_vars_in_expr(value, info);
            }
        }
        ExprKind::Index { target, index } => {
            collect_vars_in_expr(target, info);
            collect_vars_in_expr(index, info);
        }
        ExprKind::Pipe { left, right } => {
            collect_vars_in_expr(left, info);
            collect_vars_in_expr(right, info);
        }
        ExprKind::Number(_) | ExprKind::String(_) | ExprKind::Bool(_) | ExprKind::Null => {}
    }
}

fn add_used(name: &str, info: &mut VarInfo) {
    if !info.used_set.contains_key(name) {
        info.used.push(name.to_string());
        info.used_set.insert(name.to_string(), ());
    }
}
