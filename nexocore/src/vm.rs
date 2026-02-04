// src/vm.rs
use std::{
    collections::HashMap,
    io::{self, Write},
    path::{Path, PathBuf},
    process::Command,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use crate::ast::{Span, Stmt};
use crate::{
    bytecode::read_program,
    compiler::Compiler,
    lexer::Lexer,
    model::{ClosureDef, ConstValue, FunctionDef, MapKey, Program},
    opcode::OpCode,
    parser::Parser,
};

use glob::glob;
use libloading::{Library, Symbol};
use regex::Regex;
use serde_json;

pub struct Vm {
    program: Program,
    stack: Vec<ConstValue>,
    globals: HashMap<String, ConstValue>,
    frames: Vec<CallFrame>,
    try_stack: Vec<TryFrame>,
    module_cache: HashMap<String, ConstValue>,
    module_stack: Vec<ModuleContext>,
    module_paths: Vec<PathBuf>,
    ffi_libs: HashMap<usize, Library>,
    next_ffi_id: usize,
    last_span: Option<Span>,
    safe_mode: bool,
}

struct CallFrame {
    function: FunctionDef,
    ip: usize,
    locals: Vec<ConstValue>,
    upvalues: Vec<ConstValue>,
}

struct TryFrame {
    frame_depth: usize,
    catch_ip: usize,
    stack_len: usize,
}

struct ModuleContext {
    exports: HashMap<MapKey, ConstValue>,
}

impl CallFrame {
    fn new(function: FunctionDef, upvalues: Vec<ConstValue>) -> Self {
        let locals = vec![ConstValue::Null; function.local_count];
        Self {
            function,
            ip: 0,
            locals,
            upvalues,
        }
    }
}

impl Vm {
    pub fn new(program: Program, source_path: &str, safe_mode: bool) -> Self {
        Self {
            program,
            stack: Vec::new(),
            globals: HashMap::new(),
            frames: Vec::new(),
            try_stack: Vec::new(),
            module_cache: HashMap::new(),
            module_stack: Vec::new(),
            module_paths: vec![PathBuf::from(source_path)],
            ffi_libs: HashMap::new(),
            next_ffi_id: 1,
            last_span: None,
            safe_mode,
        }
    }

    pub fn run(&mut self) -> Result<(), String> {
        let entry = self.program.entry.clone();
        self.push_program(entry);

        while !self.frames.is_empty() {
            let step_result = self.execute_step();
            if let Err(err) = step_result {
                if self.handle_error(err.clone())? {
                    continue;
                }
                return Err(self.format_error(&err));
            }
        }

        Ok(())
    }

    pub fn run_program(&mut self, program: Program, source_path: &str) -> Result<(), String> {
        self.program = program;
        self.stack.clear();
        self.frames.clear();
        self.try_stack.clear();
        self.module_paths = vec![PathBuf::from(source_path)];
        self.last_span = None;

        let entry = self.program.entry.clone();
        self.push_program(entry);

        while !self.frames.is_empty() {
            let step_result = self.execute_step();
            if let Err(err) = step_result {
                if self.handle_error(err.clone())? {
                    continue;
                }
                return Err(self.format_error(&err));
            }
        }

        Ok(())
    }

    fn execute_step(&mut self) -> Result<(), String> {
        let frame_index = self.frames.len() - 1;
        let instr = {
            let frame = &mut self.frames[frame_index];
            if frame.ip >= frame.function.chunk.code.len() {
                return Err("Instruction pointer out of range".into());
            }
            let instr = frame.function.chunk.code[frame.ip].clone();
            frame.ip += 1;
            instr
        };

        let constants = self.frames[frame_index].function.chunk.constants.clone();

        self.last_span = instr.span;

        let step_result: Result<(), String> = (|| {
            match instr.op {
                OpCode::PushConst => {
                    let idx = instr.a as usize;
                    let c = constants
                        .get(idx)
                        .ok_or_else(|| format!("Invalid const index {}", idx))?
                        .clone();
                    self.stack.push(c);
                }

                OpCode::Pop => {
                    self.stack.pop();
                }

                OpCode::Add => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;

                    let out = match (a, b) {
                        (ConstValue::Number(x), ConstValue::Number(y)) => ConstValue::Number(x + y),
                        (ConstValue::String(x), ConstValue::String(y)) => {
                            ConstValue::String(x + &y)
                        }
                        (ConstValue::String(x), v) => ConstValue::String(x + &to_string(&v)),
                        (v, ConstValue::String(y)) => ConstValue::String(to_string(&v) + &y),
                        _ => {
                            return Err(
                                "Add only supports number+number or string concatenation".into()
                            )
                        }
                    };

                    self.stack.push(out);
                }

                OpCode::Mul => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;

                    let out = match (a, b) {
                        (ConstValue::Number(x), ConstValue::Number(y)) => ConstValue::Number(x * y),
                        _ => return Err("Mul only supports number * number".into()),
                    };

                    self.stack.push(out);
                }

                OpCode::Greater => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;

                    match (a, b) {
                        (ConstValue::Number(x), ConstValue::Number(y)) => {
                            self.stack.push(ConstValue::Bool(x > y));
                        }
                        _ => return Err("Greater only supports number > number".into()),
                    }
                }

                OpCode::And => {
                    let b = as_bool(self.stack.pop().ok_or("Stack underflow")?)?;
                    let a = as_bool(self.stack.pop().ok_or("Stack underflow")?)?;
                    self.stack.push(ConstValue::Bool(a && b));
                }

                OpCode::Or => {
                    let b = as_bool(self.stack.pop().ok_or("Stack underflow")?)?;
                    let a = as_bool(self.stack.pop().ok_or("Stack underflow")?)?;
                    self.stack.push(ConstValue::Bool(a || b));
                }

                OpCode::Not => {
                    let a = as_bool(self.stack.pop().ok_or("Stack underflow")?)?;
                    self.stack.push(ConstValue::Bool(!a));
                }

                OpCode::GreaterEq => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    match (a, b) {
                        (ConstValue::Number(x), ConstValue::Number(y)) => {
                            self.stack.push(ConstValue::Bool(x >= y));
                        }
                        _ => return Err("GreaterEq only supports number >= number".into()),
                    }
                }

                OpCode::Less => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    match (a, b) {
                        (ConstValue::Number(x), ConstValue::Number(y)) => {
                            self.stack.push(ConstValue::Bool(x < y));
                        }
                        _ => return Err("Less only supports number < number".into()),
                    }
                }

                OpCode::LessEq => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    match (a, b) {
                        (ConstValue::Number(x), ConstValue::Number(y)) => {
                            self.stack.push(ConstValue::Bool(x <= y));
                        }
                        _ => return Err("LessEq only supports number <= number".into()),
                    }
                }

                OpCode::Equal => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    self.stack.push(ConstValue::Bool(values_equal(&a, &b)));
                }

                OpCode::NotEqual => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    self.stack.push(ConstValue::Bool(!values_equal(&a, &b)));
                }

                OpCode::LoadGlobal => {
                    let name = const_string(&constants, instr.a)?;
                    if self.safe_mode && is_unsafe_builtin(&name) {
                        return Err(safe_block_message(&name));
                    }
                    let v = self.globals.get(&name).cloned().unwrap_or(ConstValue::Null);
                    self.stack.push(v);
                }

                OpCode::StoreGlobal => {
                    let name = const_string(&constants, instr.a)?;
                    let v = self.stack.pop().ok_or("Stack underflow")?;
                    self.globals.insert(name, v);
                }

                OpCode::LoadLocal => {
                    let idx = instr.a as usize;
                    let frame = &self.frames[frame_index];
                    if idx >= frame.locals.len() {
                        return Err("Invalid local index".into());
                    }
                    self.stack.push(frame.locals[idx].clone());
                }

                OpCode::StoreLocal => {
                    let idx = instr.a as usize;
                    let frame = &mut self.frames[frame_index];
                    if idx >= frame.locals.len() {
                        return Err("Invalid local index".into());
                    }
                    let v = self.stack.pop().ok_or("Stack underflow")?;
                    frame.locals[idx] = v;
                }

                OpCode::LoadUpvalue => {
                    let idx = instr.a as usize;
                    let frame = &self.frames[frame_index];
                    if idx >= frame.upvalues.len() {
                        return Err("Invalid upvalue index".into());
                    }
                    self.stack.push(frame.upvalues[idx].clone());
                }

                OpCode::JumpIfFalse => {
                    let cond = as_bool(self.stack.pop().ok_or("Stack underflow")?)?;
                    if !cond {
                        let target = instr.a;
                        if target < 0 {
                            return Err("Invalid jump target".into());
                        }
                        self.frames[frame_index].ip = target as usize;
                    }
                }

                OpCode::Jump => {
                    let target = instr.a;
                    if target < 0 {
                        return Err("Invalid jump target".into());
                    }
                    self.frames[frame_index].ip = target as usize;
                }

                OpCode::CallBuiltin => {
                    let name = const_string(&constants, instr.a)?;
                    let argc = instr.b as usize;

                    if self.safe_mode && is_unsafe_builtin(&name) {
                        return Err(safe_block_message(&name));
                    }

                    match name.as_str() {
                        "print" => {
                            let mut args = Vec::with_capacity(argc);
                            for _ in 0..argc {
                                args.push(self.stack.pop().ok_or("Stack underflow")?);
                            }
                            args.reverse();

                            for a in args {
                                print!("{}", to_string(&a));
                            }
                            println!();
                        }

                        "len" => {
                            if argc != 1 {
                                return Err("len() expects exactly one argument".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let result = match value {
                                ConstValue::String(s) => ConstValue::Number(s.len() as f64),
                                ConstValue::Array(items) => ConstValue::Number(items.len() as f64),
                                ConstValue::Map(items) => ConstValue::Number(items.len() as f64),
                                _ => return Err("len() expects a string, array, or map".into()),
                            };
                            self.stack.push(result);
                        }

                        "str" => {
                            if argc != 1 {
                                return Err("str() expects exactly one argument".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            self.stack.push(ConstValue::String(to_string(&value)));
                        }

                        "int" => {
                            if argc != 1 {
                                return Err("int() expects exactly one argument".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let n = to_number(value)?;
                            self.stack.push(ConstValue::Number(n.trunc()));
                        }

                        "round" => {
                            if argc != 1 {
                                return Err("round() expects exactly one argument".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let n = to_number(value)?;
                            self.stack.push(ConstValue::Number(n.round()));
                        }

                        "floor" => {
                            if argc != 1 {
                                return Err("floor() expects exactly one argument".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let n = to_number(value)?;
                            self.stack.push(ConstValue::Number(n.floor()));
                        }

                        "ceil" => {
                            if argc != 1 {
                                return Err("ceil() expects exactly one argument".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let n = to_number(value)?;
                            self.stack.push(ConstValue::Number(n.ceil()));
                        }

                        "input" => {
                            if argc != 0 {
                                return Err("input() takes no arguments".into());
                            }
                            io::stdout().flush().unwrap();
                            let mut line = String::new();
                            io::stdin()
                                .read_line(&mut line)
                                .map_err(|_| "Failed to read input".to_string())?;
                            let line = line.trim_end().to_string();
                            self.stack.push(ConstValue::String(line));
                        }

                        "append" => {
                            if argc != 2 {
                                return Err("append() expects exactly two arguments".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let array = self.stack.pop().ok_or("Stack underflow")?;
                            match array {
                                ConstValue::Array(mut items) => {
                                    items.push(value);
                                    self.stack.push(ConstValue::Array(items));
                                }
                                _ => return Err("append() expects an array".into()),
                            }
                        }

                        "log_info" => {
                            let value = log_value(argc, &mut self.stack, "info")?;
                            self.stack.push(value);
                        }

                        "log_warn" => {
                            let value = log_value(argc, &mut self.stack, "warn")?;
                            self.stack.push(value);
                        }

                        "log_error" => {
                            let value = log_value(argc, &mut self.stack, "error")?;
                            self.stack.push(value);
                        }

                        "fs_read" => {
                            if argc != 1 {
                                return Err("fs_read() expects exactly one argument".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let path = match value {
                                ConstValue::String(s) => s,
                                _ => return Err("fs_read() expects a string path".into()),
                            };
                            let data = std::fs::read_to_string(&path)
                                .map_err(|e| format!("fs_read error: {}", e))?;
                            self.stack.push(ConstValue::String(data));
                        }

                        "fs_write" => {
                            if argc != 2 {
                                return Err("fs_write() expects path and data".into());
                            }
                            let data = self.stack.pop().ok_or("Stack underflow")?;
                            let path = self.stack.pop().ok_or("Stack underflow")?;
                            let path = match path {
                                ConstValue::String(s) => s,
                                _ => return Err("fs_write() expects a string path".into()),
                            };
                            let data = match data {
                                ConstValue::String(s) => s,
                                _ => return Err("fs_write() expects string data".into()),
                            };
                            std::fs::write(&path, data)
                                .map_err(|e| format!("fs_write error: {}", e))?;
                            self.stack.push(ConstValue::Null);
                        }

                        "fs_exists" => {
                            if argc != 1 {
                                return Err("fs_exists() expects exactly one argument".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let path = match value {
                                ConstValue::String(s) => s,
                                _ => return Err("fs_exists() expects a string path".into()),
                            };
                            self.stack.push(ConstValue::Bool(Path::new(&path).exists()));
                        }

                        "fs_list" => {
                            if argc != 1 {
                                return Err("fs_list() expects exactly one argument".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let path = match value {
                                ConstValue::String(s) => s,
                                _ => return Err("fs_list() expects a string path".into()),
                            };
                            let mut items = Vec::new();
                            let entries = std::fs::read_dir(&path)
                                .map_err(|e| format!("fs_list error: {}", e))?;
                            for entry in entries {
                                let entry = entry.map_err(|e| e.to_string())?;
                                if let Some(name) = entry.file_name().to_str() {
                                    items.push(ConstValue::String(name.to_string()));
                                }
                            }
                            self.stack.push(ConstValue::Array(items));
                        }

                        "proc_run" => {
                            if argc != 1 {
                                return Err("proc_run() expects exactly one argument".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let cmd = match value {
                                ConstValue::String(s) => s,
                                _ => return Err("proc_run() expects a string command".into()),
                            };

                            let output = if cfg!(windows) {
                                Command::new("cmd").args(["/C", &cmd]).output()
                            } else {
                                Command::new("sh").args(["-c", &cmd]).output()
                            }
                            .map_err(|e| format!("proc_run error: {}", e))?;

                            let mut map = HashMap::new();
                            map.insert(
                                MapKey::String("code".to_string()),
                                ConstValue::Number(output.status.code().unwrap_or(-1) as f64),
                            );
                            map.insert(
                                MapKey::String("out".to_string()),
                                ConstValue::String(
                                    String::from_utf8_lossy(&output.stdout).to_string(),
                                ),
                            );
                            map.insert(
                                MapKey::String("err".to_string()),
                                ConstValue::String(
                                    String::from_utf8_lossy(&output.stderr).to_string(),
                                ),
                            );
                            self.stack.push(ConstValue::Map(map));
                        }

                        "env_get" => {
                            if argc != 1 {
                                return Err("env_get() expects exactly one argument".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let key = match value {
                                ConstValue::String(s) => s,
                                _ => return Err("env_get() expects a string key".into()),
                            };
                            let out = std::env::var(&key).ok();
                            match out {
                                Some(v) => self.stack.push(ConstValue::String(v)),
                                None => self.stack.push(ConstValue::Null),
                            }
                        }

                        "args" => {
                            if argc != 0 {
                                return Err("args() expects no arguments".into());
                            }
                            let items = collect_cli_args()
                                .into_iter()
                                .map(ConstValue::String)
                                .collect();
                            self.stack.push(ConstValue::Array(items));
                        }

                        "arg_has" => {
                            if argc != 1 {
                                return Err("arg_has() expects exactly one argument".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let flag = match value {
                                ConstValue::String(s) => s,
                                _ => return Err("arg_has() expects a string flag".into()),
                            };
                            let args = collect_cli_args();
                            self.stack
                                .push(ConstValue::Bool(args.iter().any(|a| a == &flag)));
                        }

                        "arg_get" => {
                            if argc != 1 && argc != 2 {
                                return Err("arg_get() expects flag and optional default".into());
                            }
                            let default = if argc == 2 {
                                Some(self.stack.pop().ok_or("Stack underflow")?)
                            } else {
                                None
                            };
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let flag = match value {
                                ConstValue::String(s) => s,
                                _ => return Err("arg_get() expects a string flag".into()),
                            };
                            let args = collect_cli_args();
                            let mut out = ConstValue::Null;
                            for (i, arg) in args.iter().enumerate() {
                                if arg == &flag {
                                    if let Some(next) = args.get(i + 1) {
                                        out = ConstValue::String(next.clone());
                                    }
                                    break;
                                }
                            }
                            if matches!(out, ConstValue::Null) {
                                if let Some(default) = default {
                                    out = default;
                                }
                            }
                            self.stack.push(out);
                        }

                        "time_sleep" => {
                            if argc != 1 {
                                return Err("time_sleep() expects exactly one argument".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let ms = match value {
                                ConstValue::Number(n) => n,
                                _ => return Err("time_sleep() expects a number".into()),
                            };
                            if ms < 0.0 {
                                return Err("time_sleep() expects non-negative ms".into());
                            }
                            std::thread::sleep(Duration::from_millis(ms as u64));
                            self.stack.push(ConstValue::Null);
                        }

                        "time_now" => {
                            if argc != 0 {
                                return Err("time_now() expects no arguments".into());
                            }
                            let now = SystemTime::now()
                                .duration_since(UNIX_EPOCH)
                                .map_err(|e| e.to_string())?;
                            self.stack.push(ConstValue::Number(now.as_millis() as f64));
                        }

                        "ffi_load" => {
                            if argc != 1 {
                                return Err("ffi_load() expects exactly one argument".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let path = match value {
                                ConstValue::String(s) => s,
                                _ => return Err("ffi_load() expects a string path".into()),
                            };
                            let lib = unsafe { Library::new(&path) }
                                .map_err(|e| format!("ffi_load error: {}", e))?;
                            let id = self.next_ffi_id;
                            self.next_ffi_id += 1;
                            self.ffi_libs.insert(id, lib);
                            self.stack.push(ConstValue::FfiHandle(id));
                        }

                        "ffi_call" => {
                            if argc != 3 {
                                return Err("ffi_call() expects handle, name, args array".into());
                            }
                            let args_val = self.stack.pop().ok_or("Stack underflow")?;
                            let name_val = self.stack.pop().ok_or("Stack underflow")?;
                            let handle_val = self.stack.pop().ok_or("Stack underflow")?;

                            let handle = match handle_val {
                                ConstValue::FfiHandle(id) => id,
                                _ => return Err("ffi_call() expects a ffi handle".into()),
                            };
                            let name = match name_val {
                                ConstValue::String(s) => s,
                                _ => return Err("ffi_call() expects a symbol name".into()),
                            };
                            let args = match args_val {
                                ConstValue::Array(items) => items,
                                _ => return Err("ffi_call() expects args array".into()),
                            };

                            let mut nums = Vec::with_capacity(args.len());
                            for a in args {
                                match a {
                                    ConstValue::Number(n) => nums.push(n),
                                    _ => return Err("ffi_call() args must be numbers".into()),
                                }
                            }

                            let lib = self
                                .ffi_libs
                                .get(&handle)
                                .ok_or("ffi_call() invalid handle")?;

                            let result = unsafe { ffi_call_f64(lib, &name, &nums) }?;
                            self.stack.push(ConstValue::Number(result));
                        }

                        "py_run" => {
                            if argc != 2 {
                                return Err("py_run() expects script and input".into());
                            }
                            let input_val = self.stack.pop().ok_or("Stack underflow")?;
                            let script_val = self.stack.pop().ok_or("Stack underflow")?;

                            let script = match script_val {
                                ConstValue::String(s) => s,
                                _ => return Err("py_run() expects a script path".into()),
                            };
                            let input = match input_val {
                                ConstValue::String(s) => s,
                                ConstValue::Null => String::new(),
                                _ => return Err("py_run() expects string input or null".into()),
                            };

                            let (stdout, stderr, code) = run_python(&script, &input)?;
                            if code != 0 {
                                let msg = if !stderr.trim().is_empty() {
                                    stderr
                                } else {
                                    stdout
                                };
                                return Err(format!("py_run error: {}", msg.trim()));
                            }

                            let out = stdout.trim();
                            if out.is_empty() {
                                return Err("py_run returned empty output".into());
                            }
                            let parsed: serde_json::Value = serde_json::from_str(out)
                                .map_err(|e| format!("py_run json error: {}", e))?;
                            let out = json_to_value(&parsed)?;
                            self.stack.push(out);
                        }

                        "http_get" => {
                            if argc != 1 {
                                return Err("http_get() expects url".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let url = match value {
                                ConstValue::String(s) => s,
                                _ => return Err("http_get() expects a string url".into()),
                            };
                            let result = http_request("GET", &url, None, None)?;
                            self.stack.push(result);
                        }

                        "http_post" => {
                            if argc != 3 {
                                return Err("http_post() expects url, body, headers".into());
                            }
                            let headers_val = self.stack.pop().ok_or("Stack underflow")?;
                            let body_val = self.stack.pop().ok_or("Stack underflow")?;
                            let url_val = self.stack.pop().ok_or("Stack underflow")?;

                            let url = match url_val {
                                ConstValue::String(s) => s,
                                _ => return Err("http_post() expects a string url".into()),
                            };
                            let body = match body_val {
                                ConstValue::String(s) => s,
                                ConstValue::Null => String::new(),
                                _ => return Err("http_post() expects a string body".into()),
                            };
                            let headers = match headers_val {
                                ConstValue::Map(m) => Some(m),
                                ConstValue::Null => None,
                                _ => return Err("http_post() expects headers map or null".into()),
                            };

                            let result = http_request("POST", &url, Some(&body), headers)?;
                            self.stack.push(result);
                        }

                        "re_match" => {
                            if argc != 2 {
                                return Err("re_match() expects pattern and text".into());
                            }
                            let text = self.stack.pop().ok_or("Stack underflow")?;
                            let pattern = self.stack.pop().ok_or("Stack underflow")?;
                            let pattern = match pattern {
                                ConstValue::String(s) => s,
                                _ => return Err("re_match() expects string pattern".into()),
                            };
                            let text = match text {
                                ConstValue::String(s) => s,
                                _ => return Err("re_match() expects string text".into()),
                            };
                            let re = Regex::new(&pattern).map_err(|e| e.to_string())?;
                            self.stack.push(ConstValue::Bool(re.is_match(&text)));
                        }

                        "re_findall" => {
                            if argc != 2 {
                                return Err("re_findall() expects pattern and text".into());
                            }
                            let text = self.stack.pop().ok_or("Stack underflow")?;
                            let pattern = self.stack.pop().ok_or("Stack underflow")?;
                            let pattern = match pattern {
                                ConstValue::String(s) => s,
                                _ => return Err("re_findall() expects string pattern".into()),
                            };
                            let text = match text {
                                ConstValue::String(s) => s,
                                _ => return Err("re_findall() expects string text".into()),
                            };
                            let re = Regex::new(&pattern).map_err(|e| e.to_string())?;
                            let mut items = Vec::new();
                            for m in re.find_iter(&text) {
                                items.push(ConstValue::String(m.as_str().to_string()));
                            }
                            self.stack.push(ConstValue::Array(items));
                        }

                        "re_replace" => {
                            if argc != 3 {
                                return Err("re_replace() expects pattern, text, repl".into());
                            }
                            let repl = self.stack.pop().ok_or("Stack underflow")?;
                            let text = self.stack.pop().ok_or("Stack underflow")?;
                            let pattern = self.stack.pop().ok_or("Stack underflow")?;
                            let pattern = match pattern {
                                ConstValue::String(s) => s,
                                _ => return Err("re_replace() expects string pattern".into()),
                            };
                            let text = match text {
                                ConstValue::String(s) => s,
                                _ => return Err("re_replace() expects string text".into()),
                            };
                            let repl = match repl {
                                ConstValue::String(s) => s,
                                _ => return Err("re_replace() expects string repl".into()),
                            };
                            let re = Regex::new(&pattern).map_err(|e| e.to_string())?;
                            let out = re.replace_all(&text, repl.as_str()).to_string();
                            self.stack.push(ConstValue::String(out));
                        }

                        "path_join" => {
                            if argc != 2 {
                                return Err("path_join() expects a and b".into());
                            }
                            let b = self.stack.pop().ok_or("Stack underflow")?;
                            let a = self.stack.pop().ok_or("Stack underflow")?;
                            let a = match a {
                                ConstValue::String(s) => s,
                                _ => return Err("path_join() expects string".into()),
                            };
                            let b = match b {
                                ConstValue::String(s) => s,
                                _ => return Err("path_join() expects string".into()),
                            };
                            let joined = Path::new(&a).join(&b).to_string_lossy().to_string();
                            self.stack.push(ConstValue::String(joined));
                        }

                        "path_dirname" => {
                            if argc != 1 {
                                return Err("path_dirname() expects a path".into());
                            }
                            let p = self.stack.pop().ok_or("Stack underflow")?;
                            let p = match p {
                                ConstValue::String(s) => s,
                                _ => return Err("path_dirname() expects string".into()),
                            };
                            let dir = Path::new(&p)
                                .parent()
                                .map(|p| p.to_string_lossy().to_string())
                                .unwrap_or_else(|| "".to_string());
                            self.stack.push(ConstValue::String(dir));
                        }

                        "glob" => {
                            if argc != 1 {
                                return Err("glob() expects a pattern".into());
                            }
                            let p = self.stack.pop().ok_or("Stack underflow")?;
                            let pattern = match p {
                                ConstValue::String(s) => s,
                                _ => return Err("glob() expects string pattern".into()),
                            };
                            let mut items = Vec::new();
                            for entry in glob(&pattern).map_err(|e| e.to_string())? {
                                if let Ok(path) = entry {
                                    items.push(ConstValue::String(
                                        path.to_string_lossy().to_string(),
                                    ));
                                }
                            }
                            items.sort_by(|a, b| match (a, b) {
                                (ConstValue::String(x), ConstValue::String(y)) => x.cmp(y),
                                _ => std::cmp::Ordering::Equal,
                            });
                            self.stack.push(ConstValue::Array(items));
                        }

                        "export" => {
                            if argc != 2 {
                                return Err("export() expects name and value".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let name = self.stack.pop().ok_or("Stack underflow")?;
                            let name = match name {
                                ConstValue::String(s) => s,
                                _ => return Err("export() expects a string name".into()),
                            };
                            let ctx = self
                                .module_stack
                                .last_mut()
                                .ok_or("export used outside module")?;
                            ctx.exports.insert(MapKey::String(name), value);
                            self.stack.push(ConstValue::Null);
                        }

                        "import" => {
                            if argc != 1 {
                                return Err("import() expects exactly one argument".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let path = match value {
                                ConstValue::String(s) => s,
                                _ => return Err("import() expects a string".into()),
                            };

                            let resolved = if Path::new(&path).is_absolute() {
                                PathBuf::from(&path)
                            } else {
                                let base = self
                                    .module_paths
                                    .last()
                                    .and_then(|p| p.parent())
                                    .map(|p| p.to_path_buf())
                                    .unwrap_or_else(|| PathBuf::from("."));
                                base.join(&path)
                            };

                            let resolved_str = resolved.to_string_lossy().to_string();
                            let depth = self.frames.len();
                            self.module_stack.push(ModuleContext {
                                exports: HashMap::new(),
                            });
                            self.module_paths.push(resolved);

                            if resolved_str.ends_with(".nxbc") {
                                let program = read_program(&resolved_str)
                                    .map_err(|e| format!("Failed to read bytecode: {}", e))?;
                                self.push_program(program.entry);
                            } else {
                                let src = std::fs::read_to_string(&resolved_str)
                                    .map_err(|e| format!("Failed to read file: {}", e))?;
                                let ast = parse_source(&src)?;
                                let mut compiler = Compiler::new();
                                let program = compiler.compile(ast);
                                self.push_program(program.entry);
                            }

                            self.run_until_depth(depth)?;
                            self.module_paths.pop();
                            self.module_stack.pop();

                            self.stack.push(ConstValue::Null);
                        }

                        "import_module" => {
                            if argc != 1 {
                                return Err("import_module() expects one argument".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let name = match value {
                                ConstValue::String(s) => s,
                                _ => return Err("import_module() expects a string".into()),
                            };

                            let resolved = self.resolve_module_path(&name)?;
                            let key = resolved.to_string_lossy().to_string();
                            if let Some(cached) = self.module_cache.get(&key).cloned() {
                                self.stack.push(cached);
                                return Ok(());
                            }

                            let depth = self.frames.len();
                            self.module_stack.push(ModuleContext {
                                exports: HashMap::new(),
                            });
                            self.module_paths.push(resolved.clone());

                            if key.ends_with(".nxbc") {
                                let program = read_program(&key)
                                    .map_err(|e| format!("Failed to read bytecode: {}", e))?;
                                self.push_program(program.entry);
                            } else {
                                let src = std::fs::read_to_string(&key)
                                    .map_err(|e| format!("Failed to read file: {}", e))?;
                                let ast = parse_source(&src)?;
                                let mut compiler = Compiler::new();
                                let program = compiler.compile(ast);
                                self.push_program(program.entry);
                            }

                            self.run_until_depth(depth)?;
                            self.module_paths.pop();

                            let ctx = self.module_stack.pop().ok_or("Missing module context")?;
                            let exports = ConstValue::Map(ctx.exports);
                            self.module_cache.insert(key, exports.clone());
                            self.stack.push(exports);
                        }

                        "json_parse" => {
                            if argc != 1 {
                                return Err("json_parse() expects exactly one argument".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let s = match value {
                                ConstValue::String(s) => s,
                                _ => return Err("json_parse() expects a string".into()),
                            };
                            let parsed: serde_json::Value = serde_json::from_str(&s)
                                .map_err(|e| format!("json_parse error: {}", e))?;
                            let out = json_to_value(&parsed)?;
                            self.stack.push(out);
                        }

                        "json_stringify" => {
                            if argc != 1 {
                                return Err("json_stringify() expects exactly one argument".into());
                            }
                            let value = self.stack.pop().ok_or("Stack underflow")?;
                            let json = value_to_json(&value)?;
                            let out = serde_json::to_string(&json)
                                .map_err(|e| format!("json_stringify error: {}", e))?;
                            self.stack.push(ConstValue::String(out));
                        }

                        _ => return Err(format!("Unknown builtin '{}'", name)),
                    }
                }

                OpCode::CallFunction => {
                    let argc = instr.a as usize;
                    let mut args = Vec::with_capacity(argc);
                    for _ in 0..argc {
                        args.push(self.stack.pop().ok_or("Stack underflow")?);
                    }
                    args.reverse();

                    let callee = self.stack.pop().ok_or("Stack underflow")?;
                    let (func, upvalues) = match callee {
                        ConstValue::Closure(c) => (c.function, c.upvalues),
                        ConstValue::Function(f) => (f, Vec::new()),
                        _ => return Err("Call expects a function".into()),
                    };

                    if func.arity != argc {
                        return Err(format!(
                            "Function '{}' expects {} args, got {}",
                            func.name, func.arity, argc
                        ));
                    }

                    let mut new_frame = CallFrame::new(func, upvalues);
                    for (i, arg) in args.into_iter().enumerate() {
                        if i < new_frame.locals.len() {
                            new_frame.locals[i] = arg;
                        }
                    }
                    self.frames.push(new_frame);
                }

                OpCode::MakeClosure => {
                    let const_idx = instr.a as usize;
                    let capture_count = instr.b as usize;
                    let func = match constants.get(const_idx) {
                        Some(ConstValue::Function(f)) => f.clone(),
                        _ => return Err("MakeClosure expects a function constant".into()),
                    };
                    let mut captures = Vec::with_capacity(capture_count);
                    for _ in 0..capture_count {
                        captures.push(self.stack.pop().ok_or("Stack underflow")?);
                    }
                    captures.reverse();
                    self.stack.push(ConstValue::Closure(ClosureDef {
                        function: func,
                        upvalues: captures,
                    }));
                }

                OpCode::BuildArray => {
                    let count = instr.a as usize;
                    let mut items = Vec::with_capacity(count);
                    for _ in 0..count {
                        items.push(self.stack.pop().ok_or("Stack underflow")?);
                    }
                    items.reverse();
                    self.stack.push(ConstValue::Array(items));
                }

                OpCode::BuildMap => {
                    let count = instr.a as usize;
                    let mut items = std::collections::HashMap::new();
                    for _ in 0..count {
                        let value = self.stack.pop().ok_or("Stack underflow")?;
                        let key = self.stack.pop().ok_or("Stack underflow")?;
                        let map_key = map_key_from_value(&key)?;
                        items.insert(map_key, value);
                    }
                    self.stack.push(ConstValue::Map(items));
                }

                OpCode::Index => {
                    let index = self.stack.pop().ok_or("Stack underflow")?;
                    let target = self.stack.pop().ok_or("Stack underflow")?;

                    match target {
                        ConstValue::Array(items) => {
                            let idx = index_to_usize(&index)?;
                            if idx >= items.len() {
                                return Err("Index out of bounds".into());
                            }
                            self.stack.push(items[idx].clone());
                        }
                        ConstValue::Map(items) => {
                            let key = map_key_from_value(&index)?;
                            let value = items.get(&key).cloned().unwrap_or(ConstValue::Null);
                            self.stack.push(value);
                        }
                        _ => return Err("Indexing only supports arrays or maps".into()),
                    }
                }

                OpCode::SetIndex => {
                    let value = self.stack.pop().ok_or("Stack underflow")?;
                    let index = self.stack.pop().ok_or("Stack underflow")?;
                    let target = self.stack.pop().ok_or("Stack underflow")?;

                    match target {
                        ConstValue::Array(mut items) => {
                            let idx = index_to_usize(&index)?;
                            if idx >= items.len() {
                                return Err("Index out of bounds".into());
                            }
                            items[idx] = value;
                            self.stack.push(ConstValue::Array(items));
                        }
                        ConstValue::Map(mut items) => {
                            let key = map_key_from_value(&index)?;
                            items.insert(key, value);
                            self.stack.push(ConstValue::Map(items));
                        }
                        _ => return Err("Indexing only supports arrays or maps".into()),
                    }
                }

                OpCode::TryStart => {
                    if instr.a < 0 {
                        return Err("Invalid try target".into());
                    }
                    self.try_stack.push(TryFrame {
                        frame_depth: frame_index,
                        catch_ip: instr.a as usize,
                        stack_len: self.stack.len(),
                    });
                }

                OpCode::TryEnd => {
                    self.try_stack.pop();
                }

                OpCode::Return => {
                    let ret = self.stack.pop().unwrap_or(ConstValue::Null);
                    self.frames.pop();
                    if self.frames.is_empty() {
                        return Ok(());
                    }
                    self.stack.push(ret);
                }
            }

            Ok(())
        })();

        step_result
    }

    fn push_program(&mut self, entry: FunctionDef) {
        self.frames.push(CallFrame::new(entry, Vec::new()));
    }

    fn run_until_depth(&mut self, depth: usize) -> Result<(), String> {
        while self.frames.len() > depth {
            let step_result = self.execute_step();
            if let Err(err) = step_result {
                if self.handle_error(err.clone())? {
                    continue;
                }
                return Err(self.format_error(&err));
            }
        }
        Ok(())
    }

    fn resolve_module_path(&self, name: &str) -> Result<PathBuf, String> {
        let base_dir = self
            .module_paths
            .last()
            .and_then(|p| p.parent())
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| PathBuf::from("."));

        let parent_dir = base_dir.parent().map(|p| p.to_path_buf());

        let mut candidates = vec![
            base_dir.join(format!("{}.nxbc", name)),
            base_dir.join(format!("{}.nx", name)),
            base_dir.join("nexo_modules").join(format!("{}.nxbc", name)),
            base_dir.join("nexo_modules").join(format!("{}.nx", name)),
        ];

        if let Some(parent) = parent_dir {
            candidates.push(parent.join("nexo_modules").join(format!("{}.nxbc", name)));
            candidates.push(parent.join("nexo_modules").join(format!("{}.nx", name)));
        }

        for path in candidates {
            if path.exists() {
                return Ok(path);
            }
        }

        let mut extra_dirs = Vec::new();
        if let Ok(path) = std::env::var("NEXO_MODULES") {
            extra_dirs.push(PathBuf::from(path));
        }
        if let Ok(home) = std::env::var("NEXO_HOME") {
            extra_dirs.push(PathBuf::from(home).join("nexo_modules"));
        }
        if let Ok(exe) = std::env::current_exe() {
            if let Some(dir) = exe.parent() {
                extra_dirs.push(dir.join("nexo_modules"));
            }
        }

        for dir in extra_dirs {
            let nxbc = dir.join(format!("{}.nxbc", name));
            if nxbc.exists() {
                return Ok(nxbc);
            }
            let nx = dir.join(format!("{}.nx", name));
            if nx.exists() {
                return Ok(nx);
            }
        }

        Err(format!("Module '{}' not found", name))
    }

    fn handle_error(&mut self, err: String) -> Result<bool, String> {
        let Some(frame) = self.try_stack.pop() else {
            return Ok(false);
        };

        if frame.frame_depth >= self.frames.len() {
            return Err(err);
        }

        while self.frames.len() - 1 > frame.frame_depth {
            self.frames.pop();
        }

        let frame_ref = self.frames.last_mut().ok_or(err.clone())?;
        frame_ref.ip = frame.catch_ip;

        if self.stack.len() > frame.stack_len {
            self.stack.truncate(frame.stack_len);
        }

        self.stack.push(ConstValue::String(err));
        Ok(true)
    }

    fn format_error(&self, err: &str) -> String {
        let mut out = String::new();
        out.push_str("Runtime error: ");
        out.push_str(err);

        if let Some(path) = self.module_paths.last() {
            out.push_str("\nFile: ");
            out.push_str(&path.to_string_lossy());
        }

        if let Some(span) = self.last_span {
            out.push_str(&format!(
                "\nLine {} Col {}",
                span.start_line, span.start_col
            ));
            if let Some(path) = self.module_paths.last() {
                if let Ok(src) = std::fs::read_to_string(path) {
                    let lines: Vec<&str> = src.lines().collect();
                    if span.start_line > 0 && span.start_line <= lines.len() {
                        let line = lines[span.start_line - 1];
                        out.push_str("\n");
                        out.push_str(line);
                        out.push_str("\n");
                        if span.start_col > 0 {
                            let caret_pos = span.start_col - 1;
                            for _ in 0..caret_pos {
                                out.push(' ');
                            }
                            out.push('^');
                        }

                        if let Some((stage, segment, func)) =
                            pipeline_stage_info(line, span.start_col)
                        {
                            out.push_str(&format!(
                                "\nPipeline error at stage {}: {}",
                                stage, segment
                            ));
                            if let Some(func) = func {
                                out.push_str(&format!(" (function '{}')", func));
                            }
                        }
                    }
                }
            }
        }

        if !self.frames.is_empty() {
            out.push_str("\nStack trace:");
            for frame in self.frames.iter().rev() {
                out.push_str("\n - ");
                out.push_str(&frame.function.name);
            }
        }

        out
    }

    pub fn globals_snapshot(&self) -> Vec<(String, String)> {
        let mut out: Vec<(String, String)> = self
            .globals
            .iter()
            .map(|(k, v)| (k.clone(), to_string(v)))
            .collect();
        out.sort_by(|a, b| a.0.cmp(&b.0));
        out
    }

    pub fn clear_repl_state(&mut self, source_path: &str) {
        self.stack.clear();
        self.frames.clear();
        self.try_stack.clear();
        self.globals.clear();
        self.module_cache.clear();
        self.module_stack.clear();
        self.module_paths = vec![PathBuf::from(source_path)];
    }
}

fn const_string(constants: &Vec<ConstValue>, idx_i32: i32) -> Result<String, String> {
    if idx_i32 < 0 {
        return Err("Builtin name must be string".into());
    }
    let idx = idx_i32 as usize;
    match constants.get(idx) {
        Some(ConstValue::String(s)) => Ok(s.clone()),
        _ => Err("Builtin name must be string".into()),
    }
}

fn as_bool(v: ConstValue) -> Result<bool, String> {
    match v {
        ConstValue::Bool(b) => Ok(b),
        _ => Err("Expected boolean".into()),
    }
}

fn index_to_usize(v: &ConstValue) -> Result<usize, String> {
    match v {
        ConstValue::Number(n) => {
            if n.fract() != 0.0 || *n < 0.0 {
                return Err("Index must be a non-negative integer".into());
            }
            Ok(*n as usize)
        }
        _ => Err("Index must be a number".into()),
    }
}

fn to_string(v: &ConstValue) -> String {
    match v {
        ConstValue::String(s) => s.clone(),
        ConstValue::Number(n) => n.to_string(),
        ConstValue::Bool(b) => b.to_string(),
        ConstValue::Null => "null".to_string(),
        ConstValue::Function(f) => format!("<function {}>", f.name),
        ConstValue::Closure(c) => format!("<closure {}>", c.function.name),
        ConstValue::FfiHandle(id) => format!("<ffi {}>", id),
        ConstValue::Array(items) => {
            let mut out = String::new();
            out.push('[');
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(&to_string(item));
            }
            out.push(']');
            out
        }
        ConstValue::Map(items) => {
            let mut pairs: Vec<(String, String)> = items
                .iter()
                .map(|(k, v)| (map_key_to_string(k), to_string(v)))
                .collect();
            pairs.sort_by(|a, b| a.0.cmp(&b.0));
            let mut out = String::new();
            out.push('{');
            for (i, (k, v)) in pairs.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(k);
                out.push_str(": ");
                out.push_str(v);
            }
            out.push('}');
            out
        }
    }
}

fn to_number(v: ConstValue) -> Result<f64, String> {
    match v {
        ConstValue::Number(n) => Ok(n),
        ConstValue::String(s) => s
            .trim()
            .parse::<f64>()
            .map_err(|_| "Expected number or numeric string".into()),
        _ => Err("Expected number or numeric string".into()),
    }
}

fn log_value(argc: usize, stack: &mut Vec<ConstValue>, level: &str) -> Result<ConstValue, String> {
    if argc != 1 && argc != 2 {
        return Err(format!("log_{}() expects 1 or 2 arguments", level));
    }
    let value = stack.pop().ok_or("Stack underflow")?;
    let message = if argc == 2 {
        let label = stack.pop().ok_or("Stack underflow")?;
        format!("{}: {}", to_string(&label), to_string(&value))
    } else {
        to_string(&value)
    };
    let line = format!("[{}] {}", level, message);
    if level == "warn" || level == "error" {
        eprintln!("{}", line);
    } else {
        println!("{}", line);
    }
    Ok(value)
}

fn values_equal(a: &ConstValue, b: &ConstValue) -> bool {
    match (a, b) {
        (ConstValue::Null, ConstValue::Null) => true,
        (ConstValue::Bool(x), ConstValue::Bool(y)) => x == y,
        (ConstValue::Number(x), ConstValue::Number(y)) => x == y,
        (ConstValue::String(x), ConstValue::String(y)) => x == y,
        (ConstValue::Array(x), ConstValue::Array(y)) => {
            if x.len() != y.len() {
                return false;
            }
            for (a, b) in x.iter().zip(y.iter()) {
                if !values_equal(a, b) {
                    return false;
                }
            }
            true
        }
        (ConstValue::Map(x), ConstValue::Map(y)) => {
            if x.len() != y.len() {
                return false;
            }
            for (k, v) in x.iter() {
                match y.get(k) {
                    Some(other) if values_equal(v, other) => {}
                    _ => return false,
                }
            }
            true
        }
        (ConstValue::Closure(_), ConstValue::Closure(_)) => false,
        (ConstValue::Function(_), ConstValue::Function(_)) => false,
        (ConstValue::FfiHandle(_), ConstValue::FfiHandle(_)) => false,
        _ => false,
    }
}

fn map_key_from_value(v: &ConstValue) -> Result<MapKey, String> {
    match v {
        ConstValue::String(s) => Ok(MapKey::String(s.clone())),
        ConstValue::Bool(b) => Ok(MapKey::Bool(*b)),
        ConstValue::Number(n) => Ok(MapKey::Number(n.to_bits())),
        _ => Err("Map keys must be string, number, or bool".into()),
    }
}

fn map_key_to_string(k: &MapKey) -> String {
    match k {
        MapKey::String(s) => format!("\"{}\"", s),
        MapKey::Bool(b) => b.to_string(),
        MapKey::Number(bits) => f64::from_bits(*bits).to_string(),
    }
}

unsafe fn ffi_call_f64(lib: &Library, name: &str, args: &[f64]) -> Result<f64, String> {
    match args.len() {
        0 => {
            let func: Symbol<unsafe extern "C" fn() -> f64> =
                lib.get(name.as_bytes()).map_err(|e| e.to_string())?;
            Ok(func())
        }
        1 => {
            let func: Symbol<unsafe extern "C" fn(f64) -> f64> =
                lib.get(name.as_bytes()).map_err(|e| e.to_string())?;
            Ok(func(args[0]))
        }
        2 => {
            let func: Symbol<unsafe extern "C" fn(f64, f64) -> f64> =
                lib.get(name.as_bytes()).map_err(|e| e.to_string())?;
            Ok(func(args[0], args[1]))
        }
        3 => {
            let func: Symbol<unsafe extern "C" fn(f64, f64, f64) -> f64> =
                lib.get(name.as_bytes()).map_err(|e| e.to_string())?;
            Ok(func(args[0], args[1], args[2]))
        }
        4 => {
            let func: Symbol<unsafe extern "C" fn(f64, f64, f64, f64) -> f64> =
                lib.get(name.as_bytes()).map_err(|e| e.to_string())?;
            Ok(func(args[0], args[1], args[2], args[3]))
        }
        _ => Err("ffi_call supports 0-4 numeric args".into()),
    }
}

fn run_python(script: &str, input: &str) -> Result<(String, String, i32), String> {
    let python = std::env::var("NEXO_PYTHON").unwrap_or_else(|_| "python".to_string());
    let mut cmd = Command::new(&python);
    cmd.arg(script)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped());

    let mut child = cmd.spawn().or_else(|_| {
        if python == "python" {
            let mut cmd = Command::new("python3");
            cmd.arg(script)
                .stdin(std::process::Stdio::piped())
                .stdout(std::process::Stdio::piped())
                .stderr(std::process::Stdio::piped());
            cmd.spawn().map_err(|e| e.to_string())
        } else {
            Err("Failed to start python".into())
        }
    })?;

    if let Some(mut stdin) = child.stdin.take() {
        use std::io::Write;
        stdin
            .write_all(input.as_bytes())
            .map_err(|e| e.to_string())?;
    }

    let output = child.wait_with_output().map_err(|e| e.to_string())?;
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    let code = output.status.code().unwrap_or(-1);

    Ok((stdout, stderr, code))
}

fn is_unsafe_builtin(name: &str) -> bool {
    matches!(
        name,
        "fs_read"
            | "fs_write"
            | "fs_exists"
            | "fs_list"
            | "proc_run"
            | "ffi_load"
            | "ffi_call"
            | "py_run"
            | "http_get"
            | "http_post"
            | "glob"
    )
}

fn safe_block_message(name: &str) -> String {
    let hint = match name {
        "fs_read" | "fs_write" | "fs_exists" | "fs_list" => {
            "File system access is disabled in --safe. Consider passing data via stdin or run without --safe."
        }
        "proc_run" => {
            "Process execution is disabled in --safe. Consider running without --safe."
        }
        "ffi_load" | "ffi_call" => {
            "FFI is disabled in --safe. Consider running without --safe."
        }
        "py_run" => {
            "Python execution is disabled in --safe. Consider running without --safe."
        }
        "http_get" | "http_post" => {
            "Network access is disabled in --safe. Consider using cached data or run without --safe."
        }
        "glob" => {
            "Filesystem globbing is disabled in --safe. Consider providing explicit paths or run without --safe."
        }
        _ => "This builtin is disabled in --safe.",
    };

    format!("unsafe builtin '{}' disabled in --safe. {}", name, hint)
}

fn collect_cli_args() -> Vec<String> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        return Vec::new();
    }
    if args[1] == "run" || args[1] == "--emit" {
        args.iter().skip(3).cloned().collect()
    } else {
        args.iter().skip(2).cloned().collect()
    }
}

fn pipeline_stage_info(line: &str, col: usize) -> Option<(usize, String, Option<String>)> {
    if !line.contains("|>") {
        return None;
    }
    let mut stages = Vec::new();
    let mut start = 0usize;
    let bytes = line.as_bytes();
    let mut i = 0usize;
    while i + 1 < bytes.len() {
        if bytes[i] == b'|' && bytes[i + 1] == b'>' {
            stages.push((start, i));
            start = i + 2;
            i += 2;
            continue;
        }
        i += 1;
    }
    stages.push((start, bytes.len()));

    let col0 = col.saturating_sub(1);
    for (idx, (s, e)) in stages.iter().enumerate() {
        if col0 >= *s && col0 <= *e {
            let segment = line[*s..*e].trim().to_string();
            let func = pipeline_stage_function(&segment);
            return Some((idx + 1, segment, func));
        }
    }
    None
}

fn pipeline_stage_function(segment: &str) -> Option<String> {
    let trimmed = segment.trim();
    if trimmed.is_empty() {
        return None;
    }
    let end = trimmed.find('(').unwrap_or(trimmed.len());
    let name = trimmed[..end].trim();
    if name.is_empty() {
        return None;
    }
    Some(name.to_string())
}

fn http_request(
    method: &str,
    url: &str,
    body: Option<&str>,
    headers: Option<HashMap<MapKey, ConstValue>>,
) -> Result<ConstValue, String> {
    let client = reqwest::blocking::Client::new();
    let mut req = match method {
        "GET" => client.get(url),
        "POST" => client.post(url),
        _ => return Err("unsupported http method".into()),
    };

    if let Some(h) = headers {
        for (k, v) in h {
            let key = match k {
                MapKey::String(s) => s,
                _ => return Err("http headers keys must be strings".into()),
            };
            let val = match v {
                ConstValue::String(s) => s,
                _ => return Err("http headers values must be strings".into()),
            };
            req = req.header(&key, val);
        }
    }

    if let Some(b) = body {
        req = req.body(b.to_string());
    }

    let resp = req.send().map_err(|e| format!("http error: {}", e))?;
    let status = resp.status().as_u16() as f64;
    let mut header_map = HashMap::new();
    for (k, v) in resp.headers().iter() {
        let val = v.to_str().unwrap_or("").to_string();
        header_map.insert(MapKey::String(k.to_string()), ConstValue::String(val));
    }
    let body = resp.text().map_err(|e| format!("http error: {}", e))?;

    let mut out = HashMap::new();
    out.insert(
        MapKey::String("status".to_string()),
        ConstValue::Number(status),
    );
    out.insert(
        MapKey::String("headers".to_string()),
        ConstValue::Map(header_map),
    );
    out.insert(MapKey::String("body".to_string()), ConstValue::String(body));

    Ok(ConstValue::Map(out))
}

fn parse_source(src: &str) -> Result<Vec<Stmt>, String> {
    let tokens = Lexer::new(src).tokenize().map_err(|e| e)?;
    catch_parse(|| Parser::new_with_source(tokens, src).parse())
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

fn json_to_value(v: &serde_json::Value) -> Result<ConstValue, String> {
    match v {
        serde_json::Value::Null => Ok(ConstValue::Null),
        serde_json::Value::Bool(b) => Ok(ConstValue::Bool(*b)),
        serde_json::Value::Number(n) => n
            .as_f64()
            .map(ConstValue::Number)
            .ok_or_else(|| "Invalid JSON number".into()),
        serde_json::Value::String(s) => Ok(ConstValue::String(s.clone())),
        serde_json::Value::Array(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(json_to_value(item)?);
            }
            Ok(ConstValue::Array(out))
        }
        serde_json::Value::Object(map) => {
            let mut out = std::collections::HashMap::new();
            for (k, v) in map {
                out.insert(MapKey::String(k.clone()), json_to_value(v)?);
            }
            Ok(ConstValue::Map(out))
        }
    }
}

fn value_to_json(v: &ConstValue) -> Result<serde_json::Value, String> {
    match v {
        ConstValue::Null => Ok(serde_json::Value::Null),
        ConstValue::Bool(b) => Ok(serde_json::Value::Bool(*b)),
        ConstValue::Number(n) => {
            if n.fract() == 0.0 {
                if *n <= i64::MAX as f64 && *n >= i64::MIN as f64 {
                    return Ok(serde_json::Value::Number(serde_json::Number::from(
                        *n as i64,
                    )));
                }
            }
            let num = serde_json::Number::from_f64(*n)
                .ok_or_else(|| "Invalid JSON number".to_string())?;
            Ok(serde_json::Value::Number(num))
        }
        ConstValue::String(s) => Ok(serde_json::Value::String(s.clone())),
        ConstValue::Array(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(value_to_json(item)?);
            }
            Ok(serde_json::Value::Array(out))
        }
        ConstValue::Map(items) => {
            let mut keys: Vec<&MapKey> = items.keys().collect();
            keys.sort_by(|a, b| map_key_to_string(a).cmp(&map_key_to_string(b)));
            let mut map = serde_json::Map::new();
            for key in keys {
                let name = match key {
                    MapKey::String(s) => s.clone(),
                    _ => return Err("json_stringify() map keys must be strings".into()),
                };
                let value = items.get(key).ok_or("Missing map value")?;
                map.insert(name, value_to_json(value)?);
            }
            Ok(serde_json::Value::Object(map))
        }
        ConstValue::Function(_) | ConstValue::Closure(_) | ConstValue::FfiHandle(_) => {
            Err("json_stringify() does not support functions".into())
        }
    }
}
