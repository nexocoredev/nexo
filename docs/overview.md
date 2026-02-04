# Nexo / Nexocore - Technical Overview

## 1) Language Focus
Nexo is a **scripting and automation** language with:
- Python-style syntax (indentation + `:`)
- Pipeline `|>` for fast composition
- Controlled interop (C FFI + external Python)
- Minimal but practical stdlib (fs/proc/env/time/json)

Goal: **high DX**, minimal ceremony, fast execution, strong stability.
Mental model: **flow-first** pipelines where values move forward in clear stages.

## 2) Architecture (Full Pipeline)
```
.nx source
  -> Lexer (tokens with line/col)
  -> Parser (AST with spans)
  -> Compiler (bytecode)
  -> VM (stack-based)
```

### Inputs
- `nexo file.nx`
- `nexo --emit file.nx` -> `file.nxbc`
- `nexo run file.nx|file.nxbc`

### Bytecode
- Versioned (VERSION = 9)
- Designed for stability in v1.0+ (currently breaking across versions)

## 3) AST With Spans
AST nodes include exact ranges:
- `Expr { kind, span }`
- `Stmt { kind, span }`

Span format: `start_line/start_col` and `end_line/end_col` (1-based).

## 4) Syntax & Semantics

### Types
- `Number` (f64)
- `String`
- `Bool`
- `Null`
- `Array`
- `Map` (keys: string/number/bool)
- `Function`, `Closure`, `FfiHandle`

### Operators
- Arithmetic: `+`, `*`
- Comparisons: `> >= < <= == !=`
- Logic: `and or not`
- Pipeline: `|>`

### Comments
- Single-line comments with `#`

### Control Flow
- `if/else`, `while`
- `break`, `continue`
- `try/catch` (errors are strings)

### Functions
- `def name(a, b): ...`
- `return expr`
- Closures are **by value** (captured at definition time)

### Modules
- `import mod` / `import mod as alias`
- `export name`
- Resolution: relative + `nexo_modules` + parent `nexo_modules`
- Extra resolution: `NEXO_MODULES`, `NEXO_HOME/nexo_modules`, and `nexo_modules` next to the `nexo` binary
- Cache to avoid re-execution

## 5) VM
Stack-based VM with:
- `CallFrame` per function
- Locals per function, globals per program
- `TryFrame` for try/catch
- Runtime stack traces (function names + file)

## 6) Builtins
Core:
- `print`, `input`, `len`, `str`, `append`
- `int`, `round`, `floor`, `ceil`
- `arg_has`, `arg_get`
- `log_info`, `log_warn`, `log_error`

Stdlib (builtins):
- `fs_read`, `fs_write`, `fs_exists`, `fs_list`
- `proc_run` -> `{code,out,err}`
- `env_get`, `args`
- `time_sleep`, `time_now`

JSON:
- `json_parse`, `json_stringify`

Interop:
- `ffi_load`, `ffi_call`
- `py_run` (Python external, JSON stdout)

## 7) Stdlib Modules (Wrappers)
- `fs.read/write/exists/list`
- `proc.run`
- `env.get/args`
- `time.sleep/now`
- `py.run`
- `log.info/warn/error`

## 8) Interop
### C FFI
```
lib = ffi_load("mylib.dll")
print(ffi_call(lib, "sum2", [1, 2]))
```
Supports `extern "C" fn(f64...) -> f64` with 0–4 args.

### Python External
```
data = py_run("script.py", "input")
```
Python returns JSON over stdout. Nexo converts it to map/array.

## 9) Lint / Static Analysis
Command:
- `nexo lint <file.nx>`
- `nexo lint --strict <file.nx>`

Errors (always):
- `return` outside function
- `break/continue` outside loop
- duplicate function
- duplicate parameter

Errors in `--strict`:
- undefined variable
- undefined function (except builtins/import)
- shadowing

Warnings:
- unused variable
- unused import
- unreachable code
- shadowing (non-strict)
- import name collision (non-strict)

## 10) LSP (Minimal)
Command:
- `nexo lsp`

Capabilities:
- Diagnostics from lint
- Document symbols (funcs + globals)
- Hover docstrings for functions

## 11) REPL
Command:
- `nexo repl`

Features:
- Multiline input
- Commands: `:help`, `:load`, `:ast`, `:bc`, `:time`, `:vars`, `:clear`, `:pwd`, `:exit`

## 12) QA
- Golden tests (source + bytecode)
- Demos validated across features
- `cargo test` passes

## 13) Core Stable (1.0)
The following are considered stable and backward-compatible targets for Nexo 1.0:

Syntax & Semantics:
- Indentation-based blocks with `:`
- `if/else`, `while`, `break`, `continue`, `return`, `try/catch`
- Functions with `def` and closures (by value)
- Arrays, maps, and indexing
- Operator pipeline `|>`

Bytecode & VM:
- NXBC versioned bytecode format
- Stack-based VM with deterministic execution
- Runtime errors with file/line/caret + stack trace

Essential Builtins:
- `print`, `input`, `len`, `str`, `append`
- `int`, `round`, `floor`, `ceil`
- `fs_*`, `proc_run`, `env_get`, `args`, `time_*`
- `json_parse`, `json_stringify`

Modules:
- `import` / `export` and module cache
- `nexo_modules` resolution strategy
