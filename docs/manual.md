# Nexo 1.0 Manual

This is the complete, single‑file reference for Nexo v1.0. It includes syntax, standard modules, CLI, and practical examples.

## Index
1. Quick Start
2. Installation (Summary)
3. Hello Real World
4. Language Basics
5. Comments
6. Pipelines (`|>`)
7. Control Flow
8. Functions and Closures
9. Arrays and Maps
10. Errors and try/catch
11. Modules and Imports
12. Builtins (Core)
13. Standard Modules
14. Interop (FFI and Python)
15. CLI Commands
16. Linting and Formatting
17. REPL
18. LSP
19. Bytecode (`.nxbc`)
20. Safe Mode
21. Recipes (Practical Examples)

---

## 1) Quick Start

Run the official starter script:

```
nexo run docs/hello_real_world.nx
```

Create your first script:

```nexo
print("hello")
```

Run it:

```
nexo run hello.nx
```

---

## 2) Installation (Summary)

- Windows: use the installer or ZIP, add folder to PATH
- Linux/macOS: extract tar.gz, `chmod +x nexo`, add to PATH

---

## 3) Hello Real World

This script is the recommended starting point and is included as:
`docs/hello_real_world.nx`

```nexo
import log
import re

def extract_title(res):
    titles = re.findall("<title>[^<]+</title>", res["body"])
    if len(titles) == 0:
        return "<no title>"
    return re.replace("<[^>]+>", titles[0], "")

"https://example.com" |> http_get |> extract_title |> log.info |> print
```

---

## 4) Language Basics

Values:
- Number (f64)
- String
- Bool (`true` / `false`)
- Null (`null`)
- Array
- Map
- Function / Closure

Variables:

```nexo
x = 10
name = "Nexo"
```

Strings:

```nexo
print("hello")
print("# is not a comment in strings")
```

---

## 5) Comments

Single‑line comments use `#` and are ignored by the lexer:

```nexo
# full line comment
print("ok")  # inline comment
```

Comments do not work inside strings (they are literal text).

---

## 6) Pipelines (`|>`)

Pipelines pass the left value into the right expression:

```nexo
"hello" |> len |> print
```

Multiline pipelines are supported with indentation:

```nexo
"https://example.com"
    |> http_get
    |> len
    |> print
```

Tips:
- Keep each stage simple.
- Use small functions for readability.

---

## 7) Control Flow

If/else:

```nexo
if x > 10:
    print("big")
else:
    print("small")
```

While loop:

```nexo
i = 0
while i < 3:
    print(i)
    i = i + 1
```

`break` and `continue` are supported inside loops.

---

## 8) Functions and Closures

```nexo
def add(a, b):
    return a + b

print(add(1, 2))
```

Closures capture by value:

```nexo
def make_adder(x):
    def add(y):
        return x + y
    return add

add5 = make_adder(5)
print(add5(3))
```

---

## 9) Arrays and Maps

Arrays:

```nexo
items = [1, 2, 3]
print(items[0])
```

Maps:

```nexo
user = {"name": "Ana", "age": 30}
print(user["name"])
```

Index assignment:

```nexo
items[1] = 99
user["age"] = 31
```

---

## 10) Errors and try/catch

Errors are strings. Use `try/catch`:

```nexo
try:
    print(len(1))
catch e:
    print("error: " + e)
```

---

## 11) Modules and Imports

Import modules by name:

```nexo
import fs
print(fs.read("file.txt"))
```

Import a file directly:

```nexo
import "./my_module.nx"
```

Module resolution order:
1) Relative to the current file
2) `nexo_modules` next to the current file
3) Parent `nexo_modules`
4) `NEXO_MODULES` (env var)
5) `NEXO_HOME/nexo_modules`
6) `nexo_modules` next to the `nexo` binary

Exports:

```nexo
def greet(name):
    return "hi " + name

export greet
```

---

## 12) Builtins (Core)

Core:
- `print(x)`
- `input()`
- `len(x)`
- `str(x)`
- `int(x)`
- `round(x)`
- `floor(x)`
- `ceil(x)`
- `append(array, value)`

Args:
- `args()` -> array of CLI args
- `arg_has("--flag")` -> bool
- `arg_get("--flag", default)` -> string or default

Logging:
- `log_info(x)`
- `log_warn(x)`
- `log_error(x)`

JSON:
- `json_parse(string)`
- `json_stringify(value)`

File/Process/Env/Time:
- `fs_read(path)` / `fs_write(path, data)` / `fs_exists(path)` / `fs_list(path)`
- `proc_run(cmd)` -> `{code,out,err}`
- `env_get(key)`
- `time_sleep(ms)` / `time_now()`

HTTP/Regex/Path:
- `http_get(url)` / `http_post(url, body, headers)`
- `re_match(pattern, text)` / `re_findall(pattern, text)` / `re_replace(pattern, text, repl)`
- `path_join(a, b)` / `path_dirname(path)`
- `glob(pattern)`

Modules:
- `import_module(name)`

---

## 13) Standard Modules

These are simple wrappers around builtins.

### env
```nexo
import env
print(env.get("HOME"))
```

### fs
```nexo
import fs
print(fs.exists("config.json"))
```

### http
```nexo
import http
res = http.get("https://example.com")
print(res["status"])
```

### log
```nexo
import log
"hello" |> log.info |> print
```

### path
```nexo
import path
print(path.join("a", "b"))
```

### proc
```nexo
import proc
print(proc.run("echo hi"))
```

### py
```nexo
import py
print(py.run("script.py", null))
```

### re
```nexo
import re
print(re.findall("[ab]", "abba"))
```

### time
```nexo
import time
time.sleep(500)
```

---

## 14) Interop (FFI and Python)

C FFI:

```nexo
h = ffi_load("mylib.dll")
print(ffi_call(h, "nexo_square", [3.0]))
```

Python external:

```nexo
print(py_run("script.py", null))
```

---

## 15) CLI Commands

```
nexo <file.nx>
nexo run <file.nx|file.nxbc>
nexo --emit <file.nx>
nexo lint [--strict] <file.nx>
nexo fmt <file.nx>
nexo repl [--safe]
nexo lsp
nexo add <github:user/repo[@tag]|url>
nexo add --path <dir>
nexo add --update <name>
nexo remove <name>
nexo bc-upgrade <file.nxbc> [--out <file.nxbc>]
nexo --version
```

---

## 16) Linting and Formatting

Lint:

```
nexo lint file.nx
nexo lint --strict file.nx
```

Format:

```
nexo fmt file.nx
```

---

## 17) REPL

Start with:

```
nexo repl
```

Commands:
- `:help`, `:load`, `:ast`, `:bc`, `:time`, `:vars`, `:clear`, `:pwd`, `:exit`

---

## 18) LSP

```
nexo lsp
```

Capabilities:
- Diagnostics
- Document symbols
- Hover docstrings for functions

---

## 19) Bytecode (`.nxbc`)

Compile to bytecode:

```
nexo --emit script.nx
```

Run bytecode:

```
nexo run script.nxbc
```

---

## 20) Safe Mode

Safe mode disables builtins that access the file system, network, processes, and interop:

```
nexo --safe script.nx
nexo repl --safe
```

---

## 21) Recipes (Practical Examples)

### Watch a folder
```nexo
import fs
import time
dir = "."
prev = fs.list(dir)
prev |> len |> print
i = 0
while i < 3:
    time.sleep(500)
    cur = fs.list(dir)
    if len(cur) != len(prev):
        print("change detected")
        prev = cur
    i = i + 1
print("watch done")
```

### CSV to JSON
```nexo
import re
csv = "name,age\nAna,30"
parts = re.findall("[^,\n]+", csv)
row = {parts[0]: parts[2], parts[1]: parts[3]}
rows = [row]
rows |> json_stringify |> print
```

### CLI flags
```nexo
name = arg_get("--name", "world")
verbose = arg_has("--verbose")
if verbose:
    print("hello " + name)
else:
    print(name)
```

### Simple scraper
```nexo
import re
def extract_title(res):
    titles = re.findall("<title>[^<]+</title>", res["body"])
    if len(titles) == 0:
        return "<no title>"
    return re.replace("<[^>]+>", titles[0], "")
"https://example.com" |> http_get |> extract_title |> print
```

### Pipeline automation
```nexo
import re
def normalize(text):
    return re.replace("\\s+", text, " ")
out = proc_run("echo alpha   beta")
out["out"] |> normalize |> print
```

### Logging pipeline
```nexo
import log
"task start" |> log.info |> print
```
