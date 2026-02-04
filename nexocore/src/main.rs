use std::env;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::Command;

use nexocore::{
    bytecode::{current_version, read_program, read_version, write_program},
    compiler::Compiler,
    lexer::Lexer,
    model::{ChunkDef, FunctionDef, Program},
    parser::Parser,
    vm::Vm,
};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage();
        return;
    }

    match args[1].as_str() {
        "--version" | "-v" => {
            println!("nexo {}", env!("CARGO_PKG_VERSION"));
        }
        "help" | "--help" | "-h" => {
            print_usage();
        }
        "--safe" => {
            if args.len() < 3 {
                print_usage();
                std::process::exit(1);
            }
            let filename = &args[2];
            if let Err(e) = run_source_safe(filename, true) {
                eprintln!("{}", e);
                std::process::exit(1);
            }
        }
        "repl" => {
            let safe = args.len() > 2 && args[2] == "--safe";
            run_repl(safe);
        }
        "lsp" => {
            if let Err(e) = nexocore::lsp::run_lsp() {
                eprintln!("LSP error: {}", e);
                std::process::exit(1);
            }
        }
        "lint" => {
            if args.len() < 3 {
                eprintln!("Usage: nexo lint [--strict] <file.nx>");
                std::process::exit(1);
            }
            let (strict, filename) = if args[2] == "--strict" {
                if args.len() < 4 {
                    eprintln!("Usage: nexo lint [--strict] <file.nx>");
                    std::process::exit(1);
                }
                (true, &args[3])
            } else {
                (false, &args[2])
            };
            let src = match fs::read_to_string(filename) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("Failed to read file: {}", e);
                    std::process::exit(1);
                }
            };
            let lints = match nexocore::lint::lint_source(&src, strict) {
                Ok(l) => l,
                Err(e) => {
                    eprintln!("Lint error: {}", e);
                    std::process::exit(1);
                }
            };
            if lints.is_empty() {
                println!("lint: ok");
            } else {
                let mut error_count = 0;
                for lint in lints {
                    let label = match lint.severity {
                        nexocore::lint::Severity::Error => {
                            error_count += 1;
                            "error"
                        }
                        nexocore::lint::Severity::Warning => "warning",
                    };
                    println!("lint: {}: {}", label, lint.message);
                }
                if error_count > 0 {
                    std::process::exit(1);
                }
            }
        }
        "add" => {
            if args.len() < 3 {
                eprintln!("Usage: nexo add <github:user/repo[@tag]|url>");
                eprintln!("       nexo add --path <dir>");
                eprintln!("       nexo add --update <name>");
                std::process::exit(1);
            }
            if args[2] == "--update" {
                if args.len() < 4 {
                    eprintln!("Usage: nexo add --update <name>");
                    std::process::exit(1);
                }
                let name = &args[3];
                if let Err(e) = run_add_update(name) {
                    eprintln!("add error: {}", e);
                    std::process::exit(1);
                }
            } else if args[2] == "--path" {
                if args.len() < 4 {
                    eprintln!("Usage: nexo add --path <dir>");
                    std::process::exit(1);
                }
                let path = &args[3];
                if let Err(e) = run_add_path(path) {
                    eprintln!("add error: {}", e);
                    std::process::exit(1);
                }
            } else {
                let spec = &args[2];
                if let Err(e) = run_add(spec) {
                    eprintln!("add error: {}", e);
                    std::process::exit(1);
                }
            }
        }
        "remove" => {
            if args.len() < 3 {
                eprintln!("Usage: nexo remove <name>");
                std::process::exit(1);
            }
            let name = &args[2];
            if let Err(e) = run_remove(name) {
                eprintln!("remove error: {}", e);
                std::process::exit(1);
            }
        }
        "bc-upgrade" => {
            if args.len() < 3 {
                eprintln!("Usage: nexo bc-upgrade <file.nxbc> [--out <file.nxbc>]");
                std::process::exit(1);
            }
            let input = &args[2];
            let mut output: Option<&str> = None;
            if args.len() >= 5 && args[3] == "--out" {
                output = Some(&args[4]);
            }
            if let Err(e) = run_bc_upgrade(input, output) {
                eprintln!("bc-upgrade error: {}", e);
                std::process::exit(1);
            }
        }
        "test" => {
            let mut watch = false;
            let mut update = false;

            for arg in args.iter().skip(2) {
                if arg == "--watch" {
                    watch = true;
                }
                if arg == "--update" {
                    update = true;
                }
            }

            if watch {
                run_test_watch(update);
            } else {
                run_test_once(update).unwrap_or_else(|e| {
                    eprintln!("{}", e);
                    std::process::exit(1);
                });
            }
        }

        "fmt" => {
            if args.len() < 3 {
                eprintln!("Usage: nexo fmt <file.nx>");
                std::process::exit(1);
            }
            let filename = &args[2];
            let src = match fs::read_to_string(filename) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("Failed to read file: {}", e);
                    std::process::exit(1);
                }
            };
            let formatted = match nexocore::format::format_source(&src) {
                Ok(out) => out,
                Err(e) => {
                    eprintln!("fmt error: {}", e);
                    std::process::exit(1);
                }
            };
            if let Err(e) = fs::write(filename, formatted) {
                eprintln!("Failed to write file: {}", e);
                std::process::exit(1);
            }
        }

        "--emit" => {
            let filename = &args[2];
            if let Err(e) = emit_bytecode(filename) {
                eprintln!("{}", e);
                std::process::exit(1);
            }
        }

        "run" => {
            if args.len() < 3 {
                eprintln!("Usage: nexo run [--safe] <file.nx|file.nxbc>");
                std::process::exit(1);
            }
            let (safe, filename) = if args[2] == "--safe" {
                if args.len() < 4 {
                    eprintln!("Usage: nexo run [--safe] <file.nx|file.nxbc>");
                    std::process::exit(1);
                }
                (true, &args[3])
            } else {
                (false, &args[2])
            };
            if filename.ends_with(".nxbc") {
                let program = match read_program(filename) {
                    Ok(p) => p,
                    Err(e) => {
                        eprintln!("Failed to read bytecode: {}", e);
                        std::process::exit(1);
                    }
                };
                let mut vm = Vm::new(program, filename, safe);
                if let Err(e) = vm.run() {
                    eprintln!("{}", e);
                    std::process::exit(1);
                }
            } else {
                if let Err(e) = run_source_safe(filename, safe) {
                    eprintln!("{}", e);
                    std::process::exit(1);
                }
            }
        }

        _ => {
            if args[1].starts_with('-') {
                eprintln!("Unknown option: {}", args[1]);
                print_usage();
                std::process::exit(1);
            }
            let filename = &args[1];
            if let Err(e) = run_source_safe(filename, false) {
                eprintln!("{}", e);
                std::process::exit(1);
            }
        }
    }
}

fn run_source_safe(filename: &str, safe: bool) -> Result<(), String> {
    let src = fs::read_to_string(filename).map_err(|e| format!("Failed to read file: {}", e))?;
    let ast = parse_source(&src)?;
    let mut compiler = Compiler::new();
    let program = compiler.compile(ast);
    let mut vm = Vm::new(program, filename, safe);
    vm.run()
}

fn run_repl(safe: bool) {
    let cwd = env::current_dir()
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|_| "<repl>".to_string());
    let mut vm = Vm::new(
        Program {
            entry: FunctionDef {
                name: "__repl__".into(),
                arity: 0,
                local_count: 0,
                chunk: ChunkDef {
                    constants: Vec::new(),
                    code: Vec::new(),
                },
            },
        },
        &cwd,
        safe,
    );

    let mut buffer = String::new();
    let mut in_block = false;
    let mut show_ast = false;
    let mut show_bc = false;
    let mut show_time = false;

    loop {
        let prompt = if in_block { "... " } else { ">>> " };
        print!("{}", prompt);
        io::stdout().flush().ok();

        let mut line = String::new();
        if io::stdin().read_line(&mut line).is_err() {
            break;
        }
        let line = line.trim_end_matches(['\r', '\n']);

        if line == ":exit" || line == ":quit" {
            break;
        }

        if line.starts_with(':') {
            if handle_repl_command(
                line,
                &mut vm,
                &cwd,
                &mut show_ast,
                &mut show_bc,
                &mut show_time,
            ) {
                continue;
            }
        }

        if line.trim().is_empty() {
            if buffer.trim().is_empty() {
                in_block = false;
                continue;
            }
            let src = buffer.clone();
            buffer.clear();
            in_block = false;
            if let Err(e) = run_repl_source(&mut vm, &src, &cwd, show_ast, show_bc, show_time) {
                eprintln!("{}", e);
            }
            continue;
        }

        buffer.push_str(line);
        buffer.push('\n');

        if line.trim_end().ends_with(':') {
            in_block = true;
            continue;
        }

        if !in_block {
            let src = buffer.clone();
            buffer.clear();
            if let Err(e) = run_repl_source(&mut vm, &src, &cwd, show_ast, show_bc, show_time) {
                eprintln!("{}", e);
            }
        }
    }
}

fn run_repl_source(
    vm: &mut Vm,
    src: &str,
    path: &str,
    show_ast: bool,
    show_bc: bool,
    show_time: bool,
) -> Result<(), String> {
    let ast = parse_source(src)?;
    if show_ast {
        println!("{:#?}", ast);
    }
    let mut compiler = Compiler::new();
    let program = compiler.compile(ast);
    if show_bc {
        print_bytecode(&program);
    }
    if show_time {
        let start = std::time::Instant::now();
        let result = vm.run_program(program, path);
        let elapsed = start.elapsed();
        println!("[time] {:?}", elapsed);
        return result;
    }
    vm.run_program(program, path)
}

fn parse_source(src: &str) -> Result<Vec<nexocore::ast::Stmt>, String> {
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

fn emit_bytecode(filename: &str) -> Result<(), String> {
    let src = fs::read_to_string(filename).map_err(|e| format!("Failed to read file: {}", e))?;
    let ast = parse_source(&src)?;
    let mut compiler = Compiler::new();
    let program = compiler.compile(ast);
    let out = filename.replace(".nx", ".nxbc");
    write_program(&program, &out).map_err(|e| format!("Failed to write bytecode: {}", e))?;
    println!("Generated {}", out);
    Ok(())
}

fn print_usage() {
    eprintln!("Usage:");
    eprintln!("  nexo <file.nx>");
    eprintln!("  nexo --safe <file.nx>");
    eprintln!("  nexo run <file.nx|file.nxbc>");
    eprintln!("  nexo run --safe <file.nx|file.nxbc>");
    eprintln!("  nexo --emit <file.nx>");
    eprintln!("  nexo test [--watch] [--update]");
    eprintln!("  nexo lint [--strict] <file.nx>");
    eprintln!("  nexo fmt <file.nx>");
    eprintln!("  nexo repl [--safe]");
    eprintln!("  nexo lsp");
    eprintln!("  nexo add <github:user/repo[@tag]|url>");
    eprintln!("  nexo add --path <dir>");
    eprintln!("  nexo add --update <name>");
    eprintln!("  nexo remove <name>");
    eprintln!("  nexo bc-upgrade <file.nxbc> [--out <file.nxbc>]");
    eprintln!("  nexo --version");
    eprintln!("  nexo help");
}

fn handle_repl_command(
    line: &str,
    vm: &mut Vm,
    cwd: &str,
    show_ast: &mut bool,
    show_bc: &mut bool,
    show_time: &mut bool,
) -> bool {
    let mut parts = line.splitn(2, ' ');
    let cmd = parts.next().unwrap_or("");
    let arg = parts.next().unwrap_or("").trim();

    match cmd {
        ":help" => {
            println!(":load <file>  load and run file");
            println!(":ast          toggle AST print");
            println!(":bc           toggle bytecode print");
            println!(":time         toggle timing");
            println!(":vars         list globals");
            println!(":clear        clear REPL state");
            println!(":exit         exit repl");
            true
        }
        ":load" => {
            if arg.is_empty() {
                eprintln!("Usage: :load <file>");
                return true;
            }
            let src = match fs::read_to_string(arg) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("Failed to read file: {}", e);
                    return true;
                }
            };
            if let Err(e) = run_repl_source(vm, &src, arg, *show_ast, *show_bc, *show_time) {
                eprintln!("{}", e);
            }
            true
        }
        ":ast" => {
            *show_ast = !*show_ast;
            println!("AST: {}", if *show_ast { "on" } else { "off" });
            true
        }
        ":bc" => {
            *show_bc = !*show_bc;
            println!("Bytecode: {}", if *show_bc { "on" } else { "off" });
            true
        }
        ":time" => {
            *show_time = !*show_time;
            println!("Timing: {}", if *show_time { "on" } else { "off" });
            true
        }
        ":pwd" => {
            println!("{}", cwd);
            true
        }
        ":vars" => {
            let vars = vm.globals_snapshot();
            if vars.is_empty() {
                println!("<no globals>");
                return true;
            }
            for (k, v) in vars {
                println!("{} = {}", k, v);
            }
            true
        }
        ":clear" => {
            vm.clear_repl_state(cwd);
            println!("REPL state cleared");
            true
        }
        _ => false,
    }
}

fn print_bytecode(program: &Program) {
    println!("== constants ==");
    for (i, c) in program.entry.chunk.constants.iter().enumerate() {
        println!("{}: {:?}", i, c);
    }
    println!("== code ==");
    for (i, instr) in program.entry.chunk.code.iter().enumerate() {
        println!("{}: {:?}", i, instr);
    }
}

fn run_test_once(update: bool) -> Result<(), String> {
    let output = Command::new("cargo")
        .arg("test")
        .output()
        .map_err(|e| e.to_string())?;

    if output.status.success() {
        println!("tests: ok");
        return Ok(());
    }

    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();

    if update {
        if let Some((script_key, got)) = parse_golden_mismatch(&stdout) {
            let expected_path = build_expected_path(&script_key)?;
            update_expected_from_output(&expected_path, &got)?;
            println!("updated expected: {}", expected_path.to_string_lossy());
            return Ok(());
        }
        return Err("tests failed; no golden mismatch to update".into());
    }

    Err(format!("tests failed\n{}{}", stdout, stderr))
}

fn run_test_watch(update: bool) {
    loop {
        if let Err(e) = run_test_once(update) {
            eprintln!("{}", e);
        }
        std::thread::sleep(std::time::Duration::from_millis(1000));
    }
}

fn parse_golden_mismatch(output: &str) -> Option<(String, String)> {
    let mut current_key: Option<String> = None;
    let mut got: Option<String> = None;

    for line in output.lines() {
        if line.contains("Source mismatch for ") || line.contains("Bytecode mismatch for ") {
            if let Some(idx) = line.find("for ") {
                let rest = &line[idx + 4..];
                if let Some(end) = rest.find(':') {
                    current_key = Some(rest[..end].trim().to_string());
                }
            }
        }
        if line.contains("got '") {
            if let Some(start) = line.find("got '") {
                let text = &line[start + 5..];
                if let Some(end) = text.rfind('\'') {
                    got = Some(text[..end].to_string());
                }
            }
        }
    }

    match (current_key, got) {
        (Some(k), Some(g)) => Some((k, g)),
        _ => None,
    }
}

fn build_expected_path(key: &str) -> Result<std::path::PathBuf, String> {
    let root = std::env::current_dir().map_err(|e| e.to_string())?;
    Ok(root
        .join("tests")
        .join("expected")
        .join(format!("{}.out", key)))
}

fn update_expected_from_output(
    expected_path: &std::path::Path,
    output: &str,
) -> Result<(), String> {
    if let Some(parent) = expected_path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| e.to_string())?;
    }
    std::fs::write(expected_path, output).map_err(|e| e.to_string())
}

fn run_bc_upgrade(input: &str, output: Option<&str>) -> Result<(), String> {
    let version = read_version(input).map_err(|e| e.to_string())?;
    let current = current_version();
    if version != current {
        return Err(format!(
            "cannot upgrade from version {} to {}; recompile from source",
            version, current
        ));
    }
    let program = read_program(input).map_err(|e| e.to_string())?;
    let out_path = output.unwrap_or(input);
    write_program(&program, out_path).map_err(|e| e.to_string())?;
    println!("upgraded {}", out_path);
    Ok(())
}

fn run_add(spec: &str) -> Result<(), String> {
    let (name, url, tag) = parse_dep_spec(spec)?;
    let project_root = find_project_root()?;
    let cache_dir = project_root.join(".nexo_cache");
    let modules_dir = project_root.join("nexo_modules");
    fs::create_dir_all(&cache_dir).map_err(|e| e.to_string())?;
    fs::create_dir_all(&modules_dir).map_err(|e| e.to_string())?;

    let cache_repo = cache_dir.join(&name);
    if !cache_repo.exists() {
        run_git(
            &["clone", &url, cache_repo.to_str().ok_or("bad cache path")?],
            None,
        )?;
    } else {
        run_git(&["fetch", "--all"], Some(&cache_repo))?;
    }

    if let Some(tag) = &tag {
        run_git(&["checkout", tag], Some(&cache_repo))?;
    } else {
        run_git(&["checkout", "HEAD"], Some(&cache_repo))?;
    }

    let rev = run_git_capture(&["rev-parse", "HEAD"], Some(&cache_repo))?;

    let target = modules_dir.join(&name);
    if target.exists() {
        fs::remove_dir_all(&target).map_err(|e| e.to_string())?;
    }
    copy_dir(&cache_repo, &target)?;

    let toml_path = project_root.join("nexo.toml");
    update_toml_dependencies(&toml_path, &name, &url)?;

    let lock_path = project_root.join("nexo.lock");
    update_lockfile(&lock_path, &name, &url, &rev)?;

    println!("added {} ({})", name, rev.trim());
    Ok(())
}

fn run_add_path(path: &str) -> Result<(), String> {
    let project_root = find_project_root()?;
    let modules_dir = project_root.join("nexo_modules");
    fs::create_dir_all(&modules_dir).map_err(|e| e.to_string())?;

    let src = PathBuf::from(path);
    if !src.exists() {
        return Err("path not found".into());
    }
    let name = src
        .file_name()
        .and_then(|s| s.to_str())
        .ok_or("invalid path name")?
        .to_string();

    let target = modules_dir.join(&name);
    if target.exists() {
        fs::remove_dir_all(&target).map_err(|e| e.to_string())?;
    }
    copy_dir(&src, &target)?;

    let toml_path = project_root.join("nexo.toml");
    update_toml_dependencies(&toml_path, &name, &format!("path:{}", path))?;

    let lock_path = project_root.join("nexo.lock");
    update_lockfile(&lock_path, &name, &format!("path:{}", path), "local")?;

    println!("added {} (local)", name);
    Ok(())
}

fn run_add_update(name: &str) -> Result<(), String> {
    let project_root = find_project_root()?;
    let toml_path = project_root.join("nexo.toml");
    let deps = read_toml_dependencies(&toml_path)?;
    let url = deps
        .get(name)
        .ok_or_else(|| format!("dependency '{}' not found", name))?
        .clone();

    let cache_dir = project_root.join(".nexo_cache");
    let modules_dir = project_root.join("nexo_modules");
    fs::create_dir_all(&cache_dir).map_err(|e| e.to_string())?;
    fs::create_dir_all(&modules_dir).map_err(|e| e.to_string())?;

    let cache_repo = cache_dir.join(name);
    if !cache_repo.exists() {
        run_git(
            &["clone", &url, cache_repo.to_str().ok_or("bad cache path")?],
            None,
        )?;
    } else {
        run_git(&["fetch", "--all"], Some(&cache_repo))?;
        run_git(&["checkout", "HEAD"], Some(&cache_repo))?;
    }

    let rev = run_git_capture(&["rev-parse", "HEAD"], Some(&cache_repo))?;
    let target = modules_dir.join(name);
    if target.exists() {
        fs::remove_dir_all(&target).map_err(|e| e.to_string())?;
    }
    copy_dir(&cache_repo, &target)?;

    let lock_path = project_root.join("nexo.lock");
    update_lockfile(&lock_path, name, &url, &rev)?;

    println!("updated {} ({})", name, rev.trim());
    Ok(())
}

fn run_remove(name: &str) -> Result<(), String> {
    let project_root = find_project_root()?;
    let modules_dir = project_root.join("nexo_modules");
    let target = modules_dir.join(name);
    if target.exists() {
        fs::remove_dir_all(&target).map_err(|e| e.to_string())?;
    }

    let toml_path = project_root.join("nexo.toml");
    remove_toml_dependency(&toml_path, name)?;
    let lock_path = project_root.join("nexo.lock");
    remove_lock_dependency(&lock_path, name)?;

    println!("removed {}", name);
    Ok(())
}

fn parse_dep_spec(spec: &str) -> Result<(String, String, Option<String>), String> {
    let mut tag: Option<String> = None;
    let mut base = spec.to_string();

    if spec.starts_with("github:") {
        base = spec.trim_start_matches("github:").to_string();
    }

    if let Some(idx) = base.rfind('@') {
        tag = Some(base[idx + 1..].to_string());
        base = base[..idx].to_string();
    }

    let url = if base.starts_with("http://") || base.starts_with("https://") {
        base.clone()
    } else {
        format!("https://github.com/{}.git", base)
    };

    let name = base
        .split('/')
        .last()
        .ok_or("invalid repo spec")?
        .trim_end_matches(".git")
        .to_string();

    Ok((name, url, tag))
}

fn find_project_root() -> Result<PathBuf, String> {
    let mut dir = env::current_dir().map_err(|e| e.to_string())?;
    loop {
        let candidate = dir.join("nexo.toml");
        if candidate.exists() {
            return Ok(dir);
        }
        if !dir.pop() {
            break;
        }
    }
    Ok(env::current_dir().map_err(|e| e.to_string())?)
}

fn run_git(args: &[&str], dir: Option<&Path>) -> Result<(), String> {
    let mut cmd = Command::new("git");
    cmd.args(args);
    if let Some(d) = dir {
        cmd.current_dir(d);
    }
    let status = cmd.status().map_err(|e| e.to_string())?;
    if !status.success() {
        return Err("git command failed".into());
    }
    Ok(())
}

fn run_git_capture(args: &[&str], dir: Option<&Path>) -> Result<String, String> {
    let mut cmd = Command::new("git");
    cmd.args(args);
    if let Some(d) = dir {
        cmd.current_dir(d);
    }
    let output = cmd.output().map_err(|e| e.to_string())?;
    if !output.status.success() {
        return Err("git command failed".into());
    }
    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}

fn copy_dir(src: &Path, dst: &Path) -> Result<(), String> {
    fs::create_dir_all(dst).map_err(|e| e.to_string())?;
    for entry in fs::read_dir(src).map_err(|e| e.to_string())? {
        let entry = entry.map_err(|e| e.to_string())?;
        let path = entry.path();
        let name = entry.file_name();
        if name.to_string_lossy() == ".git" {
            continue;
        }
        let target = dst.join(&name);
        if path.is_dir() {
            copy_dir(&path, &target)?;
        } else {
            fs::copy(&path, &target).map_err(|e| e.to_string())?;
        }
    }
    Ok(())
}

fn update_toml_dependencies(path: &Path, name: &str, url: &str) -> Result<(), String> {
    let mut deps = read_toml_dependencies(path)?;
    deps.insert(name.to_string(), url.to_string());
    write_toml_dependencies(path, &deps)
}

fn update_lockfile(path: &Path, name: &str, url: &str, rev: &str) -> Result<(), String> {
    let mut deps = read_toml_dependencies(path)?;
    deps.insert(name.to_string(), format!("{}#{}", url, rev.trim()));
    write_toml_dependencies(path, &deps)
}

fn read_toml_dependencies(
    path: &Path,
) -> Result<std::collections::BTreeMap<String, String>, String> {
    let mut deps = std::collections::BTreeMap::new();
    if !path.exists() {
        return Ok(deps);
    }
    let content = fs::read_to_string(path).map_err(|e| e.to_string())?;
    parse_dependencies_from_toml(&content, &mut deps);
    Ok(deps)
}

fn write_toml_dependencies(
    path: &Path,
    deps: &std::collections::BTreeMap<String, String>,
) -> Result<(), String> {
    let content = if path.exists() {
        fs::read_to_string(path).map_err(|e| e.to_string())?
    } else {
        String::new()
    };

    let mut kept = Vec::new();
    let mut in_deps = false;
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            in_deps = trimmed == "[dependencies]";
            if in_deps {
                continue;
            }
        }
        if in_deps {
            if trimmed.starts_with('[') {
                in_deps = false;
                kept.push(line.to_string());
            }
            continue;
        }
        kept.push(line.to_string());
    }

    let mut out = String::new();
    if !kept.is_empty() {
        out.push_str(&kept.join("\n"));
        out.push('\n');
    }
    out.push_str("[dependencies]\n");
    for (k, v) in deps {
        out.push_str(&format!("{} = \"{}\"\n", k, v));
    }

    fs::write(path, out).map_err(|e| e.to_string())
}

fn parse_dependencies_from_toml(
    content: &str,
    deps: &mut std::collections::BTreeMap<String, String>,
) {
    let mut in_deps = false;
    for line in content.lines() {
        let line = line.trim();
        if line.starts_with('[') {
            in_deps = line == "[dependencies]";
            continue;
        }
        if !in_deps || line.is_empty() || line.starts_with('#') {
            continue;
        }
        if let Some((k, v)) = line.split_once('=') {
            let key = k.trim().to_string();
            let val = v.trim().trim_matches('"').to_string();
            deps.insert(key, val);
        }
    }
}

fn remove_toml_dependency(path: &Path, name: &str) -> Result<(), String> {
    let mut deps = read_toml_dependencies(path)?;
    deps.remove(name);
    if deps.is_empty() {
        return Ok(());
    }
    write_toml_dependencies(path, &deps)
}

fn remove_lock_dependency(path: &Path, name: &str) -> Result<(), String> {
    let mut deps = read_toml_dependencies(path)?;
    deps.remove(name);
    if deps.is_empty() {
        return Ok(());
    }
    write_toml_dependencies(path, &deps)
}
