use std::fs;
use std::io::{BufRead, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::thread;
use std::time::{SystemTime, UNIX_EPOCH};

use serde_json::{json, Value};

fn bin_path() -> PathBuf {
    let path = PathBuf::from(env!("CARGO_BIN_EXE_nexocore"));
    if path.exists() {
        return path;
    }
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    if let Some(root) = manifest.parent() {
        let nexo = root.join("nexo.exe");
        if nexo.exists() {
            return nexo;
        }
    }
    manifest.join("target").join("debug").join("nexocore.exe")
}

fn make_temp_dir() -> Result<PathBuf, String> {
    let base = std::env::temp_dir().join("nexo_cli_tests");
    fs::create_dir_all(&base).map_err(|e| e.to_string())?;

    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|e| e.to_string())?
        .as_millis();
    let dir = base.join(format!("run_{}", now));
    fs::create_dir_all(&dir).map_err(|e| e.to_string())?;
    Ok(dir)
}

fn run_cmd(
    args: &[&str],
    input: Option<&str>,
    cwd: Option<&Path>,
) -> Result<CommandResult, String> {
    let mut cmd = Command::new(bin_path());
    cmd.args(args).stdout(Stdio::piped()).stderr(Stdio::piped());

    if let Some(dir) = cwd {
        cmd.current_dir(dir);
    }

    let input_data = input.map(|s| s.to_string());
    if input_data.is_some() {
        cmd.stdin(Stdio::piped());
    }

    let mut child = cmd.spawn().map_err(|e| e.to_string())?;

    if let Some(stdin) = input_data {
        if let Some(mut handle) = child.stdin.take() {
            handle
                .write_all(stdin.as_bytes())
                .map_err(|e| e.to_string())?;
        }
    }

    let output = child.wait_with_output().map_err(|e| e.to_string())?;
    Ok(CommandResult {
        status_ok: output.status.success(),
        stdout: String::from_utf8_lossy(&output.stdout).to_string(),
        stderr: String::from_utf8_lossy(&output.stderr).to_string(),
    })
}

struct CommandResult {
    status_ok: bool,
    stdout: String,
    stderr: String,
}

#[test]
fn lint_ok() -> Result<(), String> {
    let dir = make_temp_dir()?;
    let file = dir.join("ok.nx");
    fs::write(&file, "print(1)\n").map_err(|e| e.to_string())?;

    let result = run_cmd(&["lint", file.to_str().ok_or("bad path")?], None, None)?;
    if !result.status_ok {
        return Err(result.stderr);
    }
    if !result.stdout.contains("lint: ok") {
        return Err(format!("expected lint ok, got: {}", result.stdout));
    }
    Ok(())
}

#[test]
fn lint_strict_errors() -> Result<(), String> {
    let dir = make_temp_dir()?;
    let file = dir.join("bad.nx");
    fs::write(&file, "print(a)\n").map_err(|e| e.to_string())?;

    let result = run_cmd(
        &["lint", "--strict", file.to_str().ok_or("bad path")?],
        None,
        None,
    )?;
    if result.status_ok {
        return Err("expected lint failure in strict mode".into());
    }
    if !result.stdout.contains("undefined variable") {
        return Err(format!(
            "expected undefined variable, got: {}",
            result.stdout
        ));
    }
    Ok(())
}

#[test]
fn safe_mode_blocks_fs() -> Result<(), String> {
    let dir = make_temp_dir()?;
    let file = dir.join("safe.nx");
    fs::write(&file, "print(fs_read(\"tests/tmp_fs.txt\"))\n").map_err(|e| e.to_string())?;

    let result = run_cmd(
        &["run", "--safe", file.to_str().ok_or("bad path")?],
        None,
        None,
    )?;
    if result.status_ok {
        return Err("expected safe mode failure".into());
    }
    let combined = format!("{}{}", result.stdout, result.stderr);
    if !combined.contains("unsafe builtin") {
        return Err(format!("expected unsafe builtin error, got: {}", combined));
    }
    Ok(())
}

#[test]
fn repl_exit() -> Result<(), String> {
    let mut child = Command::new(bin_path())
        .arg("repl")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| e.to_string())?;

    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(b":exit\n").map_err(|e| e.to_string())?;
    }

    let output = child.wait_with_output().map_err(|e| e.to_string())?;
    if !output.status.success() {
        return Err(String::from_utf8_lossy(&output.stderr).to_string());
    }
    Ok(())
}

#[test]
fn lsp_initialize_shutdown() -> Result<(), String> {
    let mut child = Command::new(bin_path())
        .arg("lsp")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| e.to_string())?;

    let mut stdin = child.stdin.take().ok_or("missing stdin")?;
    let stdout = child.stdout.take().ok_or("missing stdout")?;
    let mut reader = BufReader::new(stdout);

    write_lsp_message(
        &mut stdin,
        &json!({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {"processId": null, "rootUri": null, "capabilities": {}}
        }),
    )?;
    let init = read_lsp_message(&mut reader)?;
    if init.get("id") != Some(&Value::from(1)) {
        return Err(format!("unexpected initialize response: {}", init));
    }

    write_lsp_message(
        &mut stdin,
        &json!({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "shutdown"
        }),
    )?;
    let shutdown = read_lsp_message(&mut reader)?;
    if shutdown.get("id") != Some(&Value::from(2)) {
        return Err(format!("unexpected shutdown response: {}", shutdown));
    }

    write_lsp_message(
        &mut stdin,
        &json!({
            "jsonrpc": "2.0",
            "method": "exit"
        }),
    )?;

    let output = child.wait_with_output().map_err(|e| e.to_string())?;
    if !output.status.success() {
        return Err(String::from_utf8_lossy(&output.stderr).to_string());
    }
    Ok(())
}

#[test]
fn fmt_trims_trailing_spaces() -> Result<(), String> {
    let dir = make_temp_dir()?;
    let file = dir.join("fmt.nx");
    fs::write(&file, "print(1)   \n").map_err(|e| e.to_string())?;

    let result = run_cmd(&["fmt", file.to_str().ok_or("bad path")?], None, None)?;
    if !result.status_ok {
        return Err(result.stderr);
    }

    let formatted = fs::read_to_string(&file).map_err(|e| e.to_string())?;
    if formatted != "print(1)\n" {
        return Err(format!("unexpected format output: {}", formatted));
    }
    Ok(())
}

#[test]
fn bc_upgrade_roundtrip() -> Result<(), String> {
    let dir = make_temp_dir()?;
    let file = dir.join("demo.nx");
    fs::write(&file, "print(1)\n").map_err(|e| e.to_string())?;

    let emit = run_cmd(&["--emit", file.to_str().ok_or("bad path")?], None, None)?;
    if !emit.status_ok {
        return Err(emit.stderr);
    }

    let bc = dir.join("demo.nxbc");
    let out = dir.join("demo_out.nxbc");
    let upgrade = run_cmd(
        &[
            "bc-upgrade",
            bc.to_str().ok_or("bad path")?,
            "--out",
            out.to_str().ok_or("bad path")?,
        ],
        None,
        None,
    )?;
    if !upgrade.status_ok {
        return Err(upgrade.stderr);
    }
    if !out.exists() {
        return Err("expected upgraded bytecode file".into());
    }
    Ok(())
}

#[test]
fn package_manager_add_remove_path() -> Result<(), String> {
    let dir = make_temp_dir()?;
    let toml = dir.join("nexo.toml");
    fs::write(&toml, "").map_err(|e| e.to_string())?;

    let module_dir = dir.join("local_mod");
    fs::create_dir_all(&module_dir).map_err(|e| e.to_string())?;
    fs::write(module_dir.join("mod.nx"), "print(1)\n").map_err(|e| e.to_string())?;

    let add = run_cmd(
        &["add", "--path", module_dir.to_str().ok_or("bad path")?],
        None,
        Some(&dir),
    )?;
    if !add.status_ok {
        return Err(add.stderr);
    }
    let module_target = dir.join("nexo_modules").join("local_mod");
    if !module_target.exists() {
        return Err("expected module to be copied".into());
    }

    let remove = run_cmd(&["remove", "local_mod"], None, Some(&dir))?;
    if !remove.status_ok {
        return Err(remove.stderr);
    }
    if module_target.exists() {
        return Err("expected module to be removed".into());
    }
    Ok(())
}

#[test]
fn py_run_smoke() -> Result<(), String> {
    let python_ok = Command::new("python")
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status();
    if python_ok.is_err() || !python_ok.unwrap().success() {
        return Ok(());
    }

    let dir = make_temp_dir()?;
    let script = dir.join("py_test.py");
    fs::write(&script, "import json\nprint(json.dumps({\"n\": 3}))\n")
        .map_err(|e| e.to_string())?;

    let nx = dir.join("py_run.nx");
    let script_path = script.to_string_lossy().replace('\\', "/");
    fs::write(&nx, format!("print(py_run(\"{}\", null))\n", script_path))
        .map_err(|e| e.to_string())?;

    let result = run_cmd(&[nx.to_str().ok_or("bad path")?], None, None)?;
    if !result.status_ok {
        return Err(result.stderr);
    }
    if !result.stdout.trim().contains("{\"n\": 3}") {
        return Err(format!("unexpected py_run output: {}", result.stdout));
    }
    Ok(())
}

#[test]
fn proc_run_smoke() -> Result<(), String> {
    let dir = make_temp_dir()?;
    let nx = dir.join("proc_run.nx");
    fs::write(
        &nx,
        "res = proc_run(\"echo hi\")\nprint(res[\"code\"])\nprint(res[\"out\"])\n",
    )
    .map_err(|e| e.to_string())?;

    let result = run_cmd(&[nx.to_str().ok_or("bad path")?], None, None)?;
    if !result.status_ok {
        return Err(result.stderr);
    }
    if !result.stdout.contains("0") || !result.stdout.to_lowercase().contains("hi") {
        return Err(format!("unexpected proc_run output: {}", result.stdout));
    }
    Ok(())
}

#[test]
fn http_local_smoke() -> Result<(), String> {
    let listener = TcpListener::bind("127.0.0.1:0").map_err(|e| e.to_string())?;
    let addr = listener.local_addr().map_err(|e| e.to_string())?;

    let handle = thread::spawn(move || run_http_server(listener));

    let dir = make_temp_dir()?;
    let nx = dir.join("http_local.nx");
    fs::write(
        &nx,
        format!(
            "res1 = http_get(\"http://{}:{}/one\")\nprint(res1[\"status\"])\nprint(res1[\"body\"])\nres2 = http_post(\"http://{}:{}/two\", \"ping\", null)\nprint(res2[\"status\"])\nprint(res2[\"body\"])\n",
            addr.ip(),
            addr.port(),
            addr.ip(),
            addr.port()
        ),
    )
    .map_err(|e| e.to_string())?;

    let result = run_cmd(&[nx.to_str().ok_or("bad path")?], None, None)?;
    if !result.status_ok {
        return Err(result.stderr);
    }

    let output = result.stdout.trim();
    if !output.contains("200") || !output.contains("ok-get") || !output.contains("ok-post:") {
        return Err(format!("unexpected http output: {}", result.stdout));
    }

    handle
        .join()
        .map_err(|_| "http server thread panicked".to_string())??;
    Ok(())
}

fn write_lsp_message(stdin: &mut dyn Write, msg: &Value) -> Result<(), String> {
    let content = msg.to_string();
    let header = format!("Content-Length: {}\r\n\r\n", content.len());
    stdin
        .write_all(header.as_bytes())
        .map_err(|e| e.to_string())?;
    stdin
        .write_all(content.as_bytes())
        .map_err(|e| e.to_string())?;
    stdin.flush().map_err(|e| e.to_string())?;
    Ok(())
}

fn read_lsp_message(reader: &mut BufReader<impl Read>) -> Result<Value, String> {
    let mut content_length: Option<usize> = None;
    let mut line = String::new();

    loop {
        line.clear();
        let read = reader.read_line(&mut line).map_err(|e| e.to_string())?;
        if read == 0 {
            return Err("lsp closed".into());
        }
        let line_trim = line.trim_end_matches(['\r', '\n']);
        if line_trim.is_empty() {
            break;
        }
        if let Some(rest) = line_trim.strip_prefix("Content-Length:") {
            content_length = rest.trim().parse::<usize>().ok();
        }
    }

    let len = content_length.ok_or("missing Content-Length")?;
    let mut buf = vec![0u8; len];
    reader.read_exact(&mut buf).map_err(|e| e.to_string())?;
    let msg: Value = serde_json::from_slice(&buf).map_err(|e| e.to_string())?;
    Ok(msg)
}

fn run_http_server(listener: TcpListener) -> Result<(), String> {
    let mut handled = 0;
    for stream in listener.incoming() {
        let mut stream = stream.map_err(|e| e.to_string())?;
        let (method, body) = read_http_request(&mut stream)?;
        let response_body = if method == "POST" {
            format!("ok-post:{}", body)
        } else {
            "ok-get".to_string()
        };
        write_http_response(&mut stream, &response_body)?;
        handled += 1;
        if handled >= 2 {
            break;
        }
    }
    Ok(())
}

fn read_http_request(stream: &mut TcpStream) -> Result<(String, String), String> {
    let mut reader = BufReader::new(stream);
    let mut line = String::new();
    reader.read_line(&mut line).map_err(|e| e.to_string())?;
    let parts: Vec<&str> = line.trim_end().split_whitespace().collect();
    let method = parts.get(0).unwrap_or(&"GET").to_string();

    let mut content_length = 0usize;
    loop {
        line.clear();
        reader.read_line(&mut line).map_err(|e| e.to_string())?;
        let trimmed = line.trim_end_matches(['\r', '\n']);
        if trimmed.is_empty() {
            break;
        }
        if let Some(rest) = trimmed.strip_prefix("Content-Length:") {
            content_length = rest.trim().parse::<usize>().unwrap_or(0);
        }
    }

    let mut body = String::new();
    if content_length > 0 {
        let mut buf = vec![0u8; content_length];
        reader.read_exact(&mut buf).map_err(|e| e.to_string())?;
        body = String::from_utf8_lossy(&buf).to_string();
    }

    Ok((method, body))
}

fn write_http_response(stream: &mut TcpStream, body: &str) -> Result<(), String> {
    let response = format!(
        "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{}",
        body.len(),
        body
    );
    stream
        .write_all(response.as_bytes())
        .map_err(|e| e.to_string())?;
    stream.flush().map_err(|e| e.to_string())?;
    Ok(())
}
