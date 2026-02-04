use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

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

fn run_with_input_with_env(
    bin: &Path,
    args: &[&str],
    input: Option<&str>,
    envs: &[&str],
) -> Result<String, String> {
    let mut cmd = Command::new(bin);
    cmd.args(args)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped());

    for kv in envs {
        if let Some((k, v)) = kv.split_once('=') {
            cmd.env(k, v);
        }
    }

    let input_data = input.map(|s| s.to_string());
    if input_data.is_some() {
        cmd.stdin(std::process::Stdio::piped());
    }

    let mut child = cmd.spawn().map_err(|e| e.to_string())?;

    if let Some(stdin) = input_data {
        if let Some(mut handle) = child.stdin.take() {
            use std::io::Write;
            handle
                .write_all(stdin.as_bytes())
                .map_err(|e| e.to_string())?;
        }
    }

    let output = child.wait_with_output().map_err(|e| e.to_string())?;
    if !output.status.success() {
        return Err(String::from_utf8_lossy(&output.stderr).to_string());
    }

    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}

fn read_text(path: &Path) -> Result<String, String> {
    fs::read_to_string(path).map_err(|e| e.to_string())
}

fn normalize(s: &str) -> String {
    s.replace("\r\n", "\n").trim_end().to_string()
}

fn make_temp_dir() -> Result<PathBuf, String> {
    let base = std::env::temp_dir().join("nexo_tests");
    fs::create_dir_all(&base).map_err(|e| e.to_string())?;

    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|e| e.to_string())?
        .as_millis();
    let dir = base.join(format!("run_{}", now));
    fs::create_dir_all(&dir).map_err(|e| e.to_string())?;
    Ok(dir)
}

fn collect_scripts(dir: &Path) -> Result<Vec<PathBuf>, String> {
    let mut out = Vec::new();
    for entry in fs::read_dir(dir).map_err(|e| e.to_string())? {
        let path = entry.map_err(|e| e.to_string())?.path();
        if path.is_dir() {
            out.extend(collect_scripts(&path)?);
            continue;
        }
        if path.extension().map(|e| e == "nx").unwrap_or(false) {
            out.push(path);
        }
    }
    Ok(out)
}

fn script_key(scripts_dir: &Path, path: &Path) -> Result<String, String> {
    let rel = path.strip_prefix(scripts_dir).map_err(|e| e.to_string())?;
    let mut key = rel.to_string_lossy().replace("\\", "_").replace("/", "_");
    if key.ends_with(".nx") {
        key.truncate(key.len() - 3);
    }
    Ok(key)
}

#[test]
fn golden_scripts_source_and_bytecode() -> Result<(), String> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let scripts_dir = root.join("tests").join("scripts");
    let expected_dir = root.join("tests").join("expected");
    let input_dir = root.join("tests").join("inputs");
    let modules_dir = root.join("tests").join("modules");

    let bin = bin_path();
    let temp_dir = make_temp_dir()?;

    let mut entries: Vec<PathBuf> = collect_scripts(&scripts_dir)?;

    entries.sort();

    let mut temp_paths: Vec<(String, PathBuf)> = Vec::new();
    for script_path in &entries {
        let key = script_key(&scripts_dir, script_path)?;
        let rel = script_path
            .strip_prefix(&scripts_dir)
            .map_err(|e| e.to_string())?;
        let temp_script = temp_dir.join(rel);
        if let Some(parent) = temp_script.parent() {
            fs::create_dir_all(parent).map_err(|e| e.to_string())?;
        }
        let script_src = read_text(script_path)?;
        fs::write(&temp_script, script_src).map_err(|e| e.to_string())?;
        temp_paths.push((key, temp_script));
    }

    if modules_dir.exists() {
        let temp_modules = temp_dir.join("nexo_modules");
        fs::create_dir_all(&temp_modules).map_err(|e| e.to_string())?;
        for entry in fs::read_dir(&modules_dir).map_err(|e| e.to_string())? {
            let path = entry.map_err(|e| e.to_string())?.path();
            if path.extension().map(|e| e == "nx").unwrap_or(false) {
                let name = path
                    .file_name()
                    .and_then(|s| s.to_str())
                    .ok_or("Invalid module name")?;
                let dst = temp_modules.join(name);
                let src = read_text(&path)?;
                fs::write(&dst, src).map_err(|e| e.to_string())?;
            }
        }
    }

    for script_path in entries {
        let key = script_key(&scripts_dir, &script_path)?;
        let expected_path = expected_dir.join(format!("{}.out", key));
        let expected = normalize(&read_text(&expected_path)?);

        let input_path = input_dir.join(format!("{}.in", key));
        let input = if input_path.exists() {
            Some(read_text(&input_path)?)
        } else {
            None
        };

        let temp_script = temp_paths
            .iter()
            .find(|(n, _)| n == &key)
            .map(|(_, p)| p.clone())
            .ok_or("Missing temp script")?;

        let output_src = run_with_input_with_env(
            &bin,
            &[temp_script.to_str().ok_or("Invalid temp path")?],
            input.as_deref(),
            &["NEOX_TEST_ENV=ok"],
        )?;

        let output_src = normalize(&output_src);
        if output_src != expected {
            return Err(format!(
                "Source mismatch for {}: expected '{}' got '{}'",
                key, expected, output_src
            ));
        }

        run_with_input_with_env(
            &bin,
            &["--emit", temp_script.to_str().ok_or("Invalid temp path")?],
            None,
            &["NEOX_TEST_ENV=ok"],
        )?;

        let temp_bc = temp_script.with_extension("nxbc");
        let output_bc = run_with_input_with_env(
            &bin,
            &["run", temp_bc.to_str().ok_or("Invalid temp path")?],
            input.as_deref(),
            &["NEOX_TEST_ENV=ok"],
        )?;

        let output_bc = normalize(&output_bc);
        if output_bc != expected {
            return Err(format!(
                "Bytecode mismatch for {}: expected '{}' got '{}'",
                key, expected, output_bc
            ));
        }
    }

    Ok(())
}
