use std::collections::HashMap;
use std::io::{self, BufRead, BufReader, Read, Write};

use serde_json::{json, Value};

use crate::lint::{lint_source, Severity};
use crate::parser::Parser;
use crate::{ast::ExprKind, ast::Stmt, ast::StmtKind, lexer::Lexer};

pub fn run_lsp() -> Result<(), String> {
    let stdin = io::stdin();
    let mut reader = BufReader::new(stdin.lock());
    let stdout = io::stdout();
    let mut writer = stdout.lock();

    let mut docs: HashMap<String, String> = HashMap::new();

    loop {
        let msg = read_message(&mut reader)?;
        let Some(msg) = msg else { break };
        let method = msg.get("method").and_then(|m| m.as_str()).unwrap_or("");
        let id = msg.get("id").cloned();

        match method {
            "initialize" => {
                let result = json!({
                    "capabilities": {
                        "textDocumentSync": 1,
                        "documentSymbolProvider": true,
                        "hoverProvider": true
                    },
                    "serverInfo": {"name": "nexo", "version": "0.1"}
                });
                if let Some(id) = id {
                    send_response(&mut writer, id, result)?;
                }
            }
            "shutdown" => {
                if let Some(id) = id {
                    send_response(&mut writer, id, json!(null))?;
                }
            }
            "exit" => break,

            "textDocument/didOpen" => {
                if let Some(params) = msg.get("params") {
                    let uri = params["textDocument"]["uri"].as_str().unwrap_or("");
                    let text = params["textDocument"]["text"].as_str().unwrap_or("");
                    docs.insert(uri.to_string(), text.to_string());
                    publish_diagnostics(&mut writer, uri, text)?;
                }
            }
            "textDocument/didChange" => {
                if let Some(params) = msg.get("params") {
                    let uri = params["textDocument"]["uri"].as_str().unwrap_or("");
                    if let Some(changes) = params["contentChanges"].as_array() {
                        if let Some(change) = changes.first() {
                            let text = change["text"].as_str().unwrap_or("");
                            docs.insert(uri.to_string(), text.to_string());
                            publish_diagnostics(&mut writer, uri, text)?;
                        }
                    }
                }
            }
            "textDocument/didClose" => {
                if let Some(params) = msg.get("params") {
                    let uri = params["textDocument"]["uri"].as_str().unwrap_or("");
                    docs.remove(uri);
                    publish_empty_diagnostics(&mut writer, uri)?;
                }
            }
            "textDocument/documentSymbol" => {
                if let Some(id) = id {
                    let params = msg.get("params").cloned().unwrap_or(json!({}));
                    let uri = params["textDocument"]["uri"].as_str().unwrap_or("");
                    let text = docs.get(uri).cloned().unwrap_or_default();
                    let symbols = document_symbols(&text);
                    send_response(&mut writer, id, Value::Array(symbols))?;
                }
            }
            "textDocument/hover" => {
                if let Some(id) = id {
                    let params = msg.get("params").cloned().unwrap_or(json!({}));
                    let uri = params["textDocument"]["uri"].as_str().unwrap_or("");
                    let text = docs.get(uri).cloned().unwrap_or_default();
                    let pos = params["position"].clone();
                    let line = pos["line"].as_u64().unwrap_or(0) as usize;
                    let character = pos["character"].as_u64().unwrap_or(0) as usize;
                    let result = hover_docstring(&text, line, character)
                        .map(Value::Object)
                        .unwrap_or_else(|| json!(null));
                    send_response(&mut writer, id, result)?;
                }
            }
            _ => {
                if let Some(id) = id {
                    send_response(&mut writer, id, json!(null))?;
                }
            }
        }
    }

    Ok(())
}

fn document_symbols(src: &str) -> Vec<Value> {
    let tokens = match Lexer::new(src).tokenize() {
        Ok(t) => t,
        Err(_) => return Vec::new(),
    };
    let ast_result = std::panic::catch_unwind(|| Parser::new_with_source(tokens, src).parse());
    let ast = match ast_result {
        Ok(ast) => ast,
        Err(_) => return Vec::new(),
    };

    let mut symbols = Vec::new();
    for stmt in ast {
        match stmt.kind {
            StmtKind::FuncDef { name, .. } => {
                symbols.push(symbol(name, 12));
            }
            StmtKind::Assign { name, .. } => {
                symbols.push(symbol(name, 13));
            }
            _ => {}
        }
    }
    symbols
}

fn hover_docstring(
    src: &str,
    line0: usize,
    char0: usize,
) -> Option<serde_json::Map<String, Value>> {
    let tokens = Lexer::new(src).tokenize().ok()?;
    let ast_result =
        std::panic::catch_unwind(|| Parser::new_with_source(tokens.clone(), src).parse());
    let ast = match ast_result {
        Ok(ast) => ast,
        Err(_) => return None,
    };
    let (name, start_col, end_col) = find_ident_at(&tokens, line0 + 1, char0 + 1)?;
    let docs = collect_function_docs(&ast);
    let doc = docs.get(&name)?.clone();

    let mut map = serde_json::Map::new();
    map.insert(
        "contents".to_string(),
        json!({"kind": "markdown", "value": doc}),
    );
    map.insert(
        "range".to_string(),
        json!({
            "start": {"line": line0, "character": start_col - 1},
            "end": {"line": line0, "character": end_col}
        }),
    );
    Some(map)
}

fn find_ident_at(
    tokens: &[crate::token::Token],
    line: usize,
    col: usize,
) -> Option<(String, usize, usize)> {
    for token in tokens {
        if token.line != line {
            continue;
        }
        if let crate::token::TokenKind::Ident(name) = &token.kind {
            let len = name.chars().count();
            if len == 0 {
                continue;
            }
            let start = token.col;
            let end = token.col + len - 1;
            if col >= start && col <= end {
                return Some((name.clone(), start, end));
            }
        }
    }
    None
}

fn collect_function_docs(stmts: &[Stmt]) -> HashMap<String, String> {
    let mut out = HashMap::new();
    collect_function_docs_inner(stmts, &mut out);
    out
}

fn collect_function_docs_inner(stmts: &[Stmt], out: &mut HashMap<String, String>) {
    for stmt in stmts {
        if let StmtKind::FuncDef { name, body, .. } = &stmt.kind {
            if let Some(doc) = docstring_from_body(body) {
                out.insert(name.clone(), doc);
            }
            collect_function_docs_inner(body, out);
        }
    }
}

fn docstring_from_body(body: &[Stmt]) -> Option<String> {
    let first = body.first()?;
    if let StmtKind::ExprStmt(expr) = &first.kind {
        if let ExprKind::String(s) = &expr.kind {
            return Some(s.clone());
        }
    }
    None
}

fn symbol(name: String, kind: i32) -> Value {
    json!({
        "name": name,
        "kind": kind,
        "range": range0(),
        "selectionRange": range0()
    })
}

fn publish_diagnostics(writer: &mut dyn Write, uri: &str, src: &str) -> Result<(), String> {
    let lints = lint_source(src, false).unwrap_or_default();
    let mut diags = Vec::new();
    for lint in lints {
        let severity = match lint.severity {
            Severity::Error => 1,
            Severity::Warning => 2,
        };
        diags.push(json!({
            "range": range0(),
            "severity": severity,
            "message": lint.message
        }));
    }
    let params = json!({
        "uri": uri,
        "diagnostics": diags
    });
    send_notification(writer, "textDocument/publishDiagnostics", params)
}

fn publish_empty_diagnostics(writer: &mut dyn Write, uri: &str) -> Result<(), String> {
    let params = json!({
        "uri": uri,
        "diagnostics": []
    });
    send_notification(writer, "textDocument/publishDiagnostics", params)
}

fn range0() -> Value {
    json!({
        "start": {"line": 0, "character": 0},
        "end": {"line": 0, "character": 0}
    })
}

fn send_response(writer: &mut dyn Write, id: Value, result: Value) -> Result<(), String> {
    let msg = json!({
        "jsonrpc": "2.0",
        "id": id,
        "result": result
    });
    send_message(writer, &msg)
}

fn send_notification(writer: &mut dyn Write, method: &str, params: Value) -> Result<(), String> {
    let msg = json!({
        "jsonrpc": "2.0",
        "method": method,
        "params": params
    });
    send_message(writer, &msg)
}

fn send_message(writer: &mut dyn Write, msg: &Value) -> Result<(), String> {
    let content = msg.to_string();
    let header = format!("Content-Length: {}\r\n\r\n", content.len());
    writer
        .write_all(header.as_bytes())
        .map_err(|e| e.to_string())?;
    writer
        .write_all(content.as_bytes())
        .map_err(|e| e.to_string())?;
    writer.flush().map_err(|e| e.to_string())?;
    Ok(())
}

fn read_message(reader: &mut BufReader<impl Read>) -> Result<Option<Value>, String> {
    let mut content_length: Option<usize> = None;
    let mut line = String::new();

    loop {
        line.clear();
        let read = reader.read_line(&mut line).map_err(|e| e.to_string())?;
        if read == 0 {
            return Ok(None);
        }
        let line_trim = line.trim_end_matches(['\r', '\n']);
        if line_trim.is_empty() {
            break;
        }
        if let Some(rest) = line_trim.strip_prefix("Content-Length:") {
            content_length = rest.trim().parse::<usize>().ok();
        }
    }

    let len = content_length.ok_or("Missing Content-Length")?;
    let mut buf = vec![0u8; len];
    reader.read_exact(&mut buf).map_err(|e| e.to_string())?;
    let msg: Value = serde_json::from_slice(&buf).map_err(|e| e.to_string())?;
    Ok(Some(msg))
}
