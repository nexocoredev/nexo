use std::fs::File;
use std::io::{Read, Write};

use crate::model::{ChunkDef, ConstValue, FunctionDef, Instruction, Program};
use crate::opcode::OpCode;

const MAGIC: &[u8; 4] = b"NXBC";
const VERSION: u16 = 9;

// Flags reservados para futuro (opt/debug/compression/etc.)
const FLAGS_NONE: u16 = 0;

// =======================
// WRITE
// =======================

pub fn write_program(program: &Program, path: &str) -> std::io::Result<()> {
    let mut file = File::create(path)?;

    // header
    file.write_all(MAGIC)?;
    file.write_all(&VERSION.to_le_bytes())?;
    file.write_all(&FLAGS_NONE.to_le_bytes())?;

    write_function(&mut file, &program.entry)
}

fn write_function(file: &mut File, func: &FunctionDef) -> std::io::Result<()> {
    write_string(file, &func.name)?;
    write_u32(file, func.arity as u32)?;
    write_u32(file, func.local_count as u32)?;
    write_chunk(file, &func.chunk)
}

fn write_chunk(file: &mut File, chunk: &ChunkDef) -> std::io::Result<()> {
    write_u32(file, chunk.constants.len() as u32)?;
    for c in &chunk.constants {
        write_const(file, c)?;
    }

    write_u32(file, chunk.code.len() as u32)?;
    for i in &chunk.code {
        file.write_all(&[i.op as u8])?;
        file.write_all(&i.a.to_le_bytes())?;
        file.write_all(&i.b.to_le_bytes())?;
        let (sl, sc, el, ec) = if let Some(span) = i.span {
            (
                span.start_line as u32,
                span.start_col as u32,
                span.end_line as u32,
                span.end_col as u32,
            )
        } else {
            (0, 0, 0, 0)
        };
        write_u32(file, sl)?;
        write_u32(file, sc)?;
        write_u32(file, el)?;
        write_u32(file, ec)?;
    }

    Ok(())
}

fn write_const(file: &mut File, c: &ConstValue) -> std::io::Result<()> {
    match c {
        ConstValue::Null => file.write_all(&[0]),
        ConstValue::Bool(b) => {
            file.write_all(&[1])?;
            file.write_all(&[*b as u8])
        }
        ConstValue::Number(n) => {
            file.write_all(&[2])?;
            file.write_all(&n.to_le_bytes())
        }
        ConstValue::String(s) => {
            file.write_all(&[3])?;
            write_string(file, s)
        }
        ConstValue::Function(f) => {
            file.write_all(&[4])?;
            write_function(file, f)
        }
        ConstValue::Array(items) => {
            file.write_all(&[5])?;
            write_u32(file, items.len() as u32)?;
            for item in items {
                write_const(file, item)?;
            }
            Ok(())
        }
        ConstValue::Map(_) => Err(err_invalid("Cannot serialize map")),
        ConstValue::Closure(_) => Err(err_invalid("Cannot serialize closure")),
        ConstValue::FfiHandle(_) => Err(err_invalid("Cannot serialize ffi handle")),
    }
}

// =======================
// READ
// =======================

pub fn read_program(path: &str) -> std::io::Result<Program> {
    let mut file = File::open(path)?;

    // magic
    let mut magic = [0u8; 4];
    file.read_exact(&mut magic)?;
    if &magic != MAGIC {
        return Err(err_invalid("Invalid NXBC file (bad magic)"));
    }

    // version
    let version = read_u16(&mut file)?;
    if version > VERSION || version < 8 {
        return Err(err_invalid(&format!(
            "Unsupported NXBC version {} (expected <= {})",
            version, VERSION
        )));
    }

    // flags
    let flags = read_u16(&mut file)?;
    if flags != FLAGS_NONE {
        return Err(err_invalid(&format!("Unsupported NXBC flags {}", flags)));
    }

    let entry = read_function(&mut file, version)?;
    Ok(Program { entry })
}

pub fn read_version(path: &str) -> std::io::Result<u16> {
    let mut file = File::open(path)?;
    let mut magic = [0u8; 4];
    file.read_exact(&mut magic)?;
    if &magic != MAGIC {
        return Err(err_invalid("Invalid NXBC file (bad magic)"));
    }
    read_u16(&mut file)
}

pub fn current_version() -> u16 {
    VERSION
}

fn read_function(file: &mut File, version: u16) -> std::io::Result<FunctionDef> {
    let name = read_string(file)?;
    let arity = read_u32(file)? as usize;
    let locals = read_u32(file)? as usize;
    let chunk = read_chunk(file, version)?;

    Ok(FunctionDef {
        name,
        arity,
        local_count: locals,
        chunk,
    })
}

fn read_chunk(file: &mut File, version: u16) -> std::io::Result<ChunkDef> {
    let const_count = read_u32(file)? as usize;
    let mut constants = Vec::with_capacity(const_count);
    for _ in 0..const_count {
        constants.push(read_const(file, version)?);
    }

    let code_count = read_u32(file)? as usize;
    let mut code = Vec::with_capacity(code_count);
    for _ in 0..code_count {
        let opcode = read_opcode(file)?;

        let mut a = [0u8; 4];
        let mut b = [0u8; 4];
        file.read_exact(&mut a)?;
        file.read_exact(&mut b)?;

        let span = if version >= 9 {
            let sl = read_u32(file)?;
            let sc = read_u32(file)?;
            let el = read_u32(file)?;
            let ec = read_u32(file)?;
            if sl == 0 && sc == 0 && el == 0 && ec == 0 {
                None
            } else {
                Some(crate::ast::Span {
                    start_line: sl as usize,
                    start_col: sc as usize,
                    end_line: el as usize,
                    end_col: ec as usize,
                })
            }
        } else {
            None
        };

        code.push(Instruction {
            op: opcode,
            a: i32::from_le_bytes(a),
            b: i32::from_le_bytes(b),
            span,
        });
    }

    Ok(ChunkDef { constants, code })
}

fn read_opcode(file: &mut File) -> std::io::Result<OpCode> {
    let mut opb = [0u8; 1];
    file.read_exact(&mut opb)?;

    OpCode::try_from(opb[0]).map_err(|_| err_invalid("Invalid opcode"))
}

fn read_const(file: &mut File, version: u16) -> std::io::Result<ConstValue> {
    let mut tag = [0u8; 1];
    file.read_exact(&mut tag)?;

    match tag[0] {
        0 => Ok(ConstValue::Null),
        1 => {
            let mut b = [0u8; 1];
            file.read_exact(&mut b)?;
            Ok(ConstValue::Bool(b[0] != 0))
        }
        2 => {
            let mut n = [0u8; 8];
            file.read_exact(&mut n)?;
            Ok(ConstValue::Number(f64::from_le_bytes(n)))
        }
        3 => Ok(ConstValue::String(read_string(file)?)),
        4 => Ok(ConstValue::Function(read_function(file, version)?)),
        5 => {
            let len = read_u32(file)? as usize;
            let mut items = Vec::with_capacity(len);
            for _ in 0..len {
                items.push(read_const(file, version)?);
            }
            Ok(ConstValue::Array(items))
        }
        _ => Err(err_invalid("Invalid constant tag")),
    }
}

// =======================
// IO HELPERS
// =======================

fn err_invalid(msg: &str) -> std::io::Error {
    std::io::Error::new(std::io::ErrorKind::InvalidData, msg.to_string())
}

fn write_u32(file: &mut File, v: u32) -> std::io::Result<()> {
    file.write_all(&v.to_le_bytes())
}

fn read_u32(file: &mut File) -> std::io::Result<u32> {
    let mut b = [0u8; 4];
    file.read_exact(&mut b)?;
    Ok(u32::from_le_bytes(b))
}

fn write_string(file: &mut File, s: &str) -> std::io::Result<()> {
    write_u32(file, s.len() as u32)?;
    file.write_all(s.as_bytes())
}

fn read_string(file: &mut File) -> std::io::Result<String> {
    let len = read_u32(file)? as usize;
    let mut buf = vec![0u8; len];
    file.read_exact(&mut buf)?;
    Ok(String::from_utf8_lossy(&buf).to_string())
}

fn read_u16(file: &mut File) -> std::io::Result<u16> {
    let mut b = [0u8; 2];
    file.read_exact(&mut b)?;
    Ok(u16::from_le_bytes(b))
}
