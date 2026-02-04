use std::collections::HashMap;

use crate::ast::Span;
use crate::opcode::OpCode;

#[derive(Debug, Clone)]
pub enum ConstValue {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Function(FunctionDef),
    Array(Vec<ConstValue>),
    Closure(ClosureDef),
    Map(HashMap<MapKey, ConstValue>),
    FfiHandle(usize),
}

#[derive(Debug, Clone)]
pub struct ClosureDef {
    pub function: FunctionDef,
    pub upvalues: Vec<ConstValue>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MapKey {
    String(String),
    Number(u64),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub op: OpCode,
    pub a: i32,
    pub b: i32,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct ChunkDef {
    pub constants: Vec<ConstValue>,
    pub code: Vec<Instruction>,
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub arity: usize,
    pub local_count: usize,
    pub chunk: ChunkDef,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub entry: FunctionDef,
}
