use std::convert::TryFrom;

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
    PushConst = 0,
    Pop = 1,

    Add = 2,
    Greater = 3,

    And = 4,
    Or = 5,
    Not = 6,

    LoadGlobal = 7,
    StoreGlobal = 8,

    LoadLocal = 9,
    StoreLocal = 10,

    CallBuiltin = 11,
    CallFunction = 12,

    JumpIfFalse = 13,
    Jump = 14,

    Return = 15,

    // Added: richer comparisons
    GreaterEq = 16,
    Less = 17,
    LessEq = 18,
    Equal = 19,
    NotEqual = 20,

    BuildArray = 21,
    Index = 22,
    SetIndex = 23,

    LoadUpvalue = 24,
    MakeClosure = 25,

    Mul = 26,
    BuildMap = 27,

    TryStart = 28,
    TryEnd = 29,
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(OpCode::PushConst),
            1 => Ok(OpCode::Pop),

            2 => Ok(OpCode::Add),
            3 => Ok(OpCode::Greater),

            4 => Ok(OpCode::And),
            5 => Ok(OpCode::Or),
            6 => Ok(OpCode::Not),

            7 => Ok(OpCode::LoadGlobal),
            8 => Ok(OpCode::StoreGlobal),

            9 => Ok(OpCode::LoadLocal),
            10 => Ok(OpCode::StoreLocal),

            11 => Ok(OpCode::CallBuiltin),
            12 => Ok(OpCode::CallFunction),

            13 => Ok(OpCode::JumpIfFalse),
            14 => Ok(OpCode::Jump),

            15 => Ok(OpCode::Return),

            16 => Ok(OpCode::GreaterEq),
            17 => Ok(OpCode::Less),
            18 => Ok(OpCode::LessEq),
            19 => Ok(OpCode::Equal),
            20 => Ok(OpCode::NotEqual),

            21 => Ok(OpCode::BuildArray),
            22 => Ok(OpCode::Index),
            23 => Ok(OpCode::SetIndex),

            24 => Ok(OpCode::LoadUpvalue),
            25 => Ok(OpCode::MakeClosure),
            26 => Ok(OpCode::Mul),
            27 => Ok(OpCode::BuildMap),
            28 => Ok(OpCode::TryStart),
            29 => Ok(OpCode::TryEnd),

            _ => Err(()),
        }
    }
}
