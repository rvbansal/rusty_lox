use num_enum::{IntoPrimitive, TryFromPrimitive};

// Index of local var in stack is stored in u8 for GET_LOCAL instruction.
pub const MAX_LOCAL_VARS: usize = 256;
pub type LocalIndex = u8;

pub const MAX_UPVALUES: usize = 256;
pub type UpvalueIndex = u8;

pub type ConstantIndex = u8;

pub const UPVALUE_IMMEDIATE_VALUE: u8 = 1;
pub const UPVALUE_RECURSIVE_VALUE: u8 = 0;

#[derive(Debug)]
pub enum OpCodeError {
    UnknownOpCode(u8),
    OutOfBounds,
    UnknownUpvalueLocation(u8),
}

#[derive(Debug)]
pub enum StructOpCode {
    True,
    False,
    Nil,
    Constant(ConstantIndex),
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Not,
    Equal,
    GreaterThan,
    LessThan,
    DefineGlobal(ConstantIndex),
    GetGlobal(ConstantIndex),
    SetGlobal(ConstantIndex),
    GetLocal(u8),
    SetLocal(u8),
    Jump(u16),
    JumpIfFalse(u16),
    Loop(u16),
    MakeClosure(u8),
    GetUpvalue(u8),
    SetUpvalue(u8),
    CloseUpvalue,
    MakeClass(ConstantIndex),
    GetProperty(ConstantIndex),
    SetProperty(ConstantIndex),
    MakeMethod(ConstantIndex),
    Invoke(ConstantIndex, u8),
    Inherit,
    GetSuper(ConstantIndex),
    InvokeSuper(ConstantIndex, u8),
    Call(u8),
    Return,
    Print,
    Pop,
}

#[derive(Copy, Clone, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    True,
    False,
    Nil,
    Constant,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Not,
    Equal,
    GreaterThan,
    LessThan,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    Jump,
    JumpIfFalse,
    Loop,
    MakeClosure,
    GetUpvalue,
    SetUpvalue,
    CloseUpvalue,
    MakeClass,
    GetProperty,
    SetProperty,
    MakeMethod,
    Invoke,
    Inherit,
    GetSuper,
    InvokeSuper,
    Call,
    Return,
    Print,
    Pop,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum UpvalueLocation {
    Immediate(LocalIndex),
    Recursive(UpvalueIndex),
}

impl StructOpCode {
    pub fn encode(code: &mut Vec<u8>, op: Self) {
        fn push_op(code: &mut Vec<u8>, opcode: OpCode) {
            code.push(opcode.into());
        }

        fn push_op_byte(code: &mut Vec<u8>, opcode: OpCode, byte: u8) {
            code.push(opcode.into());
            code.push(byte);
        }

        fn push_op_short(code: &mut Vec<u8>, opcode: OpCode, short: u16) {
            let bytes = short.to_be_bytes();
            code.push(opcode.into());
            code.extend_from_slice(&bytes);
        }

        fn push_op_two_bytes(code: &mut Vec<u8>, opcode: OpCode, byte1: u8, byte2: u8) {
            code.push(opcode.into());
            code.push(byte1);
            code.push(byte2);
        }

        match op {
            StructOpCode::True => push_op(code, OpCode::True),
            StructOpCode::False => push_op(code, OpCode::False),
            StructOpCode::Nil => push_op(code, OpCode::Nil),
            StructOpCode::Constant(index) => push_op_byte(code, OpCode::Constant, index),
            StructOpCode::Add => push_op(code, OpCode::Add),
            StructOpCode::Subtract => push_op(code, OpCode::Subtract),
            StructOpCode::Multiply => push_op(code, OpCode::Multiply),
            StructOpCode::Divide => push_op(code, OpCode::Divide),
            StructOpCode::Negate => push_op(code, OpCode::Negate),
            StructOpCode::Not => push_op(code, OpCode::Not),
            StructOpCode::Equal => push_op(code, OpCode::Equal),
            StructOpCode::GreaterThan => push_op(code, OpCode::GreaterThan),
            StructOpCode::LessThan => push_op(code, OpCode::LessThan),
            StructOpCode::DefineGlobal(index) => push_op_byte(code, OpCode::DefineGlobal, index),
            StructOpCode::GetGlobal(index) => push_op_byte(code, OpCode::GetGlobal, index),
            StructOpCode::SetGlobal(index) => push_op_byte(code, OpCode::SetGlobal, index),
            StructOpCode::GetLocal(index) => push_op_byte(code, OpCode::GetLocal, index),
            StructOpCode::SetLocal(index) => push_op_byte(code, OpCode::SetLocal, index),
            StructOpCode::Jump(offset) => push_op_short(code, OpCode::Jump, offset),
            StructOpCode::JumpIfFalse(offset) => push_op_short(code, OpCode::JumpIfFalse, offset),
            StructOpCode::Loop(offset) => push_op_short(code, OpCode::Loop, offset),
            StructOpCode::MakeClosure(index) => push_op_byte(code, OpCode::MakeClosure, index),
            StructOpCode::GetUpvalue(index) => push_op_byte(code, OpCode::GetUpvalue, index),
            StructOpCode::SetUpvalue(index) => push_op_byte(code, OpCode::SetUpvalue, index),
            StructOpCode::CloseUpvalue => push_op(code, OpCode::CloseUpvalue),
            StructOpCode::MakeClass(index) => push_op_byte(code, OpCode::MakeClass, index),
            StructOpCode::GetProperty(index) => push_op_byte(code, OpCode::GetProperty, index),
            StructOpCode::SetProperty(index) => push_op_byte(code, OpCode::SetProperty, index),
            StructOpCode::MakeMethod(index) => push_op_byte(code, OpCode::MakeMethod, index),
            StructOpCode::Invoke(index, num_args) => {
                push_op_two_bytes(code, OpCode::Invoke, index, num_args)
            }
            StructOpCode::Inherit => push_op(code, OpCode::Inherit),
            StructOpCode::GetSuper(index) => push_op_byte(code, OpCode::GetSuper, index),
            StructOpCode::InvokeSuper(index, num_args) => {
                push_op_two_bytes(code, OpCode::InvokeSuper, index, num_args)
            }
            StructOpCode::Call(num_args) => push_op_byte(code, OpCode::Call, num_args),
            StructOpCode::Return => push_op(code, OpCode::Return),
            StructOpCode::Print => push_op(code, OpCode::Print),
            StructOpCode::Pop => push_op(code, OpCode::Pop),
        }
    }

    pub fn decode(code: &[u8], offset: usize) -> Result<(StructOpCode, usize), OpCodeError> {
        macro_rules! read {
            ($opcode: ident) => {
                Ok((StructOpCode::$opcode, offset + 1))
            };
        }

        macro_rules! read_with_byte {
            ($opcode: ident) => {
                match code.get(offset + 1) {
                    Some(byte) => {
                        let op = StructOpCode::$opcode(*byte);
                        Ok((op, offset + 2))
                    }
                    None => Err(OpCodeError::OutOfBounds),
                }
            };
        }

        macro_rules! read_with_short {
            ($opcode: ident) => {
                match code.get(offset + 1..offset + 3) {
                    Some(bytes) => {
                        let bytes: [u8; 2] = bytes.try_into().unwrap();
                        let short = u16::from_be_bytes(bytes);
                        let op = StructOpCode::$opcode(short);
                        Ok((op, offset + 3))
                    }
                    None => Err(OpCodeError::OutOfBounds),
                }
            };
        }

        macro_rules! read_with_two_bytes {
            ($opcode: ident) => {
                match code.get(offset + 1..offset + 3) {
                    Some(bytes) => {
                        let op = StructOpCode::$opcode(bytes[0], bytes[1]);
                        Ok((op, offset + 3))
                    }
                    None => Err(OpCodeError::OutOfBounds),
                }
            };
        }

        let opcode = match code.get(offset).copied() {
            Some(byte) => match OpCode::try_from(byte) {
                Ok(x) => x,
                Err(_) => return Err(OpCodeError::UnknownOpCode(byte)),
            },
            None => return Err(OpCodeError::OutOfBounds),
        };

        match opcode {
            OpCode::True => read!(True),
            OpCode::False => read!(False),
            OpCode::Nil => read!(Nil),
            OpCode::Constant => read_with_byte!(Constant),
            OpCode::Add => read!(Add),
            OpCode::Subtract => read!(Subtract),
            OpCode::Multiply => read!(Multiply),
            OpCode::Divide => read!(Divide),
            OpCode::Negate => read!(Negate),
            OpCode::Not => read!(Not),
            OpCode::Equal => read!(Equal),
            OpCode::GreaterThan => read!(GreaterThan),
            OpCode::LessThan => read!(LessThan),
            OpCode::DefineGlobal => read_with_byte!(DefineGlobal),
            OpCode::GetGlobal => read_with_byte!(GetGlobal),
            OpCode::SetGlobal => read_with_byte!(SetGlobal),
            OpCode::GetLocal => read_with_byte!(GetLocal),
            OpCode::SetLocal => read_with_byte!(SetLocal),
            OpCode::Jump => read_with_short!(Jump),
            OpCode::JumpIfFalse => read_with_short!(JumpIfFalse),
            OpCode::Loop => read_with_short!(Loop),
            OpCode::MakeClosure => read_with_byte!(MakeClosure),
            OpCode::GetUpvalue => read_with_byte!(GetUpvalue),
            OpCode::SetUpvalue => read_with_byte!(SetUpvalue),
            OpCode::CloseUpvalue => read!(CloseUpvalue),
            OpCode::MakeClass => read_with_byte!(MakeClass),
            OpCode::GetProperty => read_with_byte!(GetProperty),
            OpCode::SetProperty => read_with_byte!(SetProperty),
            OpCode::MakeMethod => read_with_byte!(MakeMethod),
            OpCode::Invoke => read_with_two_bytes!(Invoke),
            OpCode::Inherit => read!(Inherit),
            OpCode::GetSuper => read_with_byte!(GetSuper),
            OpCode::InvokeSuper => read_with_two_bytes!(InvokeSuper),
            OpCode::Call => read_with_byte!(Call),
            OpCode::Return => read!(Return),
            OpCode::Print => read!(Print),
            OpCode::Pop => read!(Pop),
        }
    }
}

impl UpvalueLocation {
    pub fn encode(code: &mut Vec<u8>, location: Self) {
        match location {
            UpvalueLocation::Immediate(index) => {
                code.push(UPVALUE_IMMEDIATE_VALUE);
                code.push(index);
            }
            UpvalueLocation::Recursive(index) => {
                code.push(UPVALUE_RECURSIVE_VALUE);
                code.push(index);
            }
        }
    }

    pub fn decode(code: &[u8], offset: usize) -> Result<UpvalueLocation, OpCodeError> {
        let location = code.get(offset).ok_or(OpCodeError::OutOfBounds)?;
        let index = code.get(offset + 1).ok_or(OpCodeError::OutOfBounds)?;
        match *location {
            UPVALUE_IMMEDIATE_VALUE => Ok(UpvalueLocation::Immediate(*index)),
            UPVALUE_RECURSIVE_VALUE => Ok(UpvalueLocation::Recursive(*index)),
            _ => Err(OpCodeError::UnknownUpvalueLocation(*location)),
        }
    }
}
