use std::fmt;
use std::rc::Rc;

use super::errors::{CompilerError, CompilerResult};
use super::opcode::{ConstantIndex, OpCodeError, StructOpCode, UpvalueLocation};
use super::string_interner::StringIntern;
use std::convert::TryInto;

#[derive(Clone)]
pub enum ChunkConstant {
    Number(f64),
    String(StringIntern),
    FnTemplate {
        name: StringIntern,
        arity: usize,
        chunk: Rc<Chunk>,
        upvalue_count: usize,
    },
}

#[derive(Debug)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<ChunkConstant>,
    lines: Vec<usize>,
}

impl fmt::Debug for ChunkConstant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ChunkConstant::Number(n) => write!(f, "{}", n),
            ChunkConstant::String(s) => write!(f, "\"{}\"", s),
            ChunkConstant::FnTemplate { name, .. } => write!(f, "<fn {}>", name),
        }
    }
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: vec![],
            constants: vec![],
            lines: vec![],
        }
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn is_empty(&self) -> bool {
        self.code.len() == 0
    }

    pub fn write_op(&mut self, op: StructOpCode, line: usize) {
        let orig_len = self.len();
        StructOpCode::encode(&mut self.code, op);
        let new_len = self.len();

        let delta_len = new_len - orig_len;
        self.lines.reserve(delta_len);
        for _ in 0..delta_len {
            self.lines.push(line);
        }
    }

    pub fn patch_byte(&mut self, offset: usize, byte: u8) -> Result<(), OpCodeError> {
        match self.code.get_mut(offset) {
            Some(slot) => {
                *slot = byte;
                Ok(())
            }
            None => Err(OpCodeError::OutOfBounds),
        }
    }

    pub fn patch_short(&mut self, offset: usize, short: u16) -> Result<(), OpCodeError> {
        for (i, byte) in short.to_be_bytes().iter().copied().enumerate() {
            self.patch_byte(offset + i, byte)?;
        }
        Ok(())
    }

    pub fn try_read_op(&self, offset: usize) -> Result<(StructOpCode, usize), OpCodeError> {
        StructOpCode::decode(&self.code, offset)
    }

    pub fn add_constant(&mut self, constant: ChunkConstant) -> CompilerResult<ConstantIndex> {
        self.constants.push(constant);
        let index = self.constants.len() - 1;
        index
            .try_into()
            .map_err(|_| CompilerError::TooManyConstants)
    }

    pub fn read_constant(&self, index: ConstantIndex) -> ChunkConstant {
        self.constants[index as usize].clone()
    }

    pub fn write_upvalue(&mut self, location: UpvalueLocation, line: usize) {
        UpvalueLocation::encode(&mut self.code, location);
        self.lines.push(line);
        self.lines.push(line);
    }

    pub fn try_read_upvalue(&self, offset: usize) -> Result<UpvalueLocation, OpCodeError> {
        UpvalueLocation::decode(&self.code, offset)
    }

    pub fn get_line(&self, index: usize) -> usize {
        self.lines[index]
    }

    pub fn disassemble(&self, name: &str) {
        println!("=== {} ===", name);

        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_at_offset(offset);
        }
    }

    pub fn disassemble_at_offset(&self, offset: usize) -> usize {
        // Macro rules.
        macro_rules! format_print_two {
            ($first:expr, $second:expr) => {
                println!("{:20} {:4}", $first, $second)
            };
        }

        macro_rules! format_print_three {
            ($first:expr, $second:expr, $third:expr) => {
                println!("{:20} {:04?} {:?}", $first, $second, $third)
            };
        }

        macro_rules! format_print_with_constant {
            ($op:expr, $index:expr) => {{
                let constant = self.read_constant($index);
                format_print_three!($op, $index, constant);
            }};
        }

        // Print byte offset and line.
        print!("{:4}", offset);
        if offset == 0 || self.lines[offset] != self.lines[offset - 1] {
            print!("{:4} ", self.lines[offset]);
        } else {
            print!("   | ");
        };

        // Print opcode and operands
        let (opcode, new_offset) = match StructOpCode::decode(&self.code, offset) {
            Ok(t) => t,
            Err(_) => {
                println!("Unknown upcode {}.", self.code[offset]);
                return offset + 1;
            }
        };

        match opcode {
            StructOpCode::True => println!("OP_TRUE"),
            StructOpCode::False => println!("OP_FALSE"),
            StructOpCode::Nil => println!("OP_NIL"),
            StructOpCode::Constant(index) => format_print_with_constant!("OP_CONSTANT", index),
            StructOpCode::Add => println!("OP_ADD"),
            StructOpCode::Subtract => println!("OP_SUBTRACT"),
            StructOpCode::Multiply => println!("OP_MULTIPLY"),
            StructOpCode::Divide => println!("OP_DIVIDE"),
            StructOpCode::Negate => println!("OP_NEGATE"),
            StructOpCode::Not => println!("OP_NOT"),
            StructOpCode::Equal => println!("OP_EQUAL"),
            StructOpCode::GreaterThan => println!("OP_GREATER"),
            StructOpCode::LessThan => println!("OP_LESS"),
            StructOpCode::DefineGlobal(index) => {
                format_print_with_constant!("OP_DEFINE_GLOBAL", index)
            }
            StructOpCode::GetGlobal(index) => format_print_with_constant!("OP_GET_GLOBAL", index),
            StructOpCode::SetGlobal(index) => format_print_with_constant!("OP_SET_GLOBAL", index),
            StructOpCode::GetLocal(index) => format_print_two!("OP_GET_LOCAL", index),
            StructOpCode::SetLocal(index) => format_print_two!("OP_SET_LOCAL", index),
            StructOpCode::Jump(distance) => format_print_two!("OP_JUMP", distance),
            StructOpCode::JumpIfFalse(distance) => {
                format_print_two!("OP_JUMP_IF_FALSE", distance)
            }
            StructOpCode::Loop(distance) => format_print_two!("OP_LOOP", distance),
            StructOpCode::MakeClosure(index) => {
                let constant = self.read_constant(index);
                let upvalue_count = match constant {
                    ChunkConstant::FnTemplate { upvalue_count, .. } => upvalue_count,
                    _ => panic!("Disassembler expected function template value."),
                };
                format_print_three!("OP_MAKE_CLOSURE", index, constant);

                for i in 0..upvalue_count {
                    print!("{:37}: ", i);
                    match UpvalueLocation::decode(&self.code, offset + 2 + 2 * i) {
                        Ok(location) => match location {
                            UpvalueLocation::Immediate(index) => println!("local   #{}", index),
                            UpvalueLocation::Recursive(index) => println!("upvalue #{}", index),
                        },
                        Err(_) => println!("?       #{}", index),
                    }
                }
            }
            StructOpCode::GetUpvalue(index) => format_print_two!("OP_GET_UPVALUE", index),
            StructOpCode::SetUpvalue(index) => format_print_two!("OP_SET_UPVALUE", index),
            StructOpCode::CloseUpvalue => println!("OP_CLOSE_UPVALUE"),
            StructOpCode::MakeClass(index) => format_print_with_constant!("OP_MAKE_CLASS", index),
            StructOpCode::GetProperty(index) => {
                format_print_with_constant!("OP_GET_PROPERTY", index)
            }
            StructOpCode::SetProperty(index) => {
                format_print_with_constant!("OP_SET_PROPERTY", index)
            }
            StructOpCode::MakeMethod(index) => format_print_with_constant!("OP_MAKE_METHOD", index),
            StructOpCode::Invoke(index, num_args) => {
                let method_name = self.read_constant(index);
                format_print_three!("OP_INVOKE", method_name, num_args);
            }
            StructOpCode::Inherit => println!("OP_INHERIT"),
            StructOpCode::GetSuper(index) => format_print_with_constant!("OP_GET_SUPER", index),
            StructOpCode::InvokeSuper(index, num_args) => {
                let method_name = self.read_constant(index);
                format_print_three!("OP_INVOKE", method_name, num_args);
            }
            StructOpCode::Call(num_args) => format_print_two!("OP_CALL", num_args),
            StructOpCode::Return => println!("OP_RETURN"),
            StructOpCode::Print => println!("OP_PRINT"),
            StructOpCode::Pop => println!("OP_POP"),
        };

        new_offset
    }
}
