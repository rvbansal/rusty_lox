use super::opcode::OpCode;
use super::value::Value;

pub type ConstantIndex = u8;

pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<usize>,
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

    pub fn write_op(&mut self, op: OpCode, line: usize) {
        self.write_byte(op.into(), line)
    }

    pub fn try_read_op(&self, index: usize) -> Result<OpCode, u8> {
        let byte = self.code[index];
        OpCode::try_from(byte).map_err(|e| e.number)
    }

    pub fn write_byte(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn read_byte(&self, index: usize) -> u8 {
        self.code[index]
    }

    pub fn write_short(&mut self, short: u16, line: usize) {
        let bytes = short.to_be_bytes();
        self.write_byte(bytes[0], line);
        self.write_byte(bytes[1], line);
    }

    pub fn read_short(&self, index: usize) -> u16 {
        let bytes = [self.code[index], self.code[index + 1]];
        u16::from_be_bytes(bytes)
    }

    pub fn write_op_with_byte(&mut self, op: OpCode, byte: u8, line: usize) {
        self.write_op(op, line);
        self.write_byte(byte, line);
    }

    pub fn write_op_with_short(&mut self, op: OpCode, short: u16, line: usize) {
        self.write_op(op, line);
        self.write_short(short, line);
    }

    pub fn emit_jump(&mut self, op: OpCode, line: usize) -> usize {
        self.write_op_with_short(op, 0xffff, line);
        self.code.len() - 2
    }

    pub fn patch_jump(&mut self, index: usize) {
        let jump_dist = self.code.len() - (index + 2);
        let jump_bytes = u16::try_from(jump_dist)
            .expect("Jump distance too large!")
            .to_be_bytes();

        self.code[index] = jump_bytes[0];
        self.code[index + 1] = jump_bytes[1];
    }

    pub fn emit_loop(&mut self, loop_start: usize, line: usize) {
        let jump_dist = (self.code.len() + 3) - loop_start;
        let jump_dist = u16::try_from(jump_dist).expect("Loop distance too large!");
        self.write_op_with_short(OpCode::Loop, jump_dist, line);
    }

    pub fn add_constant(&mut self, constant: Value) -> ConstantIndex {
        self.constants.push(constant);
        let index = self.constants.len() - 1;
        index
            .try_into()
            .expect("Constant array not large enough to handle this constant.")
    }

    pub fn read_constant(&self, index: ConstantIndex) -> Value {
        self.constants[index as usize].clone()
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
                println!("{:20} {:4} {:?}", $first, $second, $third)
            };
        }

        // Print byte offset and line.
        print!("{:4}", offset);
        if offset == 0 || self.lines[offset] != self.lines[offset - 1] {
            print!("{:4} ", self.lines[offset]);
        } else {
            print!("   | ");
        };

        // Print opcode and operands
        let byte_value = self.code[offset];
        let opcode = match OpCode::try_from(byte_value) {
            Ok(opcode) => opcode,
            Err(_) => {
                println!("Unknown upcode {}.", byte_value);
                return offset + 1;
            }
        };

        match opcode {
            OpCode::True => println!("OP_TRUE"),
            OpCode::False => println!("OP_FALSE"),
            OpCode::Nil => println!("OP_NIL"),
            OpCode::Constant => {
                let index = self.read_byte(offset + 1);
                let constant = self.read_constant(index);
                format_print_three!("OP_CONSTANT", index, constant);
            }
            OpCode::Add => println!("OP_ADD"),
            OpCode::Subtract => println!("OP_SUBTRACT"),
            OpCode::Multiply => println!("OP_MULTIPLY"),
            OpCode::Divide => println!("OP_DIVIDE"),
            OpCode::Negate => println!("OP_NEGATE"),
            OpCode::Not => println!("OP_NOT"),
            OpCode::Equal => println!("OP_EQUAL"),
            OpCode::GreaterThan => println!("OP_GREATER"),
            OpCode::LessThan => println!("OP_LESS"),
            OpCode::DefineGlobal => {
                let index = self.read_byte(offset + 1);
                let constant = self.read_constant(index);
                format_print_three!("OP_DEFINE_GLOBAL", index, constant);
            }
            OpCode::GetGlobal => {
                let index = self.read_byte(offset + 1);
                let constant = self.read_constant(index);
                format_print_three!("OP_GET_GLOBAL", index, constant);
            }
            OpCode::SetGlobal => {
                let index = self.read_byte(offset + 1);
                format_print_two!("OP_SET_GLOBAL", index);
            }
            OpCode::GetLocal => {
                let index = self.read_byte(offset + 1);
                format_print_two!("OP_GET_LOCAL", index);
            }
            OpCode::SetLocal => {
                let index = self.read_byte(offset + 1);
                format_print_two!("OP_SET_LOCAL", index);
            }
            OpCode::Jump => {
                let dist_in_bytes = self.read_short(offset + 1);
                format_print_two!("OP_JUMP", dist_in_bytes);
            }
            OpCode::JumpIfFalse => {
                let dist_in_bytes = self.read_short(offset + 1);
                format_print_two!("OP_JUMP_IF_FALSE", dist_in_bytes);
            }
            OpCode::Loop => {
                let dist_in_bytes = self.read_short(offset + 1);
                format_print_two!("OP_LOOP", dist_in_bytes);
            }
            OpCode::Return => println!("OP_RETURN"),
            OpCode::Print => println!("OP_PRINT"),
            OpCode::Pop => println!("OP_POP"),
        };

        offset + opcode.operand_size_in_bytes() + 1
    }
}
