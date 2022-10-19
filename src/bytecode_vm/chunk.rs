use super::opcode::OpCode;
use super::value::Value;

pub type ConstantIndex = u8;

pub struct Chunk {
    pub code: Vec<u8>,
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

    pub fn write_byte(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
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

    pub fn write_instruction(&mut self, instruction: OpCode, line: usize) {
        self.write_byte(instruction.into(), line);
    }

    pub fn disassemble(&self, name: &str) {
        println!("=== {} ===", name);

        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_at_offset(offset);
        }
    }

    pub fn disassemble_at_offset(&self, offset: usize) -> usize {
        // Print byte offset and line
        print!("{:04}", offset);
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
                let index = self.code[offset + 1];
                let constant = self.read_constant(index);
                println!("OP_CONSTANT {:4} {:?}", index, constant);
            }
            OpCode::Return => println!("OP_RETURN"),
            OpCode::Add => println!("OP_ADD"),
            OpCode::Subtract => println!("OP_SUBTRACT"),
            OpCode::Multiply => println!("OP_MULTIPLY"),
            OpCode::Divide => println!("OP_DIVIDE"),
            OpCode::Negate => println!("OP_NEGATE"),
            OpCode::Not => println!("OP_NOT"),
            OpCode::Equal => println!("OP_EQUAL"),
            OpCode::GreaterThan => println!("OP_GREATER"),
            OpCode::LessThan => println!("OP_LESS"),
        };

        offset + opcode.num_operands() + 1
    }
}
