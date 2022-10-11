use crate::cpu::AddressingMode;
use std::collections::HashMap;

pub struct OpCode {
    pub code: u8,
    pub instruction: &'static str,
    pub len: u8,
    pub cycles: u8,
    pub mode: AddressingMode,
}

impl OpCode {
    fn new(code: u8, instruction: &'static str, len: u8, cycles: u8, mode: AddressingMode) -> Self {
        OpCode {
            code,
            instruction,
            len,
            cycles,
            mode,
        }
    }
}

lazy_static! {
    pub static ref CPU_OPS_CODES: Vec<OpCode> = vec![
        OpCode::new(0x00, "BRK", 1, 7, AddressingMode::None),
        OpCode::new(0xAA, "TAX", 1, 2, AddressingMode::None),
        OpCode::new(0xA8, "TAY", 1, 2, AddressingMode::None),
        OpCode::new(0xBA, "TSX", 1, 2, AddressingMode::None),
        OpCode::new(0x8A, "TXA", 1, 2, AddressingMode::None),
        OpCode::new(0x9A, "TXS", 1, 2, AddressingMode::None),
        OpCode::new(0x98, "TYA", 1, 2, AddressingMode::None),
        
        OpCode::new(0x69, "ADC", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x65, "ADC", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x75, "ADC", 2, 4, AddressingMode::ZeroPageX),
        OpCode::new(0x6D, "ADC", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x7D, "ADC", 3, 4/* +1 if page crossed */, AddressingMode::AbsoluteX),
        OpCode::new(0x79, "ADC", 3, 4/* +1 if page crossed */, AddressingMode::AbsoluteY),
        OpCode::new(0x61, "ADC", 2, 6, AddressingMode::IndirectX),
        OpCode::new(0x71, "ADC", 2, 5/* +1 if page crossed */, AddressingMode::IndirectY),

        OpCode::new(0x29, "AND", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x25, "AND", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x35, "AND", 2, 4, AddressingMode::ZeroPageX),
        OpCode::new(0x2D, "AND", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x3D, "AND", 3, 4/* +1 if page crossed */, AddressingMode::AbsoluteX),
        OpCode::new(0x39, "AND", 3, 4/* +1 if page crossed */, AddressingMode::AbsoluteY),
        OpCode::new(0x21, "AND", 2, 6, AddressingMode::IndirectX),
        OpCode::new(0x31, "AND", 2, 5/* +1 if page crossed */, AddressingMode::IndirectY),
        
        OpCode::new(0x0A, "ASL", 1, 2, AddressingMode::None),
        OpCode::new(0x06, "ASL", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x16, "ASL", 2, 6, AddressingMode::ZeroPageX),
        OpCode::new(0x0E, "ASL", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x1E, "ASL", 3, 7, AddressingMode::AbsoluteX),

        OpCode::new(0x24, "BIT", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x2C, "BIT", 3, 4, AddressingMode::Absolute),

        OpCode::new(0xE0, "CPX", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xE4, "CPX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xEC, "CPX", 3, 4, AddressingMode::Absolute),
 
        OpCode::new(0xC0, "CPY", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xC4, "CPY", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xCC, "CPY", 3, 4, AddressingMode::Absolute),

        OpCode::new(0xE8, "INX", 1, 2, AddressingMode::None),
        OpCode::new(0xC8, "INY", 1, 2, AddressingMode::None),

        OpCode::new(0x4C, "JMP", 3, 3, AddressingMode::Absolute),
        OpCode::new(0x6C, "JMP", 3, 5, AddressingMode::None),

        OpCode::new(0x58, "CLI", 1, 2, AddressingMode::None),
        
        OpCode::new(0xB8, "CLV", 1, 2, AddressingMode::None),

        OpCode::new(0xC9, "CMP", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xC5, "CMP", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xD5, "CMP", 2, 4, AddressingMode::ZeroPageX),
        OpCode::new(0xCD, "CMP", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xDD, "CMP", 3, 4/* +1 if page crossed */, AddressingMode::AbsoluteX),
        OpCode::new(0xD9, "CMP", 3, 4/* +1 if page crossed */, AddressingMode::AbsoluteY),
        OpCode::new(0xC1, "CMP", 2, 6, AddressingMode::IndirectX),
        OpCode::new(0xD1, "CMP", 2, 5/* +1 if page crossed */, AddressingMode::IndirectY),

        OpCode::new(0xA9, "LDA", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xA5, "LDA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xB5, "LDA", 2, 4, AddressingMode::ZeroPageX),
        OpCode::new(0xAD, "LDA", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xBD, "LDA", 3, 4/* +1 if page crossed */, AddressingMode::AbsoluteX),
        OpCode::new(0xB9, "LDA", 3, 4/* +1 if page crossed */, AddressingMode::AbsoluteY),
        OpCode::new(0xA1, "LDA", 2, 6, AddressingMode::IndirectX),
        OpCode::new(0xB1, "LDA", 2, 5/* +1 if page crossed */, AddressingMode::IndirectY),
    ];

    pub static ref OPCODES_MAP: HashMap<u8, &'static OpCode> = {
        let mut map = HashMap::new();
        for cpu_op in &*CPU_OPS_CODES {
            map.insert(cpu_op.code, cpu_op);
        }
        map
    };
}