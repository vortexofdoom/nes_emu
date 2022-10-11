use std::collections::HashMap;
use crate::{opcodes, status_flags};

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: u8,
    pub program_counter: u16,
    memory: [u8; 0xFFFF]
}

#[derive(Debug)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    IndirectX,
    IndirectY,
    None,
}

trait Mem {
    fn mem_read(&self, addr: u16) -> u8;
    
    fn mem_write(&mut self, addr: u16, data: u8);

    fn mem_read_u16(&self, pos: u16) -> u16 {
        let lo = self.mem_read(pos) as u16;
        let hi = self.mem_read(pos + 1) as u16;
        (hi << 8) | (lo as u16)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        self.mem_write(pos, lo);
        self.mem_write(pos + 1, hi);
    }
}

impl Mem for CPU {
    
    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }
    
    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

}

impl CPU {
    pub fn new() -> Self {
        CPU { 
            register_a: 0, 
            register_x: 0, 
            register_y: 0, 
            status: 0, 
            program_counter: 0, 
            memory: [0; 0xFFFF] 
        }
    }

    fn add_to_register_a(&mut self, data: u8) {
        let (mut sum, mut carry) = data.overflowing_add(self.register_a);
        
        if self.status & status_flags::CARRY != 0 {
            if carry {
                sum += 1;
            } else {
                (sum, carry) = sum.overflowing_add(1);
            }
        }

        if carry {
            self.status |= status_flags::CARRY;
        } else {
            self.status &= !status_flags::CARRY;
        }

        if (data ^ sum) | (self.register_a ^ sum) & 0x80 != 0 {
            self.status |= status_flags::OVERFLOW;
        } else {
            self.status &= !status_flags::OVERFLOW;
        }

        self.register_a = sum;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.add_to_register_a(self.mem_read(addr));
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.register_a &= self.mem_read(addr);
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        if self.register_a & data == 0 {
            self.status |= status_flags::ZERO;
        }

        if data & status_flags::NEGATIVE != 0 {
            self.status |= status_flags::NEGATIVE;
        } else {
            self.status &= !status_flags::NEGATIVE;
        }

        if data & status_flags::OVERFLOW != 0 {
            self.status |= status_flags::OVERFLOW;
        } else {
            self.status &= !status_flags::OVERFLOW;
        }
    }

    fn branch(&mut self, condition: bool) {
        if condition {
            let jump = self.mem_read(self.program_counter) as i8;
            self.program_counter = self
                .program_counter
                .wrapping_add(1)
                .wrapping_add(jump as u16);
        }
    }

    fn compare(&mut self, mode: &AddressingMode, compare_with: u8) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        if compare_with >= data {
            self.status |= status_flags::CARRY;
        } else {
            self.status &= !status_flags::CARRY;
        }

        self.update_zero_and_negative_flags(compare_with.wrapping_sub(data));
    }

    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr).wrapping_sub(1);

        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
    }

    fn dex(&mut self) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_x)
    }

    fn dey(&mut self) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_y)
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.register_a ^= self.mem_read(addr);
        self.update_zero_and_negative_flags(self.register_a);
    }
    
    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr).wrapping_add(1);

        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
    }

    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn iny(&mut self) {
        self.register_x = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_x = value;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_y = value;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn asl(&mut self, mut data: u8) -> u8 {
        if data & 1 == 1 {
            self.status |= status_flags::CARRY;
        } else {
            self.status &= !status_flags::CARRY;
        }

        data <<= 1;
        self.update_zero_and_negative_flags(data);
        data
    }

    fn asl_addr(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.asl(self.mem_read(addr));
        self.mem_write(addr, data);
    } 

    fn asl_accu(&mut self) {
        self.register_a = self.asl(self.register_a);
    }

    fn lsr(&mut self, mut data: u8) -> u8 {
        if data & 1 == 1 {
            self.status |= status_flags::CARRY;
        } else {
            self.status &= !status_flags::CARRY;
        }

        data >>= 1;
        self.update_zero_and_negative_flags(data);
        data
    }

    fn lsr_addr(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.lsr(self.mem_read(addr));
        self.mem_write(addr, data);
    } 

    fn lsr_accu(&mut self) {
        self.register_a = self.lsr(self.register_a);
    }

    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.register_a |= self.mem_read(addr);
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn rol(&mut self, mut data: u8) -> u8 {
        let carry = self.status & status_flags::CARRY == 1;

        if data >> 7 == 1 {
            self.status |= status_flags::CARRY;
        } else {
            self.status &= !status_flags::CARRY;
        }

        data <<= 1;
        if carry {
            data |= 1;
        }

        self.update_zero_and_negative_flags(data);
        data
    }

    fn rol_addr(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.rol(self.mem_read(addr));
        self.mem_write(addr, data);
    }

    fn rol_accu(&mut self) {
        self.register_a = self.rol(self.register_a);
    }

    fn ror(&mut self, mut data: u8) -> u8 {
        let carry = self.status & status_flags::CARRY == 1;

        if data & 1 == 1 {
            self.status |= status_flags::CARRY;
        } else {
            self.status &= !status_flags::CARRY;
        }

        data >>= 1;
        if carry {
            data |= 0b10000000;
        }

        self.update_zero_and_negative_flags(data);
        data
    }

    fn ror_addr(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.rol(self.mem_read(addr));
        self.mem_write(addr, data);
    }

    fn ror_accu(&mut self) {
        self.register_a = self.rol(self.register_a);
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.add_to_register_a(!self.mem_read(addr) + 1);
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    fn stx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_x);
    }

    fn sty(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_y);
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }
    
    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);
    }
    
    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        if result == 0 {
            self.status |= status_flags::ZERO;
        } else {
            self.status &= !status_flags::ZERO;
        }

        if result >> 7 == 1 {
            self.status |= status_flags::NEGATIVE;
        } else {
            self.status &= !status_flags::NEGATIVE;
        }
    }

    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,

            AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,

            AddressingMode::Absolute => self.mem_read_u16(self.program_counter),

            AddressingMode::ZeroPageX => {
                self
                    .mem_read(self.program_counter)
                    .wrapping_add(self.register_x) as u16
            }

            AddressingMode::ZeroPageY => {
                self
                    .mem_read(self.program_counter)
                    .wrapping_add(self.register_y) as u16
            }

            AddressingMode::AbsoluteX => {
                self
                    .mem_read_u16(self.program_counter)
                    .wrapping_add(self.register_x as u16)
            }

            AddressingMode::AbsoluteY => {
                self
                    .mem_read_u16(self.program_counter)
                    .wrapping_add(self.register_y as u16)
            }

            AddressingMode::IndirectX => {
                let base = self.mem_read(self.program_counter);

                let ptr: u8 = base.wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read_u16(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }

            AddressingMode::IndirectY => {
                let base = self.mem_read(self.program_counter);

                let lo = self.mem_read(base as u16);
                let hi = self.mem_read(base.wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                deref_base.wrapping_add(self.register_y as u16)
            }

            AddressingMode::None => panic!("mode {:?} is not supported", mode),
        }
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[0x8000 .. (0x8000 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x8000);
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.status = 0;

        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn run(&mut self) {
        let ref op_codes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;

        loop {
            let code = self.mem_read(self.program_counter);
            self.program_counter += 1;
            let counter_state = self.program_counter;

            let op_code = op_codes.get(&code).expect(&format!("OpCode {:x} is not recognized", code));

            match op_code.instruction {
                //Add with carry
                "ADC" => self.adc(&op_code.mode),

                //AND
                "AND" => self.and(&op_code.mode),

                //Arithmetic shift left
                "ASL" => {
                    match op_code.mode {
                        AddressingMode::None => self.asl_accu(),
                        _ => self.asl_addr(&op_code.mode),                       
                    }
                }
                
                //Branch on carry flag
                "BCC" => self.branch(self.status & status_flags::CARRY == 0),
                "BCS" => self.branch(self.status & status_flags::CARRY != 0),
                
                //Branch on zero flag
                "BNE" => self.branch(self.status & status_flags::ZERO == 0),
                "BEQ" => self.branch(self.status & status_flags::ZERO != 0),

                //Check bits
                "BIT" => self.bit(&op_code.mode),

                //Branch on negative flag
                "BPL" => self.branch(self.status & status_flags::NEGATIVE == 0),
                "BMI" => self.branch(self.status & status_flags::NEGATIVE != 0),

                //Branch on overflow flag
                "BVC" => self.branch(self.status & status_flags::OVERFLOW == 0),
                "BVS" => self.branch(self.status & status_flags::OVERFLOW != 0),

                //Clear flags
                "CLC" => self.status &= !status_flags::CARRY,
                "CLD" => self.status &= !status_flags::DECIMAL_MODE,
                "CLI" => self.status &= !status_flags::INTERRUPT_DISABLE,
                "CLV" => self.status &= !status_flags::OVERFLOW,

                //Compare
                "CMP" => self.compare(&op_code.mode, self.register_a),
                "CPX" => self.compare(&op_code.mode, self.register_x),
                "CPY" => self.compare(&op_code.mode, self.register_y),

                //Decrement
                "DEC" => self.dec(&op_code.mode),
                "DEX" => self.dex(),
                "DEY" => self.dey(),

                //XOR
                "EOR" => self.eor(&op_code.mode),

                //Increment
                "INC" => self.inc(&op_code.mode),
                "INX" => self.inx(),
                "INY" => self.iny(),

                //Jump
                "JMP" => {}
                "JSR" => {}

                //Load registers
                "LDA" => self.lda(&op_code.mode),
                "LDX" => self.ldx(&op_code.mode),
                "LDY" => self.ldy(&op_code.mode),

                //Logical shift right
                "LSR" => {
                    match op_code.mode {
                        AddressingMode::None => self.lsr_accu(),
                        _ => self.lsr_addr(&op_code.mode),                       
                    }
                }

                //No operation
                "NOP" => {}

                //OR
                "ORA" => self.ora(&op_code.mode),

                //Push/Pop stack
                "PHA" => {}
                "PHP" => {}
                "PLA" => {}
                "PLP" => {}

                //Rotate
                "ROL" => match op_code.mode {
                    AddressingMode::None => self.rol_accu(),
                    _ => self.rol_addr(&op_code.mode),                       
                }
                "ROR" => match op_code.mode {
                    AddressingMode::None => self.ror_accu(),
                    _ => self.ror_addr(&op_code.mode),                       
                }

                //Return from interrupt/Subroutine
                "RTI" => {}
                "RTS" => {}

                //Subtract with carry
                "SBC" => self.sbc(&op_code.mode),

                //Set flags
                "SEC" => self.status |= status_flags::CARRY,
                "SED" => self.status |= status_flags::DECIMAL_MODE,
                "SEI" => self.status |= status_flags::INTERRUPT_DISABLE,

                //Store register contents in memory
                "STA" => self.sta(&op_code.mode),
                "STX" => self.stx(&op_code.mode),
                "STY" => self.sty(&op_code.mode),

                //Transfer between registers
                "TAX" => self.tax(),
                "TAY" => self.tay(),
                "TSX" => {}
                "TXA" => self.txa(),
                "TXS" => {}
                "TYA" => self.tya(),
                
                //Break
                "BRK" => return,
                _ => todo!()
            }

            if self.program_counter == counter_state {
                self.program_counter += (op_code.len - 1) as u16;
            }
        }
    }



}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0x00]);
        assert_eq!(cpu.register_a, 0x05);
        assert!(cpu.status & 0b0000_0010 == 0b00);
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x0a, 0xaa, 0x00]);

        assert_eq!(cpu.register_x, 10);
    }
    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0xc1);
    }
    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 1);
    }

    #[test]
    fn test_lda_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);

        cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

        assert_eq!(cpu.register_a, 0x55);
    }

    #[test]
    fn test_adc() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xA9, 0xC0, 0x69, 0xC4]);

        assert_eq!(cpu.register_a, 0x84);
    }
}