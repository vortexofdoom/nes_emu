use std::collections::HashMap;
use crate::opcodes;

bitflags! {
    pub struct StatusFlags: u8 {
        const CARRY             = 0b00000001;
        const ZERO              = 0b00000010;
        const INTERRUPT_DISABLE = 0b00000100;
        const DECIMAL_MODE      = 0b00001000;
        const BREAK             = 0b00010000;
        const BREAK2            = 0b00100000;
        const OVERFLOW          = 0b01000000;
        const NEGATIVE          = 0b10000000;
    }
}
const STACK: u16 = 0x0100;
const STACK_RESET: u8 = 0xfd;
const STATUS_RESET: StatusFlags = StatusFlags::from_bits_truncate(0b100100);

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub stack_pointer: u8,
    pub status: StatusFlags,
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

pub trait Mem {
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
            stack_pointer: STACK_RESET,
            status: STATUS_RESET,
            program_counter: 0, 
            memory: [0; 0xFFFF],
        }
    }

    fn get_address(&self, mode: &AddressingMode) -> u16 {
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

                let ptr: u8 = (base as u8).wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
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

    fn set_register_a(&mut self, data: u8) {
        self.register_a = data;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn set_register_x(&mut self, data: u8) {
        self.register_x = data;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn set_register_y(&mut self, data: u8) {
        self.register_y = data;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn add_to_register_a(&mut self, data: u8) {
        let (mut sum, mut carry) = data.overflowing_add(self.register_a);
        
        if self.status.contains(StatusFlags::CARRY) {
            if carry {
                sum += 1;
            } else {
                (sum, carry) = sum.overflowing_add(1);
            }
        }

        self.status.set(StatusFlags::CARRY, carry);
        self.status.set(StatusFlags::OVERFLOW, (data ^ sum) | (self.register_a ^ sum) & 0x80 != 0);

        self.set_register_a(sum);
    }

    fn stack_pop(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.mem_read(STACK + self.stack_pointer as u16)
    }

    fn stack_push(&mut self, data: u8) {
        self.mem_write(STACK + self.stack_pointer as u16, data);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn stack_pop_u16(&mut self) -> u16 {
        let lo = self.stack_pop() as u16;
        let hi = self.stack_pop() as u16;

        hi << 8 | lo
    }

    fn stack_push_u16(&mut self, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xFF) as u8;
        self.stack_push(hi);
        self.stack_push(lo);
    }

    fn jsr(&mut self) {
        self.stack_push_u16(self.program_counter + 2 - 1);
        let target = self.mem_read_u16(self.program_counter);
        self.program_counter = target;
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(mode);
        self.add_to_register_a(self.mem_read(addr));
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(mode);
        self.set_register_a(self.register_a & self.mem_read(addr));
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(mode);
        let data = self.mem_read(addr);

        self.status.set(StatusFlags::ZERO, self.register_a & data == 0);

        self.update_negative_flag(data);
        self.status.set(StatusFlags::OVERFLOW, data & StatusFlags::OVERFLOW.bits != 0);
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
        let addr = self.get_address(mode);
        let data = self.mem_read(addr);

        self.status.set(StatusFlags::CARRY, data <= compare_with);

        self.update_zero_and_negative_flags(compare_with.wrapping_sub(data));
    }

    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(mode);
        let data = self.mem_read(addr).wrapping_sub(1);

        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
    }

    fn dex(&mut self) {
        self.set_register_x(self.register_x.wrapping_sub(1));
    }

    fn dey(&mut self) {
        self.set_register_y(self.register_y.wrapping_sub(1));
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(mode);
        self.set_register_a(self.register_a ^ self.mem_read(addr));
    }

    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(mode);
        let data = self.mem_read(addr).wrapping_add(1);

        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
    }

    fn inx(&mut self) {
        self.set_register_x(self.register_x.wrapping_add(1));
    }

    fn iny(&mut self) {
        self.set_register_y(self.register_y.wrapping_add(1));
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(mode);
        let value = self.mem_read(addr);

        self.set_register_a(value);
    }

    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(mode);
        let value = self.mem_read(addr);

        self.set_register_x(value);
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(mode);
        let value = self.mem_read(addr);

        self.set_register_y(value);
    }

    fn asl(&mut self, mut data: u8) -> u8 {
        self.status.set(StatusFlags::CARRY, data & 1 == 1);

        data <<= 1;
        self.update_zero_and_negative_flags(data);
        data
    }

    fn asl_addr(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(mode);
        let data = self.asl(self.mem_read(addr));
        self.mem_write(addr, data);
    } 

    fn asl_accu(&mut self) {
        self.register_a = self.asl(self.register_a);
    }

    fn lsr(&mut self, data: u8) -> u8 {
        self.status.set(StatusFlags::CARRY, data & 1 == 1);

        data >> 1
    }

    fn lsr_addr(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(mode);
        let data = self.lsr(self.mem_read(addr));

        self.update_negative_flag(data);

        self.mem_write(addr, data);
    } 

    fn lsr_accu(&mut self) {
        let lsr = self.lsr(self.register_a);
        self.set_register_a(lsr);
    }

    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(mode);
        self.set_register_a(self.register_a | self.mem_read(addr));
    }

    fn php(&mut self) {
        let mut flags = self.status.clone();
        flags.insert(StatusFlags::BREAK);
        flags.insert(StatusFlags::BREAK2);
        self.stack_push(flags.bits);
    }

    fn pla(&mut self) {
        let data = self.stack_pop();
        self.set_register_a(data);
    }

    fn plp(&mut self) {
        self.status.bits = self.stack_pop();
        self.status.remove(StatusFlags::BREAK);
        self.status.remove(StatusFlags::BREAK2);
    }

    fn rol(&mut self, mut data: u8) -> u8 {
        let carry = self.status.contains(StatusFlags::CARRY);

        self.status.set(StatusFlags::CARRY, data >> 7 == 1);

        data <<= 1;
        if carry {
            data |= 1;
        }

        data
    }

    fn rol_addr(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(mode);
        let data = self.rol(self.mem_read(addr));

        self.update_negative_flag(data);

        self.mem_write(addr, data);
    }

    fn rol_accu(&mut self) {
        let rol = self.rol(self.register_a);
        self.set_register_a(rol);
    }

    fn ror(&mut self, mut data: u8) -> u8 {
        let carry = self.status.contains(StatusFlags::CARRY);

        self.status.set(StatusFlags::CARRY, data & 1 == 1);

        data >>= 1;
        if carry {
            data |= 0b10000000;
        }

        data
    }

    fn ror_addr(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(mode);
        let data = self.ror(self.mem_read(addr));

        self.update_negative_flag(data);

        self.mem_write(addr, data);
    }

    fn ror_accu(&mut self) {
        let ror = self.ror(self.register_a);
        self.set_register_a(ror);
    }

    fn rti(&mut self) {
        self.status = StatusFlags { bits: self.stack_pop() };
        self.status &= !StatusFlags::BREAK;
        self.status |= StatusFlags::BREAK2;

        self.program_counter = self.stack_pop_u16() + 1;
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(mode);
        self.add_to_register_a(self.mem_read(addr).wrapping_neg().wrapping_sub(1));
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(mode);
        self.mem_write(addr, self.register_a);
    }

    fn stx(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(mode);
        self.mem_write(addr, self.register_x);
    }

    fn sty(&mut self, mode: &AddressingMode) {
        let addr = self.get_address(mode);
        self.mem_write(addr, self.register_y);
    }

    fn tax(&mut self) {
        self.set_register_x(self.register_a);
    }

    fn tay(&mut self) {
        self.set_register_y(self.register_a);
    }

    fn tsx(&mut self) {
        self.set_register_x(self.stack_pointer);
    }

    fn txa(&mut self) {
        self.set_register_a(self.register_x);
    }

    fn txs(&mut self) {
        self.stack_pointer = self.register_x;
    }
    
    fn tya(&mut self) {
        self.set_register_a(self.register_y);
    }

    fn update_negative_flag(&mut self, result: u8) {
        self.status.set(StatusFlags::NEGATIVE, result >> 7 == 1);
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        self.status.set(StatusFlags::ZERO, result == 0);
        self.update_negative_flag(result);
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run(|_| {})
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[0x0600 .. (0x0600 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x0600);
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.stack_pointer = STACK_RESET;
        self.status = STATUS_RESET;

        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn run<F>(&mut self, mut callback: F) 
    where F: FnMut(&mut CPU) {
        let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;

        loop {
            let code = self.mem_read(self.program_counter);
            self.program_counter += 1;
            let counter_state = self.program_counter;
            let opcode = *opcodes.get(&code).unwrap();

            match code {
                //Break
                0x00 => return,

                //Add with carry
                0x69 | 0x65 | 0x75 | 0x6D | 0x7D | 0x79 | 0x61 | 0x71 => self.adc(&opcode.mode),

                //AND
                0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => self.and(&opcode.mode),

                //Arithmetic shift left
                0x0A => self.asl_accu(),
                0x06 | 0x16 | 0x0E | 0x1E => self.asl_addr(&opcode.mode),
                
                //Branch on carry flag
                0x90 => self.branch(!self.status.contains(StatusFlags::CARRY)),
                0xB0 => self.branch(self.status.contains(StatusFlags::CARRY)),
                
                //Branch on zero flag
                0xD0 => self.branch(!self.status.contains(StatusFlags::ZERO)),  //BNE
                0xF0 => self.branch(self.status.contains(StatusFlags::ZERO)),   //BEQ

                //Check bits
                0x24 | 0x2C => self.bit(&opcode.mode),

                //Branch on negative
                /*BPL*/0x10 => self.branch(!self.status.contains(StatusFlags::NEGATIVE)),
                /*BMI*/0x30 => self.branch(self.status.contains(StatusFlags::NEGATIVE)),

                //Branch on overflow flag
                0x50 => self.branch(!self.status.contains(StatusFlags::OVERFLOW)),
                0x70 => self.branch(self.status.contains(StatusFlags::OVERFLOW)),

                //Clear flags
                0x18 => self.status &= !StatusFlags::CARRY,
                0xD8 => self.status &= !StatusFlags::DECIMAL_MODE,
                0x58 => self.status &= !StatusFlags::INTERRUPT_DISABLE,
                0xB8 => self.status &= !StatusFlags::OVERFLOW,

                //Compare
                0xC9 | 0xC5 | 0xD5 | 0xCD | 0xDD | 0xD9 | 0xC1 | 0xD1 => self.compare(&opcode.mode, self.register_a),
                0xE0 | 0xE4 | 0xEC => self.compare(&opcode.mode, self.register_x),
                0xC0 | 0xC4 | 0xCC => self.compare(&opcode.mode, self.register_y),

                //Decrement
                0xC6 | 0xD6 | 0xCE | 0xDE => self.dec(&opcode.mode),
                0xCA => self.dex(),
                0x88 => self.dey(),

                //XOR
                0x49 | 0x45 | 0x55 | 0x4D | 0x5D | 0x59 | 0x41 | 0x51 => self.eor(&opcode.mode),

                //Increment
                0xE6 | 0xF6 | 0xEE | 0xFE => self.inc(&opcode.mode),
                0xE8 => self.inx(),
                0xC8 => self.iny(),

                //Jump
                0x4C => {
                    let mem_address = self.mem_read_u16(self.program_counter);
                    self.program_counter = mem_address;
                }                
                0x6C => {
                    let mem_address = self.mem_read_u16(self.program_counter);

                    let indirect_ref = if mem_address & 0x00FF == 0x00FF {
                        let hi = self.mem_read_u16(mem_address & 0xFF00);
                        let lo = self.mem_read_u16(mem_address);
    
                        (hi as u16) << 8 | (lo as u16)
                    } else {
                        self.mem_read_u16(mem_address)
                    };
    
                    self.program_counter = indirect_ref;
                }
                0x20 => self.jsr(),

                //Load registers
                0xA9 | 0xA5 | 0xB5 | 0xAD | 0xBD | 0xB9 | 0xA1 | 0xB1 => self.lda(&opcode.mode),
                0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => self.ldx(&opcode.mode),
                0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => self.ldy(&opcode.mode),

                //Logical shift right
                0x4A => self.lsr_accu(),
                0x46 | 0x56 | 0x4E | 0x5E => self.lsr_addr(&opcode.mode),

                //No operation
                0xEA => {}

                //OR
                0x09 | 0x05 | 0x15 | 0x0D | 0x1D | 0x19 | 0x01 | 0x11 => self.ora(&opcode.mode),

                //Push/Pop stack
                /*PHA*/0x48 => self.stack_push(self.register_a),
                /*PHP*/0x08 => self.php(),
                /*PLA*/0x68 => self.pla(),
                /*PLP*/0x28 => self.plp(),
                

                //ROL
                0x2A => self.rol_accu(),
                0x26 | 0x36 | 0x2E | 0x3E => self.rol_addr(&opcode.mode),
                
                //ROR
                0x6A => self.ror_accu(),
                0x66 | 0x76 | 0x6E | 0x7E => self.ror_addr(&opcode.mode),

                //Return from interrupt/Subroutine
                0x40 => self.rti(),
                0x60 => self.program_counter = self.stack_pop_u16() + 1,

                //Subtract with carry
                0xE9 | 0xE5 | 0xF5 | 0xED | 0xFD | 0xF9 | 0xE1 | 0xF1 => self.sbc(&opcode.mode),

                //Set flags
                0x38 => self.status |= StatusFlags::CARRY,
                0xF8 => self.status |= StatusFlags::DECIMAL_MODE,
                0x78 => self.status |= StatusFlags::INTERRUPT_DISABLE,

                //Store register contents in memory
                0x85 | 0x95 | 0x8D | 0x9D | 0x99 | 0x81 | 0x91 => self.sta(&opcode.mode),
                0x86 | 0x96 | 0x8E => self.stx(&opcode.mode),
                0x84 | 0x94 | 0x8C => self.sty(&opcode.mode),

                //Transfer between registers
                0xAA => self.tax(),
                0xA8 => self.tay(),
                0xBA => self.tsx(),
                0x8A => self.txa(),
                0x9A => self.txs(),
                0x98 => self.tya(),
                
                _ => todo!(),
            }

            if self.program_counter == counter_state {
                self.program_counter += (opcode.len - 1) as u16;
            }

            callback(self);
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
        assert!(cpu.status.bits & 0b0000_0010 == 0b00);
        assert!(cpu.status.bits & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status.bits & 0b0000_0010 == 0b10);
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