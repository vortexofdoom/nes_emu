use std::collections::HashMap;
use crate::{opcodes, bus::Bus};

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

impl Default for StatusFlags {
    fn default() -> Self {
        StatusFlags::INTERRUPT_DISABLE | StatusFlags::BREAK2
    }
}
const STACK: u16 = 0x0100;
const STACK_RESET: u8 = 0xfd;

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub stack_pointer: u8,
    pub status: StatusFlags,
    pub program_counter: u16,
    pub bus: Bus,
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
        self.bus.mem_read(addr)
    }
    
    fn mem_write(&mut self, addr: u16, data: u8) {
        self.bus.mem_write(addr, data);
    }

    fn mem_read_u16(&self, pos: u16) -> u16 {
        self.bus.mem_read_u16(pos)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        self.bus.mem_write_u16(pos, data);
    }
}

impl CPU {
    pub fn new(bus: Bus) -> Self {
        CPU { 
            register_a: 0, 
            register_x: 0, 
            register_y: 0,
            stack_pointer: STACK_RESET,
            status: StatusFlags::default(),
            program_counter: 0, 
            bus,
        }
    }

    fn get_op_addr(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,
            _ => self.get_address(mode, self.program_counter),
        }
    }

    pub fn get_address(&self, mode: &AddressingMode, addr: u16) -> u16 {
        match mode {
            AddressingMode::Immediate => addr,

            AddressingMode::ZeroPage => self.mem_read(addr) as u16,

            AddressingMode::Absolute => self.mem_read_u16(addr),

            AddressingMode::ZeroPageX => {
                self.mem_read(addr)
                    .wrapping_add(self.register_x) as u16
            }

            AddressingMode::ZeroPageY => {
                self.mem_read(addr)
                    .wrapping_add(self.register_y) as u16
            }

            AddressingMode::AbsoluteX => {
                self.mem_read_u16(addr)
                    .wrapping_add(self.register_x as u16)
            }

            AddressingMode::AbsoluteY => {
                self.mem_read_u16(addr)
                    .wrapping_add(self.register_y as u16)
            }

            AddressingMode::IndirectX => {
                let base = self.mem_read(addr);
                let ptr = base.wrapping_add(self.register_x);

                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }

            AddressingMode::IndirectY => {
                let base = self.mem_read(addr);
                let lo = self.mem_read(base as u16);

                let hi = self.mem_read(base.wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                deref_base.wrapping_add(self.register_y as u16)
            }

            AddressingMode::None => panic!("mode {:?} is not supported", mode),
        }
    }

    fn set_register_a(&mut self, data: u8) {
        self.update_zero_and_negative_flags(data);
        self.register_a = data;
    }

    fn set_register_x(&mut self, data: u8) {
        self.update_zero_and_negative_flags(data);
        self.register_x = data;
    }

    fn set_register_y(&mut self, data: u8) {
        self.update_zero_and_negative_flags(data);
        self.register_y = data;
    }
    // Not sure if worth reduction, but could be for some
    // fn get_addr_and_mem_data(&mut self, mode: &AddressingMode) -> (u16, u8) {
    //     let addr = self.get_op_addr(mode);
    //     let data = self.mem_read(addr);
    //     (addr, data)
    // }

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
        self.status.set(StatusFlags::OVERFLOW, (data ^ sum) & (self.register_a ^ sum) & 0x80 != 0);

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
        let addr = self.get_op_addr(mode);
        self.add_to_register_a(self.mem_read(addr));
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        self.set_register_a(self.register_a & self.mem_read(addr));
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let data = self.mem_read(addr);

        self.status.set(StatusFlags::ZERO, self.register_a & data == 0);
        self.status.set(StatusFlags::NEGATIVE, data >> 7 == 1);
        self.status.set(StatusFlags::OVERFLOW, data & StatusFlags::OVERFLOW.bits != 0);
    }

    fn branch(&mut self, condition: bool) {
        if condition {
            let jump = self.mem_read(self.program_counter) as i8;
            self.program_counter = self.program_counter
                .wrapping_add(1)
                .wrapping_add(jump as u16);
        }
    }

    fn compare(&mut self, mode: &AddressingMode, compare_with: u8) {
        let addr = self.get_op_addr(mode);
        let data = self.mem_read(addr);

        self.status.set(StatusFlags::CARRY, data <= compare_with);
        self.update_zero_and_negative_flags(compare_with.wrapping_sub(data));
    }

    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let data = self.mem_read(addr).wrapping_sub(1);

        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        self.set_register_a(self.register_a ^ self.mem_read(addr));
    }

    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let data = self.mem_read(addr).wrapping_add(1);

        self.update_zero_and_negative_flags(data);
        self.mem_write(addr, data);
    }

    
    pub fn jmp_indirect(&self, addr: u16) -> u16 {
        let indirect_ref = if addr & 0x00FF == 0x00FF {
            let hi = self.mem_read(addr & 0xFF00);
            let lo = self.mem_read(addr);

            u16::from_le_bytes([lo, hi])
        } else {
            self.mem_read_u16(addr)
        };
        indirect_ref
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let value = self.mem_read(addr);

        self.set_register_a(value);
    }

    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let value = self.mem_read(addr);

        self.set_register_x(value);
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let value = self.mem_read(addr);

        self.set_register_y(value);
    }

    fn asl(&mut self, mut data: u8) -> u8 {
        self.status.set(StatusFlags::CARRY, data >> 7 == 1);

        data <<= 1;
        self.update_zero_and_negative_flags(data);
        data
    }

    fn asl_addr(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
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
        let addr = self.get_op_addr(mode);
        let data = self.lsr(self.mem_read(addr));

        self.update_zero_and_negative_flags(data);
        self.mem_write(addr, data);
    } 

    fn lsr_accu(&mut self) {
        let lsr = self.lsr(self.register_a);
        self.set_register_a(lsr);
    }

    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        self.set_register_a(self.register_a | self.mem_read(addr));
    }

    fn php(&mut self) {
        let mut flags = self.status.clone();
        flags.insert(StatusFlags::BREAK | StatusFlags::BREAK2);
        self.stack_push(flags.bits);
    }

    fn pla(&mut self) {
        let data = self.stack_pop();
        self.set_register_a(data);
    }

    fn plp(&mut self) {
        self.status.bits = self.stack_pop();
        self.status.remove(StatusFlags::BREAK);
        self.status.insert(StatusFlags::BREAK2);
    }

    fn rol(&mut self, mut data: u8) -> u8 {
        let carry = self.status.contains(StatusFlags::CARRY);

        self.status.set(StatusFlags::CARRY, data >> 7 == 1);

        data <<= 1;
        if carry {
            data |= 0b0000_0001;
        }

        data
    }

    fn rol_addr(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let data = self.rol(self.mem_read(addr));

        self.status.set(StatusFlags::NEGATIVE, data >> 7 == 1);

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
            data |= 0b1000_0000;
        }

        data
    }

    fn ror_addr(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let data = self.ror(self.mem_read(addr));

        self.status.set(StatusFlags::NEGATIVE, data >> 7 == 1);

        self.mem_write(addr, data);
    }

    fn ror_accu(&mut self) {
        let ror = self.ror(self.register_a);
        self.set_register_a(ror);
    }

    fn rti(&mut self) {
        self.status.bits = self.stack_pop();
        self.status.remove(StatusFlags::BREAK);
        self.status.insert(StatusFlags::BREAK2);

        self.program_counter = self.stack_pop_u16();
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let data = self.mem_read(addr) as i8;
        self.add_to_register_a(data.wrapping_neg().wrapping_sub(1) as u8);
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        self.mem_write(addr, self.register_a);
    }

    fn stx(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        self.mem_write(addr, self.register_x);
    }

    fn sty(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        self.mem_write(addr, self.register_y);
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        self.status.set(StatusFlags::ZERO, result == 0);
        self.status.set(StatusFlags::NEGATIVE, result >> 7 == 1);
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run(|_| {})
    }

    pub fn load(&mut self, program: Vec<u8>) {
        for i in 0..(program.len()) {
            self.mem_write(0x0600 + i as u16, program[i]);
        }
        self.mem_write_u16(0xFFFC, 0x0600);
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.stack_pointer = STACK_RESET;
        self.status = StatusFlags::default();
        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn run<F>(&mut self, mut callback: F) 
    where F: FnMut(&mut CPU) {
        let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;

        loop {
            callback(self);

            let code = self.mem_read(self.program_counter);
            self.program_counter += 1;
            let counter_state = self.program_counter;
            let opcode = *opcodes.get(&code).unwrap();

            match code {
                //Break
                0x00 => return,

                //Add with carry
                0x69 | 0x65 | 0x75 | 0x6D | 0x7D | 0x79 | 0x61 | 0x71 => self.adc(&opcode.mode),
                //Subtract with carry
                0xE9 | 0xE5 | 0xF5 | 0xEB /*<- unofficial*/ | 0xED | 0xFD | 0xF9 | 0xE1 | 0xF1 => self.sbc(&opcode.mode),

                //AND
                0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => self.and(&opcode.mode),

                //ASL
                0x0A => self.asl_accu(),
                0x06 | 0x16 | 0x0E | 0x1E => self.asl_addr(&opcode.mode),
                
                //LSR
                0x4A => self.lsr_accu(),
                0x46 | 0x56 | 0x4E | 0x5E => self.lsr_addr(&opcode.mode),
                
                //ROL
                0x2A => self.rol_accu(),
                0x26 | 0x36 | 0x2E | 0x3E => self.rol_addr(&opcode.mode),
                
                //ROR
                0x6A => self.ror_accu(),
                0x66 | 0x76 | 0x6E | 0x7E => self.ror_addr(&opcode.mode),

                //Branch on carry flag
                0x90 => self.branch(!self.status.contains(StatusFlags::CARRY)),
                0xB0 => self.branch(self.status.contains(StatusFlags::CARRY)),
                
                //Branch on zero flag
                0xD0 => self.branch(!self.status.contains(StatusFlags::ZERO)),  //BNE
                0xF0 => self.branch(self.status.contains(StatusFlags::ZERO)),   //BEQ

                //Check bits
                0x24 | 0x2C => self.bit(&opcode.mode),

                //Branch on negative
                /*BPL*/ 0x10 => self.branch(!self.status.contains(StatusFlags::NEGATIVE)),
                /*BMI*/ 0x30 => self.branch(self.status.contains(StatusFlags::NEGATIVE)),

                //Branch on overflow flag
                0x50 => self.branch(!self.status.contains(StatusFlags::OVERFLOW)),
                0x70 => self.branch(self.status.contains(StatusFlags::OVERFLOW)),
                
                //Set flags
                /*SEC*/ 0x38 => self.status.insert(StatusFlags::CARRY),
                /*SED*/ 0xF8 => self.status.insert(StatusFlags::DECIMAL_MODE),
                /*SEI*/ 0x78 => self.status.insert(StatusFlags::INTERRUPT_DISABLE),
                
                //Clear flags
                /*CLC*/ 0x18 => self.status.remove(StatusFlags::CARRY),
                /*CLD*/ 0xD8 => self.status.remove(StatusFlags::DECIMAL_MODE),
                /*CLI*/ 0x58 => self.status.remove(StatusFlags::INTERRUPT_DISABLE),
                /*CLV*/ 0xB8 => self.status.remove(StatusFlags::OVERFLOW),

                //Compare
                /*CMP*/ 0xC9 | 0xC5 | 0xD5 | 0xCD | 0xDD | 0xD9 | 0xC1 | 0xD1 => self.compare(&opcode.mode, self.register_a),
                /*CPX*/ 0xE0 | 0xE4 | 0xEC => self.compare(&opcode.mode, self.register_x),
                /*CPY*/ 0xC0 | 0xC4 | 0xCC => self.compare(&opcode.mode, self.register_y),

                //Increment
                /*INC*/ 0xE6 | 0xF6 | 0xEE | 0xFE => self.inc(&opcode.mode),
                /*INX*/ 0xE8 => self.set_register_x(self.register_x.wrapping_add(1)),
                /*INY*/ 0xC8 => self.set_register_y(self.register_y.wrapping_add(1)),

                //Decrement
                /*DEC*/ 0xC6 | 0xD6 | 0xCE | 0xDE => self.dec(&opcode.mode),
                /*DEX*/ 0xCA => self.set_register_x(self.register_x.wrapping_sub(1)),
                /*DEY*/ 0x88 => self.set_register_y(self.register_y.wrapping_sub(1)),

                /*EOR*/ 0x49 | 0x45 | 0x55 | 0x4D | 0x5D | 0x59 | 0x41 | 0x51 => self.eor(&opcode.mode),

                //JMP
                0x4C => {
                    let mem_address = self.mem_read_u16(self.program_counter);
                    self.program_counter = mem_address;
                }                
                0x6C => {
                    let addr = self.mem_read_u16(self.program_counter);
                    let indirect_ref = self.jmp_indirect(addr);
    
                    self.program_counter = indirect_ref;
                }
                //JSR
                0x20 => self.jsr(),

                //Load registers
                /*LDA*/ 0xA9 | 0xA5 | 0xB5 | 0xAD | 0xBD | 0xB9 | 0xA1 | 0xB1 => self.lda(&opcode.mode),
                /*LDX*/ 0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => self.ldx(&opcode.mode),
                /*LDY*/ 0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => self.ldy(&opcode.mode),

                //OR A
                0x09 | 0x05 | 0x15 | 0x0D | 0x1D | 0x19 | 0x01 | 0x11 => self.ora(&opcode.mode),

                //Push/Pop stack
                /*PHA*/ 0x48 => self.stack_push(self.register_a),
                /*PHP*/ 0x08 => self.php(),
                /*PLA*/ 0x68 => self.pla(),
                /*PLP*/ 0x28 => self.plp(),

                //Return from interrupt/Subroutine
                /*RTI*/ 0x40 => self.rti(),
                /*RTS*/ 0x60 => self.program_counter = self.stack_pop_u16() + 1,

                //Store register contents in memory
                /*STA*/ 0x85 | 0x95 | 0x8D | 0x9D | 0x99 | 0x81 | 0x91 => self.sta(&opcode.mode),
                /*STX*/ 0x86 | 0x96 | 0x8E => self.stx(&opcode.mode),
                /*STY*/ 0x84 | 0x94 | 0x8C => self.sty(&opcode.mode),

                //Transfer between registers
                
                /*TAX*/ 0xAA => self.set_register_x(self.register_a),
                /*TAY*/ 0xA8 => self.set_register_y(self.register_a),
                
                /*TSX*/ 0xBA => self.set_register_x(self.stack_pointer),
                /*TXS*/ 0x9A => self.stack_pointer = self.register_x,
                
                /*TXA*/ 0x8A => self.set_register_a(self.register_x),
                /*TYA*/ 0x98 => self.set_register_a(self.register_y),

                //NOP
                /*official*/    0xEA | 
                /*unofficial*/  0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xFA |
                /*DOP 2 bytes*/ 0x04 | 0x14 | 0x34 | 0x44 | 0x54 | 0x64 | 0x74 | 0x80 | 0x82 | 0x89 | 0xC2 | 0xD4 | 0xE2 |0xF4 |
                /*TOP 3 bytes*/ 0x0C | 0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => {
                    /*No operation*/
                }


                // Unofficial OpCodes

                /*ANC*/ 0x0B | 0x2B => {self.anc(&opcode.mode)},
                /*SAX*/ 0x87 | 0x97 | 0x83 | 0x8F => self.sax(&opcode.mode),
                /*ARR*/ 0x6B => self.arr(&opcode.mode),
                /*ALR*/ 0x4B => self.alr(),
                /*LAX*/ 0xAB => self.set_register_x(self.register_x & self.register_a),
                /*AHX*/ 0x9F | 0x93 => self.ahx(&opcode.mode),
                /*AXS*/ 0xCB => self.axs(),
                /*DCP*/ 0xC7 | 0xD7 | 0xCF | 0xDF | 0xDB | 0xC3 | 0xD3 => self.dcp(&opcode.mode),
                /*ISB*/ 0xE7 | 0xF7 | 0xEF | 0xFF | 0xFB | 0xE3 | 0xF3 => self.isb(&opcode.mode),
                /*KIL*/ 0x02 | 0x12 | 0x22 | 0x32 | 0x42 | 0x52 | 0x62 | 0x72 | 0x92 | 0xB2 | 0xD2 | 0xF2 => {
                    self.program_counter -= 1;
                    continue;
                }
                /*LAS*/ 0xBB => self.las(&opcode.mode),
                /*LAX*/ 0xA7 | 0xB7 | 0xAF | 0xBF | 0xA3 | 0xB3 => self.lax(&opcode.mode),
                /*RLA*/ 0x27 | 0x37 | 0x2F | 0x3F | 0x3B | 0x23 | 0x33 => self.rla(&opcode.mode),
                /*RRA*/ 0x67 | 0x77 | 0x6F | 0x7F | 0x7B | 0x63 | 0x73 => self.rra(&opcode.mode),
                /*SLO*/ 0x07 | 0x17 | 0x0F | 0x1F | 0x1B | 0x03 | 0x13 => self.slo(&opcode.mode),
                /*SRE*/ 0x43 | 0x47 | 0x4F | 0x53 | 0x57 | 0x5B | 0x5F => self.sre(&opcode.mode),
                /*SHX*/ 0x9E => self.shx(&opcode.mode),
                /*SHY*/ 0x9C => self.shy(&opcode.mode),
                /*XAA*/ 0x8B => {/*unknown*/}
                /*TAS*/ 0x9B => self.tas(&opcode.mode),
            }

            if self.program_counter == counter_state {
                self.program_counter += (opcode.len - 1) as u16;
            }
        }
    }

    // Unofficial codes
    
    fn anc(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let data = self.register_a & self.mem_read(addr);
        self.status.set(StatusFlags::CARRY, data >> 7 == 1);
        self.set_register_a(data);
    }

    fn sax(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let data = self.register_a & self.register_x;
        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
    }

    fn arr(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let mut data = self.register_a & self.mem_read(addr);
        let rightmost = data & 1 == 1;

        data >>= 1;
        data &= if rightmost { 0b1111_1111 } else { 0b0111_1111 };

        match (data >> 5) & 0b0000_0011 {
            0b00 => {
                self.status.remove(StatusFlags::CARRY);
                self.status.remove(StatusFlags::OVERFLOW);
            }
            0b01 => {
                self.status.remove(StatusFlags::CARRY);
                self.status.insert(StatusFlags::OVERFLOW);
            }
            0b10 => {
                self.status.insert(StatusFlags::CARRY);
                self.status.insert(StatusFlags::OVERFLOW);
            }
            0b11 => {
                self.status.insert(StatusFlags::CARRY);
                self.status.remove(StatusFlags::OVERFLOW);
            }
            _ => panic!("idk how this went over 0b11"),
        }

        self.set_register_a(data);
    } 

    fn alr(&mut self) {
        self.register_a &= self.mem_read(self.program_counter);
        self.lsr_accu();
    }

    fn ahx(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let data = (self.register_a & self.register_x) & 7;
        self.mem_write(addr, data);
    }

    fn axs(&mut self) {
        let data = self.mem_read(self.program_counter);
        let (result, carry) = (self.register_x & self.register_a).overflowing_sub(data);
        self.status.set(StatusFlags::CARRY, carry);
        self.set_register_x(result);
    }

    fn dcp(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let data = self.mem_read(addr).wrapping_sub(1);

        self.mem_write(addr, data);
        self.status.set(StatusFlags::CARRY, data <= self.register_a);
        self.update_zero_and_negative_flags(self.register_a.wrapping_sub(data));
    }

    fn isb(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let data = self.mem_read(addr).wrapping_add(1);

        self.mem_write(addr, data);
        self.sbc(mode);
    }

    fn rla(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let data = self.rol(self.mem_read(addr));

        self.status.set(StatusFlags::NEGATIVE, data >> 7 == 1);

        self.mem_write(addr, data);
        self.set_register_a(self.register_a & data);
    }

    fn rra(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let data = self.ror(self.mem_read(addr));

        self.status.set(StatusFlags::NEGATIVE, data >> 7 == 1);

        self.mem_write(addr, data);
        self.set_register_a(self.register_a & data);
    }

    fn slo(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let data = self.asl(self.mem_read(addr));
        
        self.mem_write(addr, data);
        self.set_register_a(self.register_a | data);
    }

    fn sre(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let data = self.lsr(self.mem_read(addr));
        self.set_register_a(self.register_a ^ data);
    }

    fn shx(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let data = self.mem_read(addr);
        self.mem_write(addr, self.register_x & data);
    }

    fn shy(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let data = self.mem_read_u16(addr) >> 8;
        self.mem_write(addr, self.register_y & data as u8);
    }

    fn tas(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let data = (self.mem_read_u16(addr) >> 8) as u8;
        self.stack_pointer = self.register_a & self.register_x;
        self.mem_write(addr, data.wrapping_add(1) & self.stack_pointer);
    }

    fn las(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        self.stack_pointer &= self.mem_read(addr);
        self.register_x = self.stack_pointer;
        self.set_register_a(self.stack_pointer);
    }

    fn lax(&mut self, mode: &AddressingMode) {
        let addr = self.get_op_addr(mode);
        let data = self.mem_read(addr);
        self.register_x = data;
        self.set_register_a(data);
    }


}


// #[cfg(test)]
// mod test {
//     use super::*;

//     #[test]
//     fn test_0xa9_lda_immediate_load_data() {
//         let mut cpu = CPU::new();
//         cpu.load_and_run(vec![0xa9, 0x05, 0x00]);
//         assert_eq!(cpu.register_a, 0x05);
//         assert!(cpu.status.bits & 0b0000_0010 == 0b00);
//         assert!(cpu.status.bits & 0b1000_0000 == 0);
//     }

//     #[test]
//     fn test_0xa9_lda_zero_flag() {
//         let mut cpu = CPU::new();
//         cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
//         assert!(cpu.status.bits & 0b0000_0010 == 0b10);
//     }

//     #[test]
//     fn test_0xaa_tax_move_a_to_x() {
//         let mut cpu = CPU::new();
//         cpu.load_and_run(vec![0xa9, 0x0a, 0xaa, 0x00]);

//         assert_eq!(cpu.register_x, 10);
//     }
//     #[test]
//     fn test_5_ops_working_together() {
//         let mut cpu = CPU::new();
//         cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

//         assert_eq!(cpu.register_x, 0xc1);
//     }
//     #[test]
//     fn test_inx_overflow() {
//         let mut cpu = CPU::new();
//         cpu.load_and_run(vec![0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00]);

//         assert_eq!(cpu.register_x, 1);
//     }

//     #[test]
//     fn test_lda_from_memory() {
//         let mut cpu = CPU::new();
//         cpu.mem_write(0x10, 0x55);

//         cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

//         assert_eq!(cpu.register_a, 0x55);
//     }

//     #[test]
//     fn test_adc() {
//         let mut cpu = CPU::new();
//         cpu.load_and_run(vec![0xA9, 0xC0, 0x69, 0xC4]);

//         assert_eq!(cpu.register_a, 0x84);
//     }
// }