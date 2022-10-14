use crate::cpu::{AddressingMode, CPU, Mem};
use crate::opcodes;
use std::collections::HashMap;

pub fn trace(cpu: &CPU) -> String {
    let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;

    let code = cpu.mem_read(cpu.program_counter);
    let op = *opcodes.get(&code).unwrap();

    let start = cpu.program_counter;
    let mut hex_dump = vec![];
    hex_dump.push(code);

    let (mem_addr, value) = match op.mode {
        AddressingMode::Immediate | AddressingMode::None => (0, 0),
        _ => {
            let addr = cpu.get_address(&op.mode, start + 1);
            (addr, cpu.mem_read(addr))
        }
    };

    let tmp = match op.len {
        1 => match  op.code {
            0x0A | 0x2A | 0x4A | 0x6A => format!("A "),
            _ => String::from(""),
        }
        2 => {
            let address = cpu.mem_read(start + 1);
            hex_dump.push(address);

            match op.mode {
                AddressingMode::Immediate => format!("#${:02x}", address),
                AddressingMode::ZeroPage => format!("${:02x} = {:02x}", mem_addr, value),
                AddressingMode::ZeroPageX => format!("${:02x},X @ {:02x} = {:02x}",address, mem_addr, value),
                AddressingMode::ZeroPageY => format!("${:02x},Y @ {:02x} = {:02x}",address, mem_addr, value),
                AddressingMode::IndirectX => format!(
                    "(${:02x},X) @ {:02x} = {:04x} = {:02x}",
                    address, 
                    address.wrapping_add(cpu.register_x), 
                    mem_addr, 
                    value
                ),
                AddressingMode::IndirectY => format!(
                    "(${:02x}),Y @ {:04x} = {:04x} = {:02x}",
                    address, 
                    mem_addr.wrapping_sub(cpu.register_y as u16), 
                    mem_addr, 
                    value
                ),
                AddressingMode::None => {
                    let address = (start as usize + 2).wrapping_add((address as i8) as usize);
                    format!("${:04x}", address)
                }
                _ => panic!("Unexpected addressing mode {:?} is 2 bytes. Code {:02x}", op.mode, op.code),
            }
        }
        3 => {
            let addr_lo = cpu.mem_read(start + 1);
            let addr_hi = cpu.mem_read(start + 2);
            hex_dump.push(addr_lo);
            hex_dump.push(addr_hi);

            let address = cpu.mem_read_u16(start + 1);

            match op.mode {
                AddressingMode::None => {
                    if op.code == 0x6C {
                        let jmp_addr = cpu.jmp_indirect(address);
                        format!("(${:04x}) = {:04x}", address, jmp_addr)
                    } else {
                        format!("${:04x}", address)
                    }
                }
                AddressingMode::Absolute => format!("${:04x} = {:02x}", mem_addr, value),
                AddressingMode::AbsoluteX => format!("${:04x},X @ {:04x} = {:02x}",address, mem_addr, value),
                AddressingMode::AbsoluteY => format!("${:04x},Y @ {:04x} = {:02x}",address, mem_addr, value),
                _ => panic!("Unexpected addressing mode {:?} is 3 bytes. Code {:02x}", op.mode, op.code),
            }


        }
        _ => String::from("")
    };
    
    let hex_str = hex_dump
        .iter()
        .map(|x| format!("{:02x}", x))
        .collect::<Vec<String>>()
        .join(" ");
    let asm_str = format!("{:04x}  {:8} {: >4} {}", start, hex_str, op.instruction, tmp)
        .trim()
        .to_string();
    format!(
        "{:47} A:{:02x} X:{:02x} Y:{:02x} P:{:02x} SP:{:02x}",
        asm_str, cpu.register_a, cpu.register_x, cpu.register_y, cpu.status, cpu.stack_pointer,
    ).to_ascii_uppercase()
}