use crate::{cpu::Mem, rom::Rom};

const RAM_START: u16    = 0x0000;
const RAM_END: u16      = 0x1FFF;
const PPU_START: u16    = 0x2000;
const PPU_END: u16      = 0x3FFF;

pub struct Bus {
    cpu_ram: [u8; 2048],
    rom: Rom,
}

impl Bus {
    pub fn new(rom: Rom) -> Self {
        Bus {
            cpu_ram: [0; 2048],
            rom,
        }
    }

    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000;
        if addr > self.rom.prg_rom.len() as u16 {
            addr %= 0x4000;
        }
        self.rom.prg_rom[addr as usize]
    }
}

impl Mem for Bus {
    fn mem_read(&self, addr: u16) -> u8 {
        match addr {
            RAM_START..=RAM_END => self.cpu_ram[(addr & 0b0000_0111_1111_1111) as usize],
            PPU_START..=PPU_END => self.cpu_ram[(addr & 0b0010_0000_0000_0111) as usize],
            0x8000..=0xFFFF => self.read_prg_rom(addr),
            _ => todo!(),
        }
    }
    
    fn mem_write(&mut self, addr: u16, data: u8) {
        match addr {
            RAM_START..=RAM_END => self.cpu_ram[(addr & 0b0000_0111_1111_1111) as usize] = data,
            PPU_START..=PPU_END => self.cpu_ram[(addr & 0b0010_0000_0000_0111) as usize] = data,
            0x8000..=0xFFFF => panic!("Cannot write to cartridge ROM"),
            _ => println!("Ignoring mem-write request at {addr}"),
        }
    }
}