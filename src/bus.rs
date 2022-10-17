use crate::{cpu::Mem, rom::Rom, ppu::PPU};

pub struct Bus {
    cpu_ram: [u8; 2048],
    prg_rom: Vec<u8>,
    ppu: PPU,

    cycles: usize,
}

impl Bus {
    pub fn new(rom: Rom) -> Self {
        let ppu = PPU::new(rom.chr_rom, rom.mirroring);
        Bus {
            cpu_ram: [0; 2048],
            prg_rom: rom.prg_rom,
            ppu,
            cycles: 0,
        }
    }

    pub fn tick(&mut self, cycles: u8) {
        self.cycles += cycles as usize;
        self.ppu.tick(cycles * 3);
    }

    pub fn poll_nmi_interrupt(&self) -> bool {
        self.ppu.nmi_interrupt
    }

    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000;
        if addr >= self.prg_rom.len() as u16 {
            addr %= 0x4000;
        }
        self.prg_rom[addr as usize]
    }
}

impl Mem for Bus {
    fn mem_read(&mut self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.cpu_ram[(addr & 0b0000_0111_1111_1111) as usize],
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => panic!("Attempt to read from write only PPU Address {:x}", addr),
            0x2002          => self.ppu.read_status(),
            0x2004          => self.ppu.read_oam(),
            0x2007          => self.ppu.read_data(),
            0x2008..=0x3FFF => self.mem_read(addr & 0x2007), 
            0x8000..=0xFFFF => self.read_prg_rom(addr),
            _ => 0,
        }
    }
    
    fn mem_write(&mut self, addr: u16, data: u8) {
        match addr {
            0x0000..=0x1FFF => self.cpu_ram[(addr & 0b0000_0111_1111_1111) as usize] = data,
            0x2000          => self.ppu.write_to_ctrl(data),
            0x2001          => self.ppu.mask.set_bits(data),
            0x2002          => panic!("Cannot write to PPU status register"),
            0x2003          => self.ppu.set_oam_addr(data),
            0x2004          => self.ppu.write_to_oam(data),
            0x2005          => self.ppu.scroll.set(data),
            0x2006          => self.ppu.addr.set(data),
            0x2007          => self.ppu.write_to_data(data),
            0x2008..=0x3FFF => self.mem_write(addr & 0x2007, data),
            0x4014          => {
                let slice = &self.cpu_ram[(data as usize)..=(data + 0xFF) as usize];
                self.ppu.oam_dma(slice);
            }
            0x8000..=0xFFFF => panic!("Cannot write to cartridge ROM addr {:x}", addr),
            _ => {},
        }
    }
}