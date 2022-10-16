use sdl2::sys::Screen;

use crate::rom::Mirroring;
use crate::ppu::{
    addr::PPUAddr, 
    ctrl::PPUCtrl,
    mask::PPUMask,
    scroll::Scroll,
    status::PPUStatus,
};

pub mod addr;
pub mod ctrl;
pub mod mask;
pub mod scroll;
pub mod status;

pub struct PPU {
    pub ctrl: PPUCtrl,
    pub addr: PPUAddr,
    read_buf: u8,
    pub chr_rom: Vec<u8>,
    pub palette_tbl: [u8; 32],

    pub vram: [u8; 2048],
    pub oam: [u8; 256],
    pub oam_addr: u8,
    pub mirroring: Mirroring,
    pub scroll: Scroll,
    pub status: PPUStatus,
    pub mask: PPUMask,

    scanline: u16,
    cycles: usize,
}

impl PPU {
    pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        PPU {
            chr_rom,
            palette_tbl: [0; 32],
            vram: [0; 2048],
            oam: [0; 256],
            oam_addr: 0,
            mirroring,
            addr: PPUAddr::new(),
            mask: PPUMask::new(),
            ctrl: PPUCtrl::new(),
            scroll: Scroll::new(),
            status: PPUStatus::new(),
            read_buf: 0,
            scanline: 0,
            cycles: 0,
        }
    }

    pub fn tick(&mut self, cycles: u8) {
        self.cycles += cycles as usize;
        if self.cycles >= 341 {
            self.cycles -= 341;
            self.scanline += 1;

            if self.scanline == 241 {
                if self.ctrl.contains(PPUCtrl::NMI_GENERATE) {
                    self.status.set_vblank(true);
                    todo!("NMI interrupt")
                }
            }

            if self.scanline >= 262 {
                self.scanline = 0;
                self.status.set_vblank(false);
            }
        }

    }

    pub fn read_status(&mut self) -> u8 {
        let data = self.status.bits();
        self.status.remove(PPUStatus::VBLANK);
        self.addr.reset_latch();
        self.scroll.reset_latch();
        data
    }

    fn increment_vram_addr(&mut self) {
        self.addr.increment(self.ctrl.vram_addr_increment());
    }

    pub fn read_data(&mut self) -> u8 {
        let addr = self.addr.get();
        self.increment_vram_addr();

        let result = self.read_buf;

        match addr {
            0x0000..=0x1FFF => self.read_buf = self.chr_rom[addr as usize],
            0x2000..=0x2FFF => self.read_buf = self.vram[self.mirror_vram_addr(addr) as usize],
            0x3000..=0x3EFF => panic!("Unexpected request to {} between 0x3000..0x3EFF", addr),
            0x3F00..=0x3FFF => return self.palette_tbl[(addr - 0x3F00) as usize],
            _ => panic!("Unexpected request to mirrored space {}", addr),
        }
        result
    }

    pub fn set_oam_addr(&mut self, data: u8) {
        self.oam_addr = data;
    }

    pub fn read_oam(&self) -> u8 {
        self.oam[self.oam_addr as usize]
    }

    pub fn write_to_oam(&mut self, data: u8) {
        self.oam[self.oam_addr as usize] = data;
        self.oam_addr = self.oam_addr.wrapping_add(1);
    }

    pub fn oam_dma(&mut self, data: &[u8]) {
        for byte in data.iter() {
            self.oam[self.oam_addr as usize] = *byte;
            self.oam_addr = self.oam_addr.wrapping_add(1);
        }
    }

    pub fn write_to_data(&mut self, value: u8) {
        let addr = self.addr.get();
        self.increment_vram_addr();

        match addr {
            0x0000..=0x1FFF => println!("attempt to write to chr rom space {addr}"),
            0x2000..=0x2FFF => self.vram[self.mirror_vram_addr(addr) as usize] = value,
            0x3000..=0x3EFF => panic!("{addr} should not be used!"),
            0x3F10 | 0x3F14 | 0x3F18 | 0x3F1C => self.palette_tbl[((addr - 0x10) - 0x3F00) as usize] = value,
            0x3F00..=0x3FFF => self.palette_tbl[(addr - 0x3F00) as usize] = value,
            _ => panic!("Unexpected request to mirrored space {}", addr),
        }
    }

    pub fn mirror_vram_addr(&self, addr: u16) -> u16 {
        let mirrored_addr = addr & 0x2FFF;
        let vram_index = mirrored_addr - 0x2000;
        let nametable = vram_index / 0x400;

        match (&self.mirroring, nametable) {
            (Mirroring::Vertical, 2) | 
            (Mirroring::Vertical, 3) |  
            (Mirroring::Horizontal, 3) => vram_index -0x800,

            (Mirroring::Horizontal, 1) |
            (Mirroring::Horizontal, 2) => vram_index - 0x400,

            _=> vram_index,
        }
    }
}
