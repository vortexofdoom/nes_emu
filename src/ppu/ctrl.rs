//0x2000
bitflags! {
    pub struct PPUCtrl: u8 {
        const NAMETABLE1        = 0b0000_0001;
        const NAMETABLE2        = 0b0000_0010;
        const VRAM_INC          = 0b0000_0100;
        const SPRITE_ADDR       = 0b0000_1000;
        const BG_ADDR           = 0b0001_0000;
        const SPRITE_SIZE       = 0b0010_0000;
        const MASTER_SLAVE_SEL  = 0b0100_0000;
        const NMI_GENERATE      = 0b1000_0000;
    }
}

impl PPUCtrl {
    pub fn new() -> Self {
        PPUCtrl { bits: 0b00000000 }
    }

    pub fn vram_addr_increment(&self) -> u8 {
        if self.contains(PPUCtrl::VRAM_INC) { 1 } else { 32 }
    }

    pub fn update(&mut self, data: u8) {
        self.bits = data;
    }

    pub fn sprite_addr(&self) -> u16 {
        if !self.contains(PPUCtrl::SPRITE_ADDR) { 0x0000 } else { 0x1000 }
    }

    pub fn bg_addr(&self) -> u16 {
        if !self.contains(PPUCtrl::BG_ADDR) { 0x0000 } else { 0x1000 }
    }

    pub fn sprite_size(&self) -> u8 {
        if !self.contains(PPUCtrl::SPRITE_SIZE) { 8 } else { 16 }
    }

    pub fn master_slave_select(&self) -> u8 {
        if !self.contains(PPUCtrl::MASTER_SLAVE_SEL) { 0 } else { 1 }
    }
}