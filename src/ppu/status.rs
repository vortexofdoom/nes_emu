bitflags! {
    pub struct PPUStatus: u8 {
        const VBLANK            = 0b10000000;
        const SPRITE_0_HIT      = 0b01000000;
        const SPRITE_OVERFLOW   = 0b00100000;
    }
}

impl PPUStatus {
    pub fn new() -> Self {
        PPUStatus { bits: 0b00000000 }
    }

    pub fn set_vblank(&mut self, status: bool) {
        self.set(PPUStatus::VBLANK, status);
    }

    pub fn set_sprite_0_hit(&mut self, status: bool) {
        self.set(PPUStatus::SPRITE_0_HIT, status);
    }

    pub fn set_sprite_overflow(&mut self, status: bool) {
        self.set(PPUStatus::SPRITE_OVERFLOW, status);
    }
    
    pub fn is_in_vblank(&self) -> bool {
        self.contains(PPUStatus::VBLANK)
    }
}