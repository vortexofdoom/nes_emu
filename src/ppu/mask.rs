bitflags! {
    pub struct PPUMask: u8 {
        const GRAYSCALE     = 0b00000001;
        const BG_LEFT_8PX   = 0b00000010;
        const SPR_LEFT_8PX  = 0b00000100;
        const SHOW_BG       = 0b00001000;
        const SHOW_SPRITES  = 0b00010000;
        const EMPH_RED      = 0b00100000;
        const EMPH_GREEN    = 0b01000000;
        const EMPH_BLUE     = 0b10000000;
    }
}

pub enum Color {
    Red,
    Green,
    Blue,
}

impl PPUMask {
    pub fn new() -> Self {
        PPUMask { bits: 0b00000000 }
    }

    pub fn set_bits(&mut self, data: u8) {
        self.bits = data;
    }
}