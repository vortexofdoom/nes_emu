pub struct PPUAddr {
    bytes: (u8, u8),
    empty: bool,
}

impl PPUAddr {
    pub fn new() -> Self {
        PPUAddr { 
            bytes: (0, 0), 
            empty: true, 
        }
    }

    pub fn get(&self) -> u16 {
        u16::from_be_bytes([self.bytes.0, self.bytes.1])
    }

    pub fn set(&mut self, data: u8) {
        if self.empty {
            self.bytes.0 = data & 0b0011_1111;
        } else {
            self.bytes.1 = data;
        }
        self.empty = !self.empty;
    }

    pub fn increment(&mut self, val: u8) {
        let carry: bool;
        (self.bytes.1, carry) = self.bytes.1.overflowing_add(val);
        if carry {
            self.bytes.0 = self.bytes.0.wrapping_add(1) & 0b0011_1111;
        }
    }

    pub fn reset_latch(&mut self) {
        self.empty = true;
    }
}
