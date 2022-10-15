pub struct Scroll {
    x: u8,
    y: u8,
    empty: bool,
}

impl Scroll {
    pub fn new() -> Self {
        Scroll {
            x: 0,
            y: 0,
            empty: true,
        }
    }

    pub fn set(&mut self, data: u8) {
        if self.empty {
            self.x = data;
        } else {
            self.y = data;
        }

        self.empty = !self.empty;
    }

    pub fn reset_latch(&mut self) {
        self.empty = true;
    }
}