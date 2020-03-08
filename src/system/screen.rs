
/// Sega Game Gear screen
/// screen is 160x144 pixels
pub struct Screen {

    scanline:u16
}

impl Screen {
    pub fn new() -> Screen {
        Screen {
            scanline:0
        }
    }

    pub fn current_scanline(&self) -> u16 {
        self.scanline
    }

    pub fn step(&mut self) {
        self.scanline = (self.scanline + 1)%262;
    }
}
