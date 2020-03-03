use crate::cpu::Z80;
use crate::system::SystemBus;
use crate::memory::Rom;

pub struct GameGear {
 
    // bus
    bus: SystemBus,
    // Z80 CPU
    cpu: Z80,

}

impl GameGear {

    pub fn new(rom: Rom) -> GameGear {
        GameGear {
            bus: SystemBus::new(rom),
            cpu: Z80::new(),
        }
    }

    pub fn step(&mut self) {
        self.cpu.step(&mut self.bus);
    }
}

