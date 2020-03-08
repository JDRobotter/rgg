use crate::cpu::Z80;
use crate::system::SystemBus;
use crate::memory::Rom;
use crate::system::VDP;

use std::rc::Rc;
use std::cell::RefCell;

pub struct GameGear {
 
    // cpu
    cpu: Z80,
}

impl GameGear {

    pub fn new(rom: Rom) -> GameGear {

        GameGear {
            cpu: Z80::new(rom),
        }
    }

    pub fn step(&mut self) {

        self.cpu.step();

        self.cpu.bus.vdp.step();
    }
}

