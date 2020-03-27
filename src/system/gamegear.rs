use crate::cpu::Z80;
use crate::memory::Rom;
use crate::system::VDP;
use crate::system::JoystickButton;

use std::collections::VecDeque;

use std::io::prelude::*;
use std::fs::File;

pub struct GameGear {
 
    // cpu
    pub cpu: Z80,

    // debug instructions
    pub instructions: VecDeque<String>,

    // debug file
    debug_file: File,

    // step counter
    steps: u32,
}

impl GameGear {

    pub fn new(rom: Rom) -> GameGear {

        let mut cpu = Z80::new(rom);

/*
        cpu.set_breakpoint(0x2b89);
        cpu.set_breakpoint(0x2b07);
        cpu.set_breakpoint(0x2af7);
        cpu.set_breakpoint(0x2ae8);
        cpu.set_breakpoint(0x2a9e);
        cpu.set_breakpoint(0x1981);
*/
        //cpu.set_breakpoint(0x2ad4);
        //cpu.set_breakpoint(0x27e);
        //cpu.set_breakpoint(0x2b03);
        //cpu.set_breakpoint(0x33d5);

        GameGear {
            cpu: cpu,
            instructions: VecDeque::new(),
            debug_file: File::create("/tmp/rgg.trace").unwrap(),
            steps: 0,
        }
    }

    pub fn reset(&mut self) {
        self.cpu.reset();
    }

    pub fn set_button_state(&mut self, b:JoystickButton, state:bool) {
        self.cpu.bus.joystick.set_state(b,state)
    }

    pub fn step(&mut self) -> bool {

        // emulate for one scaneline
        //
        // a frame is 1/60Hz = 16.66 ms long
        // a frame is 224 scanlines
        // a scanline is 74us long
        // Z80 runs at 3.579545 MHz
        // Z80 can run approx 266 clocks cycles per scanline

        let mut will_break = self.cpu.step();
        self.steps += 1;

        // VDP may trigger a breakpoint
        will_break |= self.cpu.bus.vdp.will_break();

        // push last decoded instruction to debug
        let ass = self.cpu.dissassembly_debug_string();

        //self.debug_file.write(ass.as_bytes());
        //self.debug_file.write("\n".as_bytes());
        //self.debug_file.flush();

        self.instructions.push_front(ass);

        self.instructions.truncate(15);
        
        if self.steps > 20 {
            // currently increase one scaneline
            let irq = self.cpu.bus.vdp.step();
            self.steps = 0;

            // propagate IRQ to CPU
            if irq {
                self.cpu.interrupt();
            }
        }

        will_break
    }
}

