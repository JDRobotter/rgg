use crate::cpu::Z80;
use crate::memory::Rom;
use crate::system::VDP;

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

        cpu.set_breakpoint(0x0400);

        GameGear {
            cpu: cpu,
            instructions: VecDeque::new(),
            debug_file: File::create("/tmp/rgg.trace").unwrap(),
            steps: 0,
        }
    }

    pub fn step(&mut self) -> bool {

        // emulate for one scaneline
        //
        // a frame is 1/60Hz = 16.66 ms long
        // a frame is 224 scanlines
        // a scanline is 74us long
        // Z80 runs at 3.579545 MHz
        // Z80 can run approx 266 clocks cycles per scanline

        let will_break = self.cpu.step();
        self.steps += 1;

        // push last decoded instruction to debug
        let ass = self.cpu.dissassembly_debug_string();

        self.debug_file.write(ass.as_bytes());
        self.debug_file.write("\n".as_bytes());
        self.debug_file.flush();

        self.instructions.push_front(ass);

        self.instructions.truncate(15);
        
        if self.steps > 100 {
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

