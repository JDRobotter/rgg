use crate::cpu::Z80;
use crate::memory::Rom;
use crate::system::JoystickButton;

use std::collections::VecDeque;

pub struct GameGear {
 
    // cpu
    pub cpu: Z80,

    // debug instructions
    pub instructions: VecDeque<String>,

    // debug file

    // last scanline in cpu cycles
    last_scanline_cycle: i64,

    // first game gear step
    first_step: bool,
}

impl GameGear {

    pub fn new(rom: Rom) -> GameGear {

        let cpu = Z80::new(rom);

        GameGear {
            cpu: cpu,
            instructions: VecDeque::new(),
            last_scanline_cycle: 0,
            first_step: true,
        }
    }

    pub fn reset(&mut self) {
        self.cpu.reset();
    }

    pub fn set_button_state(&mut self, b:JoystickButton, state:bool) {
        self.cpu.bus.joystick.set_state(b,state)
    }

    pub fn step(&mut self) -> (bool,bool) {

        // if this step is the first one, synchronize CPU emulation
        // and PSG emulation timings
        if self.first_step {

            self.cpu.bus.synchronize_psg(0);

            self.first_step = false;
        }
        // emulate for one scaneline
        //
        // a frame is 1/60Hz = 16.66 ms long
        // a frame is 224 scanlines
        // a scanline is 74us long
        // Z80 runs at 3.579545 MHz
        // Z80 can run approx 266 clocks cycles per scanline

        let mut new_frame = false;
        let mut will_break = self.cpu.step();

        // bus, VDP or PSG may trigger a breakpoint
        will_break |= self.cpu.bus.will_break();
        will_break |= self.cpu.bus.vdp.will_break();
        will_break |= self.cpu.bus.psg.will_break();

        // push last decoded instruction to debug
        let ass = self.cpu.dissassembly_debug_string();

        self.instructions.push_front(ass);

        self.instructions.truncate(15);
        
        // duration in CPU cycles since last scanline
        let delta_sc = self.cpu.cycles() - self.last_scanline_cycle;
        if delta_sc > 266 {
            // currently increase one scaneline
            let (irq, nf) = self.cpu.bus.vdp.step();
            new_frame = nf;

            self.last_scanline_cycle = self.cpu.cycles();

            // propagate IRQ to CPU
            if irq {
                self.cpu.interrupt();
            }
        }

        (will_break, new_frame)
    }
}

