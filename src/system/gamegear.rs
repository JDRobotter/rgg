use crate::cpu::Z80;
use crate::memory::Rom;
use crate::system::JoystickButton;

use std::collections::VecDeque;

use std::io;
use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;

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

    pub fn save_state(&self) {
        // fetch system serialized state
        let ss = self.cpu.serialize_state();

        // ouput serialized state to file
        let mut f = File::create("./rgg-state.json").unwrap();
        f.write(ss.to_string().as_bytes());

        println!("State saved to file");
    }

    pub fn load_state(&mut self) {
        self.load_state_from_file("./rgg-state.json")
    }

    pub fn load_state_from_file(&mut self, filename:&str) {

        // open state from file
        let f = File::open(filename).unwrap();
        let mut bf = BufReader::new(f);

        let mut fss = String::new();
        bf.read_to_string(&mut fss).unwrap();
        let ss = serde_json::from_str(&fss).unwrap();
        println!("State loaded from file");

        // restore system state
        self.cpu.restore_state(ss);
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

