#[macro_use]
extern crate bitflags;

extern crate clap;
use clap::{App};

use std::io;

mod cpu;
mod system;
mod memory;

fn main() {
    let _matches = App::new("RGG emulator")
                    .about("A Game Gear emulator")
                    .get_matches();

    println!("[+] Starting RGG");

    let rom = memory::Rom::open("roms/Sonic The Hedgehog (World) (Rev 1).gg");

    if let Err(error) = rom {
        println!("[!] unable to open ROM: {}", error);
        return;
    }
    let mut rom = rom.unwrap();
    println!("[+] loaded ROM is {} bytes", rom.size());
    
    let mut gg = system::GameGear::new(rom);

    loop {
        gg.step();

        // wait for a key press
        //let mut s = String::new();
        //io::stdin().read_line(&mut s);
    }

}