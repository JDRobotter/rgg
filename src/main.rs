
extern crate clap;
use clap::{App};

mod cpu;
mod system;
mod memory;

fn main() {
    let _matches = App::new("RGG emulator")
                    .about("A Game Gear emulator")
                    .get_matches();

    println!("[+] Starting RGG");


    let mut decoder = cpu::Z80InstructionDecoder::new();

    let rom = memory::Rom::open("roms/Sonic The Hedgehog (World) (Rev 1).gg");
    //let rom = memory::Rom::open("roms/a.gg");

    if let Err(error) = rom {
        println!("[!] unable to open ROM: {}", error);
        return;
    }
    let mut rom = rom.unwrap();

    println!("[+] loaded ROM is {} bytes", rom.size());

    rom.seek(0x25e);

    let mut addr = 0;
    loop {
        match rom.read_one() {
            Some(byte) => {
                match decoder.push(byte) {
                    Some(ins) => {
                        println!("{:04x}: {}", addr, ins.to_string());
                    },
                    None => {},
                }
            },
            None => {
                break;
            },
        }
        addr += 1;
    }
}
