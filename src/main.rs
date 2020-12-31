#[macro_use]
extern crate bitflags;

use ggez::{ContextBuilder, GameResult};
use ggez::conf::{WindowMode, WindowSetup, NumSamples, FullscreenType};
use ggez::event::{self};

extern crate clap;
use clap::{Arg, App};

use std::env;
use std::path;

mod cpu;
mod system;
mod memory;
mod gui;
mod bits;
mod audio;
mod math;

fn main() -> GameResult {
    let matches = App::new("RGG emulator")
                    .about("A Game Gear emulator")
                    .arg(Arg::with_name("rom")
                            .help("ROM file to run")
                            .required(true)
                            .index(1))
                    .arg(Arg::with_name("pause")
                            .short("p")
                            .help("start emulator paused")
                            )
                    .get_matches();


    println!("[+] Starting RGG");
    
    let rom_filename = matches.value_of("rom").expect("rom filename is mandatory");
    println!("[+] loading {}", rom_filename);

    let rom = memory::Rom::open(rom_filename)?;
    println!("[+] loaded ROM is {} bytes", rom.size());
 
    let pause = matches.occurrences_of("pause") > 0;

    let rsrc_dir = if let Ok(manifest_dir) = env::var("CARGO_MANIFEST_DIR") {
        let mut path = path::PathBuf::from(manifest_dir);
        path.push("resources");
        path
    }
    else {
        path::PathBuf::from("./resources")
    };

    // do not use WINIT backend
    std::env::set_var("WINIT_UNIX_BACKEND", "x11");

    let (mut ctx, mut event_loop) = ContextBuilder::new("RGG", "RGG")
                                        .add_resource_path(rsrc_dir)
                                        .window_mode(WindowMode {
                                            width: 1024.0,
                                            height: 768.0,
                                            borderless: false,
                                            fullscreen_type: FullscreenType::Windowed,
                                            min_width: 0.0,
                                            max_width: 0.0,
                                            min_height: 0.0,
                                            max_height: 0.0,
                                            resizable: false,
                                            maximized: false,
                                        })
                                        .window_setup(WindowSetup {
                                            title: "RGG".to_owned(),
                                            samples: NumSamples::Zero,
                                            vsync: true,
                                            icon: "".to_owned(),
                                            srgb: true,
                                        })
                                        .build()
                                        .unwrap();

    // instanciate gg emulator
    let gg = system::GameGear::new(rom);

    // instanciate an emulator window
    let mut emu_window = gui::EmulatorWindow::new(&mut ctx, gg, !pause)?;

    // run window
    event::run(&mut ctx, &mut event_loop, &mut emu_window)
}
