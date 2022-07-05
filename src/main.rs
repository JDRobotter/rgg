#[macro_use]
extern crate bitflags;
extern crate itertools;

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
mod utils;

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
                    .arg(Arg::with_name("state")
                            .short("state")
                            .takes_value(true)
                            .help("immediately restore emulator state"))
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

    let (mut ctx, event_loop) = ContextBuilder::new("RGG", "RGG")
                                        .add_resource_path(rsrc_dir)
                                        .window_mode(WindowMode {
                                            width: 1024.0,
                                            height: 768.0,
                                            maximized: false,
                                            fullscreen_type: FullscreenType::Windowed,
                                            borderless: false,
                                            min_width: 1.0,
                                            max_width: 0.0,
                                            min_height: 1.0,
                                            max_height: 0.0,
                                            resizable: false,
                                            visible: true,
                                            resize_on_scale_factor_change: false,
                                        })
                                        .window_setup(WindowSetup {
                                            title: "RGG".to_owned(),
                                            samples: NumSamples::One,
                                            vsync: true,
                                            icon: "".to_owned(),
                                            srgb: true,
                                        })
                                        .build()
                                        .unwrap();

    // instanciate gg emulator
    let mut gg = system::GameGear::new(rom);

    // restore state if needed
    if let Some(state_filename) = matches.value_of("state") {
        gg.load_state_from_file(state_filename);
    }

    // instanciate an emulator window
    let emu_window = gui::EmulatorWindow::new(&mut ctx, gg, !pause)?;

    // run window
    event::run(ctx, event_loop, emu_window)
}
