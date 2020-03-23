#[macro_use]
extern crate bitflags;

use ggez::{graphics, Context, ContextBuilder, GameResult};
use ggez::conf::{WindowMode, WindowSetup, NumSamples, FullscreenType};
use ggez::event::{self, EventHandler};

extern crate clap;
use clap::{App};

use std::io;
use std::env;
use std::path;

mod cpu;
mod system;
mod memory;
mod gui;
mod bits;

fn main() -> GameResult {
    let _matches = App::new("RGG emulator")
                    .about("A Game Gear emulator")
                    .get_matches();

    println!("[+] Starting RGG");

    let rom = memory::Rom::open("roms/Sonic The Hedgehog (World) (Rev 1).gg")?;
    //let rom = memory::Rom::open("roms/Columns (USA, Europe).gg")?;
    //let rom = memory::Rom::open("roms/junction.gg")?;
    //let rom = memory::Rom::open("roms/senna.gg")?;

    println!("[+] loaded ROM is {} bytes", rom.size());
 

    let rsrc_dir = if let Ok(manifest_dir) = env::var("CARGO_MANIFEST_DIR") {
        let mut path = path::PathBuf::from(manifest_dir);
        path.push("resources");
        path
    }
    else {
        path::PathBuf::from("./resources")
    };


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
                                            vsync: false,
                                            icon: "".to_owned(),
                                            srgb: true,
                                        })
                                        .build()
                                        .unwrap();

    // instanciate gg emulator
    let mut gg = system::GameGear::new(rom);

    // instanciate an emulator window
    let mut emu_window = gui::EmulatorWindow::new(&mut ctx, gg)?;
    // run window
    event::run(&mut ctx, &mut event_loop, &mut emu_window)
}
