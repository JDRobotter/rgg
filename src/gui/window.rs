use glam;

use ggez;
use ggez::graphics::{self, DrawParam};
use ggez::{event, Context, GameResult};
use ggez::event::{GamepadId, Button, KeyCode, KeyMods};

use ggez::timer;
use std::time::Duration;

use crate::system::GameGear;

use crate::math::ScalarStatistics;
use std::time::Instant;

#[allow(dead_code)]
struct RamWatcher {
    addr: u16,
    name: String,
}

pub struct EmulatorWindow {

    gg: GameGear,

    running: bool,
    run_one_step: bool,
    run_one_frame: bool,

    font: graphics::Font,

    #[allow(dead_code)]
    ram_watchers: Vec<RamWatcher>,

    cpu_time: ScalarStatistics,
}

impl EmulatorWindow {
    
    pub fn new(ctx: &mut Context, gg: GameGear, start_immediately:bool) -> GameResult<EmulatorWindow> {

        let watchers = vec![
        ];

        Ok(EmulatorWindow {
            gg: gg,

            running: start_immediately,
            run_one_step: false,
            run_one_frame: false,

            font: graphics::Font::new(ctx, "/DejaVuSansMono.ttf")?,

            ram_watchers: watchers,
            cpu_time: ScalarStatistics::new(5*60),
        })
    }

}

use crate::system::JoystickButton as JoyButton;

impl event::EventHandler for EmulatorWindow {

    fn gamepad_button_down_event(&mut self, _ctx: &mut Context, btn: Button, _id: GamepadId) {
        match btn {
            Button::DPadUp => self.gg.set_button_state(JoyButton::Up, true),
            Button::DPadDown => self.gg.set_button_state(JoyButton::Down, true),
            Button::DPadLeft => self.gg.set_button_state(JoyButton::Left, true),
            Button::DPadRight => self.gg.set_button_state(JoyButton::Right, true),
            Button::Start => self.gg.set_button_state(JoyButton::Start, true),
            Button::South => self.gg.set_button_state(JoyButton::A, true),
            Button::East => self.gg.set_button_state(JoyButton::B, true),
            _ => {},
        }
    }
    fn gamepad_button_up_event(&mut self, _ctx: &mut Context, btn: Button, _id: GamepadId) {
        match btn {
            Button::DPadUp => self.gg.set_button_state(JoyButton::Up, false),
            Button::DPadDown => self.gg.set_button_state(JoyButton::Down, false),
            Button::DPadLeft => self.gg.set_button_state(JoyButton::Left, false),
            Button::DPadRight => self.gg.set_button_state(JoyButton::Right, false),
            Button::Start => self.gg.set_button_state(JoyButton::Start, false),
            Button::South => self.gg.set_button_state(JoyButton::A, false),
            Button::East => self.gg.set_button_state(JoyButton::B, false),
            _ => {},
        }
    }

    fn key_up_event(&mut self, _ctx: &mut Context, kc: KeyCode, _keymod: KeyMods) {
        match kc {
            KeyCode::Up => self.gg.set_button_state(JoyButton::Up, false),
            KeyCode::Down => self.gg.set_button_state(JoyButton::Down, false),
            KeyCode::Left => self.gg.set_button_state(JoyButton::Left, false),
            KeyCode::Right => self.gg.set_button_state(JoyButton::Right, false),
            KeyCode::E => self.gg.set_button_state(JoyButton::Start, false),
            KeyCode::A => self.gg.set_button_state(JoyButton::A, false),
            KeyCode::Z => self.gg.set_button_state(JoyButton::B, false),
            _ => {},
        }
    }

    fn key_down_event(&mut self, _ctx: &mut Context, kc: KeyCode, _keymod: KeyMods, _repeat:bool) {
        match kc {
            KeyCode::Up => self.gg.set_button_state(JoyButton::Up, true),
            KeyCode::Down => self.gg.set_button_state(JoyButton::Down, true),
            KeyCode::Left => self.gg.set_button_state(JoyButton::Left, true),
            KeyCode::Right => self.gg.set_button_state(JoyButton::Right, true),
            KeyCode::E=> self.gg.set_button_state(JoyButton::Start, true),
            KeyCode::A => self.gg.set_button_state(JoyButton::A, true),
            KeyCode::Z => self.gg.set_button_state(JoyButton::B, true),

            KeyCode::Space => { self.running = !self.running },
            KeyCode::S => { self.run_one_step = true },
            KeyCode::F => {
                self.running = true;
                self.run_one_frame = true;
            },
            KeyCode::R => { self.gg.reset() },

            KeyCode::M => { self.gg.save_state(); },
            KeyCode::L => { self.gg.load_state(); },

            _ => {},
        }
    }

    /// Update will happen on every frame before it is drawn
    fn update(&mut self, ctx: &mut Context) -> GameResult {

        let cpu_start = Instant::now();
        loop {
            if self.running {

                // do not trace CPU instructions
                self.gg.trace_instructions(false);

                // step till next frame is ready
                let (breakpoint,new_frame) = self.gg.step();
                if breakpoint {
                    // breakpoint reached
                    self.running = false;
                    self.run_one_step = false;
                }

                // next frame ready
                if new_frame {
                    self.gg.cpu.bus.vdp.render();
                    break;
                }

            }
            else if self.run_one_step {
                // trace CPU instructions 
                self.gg.trace_instructions(true);

                // do one CPU step
                let (_,new_frame) = self.gg.step();
                self.run_one_step = false;

                // next frame ready
                if new_frame {
                    self.gg.cpu.bus.vdp.render();
                    break;
                }

                break;
            }
            else {
                break;
            }
        }

        // manage frame by frame running mode
        if self.run_one_frame {
            self.running = false;
            self.run_one_frame = false;
        }

        self.cpu_time.update(
            cpu_start.elapsed().as_micros() as f64
        );

        Ok(())
    }

    fn draw(&mut self, ctx: &mut Context) -> GameResult {

        graphics::clear(ctx, [0.1, 0.1, 0.1, 1.0].into());

        let fps = timer::fps(ctx);

        // -- start drawing --

        // -- computing screen size --
        // game gear screen is 4:3 but screen is 160x144
        // so we stretch screen by 1.2x1.0
        let sf = 3.5;
        let sw = 160.0*1.2*sf;
        let sh = 144.0*1.0*sf;

        // -- draw emulator stats --
        graphics::draw(ctx,
                        &graphics::Text::new((
                            format!("FPS: {:1.1} CPU: ({:6.0}us {:6.0}us {:6.0}us)",
                                fps,
                                self.cpu_time.min(),
                                self.cpu_time.mean(),
                                self.cpu_time.max(),
                            ),
                            self.font,
                            14.0
                        )),
                        (glam::vec2(20.0, 1.0),),
        )?;
        // -- draw GG LCD screen --
        // draw border
        let r = graphics::Rect::new(20.0, 20.0, sw+2.0, sh+2.0);
        let rlcd = graphics::Mesh::new_rectangle(ctx,
                                    graphics::DrawMode::stroke(2.0),
                                    r,
                                    graphics::Color::new(1.0,1.0,1.0,1.0))?;
        graphics::draw(ctx, &rlcd, DrawParam::default())?;

        let mut rgba: [u8;160*144*4] = [0;160*144*4];
        for y in 0..144 {
            for x in 0..160 {

                // usable LCD screen is positionned at (48,24) in rendered area
                let color = self.gg.cpu.bus.vdp.screen_get_pixel(x + 48, y + 24);

                let p = y*160 + x;
                rgba[4*p + 0] = color.red();
                rgba[4*p + 1] = color.green();
                rgba[4*p + 2] = color.blue();
                rgba[4*p + 3] = 0xff;
            }
        }


        // scale factor
        let im = graphics::Image::from_rgba8(ctx, 160, 144, &rgba)?;
        graphics::draw(ctx,
                        &im,
                        graphics::DrawParam::new()
                            .dest(glam::vec2(21.0, 21.0))
                            .scale(glam::vec2(1.2*sf, sf)))?;

        // -- draw VDP tiles pattern numbers overlay --
        /*
        for y in 0..28 {
            for x in 0..32 {
                
                let idx = self.gg.cpu.bus.vdp.debug_get_tile_number(x,y);
                let text = graphics::Text::new(
                    graphics::TextFragment::new(format!("{:0x}",idx))
                        .font(self.font)
                        .color(Color::new(255.0,0.0,255.0,100.0))
                );
                let xy = cgmath::Point2::new(21.0 + 2.0*(x as f32)*8.0,
                                                21.0 + 2.0*(y as f32)*8.0);
                graphics::draw(ctx, &text, (xy,))?;
            }
        }
        */

        // -- draw GG instructions --
        for (idx,line) in self.gg.instructions.iter().rev().enumerate() {

            let mf = idx as f32;
            graphics::draw(ctx,
                        &graphics::Text::new((line.as_str(), self.font, 16.0)),
                        (glam::vec2(40.0 + sw, 20.0 + mf*18.0),))?;
        }
        
        
        // -- draw GG Z80 registers status --
        graphics::draw(ctx,
            &graphics::Text::new((self.gg.cpu.registers_debug_string().as_str(), self.font, 16.0)),
            (glam::vec2(20.0, sh + 40.0),))?;

        /*
        // -- draw watchers values --
        for (idx,rw) in self.ram_watchers.iter().enumerate() {
            
            let b8 = self.gg.cpu.bus.cpu_read(rw.addr);
            let b16 = self.gg.cpu.bus.cpu_read_u16(rw.addr);
            let text = graphics::Text::new((
                    format!("@{:04x} {:5}: @{:04x} {:6} @{:02x} {:3}",
                        rw.addr, rw.name,
                        b16, b16 as i16,
                        b8, b8 as i8),
                    self.font,
                    16.0));
            let mf = idx as f32;
            let xy = cgmath::Point2::new(sw + 20.0, sh + 40.0 + mf*18.0);
            graphics::draw(ctx, &text, (xy,))?;
        }
        */
        
        // -- draw GG VDP palettes --
        //
        let bx = sw + 40.0;
        let by = 300.0;
        let w = 10.0;
        let h = 20.0;
        for pi in 0..2 {
            for ci in 0..16 {
                let ucolor = self.gg.cpu.bus.vdp.get_color_from_palette(pi,ci).rgb();
                let r = graphics::Rect::new(bx + (ci as f32)*w, 
                                            by + (pi as f32) *h,
                                            w,
                                            h);
                let rlcd = graphics::Mesh::new_rectangle(ctx,
                                    graphics::DrawMode::fill(),
                                    r,
                                    graphics::Color::from_rgb_u32(ucolor))?;
                graphics::draw(ctx, &rlcd, DrawParam::default())?;
            }
        }

        let r = graphics::Rect::new(bx, by, 16.0*w, 2.0*h);
        let rlcd = graphics::Mesh::new_rectangle(ctx,
                                    graphics::DrawMode::stroke(1.0),
                                    r,
                                    graphics::Color::new(1.0,1.0,1.0,1.0))?;
        graphics::draw(ctx, &rlcd, DrawParam::default())?;

        // -- draw joystick/buttons state --
        graphics::draw(ctx,
            &graphics::Text::new((self.gg.cpu.bus.joystick.to_string(), self.font, 16.0)),
            (glam::vec2(sw + 220.0, 310.0),) )?;

        // -- draw GG pattern table --
        let bx = sw + 40.0;
        let by = 350.0;
        for py in 0..16 {
            for px in 0..16 {
                
                // pattern index
                let pidx = py*16 + px;
                // fetch pattern image from VRAM (force palette 0)
                let pattern = self.gg.cpu.bus.vdp.get_tile_pattern(pidx, 0);

                // allocate a buffer for the RGBA image
                let mut rgba: [u8;8*8*4] = [0;8*8*4];
                for y in 0..8 {
                    for x in 0..8 {
                        let color = pattern.get(x,y);
                        let k = y*8+x;
                        rgba[4*k + 0] = color.red();
                        rgba[4*k + 1] = color.green();
                        rgba[4*k + 2] = color.blue();
                        rgba[4*k + 3] = 0xff;
                    }
                }

                // draw image

                let im = graphics::Image::from_rgba8(ctx, 8, 8, &rgba)?;
                graphics::draw(ctx,
                            &im,
                            graphics::DrawParam::new()
                                .dest(glam::vec2(bx + (px as f32)*8.0,
                                                    by + (py as f32)*8.0)))?;
            }
        }

        let w = 16.0;
        let h = 16.0;
        let r = graphics::Rect::new(bx, by, 8.0*w, 8.0*h);
        let rlcd = graphics::Mesh::new_rectangle(ctx,
                                    graphics::DrawMode::stroke(1.0),
                                    r,
                                    graphics::Color::new(1.0,1.0,1.0,1.0))?;
        graphics::draw(ctx, &rlcd, DrawParam::default())?;

        // -- draw GG sprite table --
        //
        let bx = sw + 40.0 + 16.0*8.0 + 10.0;
        let by = 350.0;
        for py in 0..12 {
            for px in 0..16 {
                
                // pattern index
                let pidx = py*16 + px;
                // fetch pattern image from VRAM
                let pattern = self.gg.cpu.bus.vdp.get_sprite_pattern(pidx);

                // allocate a buffer for the RGBA image
                let mut rgba: [u8;8*8*4] = [0;8*8*4];
                for y in 0..8 {
                    for x in 0..8 {
                        let color = pattern.get(x,y);
                        let k = y*8+x;
                        rgba[4*k + 0] = color.red();
                        rgba[4*k + 1] = color.green();
                        rgba[4*k + 2] = color.blue();
                        rgba[4*k + 3] = 0xff;
                    }
                }

                // draw image
                let im = graphics::Image::from_rgba8(ctx, 8, 8, &rgba)?;
                graphics::draw(ctx, &im,
                    graphics::DrawParam::new()
                        .dest(glam::vec2(bx + (px as f32)*8.0,
                                                    by + (py as f32)*8.0)))?;

            }
        }
        let w = 16.0;
        let h = 12.0;
        let r = graphics::Rect::new(bx, by, 8.0*w, 8.0*h);
        let rlcd = graphics::Mesh::new_rectangle(ctx,
                                    graphics::DrawMode::stroke(1.0),
                                    r,
                                    graphics::Color::new(1.0,1.0,1.0,1.0))?;
        graphics::draw(ctx, &rlcd, DrawParam::default())?;
        
        // -- draw RAM --
        /*
        let bx = sw + 40.0;
        let by = 500.0;
        let mut rgba: [u8;91*91*4] = [0;91*91*4];
        for y in 0..91 {
            for x in 0..91 {

                let addr:usize  = y*91 + x;
                let byte = if addr < 8192 {
                    self.gg.cpu.bus.work_ram.read(addr as u16)
                }
                else {
                    0
                };
                rgba[4*addr + 0] = byte;
                rgba[4*addr + 1] = byte;
                rgba[4*addr + 2] = byte;
                rgba[4*addr + 3] = 0xff;
            }
        }
        let im = graphics::Image::from_rgba8(ctx, 91, 91, &rgba)?;
        graphics::draw(ctx, &im,
            graphics::DrawParam::new()
                .dest(glam::vec2(bx,by))
                .scale(glam::vec2(2.0,2.0)))?;
        */
        graphics::present(ctx)?;

        timer::yield_now();

        Ok(())
    }

}
