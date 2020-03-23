use cgmath;

use ggez;
use ggez::graphics::{self, DrawParam, Scale, Color};
use ggez::{event, Context, GameResult};
use ggez::event::{KeyCode, KeyMods};
use ggez::input::keyboard;
use ggez::timer;

use crate::system::GameGear;

pub struct EmulatorWindow {

    gg: GameGear,

    font: graphics::Font,

    running: bool,
    run_one: bool,
}

impl EmulatorWindow {
    
    pub fn new(ctx: &mut Context, gg: GameGear) -> GameResult<EmulatorWindow> {


        let font = graphics::Font::new(ctx, "/DejaVuSansMono.ttf")?;

        Ok(EmulatorWindow {
            gg: gg,
            font:font,
            running: false,
            run_one: false,
        })
    }

    fn run(&mut self, steps: usize) {
        for i in 0..steps {
            if self.gg.step() {
                self.running = false;
                self.run_one = false;
                break;
            }
        }
    }

}

use crate::system::JoystickButton as Button;

impl event::EventHandler for EmulatorWindow {

    fn key_up_event(&mut self, _ctx: &mut Context, kc: KeyCode, km: KeyMods) {
        match kc {
            KeyCode::Up => self.gg.set_button_state(Button::Up, false),
            KeyCode::Down => self.gg.set_button_state(Button::Down, false),
            KeyCode::Left => self.gg.set_button_state(Button::Left, false),
            KeyCode::Right => self.gg.set_button_state(Button::Right, false),
            KeyCode::Y => self.gg.set_button_state(Button::Start, false),
            KeyCode::U => self.gg.set_button_state(Button::A, false),
            KeyCode::J => self.gg.set_button_state(Button::B, false),
            _ => {},
        }
    }

    fn key_down_event(&mut self, _ctx: &mut Context, kc: KeyCode, km: KeyMods, repeat:bool) {
        match kc {
            KeyCode::Up => self.gg.set_button_state(Button::Up, true),
            KeyCode::Down => self.gg.set_button_state(Button::Down, true),
            KeyCode::Left => self.gg.set_button_state(Button::Left, true),
            KeyCode::Right => self.gg.set_button_state(Button::Right, true),
            KeyCode::Y => self.gg.set_button_state(Button::Start, true),
            KeyCode::U => self.gg.set_button_state(Button::A, true),
            KeyCode::J => self.gg.set_button_state(Button::B, true),

            KeyCode::Space => { self.running = !self.running },
            KeyCode::S => { self.run_one = true },
            KeyCode::R => { self.gg.reset() },
            _ => {},
        }
    }

    fn update(&mut self, ctx: &mut Context) -> GameResult {
        Ok(())
    }

    fn draw(&mut self, ctx: &mut Context) -> GameResult {

        const DESIRED_FPS: u32 = 60;

        while timer::check_update_time(ctx, DESIRED_FPS) {
            
            if self.running {
                self.run(3000);
            }
            else if self.run_one {
                self.run(1);
                self.run_one = false;
            }
        }

        // -- start drawing --
        //
        graphics::clear(ctx, graphics::BLACK);

        // -- draw GG LCD screen --
        // draw border
        let sw = 256.0*2.0;
        let sh = 224.0*2.0;
        let r = graphics::Rect::new(20.0, 20.0, sw+2.0, sh+2.0);
        let rlcd = graphics::Mesh::new_rectangle(ctx,
                                    graphics::DrawMode::stroke(2.0),
                                    r,
                                    graphics::WHITE)?;
        graphics::draw(ctx, &rlcd, DrawParam::default())?;

        // draw screen 
        
        self.gg.cpu.bus.vdp.render();

        let mut rgba: [u8;256*224*4] = [0;256*224*4];
        for y in 0..224 {
            for x in 0..256 {
                let color = self.gg.cpu.bus.vdp.screen_get_pixel(x,y);

                let p = y*256 + x;
                rgba[4*p + 0] = color.red();
                rgba[4*p + 1] = color.green();
                rgba[4*p + 2] = color.blue();
                rgba[4*p + 3] = 0xff;
            }
        }

        let im = graphics::Image::from_rgba8(ctx, 256, 224, &rgba)?;
        graphics::draw(ctx, &im, 
            graphics::DrawParam::new()
                .dest(cgmath::Point2::new(21.0, 21.0))
                .scale(cgmath::Vector2::new(2.0, 2.0)))?;

        // -- draw VDP tiles pattern numbers overlay --
        for y in 0..28 {
            for x in 0..32 {
                
                let idx = self.gg.cpu.bus.vdp.debug_get_tile_number(x,y);
                let mut text = graphics::Text::new(
                    graphics::TextFragment::new(format!("{:0x}",idx))
                        .font(self.font)
                        .color(Color::new(255.0,0.0,255.0,100.0))
                );
                let xy = cgmath::Point2::new(21.0 + 2.0*(x as f32)*8.0,
                                                21.0 + 2.0*(y as f32)*8.0);
                graphics::draw(ctx, &text, (xy,))?;
            }
        }

        // -- draw GG instructions --
        for (idx,line) in self.gg.instructions.iter().rev().enumerate() {
            let text = graphics::Text::new((line.as_str(), self.font, 16.0));
            let mf = idx as f32;
            let xy = cgmath::Point2::new(40.0 + sw, 20.0 + mf*18.0);
            graphics::draw(ctx, &text, (xy,))?;
        }

        // -- draw GG Z80 registers status --
        let text = graphics::Text::new((
                    self.gg.cpu.registers_debug_string().as_str(),
                    self.font,
                    16.0));
        let xy = cgmath::Point2::new(20.0, sh + 40.0);
        graphics::draw(ctx, &text, (xy,))?;

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
                                    graphics::WHITE)?;
        graphics::draw(ctx, &rlcd, DrawParam::default())?;


        // -- draw GG pattern table --
        //
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
                let bx = sw + 40.0;
                let by = 350.0;
                let im = graphics::Image::from_rgba8(ctx, 8, 8, &rgba)?;
                graphics::draw(ctx, &im,
                    graphics::DrawParam::new()
                        .dest(cgmath::Point2::new(bx + (px as f32)*8.0,
                                                    by + (py as f32)*8.0)))?;

            }
        }

        // -- draw GG sprite table --
        //
        for py in 0..16 {
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
                let bx = sw + 40.0 + 16.0*8.0 + 40.0;
                let by = 350.0;
                let im = graphics::Image::from_rgba8(ctx, 8, 8, &rgba)?;
                graphics::draw(ctx, &im,
                    graphics::DrawParam::new()
                        .dest(cgmath::Point2::new(bx + (px as f32)*8.0,
                                                    by + (py as f32)*8.0)))?;

            }
        }


        graphics::present(ctx)
    }

}
