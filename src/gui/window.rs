use cgmath;

use ggez;
use ggez::graphics::{self, DrawParam};
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
            running: true,
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

impl event::EventHandler for EmulatorWindow {

    fn key_down_event(&mut self, _ctx: &mut Context, kc: KeyCode, km: KeyMods, repeat:bool) {
        match kc {
            KeyCode::Space => { self.running = !self.running },
            KeyCode::S => { self.run_one = true },
            _ => {}
        }
    }

    fn update(&mut self, ctx: &mut Context) -> GameResult {
        Ok(())
    }

    fn draw(&mut self, ctx: &mut Context) -> GameResult {

        const DESIRED_FPS: u32 = 60;

        while timer::check_update_time(ctx, DESIRED_FPS) {
            
            if self.running {
                self.run(100);
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

        graphics::present(ctx)
    }

}
