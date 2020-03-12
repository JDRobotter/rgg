use cgmath;

use ggez;
use ggez::graphics::{self, DrawParam};
use ggez::{event, Context, GameResult};

use crate::system::GameGear;

use std::collections::VecDeque;

pub struct EmulatorWindow {

    gg: GameGear,

    font: graphics::Font,

    instructions: VecDeque<String>,

}

impl EmulatorWindow {
    
    pub fn new(ctx: &mut Context, gg: GameGear) -> GameResult<EmulatorWindow> {


        let font = graphics::Font::new(ctx, "/DejaVuSansMono.ttf")?;

        Ok(EmulatorWindow {
            gg: gg,
            font:font,
            instructions: VecDeque::new(),
        })
    }
}

impl event::EventHandler for EmulatorWindow {

    fn update(&mut self, ctx: &mut Context) -> GameResult {

        for i in 0..1 {
            self.gg.step();

            // push last decoded instruction to debug
            self.instructions.push_front(
                self.gg.cpu.dissassembly_debug_string()
            );
            self.instructions.truncate(30);
        }

        Ok(())
    }

    fn draw(&mut self, ctx: &mut Context) -> GameResult {


        graphics::clear(ctx, graphics::BLACK);

        // -- draw GG LCD screen --
        // draw border
        let sw = 160.0*4.0;
        let sh = 144.0*4.0;
        let r = graphics::Rect::new(20.0, 20.0, sw+2.0, sh+2.0);
        let rlcd = graphics::Mesh::new_rectangle(ctx,
                                    graphics::DrawMode::stroke(2.0),
                                    r,
                                    graphics::WHITE)?;
        graphics::draw(ctx, &rlcd, DrawParam::default())?;

        // draw screen 
        let mut rgba: [u8; 160*144*4] = [0; 160*144*4];

        let im = graphics::Image::from_rgba8(ctx, 160, 144, &rgba)?;
        graphics::draw(ctx, &im, 
            graphics::DrawParam::new()
                .dest(cgmath::Point2::new(21.0, 21.0))
                .scale(cgmath::Vector2::new(4.0, 4.0)))?;

        // -- draw GG instructions --
        for (idx,line) in self.instructions.iter().rev().enumerate() {
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

        
        graphics::present(ctx)
    }

}
