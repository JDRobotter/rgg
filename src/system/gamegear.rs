use crate::cpu::Z80;

pub struct GameGear {
    
    cpu: Z80,

}

impl GameGear {

    pub fn new() -> GameGear {
        GameGear {
            cpu: Z80::new()
        }
    }
}

