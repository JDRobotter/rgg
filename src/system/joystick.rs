

bitflags! {
    // joystick IO data port 0xDC
    struct JoystickIOReg00: u8 {
        const STT = 0b1000_0000;
    }
}

bitflags! {
    // joystick IO data port 0xDC
    struct JoystickIORegDC: u8 {
        const DWE = 0b1000_0000;
        const UPE = 0b0100_0000;
        const TR1 = 0b0010_0000;
        const TL1 = 0b0001_0000;
        const RI1 = 0b0000_1000;
        const LE1 = 0b0000_0100;
        const DW1 = 0b0000_0010;
        const UP1 = 0b0000_0001;
    }
}

bitflags! {
    // joystick IO data port 0xDD
    struct JoystickIORegDD: u8 {
        const THE = 0b1000_0000;
        const UN6 = 0b0100_0000;
        const UN5 = 0b0010_0000;
        const UN4 = 0b0001_0000;
        const TRE = 0b0000_1000;
        const TLE = 0b0000_0100;
        const RIE = 0b0000_0010;
        const LEE = 0b0000_0001;
    }
}

#[derive(Debug)]
pub enum JoystickButton {
    Up,
    Down,
    Left,
    Right,
    A,
    B,
    Start,
}

pub struct Joystick {
    reg_00: JoystickIOReg00,
    reg_dc: JoystickIORegDC,
    reg_dd: JoystickIORegDD,
}

impl Joystick {
    pub fn new() -> Joystick {
        Joystick {
            reg_00: JoystickIOReg00::all(),
            reg_dc: JoystickIORegDC::all(),
            reg_dd: JoystickIORegDD::all(),
        }
    }
    
    pub fn to_string(&self) -> String {
        let left =  "\u{21e6}";
        let up =    "\u{21e7}";
        let right = "\u{21e8}";
        let down =  "\u{21e9}";

        format!("{}{}{}{}{}{}{}",
                if self.reg_dc.contains(JoystickIORegDC::LE1) {" "} else {left},
                if self.reg_dc.contains(JoystickIORegDC::UP1) {" "} else {up},
                if self.reg_dc.contains(JoystickIORegDC::RI1) {" "} else {right},
                if self.reg_dc.contains(JoystickIORegDC::DW1) {" "} else {down},
                if self.reg_dc.contains(JoystickIORegDC::TL1) {" "} else {"A"},
                if self.reg_dc.contains(JoystickIORegDC::TR1) {" "} else {"B"},
                if self.reg_00.contains(JoystickIOReg00::STT) {" "} else {"S"},
            )
    }

    pub fn set_state(&mut self, b:JoystickButton, state:bool) {
        match b {
            JoystickButton::Up =>       self.reg_dc.set(JoystickIORegDC::UP1, !state),
            JoystickButton::Down =>     self.reg_dc.set(JoystickIORegDC::DW1, !state),
            JoystickButton::Left =>     self.reg_dc.set(JoystickIORegDC::LE1, !state),
            JoystickButton::Right =>    self.reg_dc.set(JoystickIORegDC::RI1, !state),
            JoystickButton::A =>        self.reg_dc.set(JoystickIORegDC::TL1, !state),
            JoystickButton::B =>        self.reg_dc.set(JoystickIORegDC::TR1, !state),
            JoystickButton::Start =>    self.reg_00.set(JoystickIOReg00::STT, !state),
        }
    }

    pub fn register_00(&self) -> u8 {
        self.reg_00.bits
    }

    pub fn register_dc(&self) -> u8 {
        self.reg_dc.bits
    }

    pub fn register_dd(&self) -> u8 {
        self.reg_dd.bits
    }
}
