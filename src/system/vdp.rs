use crate::system::Screen;

bitflags! {
    struct VDPRegisterModeControl1: u8 {
        // mode control #1
        // D7 - 1= Disable vertical scrolling for columns 24-31
        // D6 - 1= Disable horizontal scrolling for rows 0-1
        // D5 - 1= Mask column 0 with overscan color from register #7
        // D4 - (IE1) 1= Line interrupt enable
        // D3 - (EC) 1= Shift sprites left by 8 pixels
        // D2 - (M4) 1= Use Mode 4, 0= Use TMS9918 modes (selected with M1, M2, M3)
        // D1 - (M2) Must be 1 for M1/M3 to change screen height in Mode 4.
        //          Otherwise has no effect.
        // D0 - 1= No sync, display is monochrome, 0= Normal display


        const DISABLE_VERTICAL_SCROLLING =      0b1000_0000;
        const DISABLE_HORIZONTAL_SCROLLING =    0b0100_0000;
        // unused
        const IE1 =                             0b0000_1000;
        const EC =                              0b0000_0100;
        const M4 =                              0b0000_0010;
        const M2 =                              0b0000_0001;
    }
}

enum VDPDataPortMux {
    VRAM,
    CRAM,
}

bitflags! {
    struct VDPStatusRegister: u8 {
        const VBLANK =              0b1000_0000;
        const SPRITE_OVERFLOW =     0b0100_0000;
        const SPRITE_COLLISION =    0b0010_0000;
        // unused
        // unused
        // unused
        // unused
        // unused
    }
}

/// Sega Game Gear VDP chip
/// https://www.smspower.org/uploads/Development/msvdp-20021112.txt
pub struct VDP {

    // game gear screen
    screen: Screen,

    // flag set when second byte of command word is expected on control port
    cw_second_byte: bool,
    // command word buffer
    cw_buffer: u8,

    // internal registers (10 bytes)
    registers: [u8; 11],

    // data port address register
    dp_address_register: u16,
    dp_read_buffer: u8,

    // VDP status register
    status_register: VDPStatusRegister,

    // video ram (VRAM)
    vram: [u8; 0x4000],

    // color ram (CRAM)
    // In Game Gear mode, CRAM has been expanded to 32 words (64 bytes) Each word
    // defines a single color, like so:
    // MSB              LSB
    // --------BBBBGGGGRRRR
    // - = Unused
    // R = Red component
    // G = Green component
    // B = Blue component
    cram: [u8; 64],
    // CRAM latched byte
    cram_latch: u8,

    // data port muxer state
    data_port_mux: VDPDataPortMux,
}

impl VDP {

    pub fn new() -> VDP {
        VDP {
            screen: Screen::new(),

            cw_second_byte: false,
            cw_buffer: 0,

            registers: [0; 11],

            dp_address_register: 0,
            dp_read_buffer: 0,
            
            status_register: VDPStatusRegister::empty(),

            vram: [0; 0x4000],

            cram: [0; 64],
            cram_latch: 0,

            data_port_mux: VDPDataPortMux::VRAM,
        }
    }

    pub fn step(&mut self) {


        self.screen.step();
    }

    pub fn h_counter(&self) -> u8 {
        // The H counter is incremented with each dot clock. 
        0
    }

    pub fn v_counter(&self) -> u8 {
        // At the end of each scanline, the V counter is incremented by one,
        // and this continues until an entire
        let rl = self.screen.current_scanline();

        if rl <= 0xda {
            rl as u8
        }
        else {
            (rl - 0xDA + 0xD5) as u8
        }
    }

    pub fn read_control_port(&mut self) -> u8 {

        // clear cw flag
        self.cw_second_byte = false;

        // return status byte
        self.status_register.bits
    }

    pub fn write_control_port(&mut self, byte: u8) {

        // The command word has the following format:
        // MSB                         LSB
        // CD1 CD0 A13 A12 A11 A10 A09 A08    Second byte written
        // A07 A06 A05 A04 A03 A02 A01 A00    First byte written
        //
        // CDx : Code register 
        // Axx : Address register

        if !self.cw_second_byte {
            // byte is the first byte of a command word
            
            self.cw_buffer = byte;
            self.cw_second_byte = true;
        }
        else {
            // byte is the second byte of a command word
            
            let lobyte = self.cw_buffer;
            let hibyte = byte;

            // compute address register and code register values
            let address_register:u16 = ((lobyte as u16) | ((hibyte as u16) << 8)) & 0x3FFF;
            let code_register:u8 = hibyte >> 6;
            
            // execute command word
            self.execute_command_word(address_register, code_register);

            // reset command word flag
            self.cw_second_byte = false;
        }
    }

    pub fn read_data_port(&mut self) -> u8 {

        // clear cw flag
        self.cw_second_byte = false;

        self.dp_read_buffer
    }

    pub fn write_data_port(&mut self, byte: u8) {

        // clear cw flag
        self.cw_second_byte = false;

        match self.data_port_mux {
            VDPDataPortMux::VRAM => {

                println!("VDP VRAM W @{:04x} {:02x}", self.dp_address_register, byte);

                self.vram[self.dp_address_register as usize] = byte;
            },

            VDPDataPortMux::CRAM => {

                // CRAM address register wraps past 0x3f
                let addr = self.dp_address_register & 0x3f;

                // Writing an even CRAM address causes the data written to be stored in a latch, and writing to
                // an odd CRAM address makes the VDP write the contents of the latch as well
                // as the new data from the CPU to the current CRAM entry.
                
                let odd = addr & 0x01 == 0x01;
                if odd {
                    println!("VDP CRAM W @{:02x} {:02x} {:02x}", addr, self.cram_latch, byte);
                    // odd address, write latched byte then new byte
                    self.cram[addr as usize] = self.cram_latch;
                    self.increment_address_register();
                    self.cram[addr as usize] = byte;
                }
                else {
                    // even address, store received byte in latch
                    self.cram_latch = byte;
                }
            }
        }

        // increment address register
        self.increment_address_register();
    }

    /// Increment address register by 1 and wrap around 0x3ff
    fn increment_address_register(&mut self ) {
        self.dp_address_register += 1;
        if self.dp_address_register > 0x3ff {
            self.dp_address_register = 0;
        }
    }

    fn execute_command_word(&mut self, addr:u16, code:u8) {

        println!("VDP COMMAND @{:04x} C{}",addr,code);

        match code {
            0 => {
                // A byte of VRAM is read from the location defined by the address register and is
                // stored in the read buffer.
                self.dp_read_buffer = self.vram[self.dp_address_register as usize];
                // The address register is incremented by one.
                self.increment_address_register();
                // Write to data port go to VRAM.
                self.data_port_mux = VDPDataPortMux::VRAM;
            },

            1 => {
                // Writes to the data port go to VRAM
                self.data_port_mux = VDPDataPortMux::VRAM;
            },

            2 => {
                // VDP register write
                //
                // address register is interpreted as follow in VDP reg. write mode
                // MSB                         LSB
                //          ?   ?  R03 R02 R01 R00    Second byte written
                // D07 D06 D05 D04 D03 D02 D01 D00    First byte written
                // Rxx : VDP register number
                // Dxx : VDP register data

                let register_number = ((addr >> 8) as u8) & 0x0F;
                let register_data = addr as u8;

                self.write_register(register_number, register_data);
            },

            3 => {
                // Writes to the data port go to CRAM
                self.data_port_mux = VDPDataPortMux::CRAM;
            },

            _ => { panic!("unhandled VDP command code: {}", code) }
        }

    }

    fn write_register(&mut self, n:u8, byte:u8) {

        println!("VDP WRITE REG @{:02x} {:02x} {:08b}",n,byte,byte);

        // write to register
        self.registers[n as usize] = byte;

        // VDP only contains 11 registers
        if n > 0x0A {
            panic!("unhandled VDP register: {}", n)
        }

    }

}
