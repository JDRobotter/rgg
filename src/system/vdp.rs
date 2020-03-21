use crate::bits::Bits;

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

bitflags! {
    struct VDPRegisterModeControl2: u8 {
        // mode control #2
        // D7 - No effect
        // D6 - (BLK) 1= Display visible, 0= display blanked.
        // D5 - (IE0) 1= Frame interrupt enable.
        // D4 - (M1) Selects 224-line screen for Mode 4 if M2=1,
        //              else has no effect.
        // D3 - (M3) Selects 240-line screen for Mode 4 if M2=1,
        //              else has no effect.
        // D2 - No effect
        // D1 - Sprites are 1=16x16,0=8x8 (TMS9918),
        //          Sprites are 1=8x16,0=8x8 (Mode 4)
        // D0 - Sprite pixels are doubled in size.
        
        // unused
        const BLK       = 0b0100_0000;
        const IE0       = 0b0010_0000;
        const M1        = 0b0001_0000;
        const M3        = 0b0000_1000;
        // unused
        const SIZE      = 0b0000_0010;
        const DOUBLED   = 0b0000_0001;
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

/// Background tile table entry
#[derive(Debug)]
struct BgTile {
    priority: bool,
    palette: bool,
    vertical_flip: bool,
    horizontal_flip: bool,
    pattern: u16,
}

bitflags! {
    struct BgTileFlags: u16 {
        const PRIORITY =        0b0001_0000_0000_0000;
        const PALETTE =         0b0000_1000_0000_0000;
        const VERTICAL_FLIP =   0b0000_0100_0000_0000;
        const HORIZONTAL_FLIP = 0b0000_0010_0000_0000;
        // unused
        // unused
        // unused
        // unused
        // unused
    }
}

impl BgTile {
    fn from(word: u16) -> BgTile {
 
        let flags = BgTileFlags::from_bits_truncate(word);

        BgTile {
            priority:           flags.contains(BgTileFlags::PRIORITY),
            palette:            flags.contains(BgTileFlags::PALETTE),
            vertical_flip:      flags.contains(BgTileFlags::VERTICAL_FLIP),
            horizontal_flip:    flags.contains(BgTileFlags::HORIZONTAL_FLIP),
            pattern: word & 0x1ff,
        }
    }
}

/// Screen color representation
#[derive(Copy,Clone)]
pub struct Color {
    r:u8,
    g:u8,
    b:u8
}

impl Color {
    fn new(r:u8, g:u8, b:u8) -> Color {
        Color {
            r:r,
            g:g,
            b:b
        }
    }

    fn black() -> Color {
        Color::new(0,0,0)
    }

    pub fn rgb(&self) -> u32 {
        let r = self.r as u32;
        let g = self.g as u32;
        let b = self.b as u32;
        r << 16| (g << 8) | (b << 0)
    }

    pub fn red(&self) -> u8 { self.r }
    pub fn green(&self) -> u8 { self.g }
    pub fn blue(&self) -> u8 { self.b }
}

/// 8x8 pattern pixel block
struct Pattern {
    pixels: [Color;64],
}

impl Pattern {
    fn new() -> Pattern {
        Pattern {
            pixels: [Color::black();64]
        }
    }
    
    fn set(&mut self, x:u8, y:u8, color:Color) {
        let x = x as usize;
        let y = y as usize;
        self.pixels[8*y + x] = color;
    }

    fn get(&self, x:usize, y:usize) -> Color {
        self.pixels[8*y + x]
    }
}

/// Sega Game Gear VDP chip
/// https://www.smspower.org/uploads/Development/msvdp-20021112.txt
pub struct VDP {

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

    // screen buffer (RGBA pixels)
    screen: [Color; 256*224],

    // current scanline
    scanline: u16,
}

impl VDP {

    pub fn new() -> VDP {
        VDP {
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

            screen: [Color::black(); 256*224],
    
            scanline: 0,
        }
    }

    pub fn step(&mut self) -> bool {
        self.scanline += 1;
        if self.scanline > 224 {
            self.scanline = 0;
        }

        // check if VDP will generate an interrupt
        let mode1 = VDPRegisterModeControl2::from_bits_truncate(self.registers[1]);

        if mode1.contains(VDPRegisterModeControl2::IE0) {
            // interrupt enable bit used 
            // at the completion of the effective area

            if self.scanline == 0 {

                // raise internal IRQ flag
                self.status_register.insert(VDPStatusRegister::VBLANK);

                return true;
            }
        }
        // lower IRQ
        self.status_register.remove(VDPStatusRegister::VBLANK);
        return false;
    }

    fn read_vram_u8(&self, addr:u16) -> u8 {
        self.vram[addr as usize]
    }

    fn read_vram_u16(&self, addr:u16) -> u16 {
        let addr = addr as usize;
        let l = self.vram[addr] as u16;
        let h = self.vram[addr+1] as u16;
        (h << 8) | l
    }

    fn sprite_generator_table_base_address(&self) -> u16 {
        // VDP register 6 contains sprite generator base address upper bit
        // D7 - No effect
        // D6 - No effect
        // D5 - No effect
        // D4 - No effect
        // D3 - No effect
        // D2 - Bit 13 of the table base address
        // D1 - No effect
        // D0 - No effect

        let rb = self.registers[6];

        if (rb & 0b0000_0100) != 0 {
            0x0
        }
        else {
            0x2000
        }
    }

    fn name_table_base_address(&self) -> u16 {
        // VDP register 2 contains name table base address upper bits
        // D7 - No effect
        // D6 - No effect
        // D5 - No effect
        // D4 - No effect
        // D3 - Bit 13 of the table address
        // D2 - Bit 12 of the table address
        // D1 - Bit 11 of the table address
        // D0 - No effect

        let rb = self.registers[2];
        
        // mask unused bits
        let addr = (rb & 0x0e) as u16;
        // shift bits 3-1 to 13-11
        addr << 10
    }

    fn blit_to_screen(&mut self, bx:usize, by:usize, p:Pattern) {
        for dy in 0..8 {
            let y = by + dy;
            for dx in 0..8 {
                let x = bx + dx;
                if (x < 256) && (y < 224) {
                    self.screen[256*y + x] = p.get(dx,dy);
                }
            }
        }
    }

    pub fn render(&mut self) {

        // initialize pointer with name table base address
        let mut p: u16 = 0x3800;//self.name_table_base_address();

        for y in 0..28 {
            for x in 0..32 {
                // read background tile information from table
                let word = self.read_vram_u16(p);
                let bg = BgTile::from(word);
                
                // fetch 8x8 pixel pattern from pattern generator
                let pattern = self.get_pattern(bg.pattern);
                
                // blit pattern to screen
                let px:usize = x*8;
                let py:usize = y*8; 
                self.blit_to_screen(px,py,pattern);

                p += 2;
            }
        }

    }

    pub fn screen_get_pixel(&self, x:usize, y:usize) -> Color {
        self.screen[y*256 + x]
    }

    /// Return an 8 x 8 pattern of 32 bits RGBA pixels
    fn get_pattern(&self, idx: u16) -> Pattern {
        let mut p = Pattern::new();
 
        // each pattern is 32 bytes long
        // pattern generator base address is 0x0000
        let pa = idx * 32;

        // data is organized by bands of 4 bytes
        for band in 0..8 {

            // access the four bytes
            let pb = pa + 4*band;
            let bs0 = Bits::from(self.read_vram_u8(pb + 0));
            let bs1 = Bits::from(self.read_vram_u8(pb + 1));
            let bs2 = Bits::from(self.read_vram_u8(pb + 2));
            let bs3 = Bits::from(self.read_vram_u8(pb + 3));

            // iterate over 8 bits
            for x in 0..8 {
                // build color code with masked bits from each band
                let mut cc:u8 = 0;
                if bs0.is_set(x) { cc |= 0x01 }
                if bs1.is_set(x) { cc |= 0x02 }
                if bs2.is_set(x) { cc |= 0x04 }
                if bs3.is_set(x) { cc |= 0x08 }
                
                // get pixel color from palette
                let color = self.get_color_from_palette(0,cc);
                // set pixel color
                p.set(x, band as u8, color);

            }
        }

        p   
    }

    /// Return as RGB 32bits color n in palette p
    pub fn get_color_from_palette(&self, p:u8, n:u8) -> Color {

        // MSB              LSB
        // --------BBBBGGGGRRRR 
        //             76543210
        
        let pw = 16*2;
        let base:usize = (pw*p + 2*n) as usize;

        let h = self.cram[base] as u8; 
        let l = self.cram[base + 1] as u8;
        
        let r4:u8 = l & 0x0f;
        let g4:u8 = (l>>4) & 0x0f;
        let b4:u8 = h & 0x0f;

        // convert RGB 4 bits to RGB 8 bits
        Color::new(r4 << 4, g4 << 4, b4 << 4)
    }

    pub fn h_counter(&self) -> u8 {
        // The H counter is incremented with each dot clock. 
        0
    }

    pub fn v_counter(&self) -> u8 {
        // At the end of each scanline, the V counter is incremented by one,
        // and this continues until an entire
        let rl = self.scanline;

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

                //println!("VDP VRAM W @{:04x} {:02x}", self.dp_address_register, byte);

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
                    //println!("VDP CRAM W @{:02x} {:02x} {:02x}", addr, self.cram_latch, byte);
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

    /// Increment address register by 1 and wrap around 0x3fff
    fn increment_address_register(&mut self ) {
        self.dp_address_register += 1;
        if self.dp_address_register > 0x3fff {
            self.dp_address_register = 0;
        }
    }

    fn execute_command_word(&mut self, addr:u16, code:u8) {

        println!("VDP COMMAND @{:04x} C{}",addr,code);

        match code {
            0 => {
                self.dp_address_register = addr;
                // A byte of VRAM is read from the location defined by the address register and is
                // stored in the read buffer.
                self.dp_read_buffer = self.vram[self.dp_address_register as usize];
                // The address register is incremented by one.
                self.increment_address_register();
                // Write to data port go to VRAM.
                self.data_port_mux = VDPDataPortMux::VRAM;
            },

            1 => {
                self.dp_address_register = addr;
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
                self.dp_address_register = addr;
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
