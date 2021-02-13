#![allow(non_snake_case)]

use crate::bits::Bits;
use crate::memory::MemoryBlock;

use serde_json::{json,Result};
use serde::{Deserialize,Serialize};

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
        const BLANK       = 0b0100_0000;
        const IE0       = 0b0010_0000;
        const M1        = 0b0001_0000;
        const M3        = 0b0000_1000;
        // unused
        const SIZE      = 0b0000_0010;
        const DOUBLED   = 0b0000_0001;
    }
}

#[derive(Serialize, Deserialize)]
enum VDPDataPortMux {
    VRAM,
    CRAM,
}

bitflags! {
    #[derive(Serialize, Deserialize)]
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

enum SpriteSize {
    Size8x8,
    Size8x16,
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
    b:u8,
    a:bool,
}

impl Color {
    fn new(r:u8, g:u8, b:u8) -> Color {
        Color {
            r:r,
            g:g,
            b:b,
            a:true,
        }
    }

    fn transparent() -> Color {
        Color {
            r:0,
            g:0,
            b:0,
            a:false
        }
    }

    fn alpha(&self) -> bool {
        self.a
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
pub struct Pattern {
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

    pub fn get(&self, x:usize, y:usize) -> Color {
        self.pixels[8*y + x]
    }
}

/// Sprite attributes
#[derive(Debug)]
pub struct Sprite {
    // vertical position
    x: u8,
    // horizontal position
    y: u8,
    // character number
    n: u8,
}

impl Sprite {
    fn new(x:u8, y:u8, n:u8) -> Sprite {
        Sprite {
            x:x,
            y:y,
            n:n,
        }
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
    registers: MemoryBlock,

    // data port address register
    dp_address_register: u16,
    dp_read_buffer: u8,

    // VDP status register
    status_register: VDPStatusRegister,

    // video ram (VRAM)
    vram: MemoryBlock,

    // color ram (CRAM)
    // In Game Gear mode, CRAM has been expanded to 32 words (64 bytes) Each word
    // defines a single color, like so:
    // MSB              LSB
    // --------BBBBGGGGRRRR
    // - = Unused
    // R = Red component
    // G = Green component
    // B = Blue component
    cram: MemoryBlock,
    // CRAM latched byte
    cram_latch: u8,

    // data port muxer state
    data_port_mux: VDPDataPortMux,

    // screen buffer (RGBA pixels)
    screen: [Color; 256*224],

    // tiles number matrix (debug)
    debug_tiles_matrix: [u16; 28*32],

    // current scanline
    scanline: u16,

    // debug breakpoint latch
    will_break: bool,
}

impl VDP {

    pub fn new() -> VDP {
        VDP {
            cw_second_byte: false,
            cw_buffer: 0,

            registers: MemoryBlock::new(11, 0),

            dp_address_register: 0,
            dp_read_buffer: 0,
            
            status_register: VDPStatusRegister::empty(),

            vram: MemoryBlock::new(0x4000, 0xff),

            cram: MemoryBlock::new(64, 0xff),
            cram_latch: 0,

            data_port_mux: VDPDataPortMux::VRAM,

            screen: [Color::black(); 256*224],
    
            debug_tiles_matrix: [0; 28*32],

            scanline: 0,

            will_break: false,
        }
    }

    pub fn serialize_state(&self) -> serde_json::Value {
        json!({
            "cw_second_byte": self.cw_second_byte,
            "cw_buffer": self.cw_buffer,

            "registers": self.registers.to_base64(),

            "dp_address_register": self.dp_address_register,
            "dp_read_buffer": self.dp_read_buffer,

            "status_register": self.status_register,

            "vram": self.vram.to_base64(),
            "cram": self.cram.to_base64(),

            "cram_latch": self.cram_latch,

            "data_port_mux": self.data_port_mux,

            "scanline": self.scanline,
        })
    }

    pub fn restore_state(&mut self, state:&serde_json::Value) {
        self.cw_second_byte = state["cw_second_byte"].as_bool().unwrap();
        self.cw_buffer = state["cw_buffer"].as_u64().unwrap() as u8;

        self.registers.from_base64(state["registers"].as_str().unwrap());

        self.dp_address_register = state["dp_address_register"].as_u64().unwrap() as u16;
        self.dp_read_buffer = state["dp_read_buffer"].as_u64().unwrap() as u8;

        self.status_register = VDPStatusRegister::deserialize(&state["status_register"]).unwrap();

        self.vram.from_base64(state["vram"].as_str().unwrap());
        self.cram.from_base64(state["cram"].as_str().unwrap());

        self.cram_latch = state["cram_latch"].as_u64().unwrap() as u8;

        self.data_port_mux = VDPDataPortMux::deserialize(&state["data_port_mux"]).unwrap();

        self.scanline = state["scanline"].as_u64().unwrap() as u16;
    }

    pub fn will_break(&mut self) -> bool {
        if self.will_break {
            self.will_break = false;
            true
        }
        else {
            false
        }
    }

    pub fn step(&mut self) -> (bool,bool) {

        let mut new_frame = false;

        self.scanline += 1;
        if self.scanline > 224 {
            self.scanline = 0;
            new_frame = true;
        }

        if self.scanline == 0 {
           // raise internal IRQ flag
           self.status_register.insert(VDPStatusRegister::VBLANK);
        }

        // check if VDP will generate an interrupt
        let mode2 = VDPRegisterModeControl2::from_bits_truncate(self.registers[1]);
        if mode2.contains(VDPRegisterModeControl2::IE0) {
            // interrupt enable bit used 
            // at the completion of the effective area

            if self.scanline == 0 {

                return (true, new_frame);
            }
        }
        return (false, new_frame);
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

    fn get_sprite_size(&self) -> SpriteSize {
        let bsize = 
            VDPRegisterModeControl2::from_bits_truncate(self.registers[1])
                                        .contains(VDPRegisterModeControl2::SIZE);
        match bsize {
            false => SpriteSize::Size8x8,
            true => SpriteSize::Size8x16,
        }
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
            0x2000
        }
        else {
            0x0
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

    fn sprite_attribute_table_base_address(&self) -> u16 {
        // VDP register 5 contains sprite generator base address upper bit
        // D7 - No effect
        // D6 - Bit 13 of the table address
        // D5 - Bit 12 of the table address
        // D4 - Bit 11 of the table address
        // D3 - Bit 10 of the table address
        // D2 - Bit 9 of the table address
        // D1 - Bit 8 of the table address
        // D0 - No effect

        let rb = self.registers[5];

        // mask unused bits
        let addr = (rb & 0x7e) as u16;
        // shift bits 6-1 to 13-8
        addr << 7
    }

    fn blit_to_screen(&mut self, bx:isize, by:isize, p:Pattern) {
        self.blit_to_screen_ex(bx,by,p,false,false)
    }

    fn blit_to_screen_ex(&mut self,
                            bx:isize, by:isize,
                            p:Pattern,
                            reverse_horizontal:bool,
                            reverse_vertical:bool) {
        for dy in 0..8 {
            let y = by + dy;
            for dx in 0..8 {

                let cdx = if reverse_horizontal { 7 - dx } else { dx };
                let cdy = if reverse_vertical   { 7 - dy } else { dy };
                if cdx >= 0 && cdy >= 0 {
                    let cdx = cdx as usize;
                    let cdy = cdy as usize;
                    let color = p.get(cdx,cdy);

                    // skip is color is transparent
                    if color.alpha() {
                        let x = bx + dx;
                        if (x > 0) 
                            && (y > 0) 
                            && (x < 256)
                            && (y < 224) {

                            let x = x as usize;
                            let y = y as usize;
                            self.screen[256*y + x] = color;
                        }
                    }
                }
            }
        }
    }

    #[allow(dead_code)]
    pub fn debug_get_tile_number(&self, x:usize, y:usize) -> u16 {
        self.debug_tiles_matrix[x + 32*y]
    }

    pub fn render(&mut self) {

        // check if screen is BLANKed
        let mc2 = VDPRegisterModeControl2::from_bits_truncate(self.registers[1]);
        if ! mc2.contains(VDPRegisterModeControl2::BLANK) {
            // paint screen with backdrop color
            let backdrop = self.get_color_from_palette(0,0);
            for color in self.screen.iter_mut() {
                *color = backdrop;
            }
            return
        }

        // initialize pointer with name table base address
        let mut p: u16 = self.name_table_base_address();
        
        // fetch scroll registers 
        let hscroll = self.registers[8] as isize;
        let vscroll = self.registers[9] as isize;

        // -- render tiles --
        for y in 0..28 {
            for x in 0..32 {
                // read background tile information from table
                let word = self.read_vram_u16(p);
                let bg = BgTile::from(word);
         
                // update debug tile matrix
                self.debug_tiles_matrix[32*y + x] = bg.pattern;

                // fetch 8x8 pixel pattern from pattern generator
                let pattern = self.get_tile_pattern(
                                    bg.pattern,
                                    if bg.palette { 1 } else { 0 }
                );
             
                // blit patterns to screen
                let px:isize = (hscroll + (x as isize)*8).rem_euclid(256);
                let py:isize = ((y as isize)*8 - vscroll).rem_euclid(224);

                self.blit_to_screen_ex(px, py, pattern,
                                        bg.horizontal_flip,
                                        bg.vertical_flip);

                p += 2;
            }
        }

        // fetch sprite size mode (8x8 or 8x16)
        let spsz = self.get_sprite_size();

        // -- render sprites --
        for sidx in 0..64 {

            // read sprite information and pattern from table
            let sp = self.get_sprite(sidx);
 
            let sx = sp.x as isize;
            let sy = sp.y as isize;

            match sx {
                0xd0 => {
                    // at a vertical pos., 0xd0 has the meaning of an end code
                    break;
                },
                0xe0 => {
                    // at a vertical pos., 0xe0 prevent a sprite from being displayed
                    continue;
                },
                _ => {
                    // nothing to do
                }
            };

            match spsz {
                SpriteSize::Size8x8 => {
                    // fetch pattern
                    let pattern = self.get_sprite_pattern(sp.n);
                    // blit sprite pattern to screen
                    self.blit_to_screen(sx,sy,pattern);
                },
                SpriteSize::Size8x16 => {
                    // from Sega GG HW ref. man. (rev1) p.32
                    
                    // compute masked pattern number
                    let mn = sp.n & 0xfe;
                    // fetch pattern A and pattern B
                    let pa = self.get_sprite_pattern(mn | 0x00);
                    let pb = self.get_sprite_pattern(mn | 0x01);
                    
                    // blit pattern A
                    self.blit_to_screen(sx,sy,pa);
                    // blit pattern B (8 px down from pattern A)
                    self.blit_to_screen(sx,sy+8,pb);
                },
            }

        }

    }

    pub fn screen_get_pixel(&self, x:usize, y:usize) -> Color {
        self.screen[y*256 + x]
    }

    /// Return a sprite from table
    pub fn get_sprite(&self, idx:u16) -> Sprite {

        // get sprite attribute table base address
        let baddr = self.sprite_attribute_table_base_address();

        // fetch sprite position and character number from table
        let y = self.read_vram_u8(baddr + idx);
        let x = self.read_vram_u8(baddr + 0x80 + 2*idx + 0);
        let n = self.read_vram_u8(baddr + 0x80 + 2*idx + 1);

        Sprite::new(x,y,n)
    }

    pub fn get_sprite_pattern(&self, idx: u8) -> Pattern {
        // color palette 1 is always selected for sprites
        self.get_pattern(
            self.sprite_generator_table_base_address(),
            idx as u16,
            1,
            true)
    }

    pub fn get_tile_pattern(&self, idx: u16, p:u8) -> Pattern {
        self.get_pattern(0x0000, idx, p, false)
    }

    /// Return an 8 x 8 pattern of 32 bits RGBA pixels
    /// from base_address (can be sprite OR pattern tiles)
    pub fn get_pattern(&self,
                        base_addr:u16,
                        idx: u16,
                        palette:u8,
                        is_color0_transparent:bool )-> Pattern {

        let mut p = Pattern::new();
 
        // each pattern is 32 bytes long
        // pattern generator base address is 0x0000
        let pa = base_addr + idx * 32;

        // data is organized by bands of 4 bytes
        for band in 0..8 {

            // access the four bytes
            let pb = pa + 4*band;
            let bs0 = Bits::from(self.read_vram_u8(pb + 0));
            let bs1 = Bits::from(self.read_vram_u8(pb + 1));
            let bs2 = Bits::from(self.read_vram_u8(pb + 2));
            let bs3 = Bits::from(self.read_vram_u8(pb + 3));

            // iterate over 8 bits
            for k in 0..8 {
                
                // build color code with masked bits from each band
                let mut cc:u8 = 0;
                if bs0.is_set(k) { cc |= 0x01 }
                if bs1.is_set(k) { cc |= 0x02 }
                if bs2.is_set(k) { cc |= 0x04 }
                if bs3.is_set(k) { cc |= 0x08 }
                
                // get pixel color from palette
                let color = if is_color0_transparent && (cc == 0) {
                    // color 0 is TRANSPARENT for sprites
                    Color::transparent()
                }
                else {
                    self.get_color_from_palette(palette, cc)
                };

                // bit 7 is x=0, bit 0 is x=7
                let x = 7 - k;
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
        let status = self.status_register.bits;

        // clear VBLANK
        self.status_register.remove(VDPStatusRegister::VBLANK);

        status
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

        let rbyte = self.dp_read_buffer;

        // auto increment and update read buffer
        self.increment_address_register();
        self.dp_read_buffer = self.vram[self.dp_address_register as usize];
        
        rbyte
    }

    pub fn write_data_port(&mut self, byte: u8) {

        // clear cw flag
        self.cw_second_byte = false;

        match self.data_port_mux {
            VDPDataPortMux::VRAM => {

                //println!("VDP VRAM W @{:04x} {:02x}", self.dp_address_register, byte);

                let addr = self.dp_address_register as usize;
                self.vram[addr] = byte;
            },

            VDPDataPortMux::CRAM => {

                // CRAM address register wraps past 0x3f
                let addr = self.dp_address_register & 0x3f;

                // Writing an even CRAM address causes the data written to be stored in a latch, and writing to
                // an odd CRAM address makes the VDP write the contents of the latch as well
                // as the new data from the CPU to the current CRAM entry.
                
                let odd = addr & 0x01 == 0x01;
                if odd {
                    // odd address, write latched byte then new byte
                    self.cram[(addr) as usize] = self.cram_latch;
                    self.cram[(addr-1) as usize] = byte;
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

        //println!("VDP COMMAND @{:04x} C{}",addr,code);

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

        //println!("VDP WRITE REG @{:02x} {:02x} {:08b}",n,byte,byte);

        // write to register
        self.registers[n as usize] = byte;

        // VDP only contains 11 registers
        if n > 0x0A {
            panic!("unhandled VDP register: {}", n)
        }

    }

}
