
use crate::memory::Rom;
use crate::memory::Ram;

pub struct SystemBus {
    
    rom:Rom,
    work_ram:Ram,

}

impl SystemBus {

    pub fn new(rom: Rom) -> SystemBus {
        SystemBus {
            rom: rom,
            work_ram : Ram::new(),
        }
    }

    pub fn io_read(&self, addr:u8) -> u8 {
        println!("IOR @{:02x}", addr);
        // https://www.smspower.org/uploads/Development/smstech-20021112.txt
        // Z80 I/O ports
        match addr {
            // memory control
            0x3E => {
                0
            },
            // IO port control
            0x3F => {
                0
            },
            // V counter / PSG
            0x7E => {
                // HACK
                0
            },
            // H counter / PSG
            0x7F => {
                0
            },
            // VDP data
            0xBE => {
                0
            },
            // VDP control
            0xBF | 0xBD => {
                0
            },
            // IO port A/B
            0xDC | 0xC0 => {
                0
            },
            // IO port B/misc
            0xDD | 0xC1 => {
                0
            },
            _ => { panic!("IO read from unknown address: {:02x}", addr) }
        }
    }

    pub fn io_write(&mut self, addr:u8, data:u8) {
        println!("IOW @{:02x} {:02x}", addr, data);

        // https://www.smspower.org/uploads/Development/smstech-20021112.txt
        // Z80 I/O ports
        match addr {
            // memory control
            0x3E => {
            },
            // IO port control
            0x3F => {
            },
            // V counter / PSG
            0x7E => {
            },
            // H counter / PSG
            0x7F => {
            },
            // VDP data
            0xBE => {
            },
            // VDP control
            0xBF | 0xBD => {
            },
            // IO port A/B
            0xDC | 0xC0 => {
            },
            // IO port B/misc
            0xDD | 0xC1 => {
            },
            _ => { panic!("IO write to unknown address: {:02x}", addr) }
        }
    }

    pub fn cpu_read(&self, addr:u16) -> u8 {
        // The Z80's address space is shared by several components.
        // It has the following layout:
        //  $0000-$BFFF : Slot area
        //  $C000-$FFFF : Work RAM (8K, mirrored at $E000-$FFFF)
        if addr <= 0xbfff {
            // slot area
            self.rom.read(addr)
        }
        else if addr <= 0xdfff {
            // work RAM
            self.work_ram.read(addr - 0xc000)
        }
        else /*if addr < 0xffff */ {
            // work RAM, mirrored
            self.work_ram.read(addr - 0xe000)
        }
    }

    pub fn cpu_write(&mut self, addr:u16, data:u8) {
        // The Z80's address space is shared by several components.
        // It has the following layout:
        //  $0000-$BFFF : Slot area
        //  $C000-$FFFF : Work RAM (8K, mirrored at $E000-$FFFF)
        if addr <= 0xbfff {
            println!("no writing to ROM ! (@{:04x})", addr)
        }
        else if addr <= 0xdfff {
            // work RAM
            self.work_ram.write(addr - 0xc000, data)
        }
        else /*if addr < 0xffff */ {
            // work RAM, mirrored
            self.work_ram.write(addr - 0xe000, data)
        }
    }

    pub fn cpu_read_u16(&self, addr:u16) -> u16 {
        let lo = self.cpu_read(addr) as u16; 
        let hi = self.cpu_read(addr+1) as u16;
        
        (hi << 8) | lo
    }

    pub fn cpu_write_u16(&mut self, addr:u16, word:u16) {
        let lo = word & 0xff;
        let hi = (word >> 8) & 0xff;

        self.cpu_write(addr,   lo as u8);
        self.cpu_write(addr+1, hi as u8);
    }


}


