
use crate::memory::Rom;
use crate::memory::Ram;
use crate::system::VDP;
use crate::cpu::Z80;

use std::rc::Rc;
use std::cell::{RefCell,RefMut};

struct RomMapper {
    // mapper multiplexer value
    mux: u8,
}

impl RomMapper {
    fn new(mux:u8) -> RomMapper {
        RomMapper {
            mux:mux
        }
    }

    fn set(&mut self, mux:u8) {
        self.mux = mux
    }

    fn read(&self, rom: &Rom, addr: u16) -> u8 {
        // mask address to 16k
        let addr = addr & 0x3fff;
        // prepend mux selection address
        let addr = addr | ( (self.mux as u16) << 14 );
        // read from rom
        rom.read(addr)
    }
}

pub struct SystemBus {
    
    rom: Rom,
    work_ram: Ram,

    bank0_mapper: RomMapper,
    bank1_mapper: RomMapper,
    bank2_mapper: RomMapper,

    pub vdp: VDP,
}

impl SystemBus {

    pub fn new(rom:Rom) -> SystemBus {
        SystemBus {
            rom: rom,
            work_ram : Ram::new(),

            // 315-5208 mapper power-up reset values
            bank0_mapper: RomMapper::new(0x00),
            bank1_mapper: RomMapper::new(0x01),
            bank2_mapper: RomMapper::new(0x02),

            vdp: VDP::new(),
        }
    }

    pub fn io_read(&mut self, addr:u8) -> u8 {
        println!("IOR @{:02x}", addr);
        // https://www.smspower.org/uploads/Development/smstech-20021112.txt
        // Z80 I/O ports
        match addr {
            // memory control
            0x3E => {
                panic!("not implemented")
            },
            // IO port control
            0x3F => {
                panic!("not implemented")
            },
            // V counter
            0x7E => {
                self.vdp.v_counter()
            },
            // H counter
            0x7F => {
                self.vdp.h_counter()
            },
            // VDP data port
            0xBE => {
                self.vdp.read_data_port()
            },
            // VDP control port
            0xBF | 0xBD => {
                self.vdp.read_control_port()
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
                panic!("not implemented")
            },
            // IO port control
            0x3F => {
                panic!("not implemented")
            },
            // SN 76489 data
            0x7E => {
            },
            // SN 76489 data (mirror)
            0x7F => {
            },
            // VDP data
            0xBE => {
                self.vdp.write_data_port(data)
            },
            // VDP control
            0xBF | 0xBD => {
                self.vdp.write_control_port(data)
            },
            // IO port A/B
            0xDC | 0xC0 => {
            },
            // IO port B/misc
            0xDD | 0xC1 => {
            },
            _ => {
                println!("IO write to unknown address: {:02x}", addr)
            }
        }
    }

    pub fn cpu_read(&self, addr:u16) -> u8 {
        // The Z80's address space is shared by several components.
        // It has the following layout:
        //  $0000-$BFFF : Cartridge ROM (48k):
        //      $0000-$3FFF : ROM bank 0
        //      $4000-$7FFF : ROM bank 1
        //      $8000-$BFFF : ROM bank 2
        //  $C000-$FFFF : Work RAM (8K, mirrored at $E000-$FFFF)
        if addr <= 0x3fff {
            // ROM bank 0
            self.bank0_mapper.read(&self.rom, addr)
        }
        else if addr <= 0x7fff {
            // ROM bank 1
            self.bank1_mapper.read(&self.rom, addr - 0x4000)
        }
        else if addr <= 0xbfff {
            // ROM bank 2
            self.bank2_mapper.read(&self.rom, addr - 0x8000)
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
        //  $0000-$BFFF : Cartridge ROM (48k)
        //  $C000-$FFFF : Work RAM (8K, mirrored at $E000-$FFFF)
        if addr <= 0xbfff {
            println!("no writing to ROM ! (@{:04x})", addr)
        }
        else if addr <= 0xdfff {
            // work RAM
            self.work_ram.write(addr - 0xc000, data)
        }
        else if addr < 0xfffc  {
            // work RAM, mirrored
            self.work_ram.write(addr - 0xe000, data)
        }
        // SEGA mapper control
        // https://www.smspower.org/Development/Mappers
        else if addr == 0xfffc {
            // RAM mapping and misc functions
        }
        else if addr == 0xfffd {
            // ROM mapping bank 0 configuration
            self.bank0_mapper.set(data);
        }
        else if addr == 0xfffe {
            // ROM mapping bank 1 configuration
            self.bank1_mapper.set(data);
        }
        else if addr == 0xffff {
            // ROM mapping bank 2 configuration
            self.bank2_mapper.set(data);
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


