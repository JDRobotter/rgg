
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
        return 0xB0;
    }

    pub fn io_write(&mut self, addr:u8, data:u8) {

    }

    pub fn cpu_read(&self, addr:u16) -> u8 {
        // The Z80's address space is shared by several components.
        // It has the following layout:
        //  $0000-$BFFF : Slot area
        //  $C000-$FFFF : Work RAM (8K, mirrored at $E000-$FFFF)
        if addr < 0xbfff {
            // slot area
            self.rom.read(addr)
        }
        else if addr < 0xdfff {
            // work RAM
            self.work_ram.read(addr - 0xc000)
        }
        else /*if addr < 0xffff */ {
            // work RAM, mirrored
            self.work_ram.read(addr - 0xe000)
        }
    }

    pub fn cpu_write(&mut self, addr:u16, data:u8) {
        return self.work_ram.write(addr, data);
    }
}


