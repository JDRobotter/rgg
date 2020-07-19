
use crate::memory::Rom;
use crate::memory::Ram;
use crate::system::VDP;
use crate::system::PSG;
use crate::system::Joystick;

bitflags! {
    struct BankControlRegister: u8 {
        const WRITE_PROTECT = 0b1000_0000;
        // unused               0100_0000
        // unused               0010_0000
        const WORK_RAM =      0b0001_0000;
        const EXTERNAL_RAM =  0b0000_1000;
        // unused               0000_0100
        const BANK_OFFSET =   0b0000_0011;
    }
}

bitflags! {
    struct IOPort00: u8 {
        const STI  = 0b1000_0000;
        const NJAP = 0b0100_0000;
        const NNTS = 0b0010_0000;
        // unused      0001_0000
        // unused      0000_1000
        // unused      0000_0100
        // unused      0000_0010
        // unused      0000_0001
    }
}

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

    fn read(&self, rom: &Rom, iaddr: u16) -> u8 {
        // mask address to 16k
        let addr = iaddr & 0x3fff;
        // prepend mux selection address
        let addr:usize = (addr as usize) | ( (self.mux as usize) << 14 );
        // read from rom
        rom.read(addr)
    }
}

pub struct SystemBus {
    
    rom: Rom,
    pub work_ram: Ram,

    bank0_mapper: RomMapper,
    bank1_mapper: RomMapper,
    bank2_mapper: RomMapper,

    pub vdp: VDP,
    pub psg: PSG,
    pub joystick: Joystick,
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

            psg: PSG::new(),

            joystick: Joystick::new(),
        }
    }

    pub fn io_read(&mut self, addr:u8, ncycle:u64) -> u8 {
        // https://www.smspower.org/uploads/Development/smstech-20021112.txt
        // Z80 I/O ports
        match addr {
            // IO port 00
            0x00 => {
                // return:
                // - START/PAUSE OFF
                // - overseas mode
                // - NTSC mode
                self.joystick.register_00() |  (IOPort00::NJAP).bits
            },
            // IO port 5
            // serial communication mode setting
            0x05 => {
                // not implemented
                0
            },
            // memory control
            0x3E => {
                // not implemented
                0
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
                self.joystick.register_dc()
            },
            // IO port B/misc
            0xDD | 0xC1 => {
                self.joystick.register_dd()
            },
            _ => { 
                println!("IO read from unknown address: {:02x}", addr);
                0
            },
        }
    }

    pub fn io_write(&mut self, addr:u8, data:u8, ncycle:u64) {
        // https://www.smspower.org/uploads/Development/smstech-20021112.txt
        // Z80 I/O ports
        match addr {
            // IO port 01
            // read/write to the EXT connector
            0x01 => {
                // nothing to do
            },
            // IO port 02
            0x02 => {
                // nothing to do
            },
            // IO port 5
            // serial communication mode setting
            0x05 => {
                // nothing to do
            },
            // IO port 6
            // sound related stuff
            0x06 => {
                // nothing to do
            },
            // memory control
            0x3E => {
                // not implemented
            },
            // IO port control
            0x3F => {
                // not implemented
            },
            // SN 76489 data
            0x7E => {
                self.psg.write(data, ncycle)
            },
            // SN 76489 data (mirror)
            0x7F => {
                self.psg.write(data, ncycle)
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
        else /* if addr <= 0xffff */ { 
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
            // write to RAM, mirrored
            self.work_ram.write(addr - 0xe000, data);

            // RAM mapping and misc functions
            let _flags = BankControlRegister::from_bits_truncate(data);
            // mapper control flags are not implemented
            if data & 0x7F != 0x00 {
                panic!("Unhandled mapper control: {:02x}", data);
            }
        }
        else if addr == 0xfffd {
            // write to RAM, mirrored
            self.work_ram.write(addr - 0xe000, data);

            // ROM mapping bank 0 configuration
            self.bank0_mapper.set(data);
        }
        else if addr == 0xfffe {
            // write to RAM, mirrored
            self.work_ram.write(addr - 0xe000, data);

            // ROM mapping bank 1 configuration
            self.bank1_mapper.set(data);
        }
        else if addr == 0xffff {
            // write to RAM, mirrored
            self.work_ram.write(addr - 0xe000, data);

            // ROM mapping bank 2 configuration
            self.bank2_mapper.set(data);
        }
        else {
            panic!("CPU write defaulting for {:04x}", addr);
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


