
pub struct Ram {
    // allocate 8k RAM
    data: [u8;8192],
}

impl Ram {

    pub fn new() -> Ram {
        Ram {
            data: [0xff;8192],
        }
    }

    pub fn write(&mut self, addr:u16, byte:u8) {
        let addr = addr as usize;
        let sz = self.data.len();
        if 0 < addr && addr < sz {
            self.data[addr] = byte;
        }
        else {
            panic!("out of bounds access to RAM");
        }
    }

    pub fn read(&self, addr:u16) -> u8 {
        let addr = addr as usize;
        let sz = self.data.len();
        if 0 < addr && addr < sz {
            self.data[addr]
        }
        else {
            panic!("out of bounds access to RAM");
        }
    }

}
