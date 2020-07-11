
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

    fn check_address_validity(&self, addr:usize) -> bool {
        let sz = self.data.len();
        addr < sz
    }

    pub fn write(&mut self, addr:u16, byte:u8) {
        let addr = addr as usize;
        if self.check_address_validity(addr) {
            self.data[addr] = byte;
        }
        else {
            panic!("out of bounds access to RAM ({:04x})", addr);
        }
    }

    pub fn read(&self, addr:u16) -> u8 {
        let addr = addr as usize;
        if self.check_address_validity(addr) {
            self.data[addr]
        }
        else {
            panic!("out of bounds access to RAM ({:04x}", addr);
        }
    }

}
