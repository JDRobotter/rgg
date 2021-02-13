
use crate::memory::MemoryBlock;

pub struct Ram {
    // allocate 8k RAM
    data: MemoryBlock,
}

impl Ram {

    pub fn new() -> Ram {
        Ram {
            data: MemoryBlock::new(8192,0xff),
        }
    }

    pub fn serialize_state(&self) -> String {
        self.data.to_base64()
    }

    pub fn restore_state(&mut self, state:&serde_json::Value) {
        let ss = state.as_str().unwrap();
        self.data.from_base64(ss);
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
