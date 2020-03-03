use std::io;
use std::fs::File;
use std::io::prelude::*;

pub struct Rom {
    data: Vec<u8>,
}

impl Rom {

    /// Open provided ROM file and return a Rom structure on success
    pub fn open(filename:&str) -> io::Result<Rom> {

        let mut file = File::open(filename)?;
        
        // dump whole file into vector
        let mut data = Vec::new();
        file.read_to_end(&mut data)?;

        Ok( Rom {
            data,
        } )
    }

    /// Read one byte from ROM file
    pub fn read(&self, addr:u16) -> u8 {
        match self.data.get(addr as usize) {
            Some(pb) => *pb,
            None => {
                panic!("out of bounds access in ROM");
            }
        }
    }

    /// Return ROM size in bytes
    pub fn size(& self) -> usize {
        self.data.len()
    }

}
