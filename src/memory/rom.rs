use std::io;
use std::fs::File;
use std::io::prelude::*;

pub struct Rom {

    data: Vec<u8>,

    rp: usize,
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
            rp: 0,
        } )
    }

    /// Move read head at given position inside ROM file
    pub fn seek(&mut self, position: usize) -> Result<(),&'static str> {
        if position >= self.size() {
            return Err("invalid position");
        }

        self.rp = position;
        Ok(())
    }

    /// Read one byte from ROM file
    pub fn read_one(&mut self) -> Option<u8> {
        match self.data.get(self.rp) {
            Some(v) => {
                // increment read pointer
                self.rp += 1;
                // return byte by value
                Some(*v)
            },
            None => None
        }
    }

    /// Return ROM size in bytes
    pub fn size(& self) -> usize {
        self.data.len()
    }

}
