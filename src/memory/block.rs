
use Vec;

use rand::prelude::*;

pub struct MemoryBlock {
    size: usize,
    data: Vec<u8>,
}

impl MemoryBlock {
    pub fn new(size:usize, byte:u8) -> MemoryBlock {
        MemoryBlock {
            size,
            data : vec![byte; size],
        }
    }

    pub fn to_base64(&self) -> String {
        base64::encode(&self.data)
    }

    pub fn from_base64(&mut self, edata:&str) {
        self.data = base64::decode(edata).unwrap();
        assert_eq!(self.data.len(), self.size);
    }

    pub fn len(&self) -> usize {
        self.size
    }
}

use std::ops::Index;
impl Index<usize> for MemoryBlock {
    type Output = u8;

    fn index(&self, idx: usize) -> &u8 {
        &self.data[idx]
    }
}

use std::ops::IndexMut;
impl IndexMut<usize> for MemoryBlock {

    fn index_mut(&mut self, idx: usize) -> &mut u8 {
        &mut self.data[idx]
    }
}
