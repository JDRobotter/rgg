
pub struct Bits<T> {
    word:T,
}

impl<T> Bits<T> {
    pub fn from(w:T) -> Self {
        Bits {
            word:w,
        }
    }
}

impl Bits<u8> {

    pub fn is_set(&self, idx:u8) -> bool {
        self.word & (1<<idx) != 0
    }
}
