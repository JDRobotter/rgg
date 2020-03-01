
use crate::cpu::Z80InstructionDecoder;

struct Z80Registers {
    // accumulator
    a: u8,
    b: u8,
    d: u8,
    h: u8,
    // flags
    f: u8,
    c: u8,
    e: u8,
    l: u8,
    // interrupt vector
    iv: u8,
    // memory refresh
    mr: u8,
    // program counter
    pc: u16,
    // stack pointer
    sp: u16,
    // index registers
    ix: u16,
    iy: u16,
}

impl Z80Registers {
    pub fn new() -> Z80Registers {
        Z80Registers {
            a: 0, b: 0, d:0, h:0,
            f: 0, c: 0, e:0, l:0,
            iv: 0, mr: 0, pc: 0, sp:0,
            ix:0, iy: 0
        }
    }
}

pub struct Z80 {
    // CPU registers
    registers: Z80Registers,
    // CPU intruction decoder
    decoder: Z80InstructionDecoder,
}

impl Z80 {
    pub fn new() -> Z80 {
        Z80 {
            registers: Z80Registers::new(),
            decoder: Z80InstructionDecoder::new(),
        }
    }
}
