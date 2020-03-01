
use std::vec;
use std::fmt;

pub enum Z80InstructionLocation {

    RegisterA,
    RegisterB,
    RegisterC,
    RegisterD,
    RegisterE,
    RegisterF,
    RegisterH,
    RegisterL,

    RegisterIndirectHL,
    RegisterIndirectBC,
    RegisterIndirectDE,

    RegisterBC,
    RegisterDE,
    RegisterHL,
    RegisterSP,
    RegisterIX,
    RegisterIY,

    IndexedIX(u8),
    IndexedIY(u8),
    
    Immediate(u8),
    Immediate16(u16),

    ImmediateExternal(u16),
    External(u16),
}
use Z80InstructionLocation as ZIL;

impl Z80InstructionLocation {
    pub fn to_string(&self) -> String {
        match self {
            Z80InstructionLocation::RegisterA =>          format!("A"),
            Z80InstructionLocation::RegisterB =>          format!("B"),
            Z80InstructionLocation::RegisterC =>          format!("C"),
            Z80InstructionLocation::RegisterD =>          format!("D"),
            Z80InstructionLocation::RegisterE =>          format!("E"),
            Z80InstructionLocation::RegisterF =>          format!("F"),
            Z80InstructionLocation::RegisterH =>          format!("H"),
            Z80InstructionLocation::RegisterL =>          format!("L"),
            Z80InstructionLocation::RegisterIndirectHL => format!("(HL)"),
            Z80InstructionLocation::RegisterIndirectBC => format!("(BC)"),
            Z80InstructionLocation::RegisterIndirectDE => format!("(DE)"),
            Z80InstructionLocation::RegisterBC =>          format!("BC"),
            Z80InstructionLocation::RegisterDE =>          format!("DE"),
            Z80InstructionLocation::RegisterHL =>          format!("HL"),
            Z80InstructionLocation::RegisterSP =>          format!("SP"),
            Z80InstructionLocation::RegisterIX =>          format!("IY"),
            Z80InstructionLocation::RegisterIY =>          format!("IX"),

            Z80InstructionLocation::IndexedIX(v) =>         format!("(IX+0x{:02x})",v),
            Z80InstructionLocation::IndexedIY(v) =>         format!("(IY+0x{:02x})",v),

            Z80InstructionLocation::Immediate(v) =>         format!("0x{:02x}",v),
            Z80InstructionLocation::Immediate16(v) =>       format!("0x{:02x}",v),

            Z80InstructionLocation::ImmediateExternal(v) => format!("0x{:04x}", v),
            Z80InstructionLocation::External(v) =>          format!("(0x{:04x})", v),
        }
    }
}

pub enum Z80JumpCondition {
    Unconditionnal,
    Carry,
    NonCarry,
    Zero,
    NonZero,
    ParityEven,
    ParityOdd,
    SignNegative,
    SignPositive,
}
use Z80JumpCondition as ZJC;

impl Z80JumpCondition {
    pub fn to_string(&self) -> String {
        match self {
            Z80JumpCondition::Unconditionnal => format!(""),
            Z80JumpCondition::Carry =>          format!("c"),
            Z80JumpCondition::NonCarry =>       format!("nc"),
            Z80JumpCondition::Zero =>           format!("z "),
            Z80JumpCondition::NonZero =>        format!("nz"),
            Z80JumpCondition::ParityEven =>     format!("pe"),
            Z80JumpCondition::ParityOdd =>      format!("po"),
            Z80JumpCondition::SignNegative =>   format!("s-"),
            Z80JumpCondition::SignPositive =>   format!("s+"),
        }
    }
}

pub enum Z80Instruction {
    
    // misc operations
    NOP,
    Halt,
    DisableINT,
    EnableINT,
    SetINTMode0,
    SetINTMode1,
    SetINTMode2,

    // jump, call and return group
    JumpImmediate(Z80JumpCondition, Z80InstructionLocation),
    JumpRelative(Z80JumpCondition, Z80InstructionLocation),
    Call(Z80JumpCondition, Z80InstructionLocation),

    // load group
    Load(Z80InstructionLocation, Z80InstructionLocation),
    Push(Z80InstructionLocation),

    // load transfer group
    LoadIncrement,
    LoadIncrementRepeat,
    LoadDecrement,
    LoadDecrementRepeat,

    // arithmetic and logic 
    Add(Z80InstructionLocation),
    AddCarry(Z80InstructionLocation),
    Compare(Z80InstructionLocation),
    Increment(Z80InstructionLocation),
    Decrement(Z80InstructionLocation),

}
use Z80Instruction as ZI;

impl Z80Instruction {
    pub fn to_string(&self) -> String {
        match self {
            Z80Instruction::NOP =>          format!("nop"),
            Z80Instruction::Halt =>         format!("halt"),
            Z80Instruction::DisableINT =>   format!("dint"),
            Z80Instruction::EnableINT =>    format!("eint"),
            Z80Instruction::SetINTMode0 =>  format!("sim0"),
            Z80Instruction::SetINTMode1 =>  format!("sim1"),
            Z80Instruction::SetINTMode2 =>  format!("sim2"),

            Z80Instruction::JumpImmediate(cond, addr) =>    format!("jp {} {}",cond.to_string(), addr.to_string()),
            Z80Instruction::JumpRelative(cond, v) =>        format!("jr {} PC+{}", cond.to_string(), v.to_string()),
            Z80Instruction::Call(cond, addr) =>             format!("call {} {}", cond.to_string(), addr.to_string()),

            Z80Instruction::Load(dst, src) =>   format!("ld {} {}", dst.to_string(), src.to_string()),
            Z80Instruction::Push(src) =>        format!("push {}", src.to_string()),
            
            Z80Instruction::LoadIncrement       => format!("ldi"),
            Z80Instruction::LoadIncrementRepeat => format!("ldir"),
            Z80Instruction::LoadDecrement       => format!("ldd"),
            Z80Instruction::LoadDecrementRepeat => format!("lddr"),

            Z80Instruction::Add(src)       => format!("add {}", src.to_string()),
            Z80Instruction::AddCarry(src)  => format!("adc {}", src.to_string()),
            Z80Instruction::Compare(src)   => format!("cp {}", src.to_string()),
            Z80Instruction::Increment(src) => format!("inc {}", src.to_string()),
            Z80Instruction::Decrement(src) => format!("dec {}", src.to_string()),
            
        }
    }
}

enum Z80InstructionByte {
    Byte(u8),
    Placeholder,
}
use Z80InstructionByte as ZIB;

pub struct Z80InstructionDecoder {
    
    in_values: Vec<u8>,
    //
    in_buffer: Vec<u8>,
    
}

impl Z80InstructionDecoder {

    pub fn new() -> Z80InstructionDecoder {
        Z80InstructionDecoder {
            in_values: Vec::new(),
            in_buffer: Vec::new(),
        }
    }

    /// Push one byte to instruction decoder
    pub fn push(&mut self, byte:u8) -> Option<Z80Instruction> {
        
        // push byte and shift
        self.in_buffer.push(byte);

        // panic if input buffer is full
        if self.in_buffer.len() > 6 {
            self.debug_show_in_buffer();
            panic!("failed to decode instructions");
        }

        // try to decode an instruction
        match self.decode_instruction() {

            Some(instruction) => {
                // instruction decoded, reset input buffer
                self.in_buffer.clear();

                Some(instruction)
            },
            None => None,
        }
    }

    pub fn match_byte(& self, byte:u8) -> bool {
        return self.in_buffer.get(0) == Some(&byte);
    }

    pub fn match_bytes(& self, bytes:&[u8]) -> bool {
        
        // check size
        let n = bytes.len();
        if n != self.in_buffer.len() {
            return false;
        }

        // check byte by byte
        for i in 0..n {
            if Some(&bytes[i]) == self.in_buffer.get(i) {
                // nothing to do
            }
            else {
                return false;
            }
        }

        return true;
    }

    pub fn match_special(&mut self, mbytes:&[Z80InstructionByte]) -> bool {

        // check size
        let n = mbytes.len();
        if n != self.in_buffer.len() {
            return false;
        }
        
        self.in_values.clear();

        for i in 0..n {
            match self.in_buffer.get(i) {
                None => {
                    return false;
                },
                Some(byte) => {
                    match mbytes[i] {
                        // on ZIB::Byte check that value is matching
                        ZIB::Byte(mbyte) => { if *byte != mbyte { return false; } }
                        // on ZIB::Placeholder store value
                        ZIB::Placeholder => {
                            self.in_values.push(*byte);
                        }
                    }
                }
            }// match
        }// for
            
        return true;
    }

    pub fn matched_value(&self, i: usize) -> u8 {
        return *self.in_values.get(i).unwrap();
    }

    pub fn to_u16(lo: u8, hi: u8) -> u16 {
        (hi as u16) << 8 | (lo as u16)
    }
    
    pub fn matched_value_u16(&self, i: usize) -> u16 {
        Z80InstructionDecoder::to_u16(
            *self.in_values.get(i).unwrap(),
            *self.in_values.get(i+1).unwrap()
        )
    }


    pub fn debug_show_in_buffer(&self) {
        let mut s = String::new();
        for byte in self.in_buffer.iter() {
            s = format!("{}:{:02x}",s,byte);
        }
        println!("IN: {}", s);
    }

    pub fn decode_instruction(&mut self) -> Option<Z80Instruction> {

        // Z80 manual table 20 - misc operations
        if self.match_byte(0x00)                { Some(ZI::NOP) }
        else if self.match_byte(0xF3)           { Some(ZI::DisableINT) }
        else if self.match_byte(0xFB)           { Some(ZI::EnableINT) }
        else if self.match_bytes(&[0xED, 0x46])  { Some(ZI::SetINTMode0) }
        else if self.match_bytes(&[0xED, 0x56])  { Some(ZI::SetINTMode1) }
        else if self.match_bytes(&[0xED, 0x5E])  { Some(ZI::SetINTMode2) }

        // Z80 manual table 13 - 16 bits arithmetic
        else if self.match_byte(0x03)           { Some(ZI::Increment(ZIL::RegisterBC)) }
        else if self.match_byte(0x13)           { Some(ZI::Increment(ZIL::RegisterDE)) }
        else if self.match_byte(0x23)           { Some(ZI::Increment(ZIL::RegisterHL)) }
        else if self.match_byte(0x33)           { Some(ZI::Increment(ZIL::RegisterSP)) }
        else if self.match_bytes(&[0xDD,0x23])  { Some(ZI::Increment(ZIL::RegisterIX)) }
        else if self.match_bytes(&[0xFD,0x23])  { Some(ZI::Increment(ZIL::RegisterIY)) }
        else if self.match_byte(0xDB)           { Some(ZI::Decrement(ZIL::RegisterBC)) }
        else if self.match_byte(0x1B)           { Some(ZI::Decrement(ZIL::RegisterDE)) }
        else if self.match_byte(0x2B)           { Some(ZI::Decrement(ZIL::RegisterHL)) }
        else if self.match_byte(0x3B)           { Some(ZI::Decrement(ZIL::RegisterSP)) }
        else if self.match_bytes(&[0xDD,0x2B])  { Some(ZI::Decrement(ZIL::RegisterIX)) }
        else if self.match_bytes(&[0xFD,0x2B])  { Some(ZI::Decrement(ZIL::RegisterIY)) } 
        
        // Z80 manual table 6 - 8 bit load group
        // destination register A
        else if self.match_byte(0x7F) { Some(ZI::Load(ZIL::RegisterA, ZIL::RegisterA)) }
        else if self.match_byte(0x78) { Some(ZI::Load(ZIL::RegisterA, ZIL::RegisterB)) }
        else if self.match_byte(0x79) { Some(ZI::Load(ZIL::RegisterA, ZIL::RegisterC)) }
        else if self.match_byte(0x7A) { Some(ZI::Load(ZIL::RegisterA, ZIL::RegisterD)) }
        else if self.match_byte(0x7B) { Some(ZI::Load(ZIL::RegisterA, ZIL::RegisterE)) }
        else if self.match_byte(0x7C) { Some(ZI::Load(ZIL::RegisterA, ZIL::RegisterF)) }
        else if self.match_byte(0x7D) { Some(ZI::Load(ZIL::RegisterA, ZIL::RegisterL)) }
        else if self.match_byte(0x7E) { Some(ZI::Load(ZIL::RegisterA, ZIL::RegisterIndirectHL)) }
        else if self.match_byte(0x0A) { Some(ZI::Load(ZIL::RegisterA, ZIL::RegisterIndirectBC)) }
        else if self.match_byte(0x1A) { Some(ZI::Load(ZIL::RegisterA, ZIL::RegisterIndirectDE)) }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x7E), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterA, ZIL::IndexedIX(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x7E), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterA, ZIL::IndexedIY(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0x3E), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterA, ZIL::Immediate(self.matched_value(0))))
        }
        // destination register indirect HL
        else if self.match_byte(0x77) { Some(ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterA)) }
        else if self.match_byte(0x70) { Some(ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterB)) }
        else if self.match_byte(0x71) { Some(ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterC)) }
        else if self.match_byte(0x72) { Some(ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterD)) }
        else if self.match_byte(0x73) { Some(ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterE)) }
        else if self.match_byte(0x74) { Some(ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterF)) }
        else if self.match_byte(0x75) { Some(ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterL)) }
        // destination register indirect BC
        else if self.match_byte(0x02) { Some(ZI::Load(ZIL::RegisterIndirectBC, ZIL::RegisterA)) }
        // destination register indirect DE
        else if self.match_byte(0x12) { Some(ZI::Load(ZIL::RegisterIndirectDE, ZIL::RegisterA)) }

        // Z80 manual table 9 - block transfer group
        else if self.match_bytes(&[0xED,0xA0]) { Some(ZI::LoadIncrement) }
        else if self.match_bytes(&[0xED,0xB0]) { Some(ZI::LoadIncrementRepeat) }
        else if self.match_bytes(&[0xED,0xA8]) { Some(ZI::LoadDecrement) }
        else if self.match_bytes(&[0xED,0xB8]) { Some(ZI::LoadDecrementRepeat) }

        // Z80 manual table 7 - 16 bit load group
        else if self.match_byte(0xF9) { Some(ZI::Load(ZIL::RegisterSP, ZIL::RegisterHL)) }
        else if self.match_special(&[ZIB::Byte(0x01), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterBC, ZIL::ImmediateExternal(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0x11), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterDE, ZIL::ImmediateExternal(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0x21), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterHL, ZIL::ImmediateExternal(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0x31), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterSP, ZIL::ImmediateExternal(self.matched_value_u16(0))))
        }

        // destination register B
        else if self.match_byte(0x47) { Some(ZI::Load(ZIL::RegisterB, ZIL::RegisterA)) }
        else if self.match_byte(0x40) { Some(ZI::Load(ZIL::RegisterB, ZIL::RegisterB)) }
        else if self.match_byte(0x41) { Some(ZI::Load(ZIL::RegisterB, ZIL::RegisterC)) }
        else if self.match_byte(0x42) { Some(ZI::Load(ZIL::RegisterB, ZIL::RegisterD)) }
        else if self.match_byte(0x43) { Some(ZI::Load(ZIL::RegisterB, ZIL::RegisterE)) }
        else if self.match_byte(0x44) { Some(ZI::Load(ZIL::RegisterB, ZIL::RegisterF)) }
        else if self.match_byte(0x45) { Some(ZI::Load(ZIL::RegisterB, ZIL::RegisterL)) }
        else if self.match_byte(0x46) { Some(ZI::Load(ZIL::RegisterB, ZIL::RegisterIndirectHL)) }
        // destination register C
        else if self.match_byte(0x4F) { Some(ZI::Load(ZIL::RegisterC, ZIL::RegisterA)) }
        else if self.match_byte(0x48) { Some(ZI::Load(ZIL::RegisterC, ZIL::RegisterB)) }
        else if self.match_byte(0x49) { Some(ZI::Load(ZIL::RegisterC, ZIL::RegisterC)) }
        else if self.match_byte(0x4A) { Some(ZI::Load(ZIL::RegisterC, ZIL::RegisterD)) }
        else if self.match_byte(0x4B) { Some(ZI::Load(ZIL::RegisterC, ZIL::RegisterE)) }
        else if self.match_byte(0x4C) { Some(ZI::Load(ZIL::RegisterC, ZIL::RegisterF)) }
        else if self.match_byte(0x4D) { Some(ZI::Load(ZIL::RegisterC, ZIL::RegisterL)) }
        else if self.match_byte(0x4E) { Some(ZI::Load(ZIL::RegisterC, ZIL::RegisterIndirectHL)) }
        // destination register D
        else if self.match_byte(0x57) { Some(ZI::Load(ZIL::RegisterD, ZIL::RegisterA)) }
        else if self.match_byte(0x50) { Some(ZI::Load(ZIL::RegisterD, ZIL::RegisterB)) }
        else if self.match_byte(0x51) { Some(ZI::Load(ZIL::RegisterD, ZIL::RegisterC)) }
        else if self.match_byte(0x52) { Some(ZI::Load(ZIL::RegisterD, ZIL::RegisterD)) }
        else if self.match_byte(0x53) { Some(ZI::Load(ZIL::RegisterD, ZIL::RegisterE)) }
        else if self.match_byte(0x54) { Some(ZI::Load(ZIL::RegisterD, ZIL::RegisterF)) }
        else if self.match_byte(0x55) { Some(ZI::Load(ZIL::RegisterD, ZIL::RegisterL)) }
        else if self.match_byte(0x56) { Some(ZI::Load(ZIL::RegisterD, ZIL::RegisterIndirectHL)) }
        // destination register E
        else if self.match_byte(0x5F) { Some(ZI::Load(ZIL::RegisterE, ZIL::RegisterA)) }
        else if self.match_byte(0x58) { Some(ZI::Load(ZIL::RegisterE, ZIL::RegisterB)) }
        else if self.match_byte(0x59) { Some(ZI::Load(ZIL::RegisterE, ZIL::RegisterC)) }
        else if self.match_byte(0x5A) { Some(ZI::Load(ZIL::RegisterE, ZIL::RegisterD)) }
        else if self.match_byte(0x5B) { Some(ZI::Load(ZIL::RegisterE, ZIL::RegisterE)) }
        else if self.match_byte(0x5C) { Some(ZI::Load(ZIL::RegisterE, ZIL::RegisterF)) }
        else if self.match_byte(0x5D) { Some(ZI::Load(ZIL::RegisterE, ZIL::RegisterL)) }
        else if self.match_byte(0x5E) { Some(ZI::Load(ZIL::RegisterE, ZIL::RegisterIndirectHL)) }
        // destination register H
        else if self.match_byte(0x67) { Some(ZI::Load(ZIL::RegisterH, ZIL::RegisterA)) }
        else if self.match_byte(0x60) { Some(ZI::Load(ZIL::RegisterH, ZIL::RegisterB)) }
        else if self.match_byte(0x61) { Some(ZI::Load(ZIL::RegisterH, ZIL::RegisterC)) }
        else if self.match_byte(0x62) { Some(ZI::Load(ZIL::RegisterH, ZIL::RegisterD)) }
        else if self.match_byte(0x63) { Some(ZI::Load(ZIL::RegisterH, ZIL::RegisterE)) }
        else if self.match_byte(0x64) { Some(ZI::Load(ZIL::RegisterH, ZIL::RegisterF)) }
        else if self.match_byte(0x65) { Some(ZI::Load(ZIL::RegisterH, ZIL::RegisterL)) }
        else if self.match_byte(0x66) { Some(ZI::Load(ZIL::RegisterH, ZIL::RegisterIndirectHL)) }
        // destination register L
        else if self.match_byte(0x6F) { Some(ZI::Load(ZIL::RegisterL, ZIL::RegisterA)) }
        else if self.match_byte(0x68) { Some(ZI::Load(ZIL::RegisterL, ZIL::RegisterB)) }
        else if self.match_byte(0x69) { Some(ZI::Load(ZIL::RegisterL, ZIL::RegisterC)) }
        else if self.match_byte(0x6A) { Some(ZI::Load(ZIL::RegisterL, ZIL::RegisterD)) }
        else if self.match_byte(0x6B) { Some(ZI::Load(ZIL::RegisterL, ZIL::RegisterE)) }
        else if self.match_byte(0x6C) { Some(ZI::Load(ZIL::RegisterL, ZIL::RegisterF)) }
        else if self.match_byte(0x6D) { Some(ZI::Load(ZIL::RegisterL, ZIL::RegisterL)) }
        else if self.match_byte(0x6E) { Some(ZI::Load(ZIL::RegisterL, ZIL::RegisterIndirectHL)) }
        // external address
        else if self.match_special(&[ZIB::Byte(0x32), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::External(self.matched_value_u16(0)), ZIL::RegisterA))
        }

        // Z80 manual table 11 - 8 bit arithmetic and logic
        else if self.match_byte(0x87) { Some(ZI::Add(ZIL::RegisterA)) }
        else if self.match_byte(0x80) { Some(ZI::Add(ZIL::RegisterB)) }
        else if self.match_byte(0x81) { Some(ZI::Add(ZIL::RegisterC)) }
        else if self.match_byte(0x82) { Some(ZI::Add(ZIL::RegisterD)) }
        else if self.match_byte(0x83) { Some(ZI::Add(ZIL::RegisterE)) }
        else if self.match_byte(0x84) { Some(ZI::Add(ZIL::RegisterF)) }
        else if self.match_byte(0x85) { Some(ZI::Add(ZIL::RegisterL)) }

        else if self.match_byte(0x8F) { Some(ZI::AddCarry(ZIL::RegisterA)) }
        else if self.match_byte(0x88) { Some(ZI::AddCarry(ZIL::RegisterB)) }
        else if self.match_byte(0x89) { Some(ZI::AddCarry(ZIL::RegisterC)) }
        else if self.match_byte(0x8A) { Some(ZI::AddCarry(ZIL::RegisterD)) }
        else if self.match_byte(0x8B) { Some(ZI::AddCarry(ZIL::RegisterE)) }
        else if self.match_byte(0x8C) { Some(ZI::AddCarry(ZIL::RegisterF)) }
        else if self.match_byte(0x8D) { Some(ZI::AddCarry(ZIL::RegisterL)) }

        else if self.match_special(&[ZIB::Byte(0xFE), ZIB::Placeholder]) {
            Some(ZI::Compare(ZIL::Immediate(self.matched_value(0))))
        }

        // Z80 manual table 15 - jump, call and return group
        else if self.match_special(&[ZIB::Byte(0xC3), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::JumpImmediate(ZJC::Unconditionnal, 
                    ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xD8), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::JumpImmediate(ZJC::Carry,
                    ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xD2), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::JumpImmediate(ZJC::NonCarry,
                    ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xCA), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::JumpImmediate(ZJC::Zero,
                    ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xC2), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::JumpImmediate(ZJC::NonZero,
                    ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xEA), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::JumpImmediate(ZJC::ParityEven,
                    ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xE2), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::JumpImmediate(ZJC::ParityOdd,
                    ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFA), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::JumpImmediate(ZJC::SignNegative,
                    ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xF2), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::JumpImmediate(ZJC::SignPositive,
                    ZIL::Immediate16(self.matched_value_u16(0))))
        }

        else if self.match_special(&[ZIB::Byte(0x18), ZIB::Placeholder]) {
            Some(ZI::JumpRelative(ZJC::Unconditionnal, ZIL::Immediate(2 + self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0x38), ZIB::Placeholder]) {
            Some(ZI::JumpRelative(ZJC::Carry, ZIL::Immediate(2 + self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0x30), ZIB::Placeholder]) {
            Some(ZI::JumpRelative(ZJC::NonCarry, ZIL::Immediate(2 + self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0x28), ZIB::Placeholder]) {
            Some(ZI::JumpRelative(ZJC::Zero, ZIL::Immediate(2 + self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0x20), ZIB::Placeholder]) {
            Some(ZI::JumpRelative(ZJC::NonZero, ZIL::Immediate(2 + self.matched_value(0))))
        }

        else { None }
    }
}
