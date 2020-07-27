#![allow(non_snake_case)]

#[derive(Copy, Clone, Debug)]
pub enum Z80InstructionLocation {

    RegisterA,
    RegisterB,
    RegisterC,
    RegisterD,
    RegisterE,
    RegisterH,
    RegisterL,
    RegisterR,

    RegisterIndirectA,
    RegisterIndirectB,
    RegisterIndirectC,
    RegisterIndirectD,
    RegisterIndirectE,
    RegisterIndirectH,
    RegisterIndirectL,

    RegisterIndirectHL,
    RegisterIndirectBC,
    RegisterIndirectDE,
    RegisterIndirectIX,
    RegisterIndirectIY,
    RegisterIndirectSP,

    RegisterAF,
    RegisterAFp,
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

    Indirect(u8),
    Indirect16(u16),
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
            Z80InstructionLocation::RegisterH =>          format!("H"),
            Z80InstructionLocation::RegisterL =>          format!("L"),
            Z80InstructionLocation::RegisterR =>          format!("R"),
            Z80InstructionLocation::RegisterIndirectA =>  format!("(A)"),
            Z80InstructionLocation::RegisterIndirectB =>  format!("(B)"),
            Z80InstructionLocation::RegisterIndirectC =>  format!("(C)"),
            Z80InstructionLocation::RegisterIndirectD =>  format!("(D)"),
            Z80InstructionLocation::RegisterIndirectE =>  format!("(E)"),
            Z80InstructionLocation::RegisterIndirectH =>  format!("(H)"),
            Z80InstructionLocation::RegisterIndirectL =>  format!("(L)"),
            Z80InstructionLocation::RegisterIndirectHL => format!("(HL)"),
            Z80InstructionLocation::RegisterIndirectBC => format!("(BC)"),
            Z80InstructionLocation::RegisterIndirectDE => format!("(DE)"),
            Z80InstructionLocation::RegisterIndirectIX => format!("(IX)"),
            Z80InstructionLocation::RegisterIndirectIY => format!("(IY)"),
            Z80InstructionLocation::RegisterIndirectSP => format!("(SP)"),
            Z80InstructionLocation::RegisterAF =>          format!("AF"),
            Z80InstructionLocation::RegisterAFp =>         format!("AF'"),
            Z80InstructionLocation::RegisterBC =>          format!("BC"),
            Z80InstructionLocation::RegisterDE =>          format!("DE"),
            Z80InstructionLocation::RegisterHL =>          format!("HL"),
            Z80InstructionLocation::RegisterSP =>          format!("SP"),
            Z80InstructionLocation::RegisterIX =>          format!("IX"),
            Z80InstructionLocation::RegisterIY =>          format!("IY"),

            Z80InstructionLocation::IndexedIX(v) =>         format!("(IX{:+})", *v as i8),
            Z80InstructionLocation::IndexedIY(v) =>         format!("(IY{:+})", *v as i8),

            Z80InstructionLocation::Immediate(v) =>         format!("0x{:02x}",v),
            Z80InstructionLocation::Immediate16(v) =>       format!("0x{:04x}",v),

            Z80InstructionLocation::Indirect(v) =>          format!("(0x{:02x})",v),
            Z80InstructionLocation::Indirect16(v) =>        format!("(0x{:04x})", v),
        }
    }
}

#[derive(Copy, Clone, Debug)]
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

#[derive(Copy, Clone, Debug)]
pub enum Z80Instruction {

    // misc operations
    NOP,
    Halt,
    DisableInt,
    EnableInt,
    SetINTMode0,
    SetINTMode1,
    SetINTMode2,

    // jump, call and return group
    JumpImmediate(Z80JumpCondition, Z80InstructionLocation),
    JumpRelative(Z80JumpCondition, Z80InstructionLocation),
    Call(Z80JumpCondition, Z80InstructionLocation),
    DecrementJumpNZ(Z80InstructionLocation),
    Return(Z80JumpCondition),
    ReturnInterrupt,

    // load group
    Load(Z80InstructionLocation, Z80InstructionLocation),
    Load16(Z80InstructionLocation, Z80InstructionLocation),
    Push(Z80InstructionLocation),
    Pop(Z80InstructionLocation),

    // load transfer group
    LoadIncrement,
    LoadIncrementRepeat,
    LoadDecrement,
    LoadDecrementRepeat,

    // arithmetic and logic
    Add(Z80InstructionLocation),
    Add16(Z80InstructionLocation, Z80InstructionLocation),
    AddCarry(Z80InstructionLocation),
    Add16Carry(Z80InstructionLocation, Z80InstructionLocation),
    Sub(Z80InstructionLocation),
    SubCarry(Z80InstructionLocation),
    Sub16Carry(Z80InstructionLocation, Z80InstructionLocation),
    And(Z80InstructionLocation),
    Or(Z80InstructionLocation),
    Xor(Z80InstructionLocation),
    Compare(Z80InstructionLocation),
    Increment(Z80InstructionLocation),
    Increment16(Z80InstructionLocation),
    Decrement(Z80InstructionLocation),
    Decrement16(Z80InstructionLocation),

    Bit(u8, Z80InstructionLocation),
    Set(u8, Z80InstructionLocation),
    Reset(u8, Z80InstructionLocation),

    RotateLeftCarry(Z80InstructionLocation),
    RotateLeft(Z80InstructionLocation),
    RotateRightCarry(Z80InstructionLocation),
    RotateRight(Z80InstructionLocation),
    RotateRightDecimal,
    ShiftLeftArithmetic(Z80InstructionLocation),
    ShiftRightArithmetic(Z80InstructionLocation),
    ShiftRightLogic(Z80InstructionLocation),

    Exchange(Z80InstructionLocation,Z80InstructionLocation),
    ExchangeX,

    // general-purpose AF operation
    DecimalAdujstAccumulator,
    ComplementAccumulator,
    NegateAccumulator,
    ComplementCarryFlag,
    SetCarryFlag,

    In(Z80InstructionLocation, Z80InstructionLocation),
    Out(Z80InstructionLocation, Z80InstructionLocation),
    OutIncrement,
    OutIncrementRepeat,
    OutDecrement,
    OutDecrementRepeat,

    // restart group
    Restart(u16),
}
use Z80Instruction as ZI;

impl Z80Instruction {
    pub fn to_string(&self) -> String {
        match self {
            Z80Instruction::NOP =>          format!("nop"),
            Z80Instruction::Halt =>         format!("halt"),
            Z80Instruction::DisableInt =>   format!("di"),
            Z80Instruction::EnableInt =>    format!("ei"),
            Z80Instruction::SetINTMode0 =>  format!("im0"),
            Z80Instruction::SetINTMode1 =>  format!("im1"),
            Z80Instruction::SetINTMode2 =>  format!("im2"),

            Z80Instruction::JumpImmediate(cond, addr) =>    format!("jp {} {}",cond.to_string(), addr.to_string()),
            Z80Instruction::JumpRelative(cond, v) =>        {
                // special case to show immediate value as signed displacement
                match v {
                    ZIL::Immediate(e)   => format!("jr {} PC{:+}", cond.to_string(), *e as i8),
                    _                   => format!("jr {} PC+{}", cond.to_string(), v.to_string()),
                }
            }
            Z80Instruction::Call(cond, addr) =>             format!("call {} {}", cond.to_string(), addr.to_string()),
            Z80Instruction::DecrementJumpNZ(addr) =>        format!("djnz {}", addr.to_string()),
            Z80Instruction::Return(cond) =>                 format!("ret {}", cond.to_string()),
            Z80Instruction::ReturnInterrupt =>              format!("reti"),

            Z80Instruction::Load(dst, src) =>   format!("ld {} {}", dst.to_string(), src.to_string()),
            Z80Instruction::Load16(dst, src) =>   format!("ld16 {} {}", dst.to_string(), src.to_string()),
            Z80Instruction::Push(src) =>        format!("push {}", src.to_string()),
            Z80Instruction::Pop(dst) =>         format!("pop {}", dst.to_string()),

            Z80Instruction::LoadIncrement       => format!("ldi"),
            Z80Instruction::LoadIncrementRepeat => format!("ldir"),
            Z80Instruction::LoadDecrement       => format!("ldd"),
            Z80Instruction::LoadDecrementRepeat => format!("lddr"),

            Z80Instruction::Add(src)        => format!("add {}", src.to_string()),
            Z80Instruction::Add16(dst,src)  => format!("add {} {}", dst.to_string(), src.to_string()),
            Z80Instruction::AddCarry(src)   => format!("adc {}", src.to_string()),
            Z80Instruction::Add16Carry(dst, src) => format!("add {} {}", dst.to_string(), src.to_string()),
            Z80Instruction::Sub(src)        => format!("sub {}", src.to_string()),
            Z80Instruction::Sub16Carry(dst, src) => format!("sbc {} {}", dst.to_string(), src.to_string()),
            Z80Instruction::SubCarry(src)   => format!("sbc {}", src.to_string()),
            Z80Instruction::Xor(src)        => format!("xor {}", src.to_string()),
            Z80Instruction::Or(src)         => format!("or  {}", src.to_string()),
            Z80Instruction::And(src)        => format!("and {}", src.to_string()),
            Z80Instruction::Compare(src)    => format!("cp  {}", src.to_string()),
            Z80Instruction::Increment(src)  => format!("inc {}", src.to_string()),
            Z80Instruction::Increment16(src)  => format!("inc16 {}", src.to_string()),
            Z80Instruction::Decrement(src)  => format!("dec {}", src.to_string()),
            Z80Instruction::Decrement16(src)  => format!("dec16 {}", src.to_string()),

            Z80Instruction::Bit(n,src)  => format!("bit {} {}", n, src.to_string()),
            Z80Instruction::Set(n,src)  => format!("set {} {}", n, src.to_string()),
            Z80Instruction::Reset(n,src)  => format!("res {} {}", n, src.to_string()),

            Z80Instruction::RotateLeftCarry(r)          => format!("rlc {}", r.to_string()),
            Z80Instruction::RotateLeft(r)               => format!("rl  {}", r.to_string()),
            Z80Instruction::RotateRightCarry(r)         => format!("rrc {}", r.to_string()),
            Z80Instruction::RotateRight(r)              => format!("rr  {}", r.to_string()),
            Z80Instruction::RotateRightDecimal          => format!("rrd"),
            Z80Instruction::ShiftLeftArithmetic(r)      => format!("sla {}", r.to_string()),
            Z80Instruction::ShiftRightArithmetic(r)     => format!("sra {}", r.to_string()),
            Z80Instruction::ShiftRightLogic(r)          => format!("srl {}", r.to_string()),

            Z80Instruction::Exchange(a,b)  => format!("ex {} {}", a.to_string(), b.to_string()),
            Z80Instruction::ExchangeX       => format!("exx"),

            Z80Instruction::DecimalAdujstAccumulator    => format!("daa"),
            Z80Instruction::ComplementAccumulator		=> format!("cpl"),
            Z80Instruction::NegateAccumulator		    => format!("neg"),
            Z80Instruction::ComplementCarryFlag		    => format!("ccf"),
            Z80Instruction::SetCarryFlag		        => format!("scf"),

            Z80Instruction::Out(dst,src) => format!("out {},{}", dst.to_string(), src.to_string()),

            Z80Instruction::OutIncrement => format!("outi"),
            Z80Instruction::OutIncrementRepeat => format!("otir"),
            Z80Instruction::OutDecrement => format!("outd"),
            Z80Instruction::OutDecrementRepeat => format!("otdr"),

            Z80Instruction::In(dst,src) => format!("in {},{}", dst.to_string(), src.to_string()),

            Z80Instruction::Restart(addr)   => format!("rst {:02x}", addr),
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

    fn op_r_to_location(r: u8) -> ZIL {
        match r {
            0b000 => ZIL::RegisterB,
            0b001 => ZIL::RegisterC,
            0b010 => ZIL::RegisterD,
            0b011 => ZIL::RegisterE,
            0b100 => ZIL::RegisterH,
            0b110 => ZIL::RegisterIndirectHL,
            0b101 => ZIL::RegisterL,
            0b111 => ZIL::RegisterA,
            _ => { panic!("invalid bit operation") }
        }
    }

    fn decode_CB_bit_manipulation(&mut self, byte: u8, oix: Option<Z80InstructionLocation>) -> Option<Z80Instruction> {
        let op = (byte >> 6) & 0x03;
        let b = (byte >> 3) & 0x07;

        let loc = match oix {
            None => {
                let r = byte & 0x07;
                Z80InstructionDecoder::op_r_to_location(r)
            },
            Some(loc) => loc,
        };

        match op {
            0b00 => {
                // Z80 manual p.213 onward
                //  RLC : 00 000 rrr
                //  RL  : 00 010 rrr
                //  RRC : 00 001 rrr
                //  RR  : 00 011 rrr
                //  SLA : 00 100 rrr
                //  SRA : 00 101 rrr
                //  SRL : 00 111 rrr
                match b {
                   0b000 => Some(ZI::RotateLeftCarry(loc)),
                   0b010 => Some(ZI::RotateLeft(loc)),
                   0b001 => Some(ZI::RotateRightCarry(loc)),
                   0b011 => Some(ZI::RotateRight(loc)),
                   0b100 => Some(ZI::ShiftLeftArithmetic(loc)),
                   0b101 => Some(ZI::ShiftRightArithmetic(loc)),
                   0b111 => Some(ZI::ShiftRightLogic(loc)),
                   _ => {
                        self.debug_show_in_buffer();
                        panic!("unknown instruction");
                    }
                }
            }
            0b01 => {
                // Z80 manual p.243
                // BIT b,r
                // 01bbbrrr
                Some(ZI::Bit(b, loc))
            },
            0b11 => {
                // Z80 manual p.251
                // SET b,r
                // 11bbbrrr
                Some(ZI::Set(b, loc))
            }
            0b10 => {
                // Z80 manual p.259
                // RES b,r
                // 10bbbrrr
                Some(ZI::Reset(b, loc))
            }
            _ => {
                self.debug_show_in_buffer();
                panic!("unknown bit operation")
            }
        }
    }

    pub fn decode_instruction(&mut self) -> Option<Z80Instruction> {

        // Z80 manual table 20 - misc operations
        if self.match_byte(0x00)                    { Some(ZI::NOP) }
        else if self.match_byte(0x76)               { Some(ZI::Halt) }
        else if self.match_byte(0xF3)               { Some(ZI::DisableInt) }
        else if self.match_byte(0xFB)               { Some(ZI::EnableInt) }
        else if self.match_bytes(&[0xED, 0x46])     { Some(ZI::SetINTMode0) }
        else if self.match_bytes(&[0xED, 0x56])     { Some(ZI::SetINTMode1) }
        else if self.match_bytes(&[0xED, 0x5E])     { Some(ZI::SetINTMode2) }

        // Z80 manual table 13 - 16 bits arithmetic
        // ADD to HL
        else if self.match_byte(0x09)           { Some(ZI::Add16(ZIL::RegisterHL, ZIL::RegisterBC)) }
        else if self.match_byte(0x19)           { Some(ZI::Add16(ZIL::RegisterHL, ZIL::RegisterDE)) }
        else if self.match_byte(0x29)           { Some(ZI::Add16(ZIL::RegisterHL, ZIL::RegisterHL)) }
        else if self.match_byte(0x39)           { Some(ZI::Add16(ZIL::RegisterHL, ZIL::RegisterSP)) }

        else if self.match_bytes(&[0xDD, 0x09]) { Some(ZI::Add16(ZIL::RegisterIX, ZIL::RegisterBC)) }
        else if self.match_bytes(&[0xDD, 0x19]) { Some(ZI::Add16(ZIL::RegisterIX, ZIL::RegisterDE)) }
        else if self.match_bytes(&[0xDD, 0x39]) { Some(ZI::Add16(ZIL::RegisterIX, ZIL::RegisterSP)) }
        else if self.match_bytes(&[0xDD, 0x29]) { Some(ZI::Add16(ZIL::RegisterIX, ZIL::RegisterIX)) }

        else if self.match_bytes(&[0xFD, 0x09]) { Some(ZI::Add16(ZIL::RegisterIY, ZIL::RegisterBC)) }
        else if self.match_bytes(&[0xFD, 0x19]) { Some(ZI::Add16(ZIL::RegisterIY, ZIL::RegisterDE)) }
        else if self.match_bytes(&[0xFD, 0x39]) { Some(ZI::Add16(ZIL::RegisterIY, ZIL::RegisterSP)) }
        else if self.match_bytes(&[0xFD, 0x29]) { Some(ZI::Add16(ZIL::RegisterIY, ZIL::RegisterIY)) }

        // ADC HL
        else if self.match_bytes(&[0xED,0x4A])  { Some(ZI::Add16Carry(ZIL::RegisterHL, ZIL::RegisterBC)) }
        else if self.match_bytes(&[0xED,0x5A])  { Some(ZI::Add16Carry(ZIL::RegisterHL, ZIL::RegisterDE)) }
        else if self.match_bytes(&[0xED,0x6A])  { Some(ZI::Add16Carry(ZIL::RegisterHL, ZIL::RegisterHL)) }
        else if self.match_bytes(&[0xED,0x7A])  { Some(ZI::Add16Carry(ZIL::RegisterHL, ZIL::RegisterSP)) }

        // SDC HL
        else if self.match_bytes(&[0xED,0x42])  { Some(ZI::Sub16Carry(ZIL::RegisterHL, ZIL::RegisterBC)) }
        else if self.match_bytes(&[0xED,0x52])  { Some(ZI::Sub16Carry(ZIL::RegisterHL, ZIL::RegisterDE)) }
        else if self.match_bytes(&[0xED,0x62])  { Some(ZI::Sub16Carry(ZIL::RegisterHL, ZIL::RegisterHL)) }
        else if self.match_bytes(&[0xED,0x72])  { Some(ZI::Sub16Carry(ZIL::RegisterHL, ZIL::RegisterSP)) }

        else if self.match_byte(0x03)           { Some(ZI::Increment16(ZIL::RegisterBC)) }
        else if self.match_byte(0x13)           { Some(ZI::Increment16(ZIL::RegisterDE)) }
        else if self.match_byte(0x23)           { Some(ZI::Increment16(ZIL::RegisterHL)) }
        else if self.match_byte(0x33)           { Some(ZI::Increment16(ZIL::RegisterSP)) }
        else if self.match_bytes(&[0xDD,0x23])  { Some(ZI::Increment16(ZIL::RegisterIX)) }
        else if self.match_bytes(&[0xFD,0x23])  { Some(ZI::Increment16(ZIL::RegisterIY)) }

        else if self.match_byte(0x0B)           { Some(ZI::Decrement16(ZIL::RegisterBC)) }
        else if self.match_byte(0x1B)           { Some(ZI::Decrement16(ZIL::RegisterDE)) }
        else if self.match_byte(0x2B)           { Some(ZI::Decrement16(ZIL::RegisterHL)) }
        else if self.match_byte(0x3B)           { Some(ZI::Decrement16(ZIL::RegisterSP)) }
        else if self.match_bytes(&[0xDD,0x2B])  { Some(ZI::Decrement16(ZIL::RegisterIX)) }
        else if self.match_bytes(&[0xFD,0x2B])  { Some(ZI::Decrement16(ZIL::RegisterIY)) }

        // Z80 manual table 6 - 8 bit load group
        // destination register A
        else if self.match_byte(0x7F) { Some(ZI::Load(ZIL::RegisterA, ZIL::RegisterA)) }
        else if self.match_byte(0x78) { Some(ZI::Load(ZIL::RegisterA, ZIL::RegisterB)) }
        else if self.match_byte(0x79) { Some(ZI::Load(ZIL::RegisterA, ZIL::RegisterC)) }
        else if self.match_byte(0x7A) { Some(ZI::Load(ZIL::RegisterA, ZIL::RegisterD)) }
        else if self.match_byte(0x7B) { Some(ZI::Load(ZIL::RegisterA, ZIL::RegisterE)) }
        else if self.match_byte(0x7C) { Some(ZI::Load(ZIL::RegisterA, ZIL::RegisterH)) }
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
        else if self.match_special(&[ZIB::Byte(0x3A), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterA, ZIL::Indirect16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0x3E), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterA, ZIL::Immediate(self.matched_value(0))))
        }
        // destination register B
        else if self.match_byte(0x47) { Some(ZI::Load(ZIL::RegisterB, ZIL::RegisterA)) }
        else if self.match_byte(0x40) { Some(ZI::Load(ZIL::RegisterB, ZIL::RegisterB)) }
        else if self.match_byte(0x41) { Some(ZI::Load(ZIL::RegisterB, ZIL::RegisterC)) }
        else if self.match_byte(0x42) { Some(ZI::Load(ZIL::RegisterB, ZIL::RegisterD)) }
        else if self.match_byte(0x43) { Some(ZI::Load(ZIL::RegisterB, ZIL::RegisterE)) }
        else if self.match_byte(0x44) { Some(ZI::Load(ZIL::RegisterB, ZIL::RegisterH)) }
        else if self.match_byte(0x45) { Some(ZI::Load(ZIL::RegisterB, ZIL::RegisterL)) }
        else if self.match_byte(0x46) { Some(ZI::Load(ZIL::RegisterB, ZIL::RegisterIndirectHL)) }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x46), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterB, ZIL::IndexedIX(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x46), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterB, ZIL::IndexedIY(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0x06), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterB, ZIL::Immediate(self.matched_value(0))))
        }
        // destination register C
        else if self.match_byte(0x4F) { Some(ZI::Load(ZIL::RegisterC, ZIL::RegisterA)) }
        else if self.match_byte(0x48) { Some(ZI::Load(ZIL::RegisterC, ZIL::RegisterB)) }
        else if self.match_byte(0x49) { Some(ZI::Load(ZIL::RegisterC, ZIL::RegisterC)) }
        else if self.match_byte(0x4A) { Some(ZI::Load(ZIL::RegisterC, ZIL::RegisterD)) }
        else if self.match_byte(0x4B) { Some(ZI::Load(ZIL::RegisterC, ZIL::RegisterE)) }
        else if self.match_byte(0x4C) { Some(ZI::Load(ZIL::RegisterC, ZIL::RegisterH)) }
        else if self.match_byte(0x4D) { Some(ZI::Load(ZIL::RegisterC, ZIL::RegisterL)) }
        else if self.match_byte(0x4E) { Some(ZI::Load(ZIL::RegisterC, ZIL::RegisterIndirectHL)) }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x4E), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterC, ZIL::IndexedIX(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x4E), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterC, ZIL::IndexedIY(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0x0E), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterC, ZIL::Immediate(self.matched_value(0))))
        }
        // destination register D
        else if self.match_byte(0x57) { Some(ZI::Load(ZIL::RegisterD, ZIL::RegisterA)) }
        else if self.match_byte(0x50) { Some(ZI::Load(ZIL::RegisterD, ZIL::RegisterB)) }
        else if self.match_byte(0x51) { Some(ZI::Load(ZIL::RegisterD, ZIL::RegisterC)) }
        else if self.match_byte(0x52) { Some(ZI::Load(ZIL::RegisterD, ZIL::RegisterD)) }
        else if self.match_byte(0x53) { Some(ZI::Load(ZIL::RegisterD, ZIL::RegisterE)) }
        else if self.match_byte(0x54) { Some(ZI::Load(ZIL::RegisterD, ZIL::RegisterH)) }
        else if self.match_byte(0x55) { Some(ZI::Load(ZIL::RegisterD, ZIL::RegisterL)) }
        else if self.match_byte(0x56) { Some(ZI::Load(ZIL::RegisterD, ZIL::RegisterIndirectHL)) }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x56), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterD, ZIL::IndexedIX(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x56), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterD, ZIL::IndexedIY(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0x16), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterD, ZIL::Immediate(self.matched_value(0))))
        }
        // destination register E
        else if self.match_byte(0x5F) { Some(ZI::Load(ZIL::RegisterE, ZIL::RegisterA)) }
        else if self.match_byte(0x58) { Some(ZI::Load(ZIL::RegisterE, ZIL::RegisterB)) }
        else if self.match_byte(0x59) { Some(ZI::Load(ZIL::RegisterE, ZIL::RegisterC)) }
        else if self.match_byte(0x5A) { Some(ZI::Load(ZIL::RegisterE, ZIL::RegisterD)) }
        else if self.match_byte(0x5B) { Some(ZI::Load(ZIL::RegisterE, ZIL::RegisterE)) }
        else if self.match_byte(0x5C) { Some(ZI::Load(ZIL::RegisterE, ZIL::RegisterH)) }
        else if self.match_byte(0x5D) { Some(ZI::Load(ZIL::RegisterE, ZIL::RegisterL)) }
        else if self.match_byte(0x5E) { Some(ZI::Load(ZIL::RegisterE, ZIL::RegisterIndirectHL)) }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x5E), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterE, ZIL::IndexedIX(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x5E), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterE, ZIL::IndexedIY(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0x1E), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterE, ZIL::Immediate(self.matched_value(0))))
        }
        // destination register H
        else if self.match_byte(0x67) { Some(ZI::Load(ZIL::RegisterH, ZIL::RegisterA)) }
        else if self.match_byte(0x60) { Some(ZI::Load(ZIL::RegisterH, ZIL::RegisterB)) }
        else if self.match_byte(0x61) { Some(ZI::Load(ZIL::RegisterH, ZIL::RegisterC)) }
        else if self.match_byte(0x62) { Some(ZI::Load(ZIL::RegisterH, ZIL::RegisterD)) }
        else if self.match_byte(0x63) { Some(ZI::Load(ZIL::RegisterH, ZIL::RegisterE)) }
        else if self.match_byte(0x64) { Some(ZI::Load(ZIL::RegisterH, ZIL::RegisterH)) }
        else if self.match_byte(0x65) { Some(ZI::Load(ZIL::RegisterH, ZIL::RegisterL)) }
        else if self.match_byte(0x66) { Some(ZI::Load(ZIL::RegisterH, ZIL::RegisterIndirectHL)) }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x66), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterH, ZIL::IndexedIX(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x66), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterH, ZIL::IndexedIY(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0x26), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterH, ZIL::Immediate(self.matched_value(0))))
        }
        // destination register L
        else if self.match_byte(0x6F) { Some(ZI::Load(ZIL::RegisterL, ZIL::RegisterA)) }
        else if self.match_byte(0x68) { Some(ZI::Load(ZIL::RegisterL, ZIL::RegisterB)) }
        else if self.match_byte(0x69) { Some(ZI::Load(ZIL::RegisterL, ZIL::RegisterC)) }
        else if self.match_byte(0x6A) { Some(ZI::Load(ZIL::RegisterL, ZIL::RegisterD)) }
        else if self.match_byte(0x6B) { Some(ZI::Load(ZIL::RegisterL, ZIL::RegisterE)) }
        else if self.match_byte(0x6C) { Some(ZI::Load(ZIL::RegisterL, ZIL::RegisterH)) }
        else if self.match_byte(0x6D) { Some(ZI::Load(ZIL::RegisterL, ZIL::RegisterL)) }
        else if self.match_byte(0x6E) { Some(ZI::Load(ZIL::RegisterL, ZIL::RegisterIndirectHL)) }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x6E), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterL, ZIL::IndexedIX(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x6E), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterL, ZIL::IndexedIY(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0x2E), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterL, ZIL::Immediate(self.matched_value(0))))
        }

        // external address
        else if self.match_special(&[ZIB::Byte(0x32), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::Indirect16(self.matched_value_u16(0)), ZIL::RegisterA))
        }

        // destination register indirect HL
        else if self.match_byte(0x77) { Some(ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterA)) }
        else if self.match_byte(0x70) { Some(ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterB)) }
        else if self.match_byte(0x71) { Some(ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterC)) }
        else if self.match_byte(0x72) { Some(ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterD)) }
        else if self.match_byte(0x73) { Some(ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterE)) }
        else if self.match_byte(0x74) { Some(ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterH)) }
        else if self.match_byte(0x75) { Some(ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterL)) }
        else if self.match_special(&[ZIB::Byte(0x36), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::RegisterIndirectHL, ZIL::Immediate(self.matched_value(0))))
        }
        // destination register indirect BC
        else if self.match_byte(0x02) { Some(ZI::Load(ZIL::RegisterIndirectBC, ZIL::RegisterA)) }
        // destination register indirect DE
        else if self.match_byte(0x12) { Some(ZI::Load(ZIL::RegisterIndirectDE, ZIL::RegisterA)) }

        // destination IX+d
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x77), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::IndexedIX(self.matched_value(0)), ZIL::RegisterA))
        }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x70), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::IndexedIX(self.matched_value(0)), ZIL::RegisterB))
        }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x71), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::IndexedIX(self.matched_value(0)), ZIL::RegisterC))
        }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x72), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::IndexedIX(self.matched_value(0)), ZIL::RegisterD))
        }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x73), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::IndexedIX(self.matched_value(0)), ZIL::RegisterE))
        }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x74), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::IndexedIX(self.matched_value(0)), ZIL::RegisterH))
        }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x75), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::IndexedIX(self.matched_value(0)), ZIL::RegisterL))
        }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x36), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::IndexedIX(self.matched_value(0)), ZIL::Immediate(self.matched_value(1))))
        }
        // destination IY+d
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x77), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::IndexedIY(self.matched_value(0)), ZIL::RegisterA))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x70), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::IndexedIY(self.matched_value(0)), ZIL::RegisterB))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x71), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::IndexedIY(self.matched_value(0)), ZIL::RegisterC))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x72), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::IndexedIY(self.matched_value(0)), ZIL::RegisterD))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x73), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::IndexedIY(self.matched_value(0)), ZIL::RegisterE))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x74), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::IndexedIY(self.matched_value(0)), ZIL::RegisterH))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x75), ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::IndexedIY(self.matched_value(0)), ZIL::RegisterL))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x36), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load(ZIL::IndexedIY(self.matched_value(0)), ZIL::Immediate(self.matched_value(1))))
        }

        // register R
        else if self.match_bytes(&[0xED,0x5F]) { Some(ZI::Load(ZIL::RegisterA, ZIL::RegisterR)) }
        else if self.match_bytes(&[0xED,0x4F]) { Some(ZI::Load(ZIL::RegisterR, ZIL::RegisterE)) }

        // OUT
        else if self.match_bytes(&[0xED,0x79]) { Some(ZI::Out(ZIL::RegisterIndirectA, ZIL::RegisterA)) }
        else if self.match_bytes(&[0xED,0x41]) { Some(ZI::Out(ZIL::RegisterIndirectB, ZIL::RegisterA)) }
        else if self.match_bytes(&[0xED,0x49]) { Some(ZI::Out(ZIL::RegisterIndirectC, ZIL::RegisterA)) }
        else if self.match_bytes(&[0xED,0x51]) { Some(ZI::Out(ZIL::RegisterIndirectD, ZIL::RegisterA)) }
        else if self.match_bytes(&[0xED,0x59]) { Some(ZI::Out(ZIL::RegisterIndirectE, ZIL::RegisterA)) }
        else if self.match_bytes(&[0xED,0x69]) { Some(ZI::Out(ZIL::RegisterIndirectH, ZIL::RegisterA)) }
        else if self.match_bytes(&[0xED,0x61]) { Some(ZI::Out(ZIL::RegisterIndirectL, ZIL::RegisterA)) }

        // Z80 manual table 9 - block transfer group
        else if self.match_bytes(&[0xED,0xA0]) { Some(ZI::LoadIncrement) }
        else if self.match_bytes(&[0xED,0xB0]) { Some(ZI::LoadIncrementRepeat) }
        else if self.match_bytes(&[0xED,0xA8]) { Some(ZI::LoadDecrement) }
        else if self.match_bytes(&[0xED,0xB8]) { Some(ZI::LoadDecrementRepeat) }

        // Z80 manual table 8 - exchanges EX and EXX
        else if self.match_byte(0x08) { Some(ZI::Exchange(ZIL::RegisterAF, ZIL::RegisterAFp)) }
        else if self.match_byte(0xD9) { Some(ZI::ExchangeX) }
        else if self.match_byte(0xEB) { Some(ZI::Exchange(ZIL::RegisterDE, ZIL::RegisterHL)) }
        else if self.match_byte(0xE3) { Some(ZI::Exchange(ZIL::RegisterIndirectSP, ZIL::RegisterHL)) }
        else if self.match_bytes(&[0xDD,0xE3]) { Some(ZI::Exchange(ZIL::RegisterIndirectSP, ZIL::RegisterIX)) }
        else if self.match_bytes(&[0xFD,0xE3]) { Some(ZI::Exchange(ZIL::RegisterIndirectSP, ZIL::RegisterIY)) }

        // Z80 manual table 7 - 16 bit load group

        // BC
        else if self.match_special(&[ZIB::Byte(0x01), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load16(ZIL::RegisterBC, ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xED), ZIB::Byte(0x4B), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load16(ZIL::RegisterBC, ZIL::Indirect16(self.matched_value_u16(0))))
        }

        // DE
        else if self.match_special(&[ZIB::Byte(0x11), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load16(ZIL::RegisterDE, ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xED), ZIB::Byte(0x5B), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load16(ZIL::RegisterDE, ZIL::Indirect16(self.matched_value_u16(0))))
        }

        // HL
        else if self.match_special(&[ZIB::Byte(0x21), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load16(ZIL::RegisterHL, ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0x2A), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load16(ZIL::RegisterHL, ZIL::Indirect16(self.matched_value_u16(0))))
        }

        // SP
        else if self.match_byte(0xF9)           { Some(ZI::Load16(ZIL::RegisterSP, ZIL::RegisterHL)) }
        else if self.match_bytes(&[0xDD,0xF9])  { Some(ZI::Load16(ZIL::RegisterSP, ZIL::RegisterIX)) }
        else if self.match_bytes(&[0xFD,0xF9])  { Some(ZI::Load16(ZIL::RegisterSP, ZIL::RegisterIY)) }
        else if self.match_special(&[ZIB::Byte(0x31), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load16(ZIL::RegisterSP, ZIL::Immediate16(self.matched_value_u16(0))))
        }

        // IX
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x21), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load16(ZIL::RegisterIX, ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x2A), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load16(ZIL::RegisterIX, ZIL::Indirect16(self.matched_value_u16(0))))
        }

        // IY
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x21), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load16(ZIL::RegisterIY, ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x2A), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load16(ZIL::RegisterIY, ZIL::Indirect16(self.matched_value_u16(0))))
        }

        else if self.match_special(&[ZIB::Byte(0xED), ZIB::Byte(0x43), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load16(ZIL::Indirect16(self.matched_value_u16(0)), ZIL::RegisterBC))
        }
        else if self.match_special(&[ZIB::Byte(0xED), ZIB::Byte(0x53), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load16(ZIL::Indirect16(self.matched_value_u16(0)), ZIL::RegisterDE))
        }
        else if self.match_special(&[ZIB::Byte(0x22), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load16(ZIL::Indirect16(self.matched_value_u16(0)), ZIL::RegisterHL))
        }
        else if self.match_special(&[ZIB::Byte(0xED), ZIB::Byte(0x73), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load16(ZIL::Indirect16(self.matched_value_u16(0)), ZIL::RegisterSP))
        }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x22), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load16(ZIL::Indirect16(self.matched_value_u16(0)), ZIL::RegisterIX))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x22), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Load16(ZIL::Indirect16(self.matched_value_u16(0)), ZIL::RegisterIY))
        }

        else if self.match_byte(0xF5) { Some(ZI::Push(ZIL::RegisterAF)) }
        else if self.match_byte(0xE5) { Some(ZI::Push(ZIL::RegisterHL)) }
        else if self.match_byte(0xD5) { Some(ZI::Push(ZIL::RegisterDE)) }
        else if self.match_byte(0xC5) { Some(ZI::Push(ZIL::RegisterBC)) }
        else if self.match_bytes(&[0xDD, 0xE5]) { Some(ZI::Push(ZIL::RegisterIX)) }
        else if self.match_bytes(&[0xFD, 0xE5]) { Some(ZI::Push(ZIL::RegisterIY)) }

        else if self.match_byte(0xF1) { Some(ZI::Pop(ZIL::RegisterAF)) }
        else if self.match_byte(0xE1) { Some(ZI::Pop(ZIL::RegisterHL)) }
        else if self.match_byte(0xD1) { Some(ZI::Pop(ZIL::RegisterDE)) }
        else if self.match_byte(0xC1) { Some(ZI::Pop(ZIL::RegisterBC)) }
        else if self.match_bytes(&[0xDD, 0xE1]) { Some(ZI::Pop(ZIL::RegisterIX)) }
        else if self.match_bytes(&[0xFD, 0xE1]) { Some(ZI::Pop(ZIL::RegisterIY)) }

        // Z80 manual table 11 - 8 bit arithmetic and logic
        else if self.match_byte(0x87) { Some(ZI::Add(ZIL::RegisterA)) }
        else if self.match_byte(0x80) { Some(ZI::Add(ZIL::RegisterB)) }
        else if self.match_byte(0x81) { Some(ZI::Add(ZIL::RegisterC)) }
        else if self.match_byte(0x82) { Some(ZI::Add(ZIL::RegisterD)) }
        else if self.match_byte(0x83) { Some(ZI::Add(ZIL::RegisterE)) }
        else if self.match_byte(0x84) { Some(ZI::Add(ZIL::RegisterH)) }
        else if self.match_byte(0x85) { Some(ZI::Add(ZIL::RegisterL)) }
        else if self.match_byte(0x86) { Some(ZI::Add(ZIL::RegisterIndirectHL)) }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x86), ZIB::Placeholder]) {
            Some(ZI::Add(ZIL::IndexedIX(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x86), ZIB::Placeholder]) {
            Some(ZI::Add(ZIL::IndexedIY(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xC6), ZIB::Placeholder]) {
            Some(ZI::Add(ZIL::Immediate(self.matched_value(0))))
        }

        else if self.match_byte(0x8F) { Some(ZI::AddCarry(ZIL::RegisterA)) }
        else if self.match_byte(0x88) { Some(ZI::AddCarry(ZIL::RegisterB)) }
        else if self.match_byte(0x89) { Some(ZI::AddCarry(ZIL::RegisterC)) }
        else if self.match_byte(0x8A) { Some(ZI::AddCarry(ZIL::RegisterD)) }
        else if self.match_byte(0x8B) { Some(ZI::AddCarry(ZIL::RegisterE)) }
        else if self.match_byte(0x8C) { Some(ZI::AddCarry(ZIL::RegisterH)) }
        else if self.match_byte(0x8D) { Some(ZI::AddCarry(ZIL::RegisterL)) }
        else if self.match_byte(0x8E) { Some(ZI::AddCarry(ZIL::RegisterIndirectHL)) }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x8E), ZIB::Placeholder]) {
            Some(ZI::AddCarry(ZIL::IndexedIX(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x8E), ZIB::Placeholder]) {
            Some(ZI::AddCarry(ZIL::IndexedIY(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xCE), ZIB::Placeholder]) {
            Some(ZI::AddCarry(ZIL::Immediate(self.matched_value(0))))
        }


        else if self.match_byte(0x97) { Some(ZI::Sub(ZIL::RegisterA)) }
        else if self.match_byte(0x90) { Some(ZI::Sub(ZIL::RegisterB)) }
        else if self.match_byte(0x91) { Some(ZI::Sub(ZIL::RegisterC)) }
        else if self.match_byte(0x92) { Some(ZI::Sub(ZIL::RegisterD)) }
        else if self.match_byte(0x93) { Some(ZI::Sub(ZIL::RegisterE)) }
        else if self.match_byte(0x94) { Some(ZI::Sub(ZIL::RegisterH)) }
        else if self.match_byte(0x95) { Some(ZI::Sub(ZIL::RegisterL)) }
        else if self.match_byte(0x96) { Some(ZI::Sub(ZIL::RegisterIndirectHL)) }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x96), ZIB::Placeholder]) {
            Some(ZI::Sub(ZIL::IndexedIX(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x96), ZIB::Placeholder]) {
            Some(ZI::Sub(ZIL::IndexedIY(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xD6), ZIB::Placeholder]) {
            Some(ZI::Sub(ZIL::Immediate(self.matched_value(0))))
        }

        else if self.match_byte(0x9F) { Some(ZI::SubCarry(ZIL::RegisterA)) }
        else if self.match_byte(0x98) { Some(ZI::SubCarry(ZIL::RegisterB)) }
        else if self.match_byte(0x99) { Some(ZI::SubCarry(ZIL::RegisterC)) }
        else if self.match_byte(0x9A) { Some(ZI::SubCarry(ZIL::RegisterD)) }
        else if self.match_byte(0x9B) { Some(ZI::SubCarry(ZIL::RegisterE)) }
        else if self.match_byte(0x9C) { Some(ZI::SubCarry(ZIL::RegisterH)) }
        else if self.match_byte(0x9D) { Some(ZI::SubCarry(ZIL::RegisterL)) }
        else if self.match_byte(0x9E) { Some(ZI::SubCarry(ZIL::RegisterIndirectHL)) }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x9E), ZIB::Placeholder]) {
            Some(ZI::SubCarry(ZIL::IndexedIX(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x9E), ZIB::Placeholder]) {
            Some(ZI::SubCarry(ZIL::IndexedIY(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xDE), ZIB::Placeholder]) {
            Some(ZI::SubCarry(ZIL::Immediate(self.matched_value(0))))
        }

        else if self.match_byte(0xA7) { Some(ZI::And(ZIL::RegisterA)) }
        else if self.match_byte(0xA0) { Some(ZI::And(ZIL::RegisterB)) }
        else if self.match_byte(0xA1) { Some(ZI::And(ZIL::RegisterC)) }
        else if self.match_byte(0xA2) { Some(ZI::And(ZIL::RegisterD)) }
        else if self.match_byte(0xA3) { Some(ZI::And(ZIL::RegisterE)) }
        else if self.match_byte(0xA4) { Some(ZI::And(ZIL::RegisterH)) }
        else if self.match_byte(0xA5) { Some(ZI::And(ZIL::RegisterL)) }
        else if self.match_byte(0xA6) { Some(ZI::And(ZIL::RegisterIndirectHL)) }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0xA6), ZIB::Placeholder]) {
            Some(ZI::Add(ZIL::IndexedIX(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0xA6), ZIB::Placeholder]) {
            Some(ZI::Add(ZIL::IndexedIY(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xE6), ZIB::Placeholder]) {
            Some(ZI::And(ZIL::Immediate(self.matched_value(0))))
        }

        else if self.match_byte(0xAF) { Some(ZI::Xor(ZIL::RegisterA)) }
        else if self.match_byte(0xA8) { Some(ZI::Xor(ZIL::RegisterB)) }
        else if self.match_byte(0xA9) { Some(ZI::Xor(ZIL::RegisterC)) }
        else if self.match_byte(0xAA) { Some(ZI::Xor(ZIL::RegisterD)) }
        else if self.match_byte(0xAB) { Some(ZI::Xor(ZIL::RegisterE)) }
        else if self.match_byte(0xAC) { Some(ZI::Xor(ZIL::RegisterH)) }
        else if self.match_byte(0xAD) { Some(ZI::Xor(ZIL::RegisterL)) }
        else if self.match_byte(0xAE) { Some(ZI::Xor(ZIL::RegisterIndirectHL)) }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0xAE), ZIB::Placeholder]) {
            Some(ZI::Xor(ZIL::IndexedIX(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0xAE), ZIB::Placeholder]) {
            Some(ZI::Xor(ZIL::IndexedIY(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xEE), ZIB::Placeholder]) {
            Some(ZI::Xor(ZIL::Immediate(self.matched_value(0))))
        }

        else if self.match_byte(0xB7) { Some(ZI::Or(ZIL::RegisterA)) }
        else if self.match_byte(0xB0) { Some(ZI::Or(ZIL::RegisterB)) }
        else if self.match_byte(0xB1) { Some(ZI::Or(ZIL::RegisterC)) }
        else if self.match_byte(0xB2) { Some(ZI::Or(ZIL::RegisterD)) }
        else if self.match_byte(0xB3) { Some(ZI::Or(ZIL::RegisterE)) }
        else if self.match_byte(0xB4) { Some(ZI::Or(ZIL::RegisterH)) }
        else if self.match_byte(0xB5) { Some(ZI::Or(ZIL::RegisterL)) }
        else if self.match_byte(0xB6) { Some(ZI::Or(ZIL::RegisterIndirectHL)) }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0xB6), ZIB::Placeholder]) {
            Some(ZI::Or(ZIL::IndexedIX(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0xB6), ZIB::Placeholder]) {
            Some(ZI::Or(ZIL::IndexedIY(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xF6), ZIB::Placeholder]) {
            Some(ZI::Or(ZIL::Immediate(self.matched_value(0))))
        }

        else if self.match_byte(0xBF) { Some(ZI::Compare(ZIL::RegisterA)) }
        else if self.match_byte(0xB8) { Some(ZI::Compare(ZIL::RegisterB)) }
        else if self.match_byte(0xB9) { Some(ZI::Compare(ZIL::RegisterC)) }
        else if self.match_byte(0xBA) { Some(ZI::Compare(ZIL::RegisterD)) }
        else if self.match_byte(0xBB) { Some(ZI::Compare(ZIL::RegisterE)) }
        else if self.match_byte(0xBC) { Some(ZI::Compare(ZIL::RegisterH)) }
        else if self.match_byte(0xBD) { Some(ZI::Compare(ZIL::RegisterL)) }
        else if self.match_byte(0xBE) { Some(ZI::Compare(ZIL::RegisterIndirectHL)) }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0xBE), ZIB::Placeholder]) {
            Some(ZI::Compare(ZIL::IndexedIX(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0xBE), ZIB::Placeholder]) {
            Some(ZI::Compare(ZIL::IndexedIY(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFE), ZIB::Placeholder]) {
            Some(ZI::Compare(ZIL::Immediate(self.matched_value(0))))
        }

        else if self.match_byte(0x3C) { Some(ZI::Increment(ZIL::RegisterA)) }
        else if self.match_byte(0x04) { Some(ZI::Increment(ZIL::RegisterB)) }
        else if self.match_byte(0x0C) { Some(ZI::Increment(ZIL::RegisterC)) }
        else if self.match_byte(0x14) { Some(ZI::Increment(ZIL::RegisterD)) }
        else if self.match_byte(0x1C) { Some(ZI::Increment(ZIL::RegisterE)) }
        else if self.match_byte(0x24) { Some(ZI::Increment(ZIL::RegisterH)) }
        else if self.match_byte(0x2C) { Some(ZI::Increment(ZIL::RegisterL)) }
        else if self.match_byte(0x34) { Some(ZI::Increment(ZIL::RegisterIndirectHL)) }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x34), ZIB::Placeholder]) {
            Some(ZI::Increment(ZIL::IndexedIX(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x34), ZIB::Placeholder]) {
            Some(ZI::Increment(ZIL::IndexedIY(self.matched_value(0))))
        }

        else if self.match_byte(0x3D) { Some(ZI::Decrement(ZIL::RegisterA)) }
        else if self.match_byte(0x05) { Some(ZI::Decrement(ZIL::RegisterB)) }
        else if self.match_byte(0x0D) { Some(ZI::Decrement(ZIL::RegisterC)) }
        else if self.match_byte(0x15) { Some(ZI::Decrement(ZIL::RegisterD)) }
        else if self.match_byte(0x1D) { Some(ZI::Decrement(ZIL::RegisterE)) }
        else if self.match_byte(0x25) { Some(ZI::Decrement(ZIL::RegisterH)) }
        else if self.match_byte(0x2D) { Some(ZI::Decrement(ZIL::RegisterL)) }
        else if self.match_byte(0x35) { Some(ZI::Decrement(ZIL::RegisterIndirectHL)) }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0x35), ZIB::Placeholder]) {
            Some(ZI::Decrement(ZIL::IndexedIX(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0x35), ZIB::Placeholder]) {
            Some(ZI::Decrement(ZIL::IndexedIY(self.matched_value(0))))
        }

        else if self.match_special(&[ZIB::Byte(0xFE), ZIB::Placeholder]) {
            Some(ZI::Compare(ZIL::Immediate(self.matched_value(0))))
        }

        // Z80 manual table 15 - jump, call and return group
        // jump
        else if self.match_special(&[ZIB::Byte(0xC3), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::JumpImmediate(ZJC::Unconditionnal,
                    ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xDA), ZIB::Placeholder, ZIB::Placeholder]) {
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
        // jump relative
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
        // jump indirect
        else if self.match_byte(0xE9)           { Some(ZI::JumpImmediate(ZJC::Unconditionnal, ZIL::RegisterHL)) }
        else if self.match_bytes(&[0xDD,0xE9])  { Some(ZI::JumpImmediate(ZJC::Unconditionnal, ZIL::RegisterIX)) }
        else if self.match_bytes(&[0xFD,0xE9])  { Some(ZI::JumpImmediate(ZJC::Unconditionnal, ZIL::RegisterIY)) }
        // call
        else if self.match_special(&[ZIB::Byte(0xCD), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Call(ZJC::Unconditionnal,
                    ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xDC), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Call(ZJC::Carry,
                    ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xD4), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Call(ZJC::NonCarry,
                    ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xCC), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Call(ZJC::Zero,
                    ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xC4), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Call(ZJC::NonZero,
                    ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xEC), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Call(ZJC::ParityEven,
                    ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xE4), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Call(ZJC::ParityOdd,
                    ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFC), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Call(ZJC::SignNegative,
                    ZIL::Immediate16(self.matched_value_u16(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xF4), ZIB::Placeholder, ZIB::Placeholder]) {
            Some(ZI::Call(ZJC::SignPositive,
                    ZIL::Immediate16(self.matched_value_u16(0))))
        }
        // decrement jump if non-zero
        else if self.match_special(&[ZIB::Byte(0x10), ZIB::Placeholder]) {
            Some(ZI::DecrementJumpNZ(ZIL::Immediate(2 + self.matched_value(0))))
        }
        // return
        else if self.match_byte(0xC9) { Some(ZI::Return(ZJC::Unconditionnal)) }
        else if self.match_byte(0xD8) { Some(ZI::Return(ZJC::Carry)) }
        else if self.match_byte(0xD0) { Some(ZI::Return(ZJC::NonCarry)) }
        else if self.match_byte(0xC8) { Some(ZI::Return(ZJC::Zero)) }
        else if self.match_byte(0xC0) { Some(ZI::Return(ZJC::NonZero)) }
        else if self.match_byte(0xE8) { Some(ZI::Return(ZJC::ParityEven)) }
        else if self.match_byte(0xE0) { Some(ZI::Return(ZJC::ParityOdd)) }
        else if self.match_byte(0xF8) { Some(ZI::Return(ZJC::SignNegative)) }
        else if self.match_byte(0xF0) { Some(ZI::Return(ZJC::SignPositive)) }

        // return from interrupt
        else if self.match_bytes(&[0xED,0x4D])  { Some(ZI::ReturnInterrupt) }

        // Z80 manual table 17, restart group
        else if self.match_byte(0xC7) { Some(ZI::Restart(0x00)) }
        else if self.match_byte(0xCF) { Some(ZI::Restart(0x08)) }
        else if self.match_byte(0xD7) { Some(ZI::Restart(0x10)) }
        else if self.match_byte(0xDF) { Some(ZI::Restart(0x18)) }
        else if self.match_byte(0xE7) { Some(ZI::Restart(0x20)) }
        else if self.match_byte(0xEF) { Some(ZI::Restart(0x28)) }
        else if self.match_byte(0xF7) { Some(ZI::Restart(0x30)) }
        else if self.match_byte(0xFF) { Some(ZI::Restart(0x38)) }

        // Z80 rotate and shift, specialized register A opcodes
        else if self.match_byte(0x07)           { Some(ZI::RotateLeftCarry(ZIL::RegisterA)) }
        else if self.match_byte(0x0F)           { Some(ZI::RotateRightCarry(ZIL::RegisterA)) }
        else if self.match_byte(0x17)           { Some(ZI::RotateLeft(ZIL::RegisterA)) }
        else if self.match_byte(0x1F)           { Some(ZI::RotateRight(ZIL::RegisterA)) }

        else if self.match_bytes(&[0xED,0x67])  { Some(ZI::RotateRightDecimal) }

        // Z80 manual table 14, bit operations
        else if self.match_special(&[ZIB::Byte(0xCB), ZIB::Placeholder]) {
            self.decode_CB_bit_manipulation(self.matched_value(0), None)
        }
        else if self.match_special(&[ZIB::Byte(0xDD), ZIB::Byte(0xCB), ZIB::Placeholder, ZIB::Placeholder]) {
            self.decode_CB_bit_manipulation(self.matched_value(1),
                Some(ZIL::IndexedIX(self.matched_value(0))))
        }
        else if self.match_special(&[ZIB::Byte(0xFD), ZIB::Byte(0xCB), ZIB::Placeholder, ZIB::Placeholder]) {
            self.decode_CB_bit_manipulation(self.matched_value(1),
                Some(ZIL::IndexedIY(self.matched_value(0))))
        }

        // Z80 manual table 12, general-purpose AF operation
        else if self.match_byte(0x27)           { Some(ZI::DecimalAdujstAccumulator) }
        else if self.match_byte(0x2F)           { Some(ZI::ComplementAccumulator) }
        else if self.match_bytes(&[0xED,0x44])  { Some(ZI::NegateAccumulator) }
        else if self.match_byte(0x3F)           { Some(ZI::ComplementCarryFlag) }
        else if self.match_byte(0x37)           { Some(ZI::SetCarryFlag) }

        // Z80 manual table 19, 8bit arithmetic and logic
        else if self.match_special(&[ZIB::Byte(0xD3), ZIB::Placeholder]) {
            Some(ZI::Out(ZIL::Indirect(self.matched_value(0)), ZIL::RegisterA))
        }

        else if self.match_special(&[ZIB::Byte(0xDB), ZIB::Placeholder]) {
            Some(ZI::In(ZIL::RegisterA, ZIL::Indirect(self.matched_value(0))))
        }

        else if self.match_bytes(&[0xED, 0xA3]) { Some(ZI::OutIncrement) }
        else if self.match_bytes(&[0xED, 0xB3]) { Some(ZI::OutIncrementRepeat) }
        else if self.match_bytes(&[0xED, 0xAB]) { Some(ZI::OutDecrement) }
        else if self.match_bytes(&[0xED, 0xBB]) { Some(ZI::OutDecrementRepeat) }

        else { None }
    }
}
