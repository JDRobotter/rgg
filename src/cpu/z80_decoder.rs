#![allow(non_snake_case)]

use crate::cpu::Decoder;
use crate::cpu::DecoderState;

use itertools::iproduct;

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

    RotateLeftCarryA,
    RotateLeftA,
    RotateRightCarryA,
    RotateRightA,

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
            Z80Instruction::RotateLeftCarryA            => format!("rlca"),
            Z80Instruction::RotateLeftA                 => format!("rla"),
            Z80Instruction::RotateRightCarryA           => format!("rrca"),
            Z80Instruction::RotateRightA                => format!("rra"),
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

pub struct Z80InstructionDecoder {

    decoder: Decoder<Z80Instruction>,

}

fn to_u16(lo: u8, hi: u8) -> u16 {
    (hi as u16) << 8 | (lo as u16)
}


impl Z80InstructionDecoder {

    pub fn new() -> Z80InstructionDecoder {
        let mut _self = Z80InstructionDecoder {
            decoder: Decoder::new(),
        };
        _self.register_instructions();
        _self
    }

    /// Push one byte to instruction decoder
    pub fn push(&mut self, byte:u8) {
        self.decoder.push(byte)
    }

    pub fn decode(&self) -> DecoderState<Z80Instruction> {
        self.decoder.decode()
    }

    pub fn clear(&mut self) {
        self.decoder.clear()
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

    fn decode_CB_bit_manipulation(&self, byte: u8, oix: Option<Z80InstructionLocation>) -> Option<Z80Instruction> {
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
                       None
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
                None
            }
        }
    }

    fn register_instructions(&mut self) {

        // Z80 manual table 20 - misc operations
        self.decoder.register([0x00], ZI::NOP);
        self.decoder.register([0x76], ZI::Halt);
        self.decoder.register([0xF3], ZI::DisableInt);
        self.decoder.register([0xFB], ZI::EnableInt);
        self.decoder.register([0xED, 0x46], ZI::SetINTMode0);
        self.decoder.register([0xED, 0x56], ZI::SetINTMode1);
        self.decoder.register([0xED, 0x5E], ZI::SetINTMode2);

        // Z80 manual table 13 - 16 bits arithmetic
        // ADD to HL
        self.decoder.register([0x09], ZI::Add16(ZIL::RegisterHL, ZIL::RegisterBC));
        self.decoder.register([0x19], ZI::Add16(ZIL::RegisterHL, ZIL::RegisterDE));
        self.decoder.register([0x29], ZI::Add16(ZIL::RegisterHL, ZIL::RegisterHL));
        self.decoder.register([0x39], ZI::Add16(ZIL::RegisterHL, ZIL::RegisterSP));

        self.decoder.register([0xDD, 0x09], ZI::Add16(ZIL::RegisterIX, ZIL::RegisterBC));
        self.decoder.register([0xDD, 0x19], ZI::Add16(ZIL::RegisterIX, ZIL::RegisterDE));
        self.decoder.register([0xDD, 0x39], ZI::Add16(ZIL::RegisterIX, ZIL::RegisterSP));
        self.decoder.register([0xDD, 0x29], ZI::Add16(ZIL::RegisterIX, ZIL::RegisterIX));

        self.decoder.register([0xFD, 0x09], ZI::Add16(ZIL::RegisterIY, ZIL::RegisterBC));
        self.decoder.register([0xFD, 0x19], ZI::Add16(ZIL::RegisterIY, ZIL::RegisterDE));
        self.decoder.register([0xFD, 0x39], ZI::Add16(ZIL::RegisterIY, ZIL::RegisterSP));
        self.decoder.register([0xFD, 0x29], ZI::Add16(ZIL::RegisterIY, ZIL::RegisterIY));

        // ADC HL
        self.decoder.register([0xED,0x4A], ZI::Add16Carry(ZIL::RegisterHL, ZIL::RegisterBC));
        self.decoder.register([0xED,0x5A], ZI::Add16Carry(ZIL::RegisterHL, ZIL::RegisterDE));
        self.decoder.register([0xED,0x6A], ZI::Add16Carry(ZIL::RegisterHL, ZIL::RegisterHL));
        self.decoder.register([0xED,0x7A], ZI::Add16Carry(ZIL::RegisterHL, ZIL::RegisterSP));

        // SDC HL
        self.decoder.register([0xED,0x42], ZI::Sub16Carry(ZIL::RegisterHL, ZIL::RegisterBC));
        self.decoder.register([0xED,0x52], ZI::Sub16Carry(ZIL::RegisterHL, ZIL::RegisterDE));
        self.decoder.register([0xED,0x62], ZI::Sub16Carry(ZIL::RegisterHL, ZIL::RegisterHL));
        self.decoder.register([0xED,0x72], ZI::Sub16Carry(ZIL::RegisterHL, ZIL::RegisterSP));

        self.decoder.register([0x03],         ZI::Increment16(ZIL::RegisterBC));
        self.decoder.register([0x13],         ZI::Increment16(ZIL::RegisterDE));
        self.decoder.register([0x23],         ZI::Increment16(ZIL::RegisterHL));
        self.decoder.register([0x33],         ZI::Increment16(ZIL::RegisterSP));
        self.decoder.register([0xDD,0x23],    ZI::Increment16(ZIL::RegisterIX));
        self.decoder.register([0xFD,0x23],    ZI::Increment16(ZIL::RegisterIY));

        self.decoder.register([0x0B],         ZI::Decrement16(ZIL::RegisterBC));
        self.decoder.register([0x1B],         ZI::Decrement16(ZIL::RegisterDE));
        self.decoder.register([0x2B],         ZI::Decrement16(ZIL::RegisterHL));
        self.decoder.register([0x3B],         ZI::Decrement16(ZIL::RegisterSP));
        self.decoder.register([0xDD,0x2B],    ZI::Decrement16(ZIL::RegisterIX));
        self.decoder.register([0xFD,0x2B],    ZI::Decrement16(ZIL::RegisterIY));

        // Z80 manual table 6 - 8 bit load group
        // destination register A
        self.decoder.register([0x7F], ZI::Load(ZIL::RegisterA, ZIL::RegisterA));
        self.decoder.register([0x78], ZI::Load(ZIL::RegisterA, ZIL::RegisterB));
        self.decoder.register([0x79], ZI::Load(ZIL::RegisterA, ZIL::RegisterC));
        self.decoder.register([0x7A], ZI::Load(ZIL::RegisterA, ZIL::RegisterD));
        self.decoder.register([0x7B], ZI::Load(ZIL::RegisterA, ZIL::RegisterE));
        self.decoder.register([0x7C], ZI::Load(ZIL::RegisterA, ZIL::RegisterH));
        self.decoder.register([0x7D], ZI::Load(ZIL::RegisterA, ZIL::RegisterL));
        self.decoder.register([0x7E], ZI::Load(ZIL::RegisterA, ZIL::RegisterIndirectHL));
        self.decoder.register([0x0A], ZI::Load(ZIL::RegisterA, ZIL::RegisterIndirectBC));
        self.decoder.register([0x1A], ZI::Load(ZIL::RegisterA, ZIL::RegisterIndirectDE));
        
        for b in 0..=0xff {
            self.decoder.register([0xDD, 0x7E, b], ZI::Load(ZIL::RegisterA, ZIL::IndexedIX(b)));
        }
        for b in 0..=0xff {
            self.decoder.register([0xFD, 0x7E, b], ZI::Load(ZIL::RegisterA, ZIL::IndexedIY(b)));
        }
        for (a,b) in iproduct!(0..=0xff, 0..=0xff) {
            self.decoder.register([0x3A, a, b], 
                ZI::Load(ZIL::RegisterA, ZIL::Indirect16(to_u16(a,b))));
        }
        for b in 0..=0xff {
            self.decoder.register([0x3E, b], ZI::Load(ZIL::RegisterA, ZIL::Immediate(b)));
        }

        // destination register B
        self.decoder.register([0x47], ZI::Load(ZIL::RegisterB, ZIL::RegisterA));
        self.decoder.register([0x40], ZI::Load(ZIL::RegisterB, ZIL::RegisterB));
        self.decoder.register([0x41], ZI::Load(ZIL::RegisterB, ZIL::RegisterC));
        self.decoder.register([0x42], ZI::Load(ZIL::RegisterB, ZIL::RegisterD));
        self.decoder.register([0x43], ZI::Load(ZIL::RegisterB, ZIL::RegisterE));
        self.decoder.register([0x44], ZI::Load(ZIL::RegisterB, ZIL::RegisterH));
        self.decoder.register([0x45], ZI::Load(ZIL::RegisterB, ZIL::RegisterL));
        self.decoder.register([0x46], ZI::Load(ZIL::RegisterB, ZIL::RegisterIndirectHL));
        
        for b in 0..=0xff {
            self.decoder.register([0xDD, 0x46, b], ZI::Load(ZIL::RegisterB, ZIL::IndexedIX(b)));
        }
        for b in 0..=0xff {
            self.decoder.register([0xFD, 0x46, b], ZI::Load(ZIL::RegisterB, ZIL::IndexedIY(b)));
        }
        for b in 0..=0xff {
            self.decoder.register([0x06, b], ZI::Load(ZIL::RegisterB, ZIL::Immediate(b)));
        }

        // destination register C
        self.decoder.register([0x4F], ZI::Load(ZIL::RegisterC, ZIL::RegisterA));
        self.decoder.register([0x48], ZI::Load(ZIL::RegisterC, ZIL::RegisterB));
        self.decoder.register([0x49], ZI::Load(ZIL::RegisterC, ZIL::RegisterC));
        self.decoder.register([0x4A], ZI::Load(ZIL::RegisterC, ZIL::RegisterD));
        self.decoder.register([0x4B], ZI::Load(ZIL::RegisterC, ZIL::RegisterE));
        self.decoder.register([0x4C], ZI::Load(ZIL::RegisterC, ZIL::RegisterH));
        self.decoder.register([0x4D], ZI::Load(ZIL::RegisterC, ZIL::RegisterL));
        self.decoder.register([0x4E], ZI::Load(ZIL::RegisterC, ZIL::RegisterIndirectHL));
        
        for b in 0..=0xff {
            self.decoder.register([0xDD, 0x4E, b], ZI::Load(ZIL::RegisterC, ZIL::IndexedIX(b)));
        }
        for b in 0..=0xff {
            self.decoder.register([0xFD, 0x4E, b], ZI::Load(ZIL::RegisterC, ZIL::IndexedIY(b)));
        }
        for b in 0..=0xff {
            self.decoder.register([0x0E, b], ZI::Load(ZIL::RegisterC, ZIL::Immediate(b)));
        }

        // destination register D
        self.decoder.register([0x57], ZI::Load(ZIL::RegisterD, ZIL::RegisterA));
        self.decoder.register([0x50], ZI::Load(ZIL::RegisterD, ZIL::RegisterB));
        self.decoder.register([0x51], ZI::Load(ZIL::RegisterD, ZIL::RegisterC));
        self.decoder.register([0x52], ZI::Load(ZIL::RegisterD, ZIL::RegisterD));
        self.decoder.register([0x53], ZI::Load(ZIL::RegisterD, ZIL::RegisterE));
        self.decoder.register([0x54], ZI::Load(ZIL::RegisterD, ZIL::RegisterH));
        self.decoder.register([0x55], ZI::Load(ZIL::RegisterD, ZIL::RegisterL));
        self.decoder.register([0x56], ZI::Load(ZIL::RegisterD, ZIL::RegisterIndirectHL));
        
        for b in 0..=0xff {
            self.decoder.register([0xDD, 0x56, b], ZI::Load(ZIL::RegisterD, ZIL::IndexedIX(b)));
        }
        for b in 0..=0xff {
            self.decoder.register([0xFD, 0x56, b], ZI::Load(ZIL::RegisterD, ZIL::IndexedIY(b)));
        }
        for b in 0..=0xff {
            self.decoder.register([0x16, b], ZI::Load(ZIL::RegisterD, ZIL::Immediate(b)));
        }

        // destination register E
        self.decoder.register([0x5F], ZI::Load(ZIL::RegisterE, ZIL::RegisterA));
        self.decoder.register([0x58], ZI::Load(ZIL::RegisterE, ZIL::RegisterB));
        self.decoder.register([0x59], ZI::Load(ZIL::RegisterE, ZIL::RegisterC));
        self.decoder.register([0x5A], ZI::Load(ZIL::RegisterE, ZIL::RegisterD));
        self.decoder.register([0x5B], ZI::Load(ZIL::RegisterE, ZIL::RegisterE));
        self.decoder.register([0x5C], ZI::Load(ZIL::RegisterE, ZIL::RegisterH));
        self.decoder.register([0x5D], ZI::Load(ZIL::RegisterE, ZIL::RegisterL));
        self.decoder.register([0x5E], ZI::Load(ZIL::RegisterE, ZIL::RegisterIndirectHL));
        
        for b in 0..=0xff {
            self.decoder.register([0xDD, 0x5E, b], ZI::Load(ZIL::RegisterE, ZIL::IndexedIX(b)));
        }
        for b in 0..=0xff {
            self.decoder.register([0xFD, 0x5E, b], ZI::Load(ZIL::RegisterE, ZIL::IndexedIY(b)));
        }
        for b in 0..=0xff {
            self.decoder.register([0x1E, b], ZI::Load(ZIL::RegisterE, ZIL::Immediate(b)));
        }

        // destination register H
        self.decoder.register([0x67], ZI::Load(ZIL::RegisterH, ZIL::RegisterA));
        self.decoder.register([0x60], ZI::Load(ZIL::RegisterH, ZIL::RegisterB));
        self.decoder.register([0x61], ZI::Load(ZIL::RegisterH, ZIL::RegisterC));
        self.decoder.register([0x62], ZI::Load(ZIL::RegisterH, ZIL::RegisterD));
        self.decoder.register([0x63], ZI::Load(ZIL::RegisterH, ZIL::RegisterE));
        self.decoder.register([0x64], ZI::Load(ZIL::RegisterH, ZIL::RegisterH));
        self.decoder.register([0x65], ZI::Load(ZIL::RegisterH, ZIL::RegisterL));
        self.decoder.register([0x66], ZI::Load(ZIL::RegisterH, ZIL::RegisterIndirectHL));
        
        for b in 0..=0xff {
            self.decoder.register([0xDD, 0x66, b], ZI::Load(ZIL::RegisterH, ZIL::IndexedIX(b)));
        }
        for b in 0..=0xff {
            self.decoder.register([0xFD, 0x66, b], ZI::Load(ZIL::RegisterH, ZIL::IndexedIY(b)));
        }
        for b in 0..=0xff {
            self.decoder.register([0x26, b], ZI::Load(ZIL::RegisterH, ZIL::Immediate(b)));
        }

        // destination register L
        self.decoder.register([0x6F], ZI::Load(ZIL::RegisterL, ZIL::RegisterA));
        self.decoder.register([0x68], ZI::Load(ZIL::RegisterL, ZIL::RegisterB));
        self.decoder.register([0x69], ZI::Load(ZIL::RegisterL, ZIL::RegisterC));
        self.decoder.register([0x6A], ZI::Load(ZIL::RegisterL, ZIL::RegisterD));
        self.decoder.register([0x6B], ZI::Load(ZIL::RegisterL, ZIL::RegisterE));
        self.decoder.register([0x6C], ZI::Load(ZIL::RegisterL, ZIL::RegisterH));
        self.decoder.register([0x6D], ZI::Load(ZIL::RegisterL, ZIL::RegisterL));
        self.decoder.register([0x6E], ZI::Load(ZIL::RegisterL, ZIL::RegisterIndirectHL));
        
        for b in 0..=0xff {
            self.decoder.register([0xDD, 0x6E, b], ZI::Load(ZIL::RegisterL, ZIL::IndexedIX(b)));
        }
        for b in 0..=0xff {
            self.decoder.register([0xFD, 0x6E, b], ZI::Load(ZIL::RegisterL, ZIL::IndexedIY(b)));
        }
        for b in 0..=0xff {
            self.decoder.register([0x2E, b], ZI::Load(ZIL::RegisterL, ZIL::Immediate(b)));
        }

        // external address
        for (a,b) in iproduct!(0..=0xff, 0..=0xff) {
            self.decoder.register([0x32, a, b], 
                    ZI::Load(ZIL::Indirect16(to_u16(a,b)), ZIL::RegisterA));
        }

        // destination register indirect HL
        self.decoder.register([0x77], ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterA));
        self.decoder.register([0x70], ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterB));
        self.decoder.register([0x71], ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterC));
        self.decoder.register([0x72], ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterD));
        self.decoder.register([0x73], ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterE));
        self.decoder.register([0x74], ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterH));
        self.decoder.register([0x75], ZI::Load(ZIL::RegisterIndirectHL, ZIL::RegisterL));

        for b in 0..=0xff {
            self.decoder.register([0x36, b], ZI::Load(ZIL::RegisterIndirectHL, ZIL::Immediate(b)));
        }
        
        // destination register indirect BC
        self.decoder.register([0x02], ZI::Load(ZIL::RegisterIndirectBC, ZIL::RegisterA));
        // destination register indirect DE
        self.decoder.register([0x12], ZI::Load(ZIL::RegisterIndirectDE, ZIL::RegisterA));
        
        // destination IX+d
        for b in 0..=0xff {
            self.decoder.register([0xDD, 0x77, b], ZI::Load(ZIL::IndexedIX(b), ZIL::RegisterA));
            self.decoder.register([0xDD, 0x70, b], ZI::Load(ZIL::IndexedIX(b), ZIL::RegisterB));
            self.decoder.register([0xDD, 0x71, b], ZI::Load(ZIL::IndexedIX(b), ZIL::RegisterC));
            self.decoder.register([0xDD, 0x72, b], ZI::Load(ZIL::IndexedIX(b), ZIL::RegisterD));
            self.decoder.register([0xDD, 0x73, b], ZI::Load(ZIL::IndexedIX(b), ZIL::RegisterE));
            self.decoder.register([0xDD, 0x74, b], ZI::Load(ZIL::IndexedIX(b), ZIL::RegisterH));
            self.decoder.register([0xDD, 0x75, b], ZI::Load(ZIL::IndexedIX(b), ZIL::RegisterL));
        }
        
        for (a,b) in iproduct!(0..=0xff, 0..=0xff) {
            self.decoder.register([0xDD, 0x36, a, b], ZI::Load(ZIL::IndexedIX(a), ZIL::Immediate(b)));
        }

        // destination IY+d
        for b in 0..=0xff {
            self.decoder.register([0xFD, 0x77, b], ZI::Load(ZIL::IndexedIY(b), ZIL::RegisterA));
            self.decoder.register([0xFD, 0x70, b], ZI::Load(ZIL::IndexedIY(b), ZIL::RegisterB));
            self.decoder.register([0xFD, 0x71, b], ZI::Load(ZIL::IndexedIY(b), ZIL::RegisterC));
            self.decoder.register([0xFD, 0x72, b], ZI::Load(ZIL::IndexedIY(b), ZIL::RegisterD));
            self.decoder.register([0xFD, 0x73, b], ZI::Load(ZIL::IndexedIY(b), ZIL::RegisterE));
            self.decoder.register([0xFD, 0x74, b], ZI::Load(ZIL::IndexedIY(b), ZIL::RegisterH));
            self.decoder.register([0xFD, 0x75, b], ZI::Load(ZIL::IndexedIY(b), ZIL::RegisterL));
        }

        for (a,b) in iproduct!(0..=0xff, 0..=0xff) {
            self.decoder.register([0xFD, 0x36, a, b], ZI::Load(ZIL::IndexedIY(a), ZIL::Immediate(b)));
        }

        // register R
        self.decoder.register([0xED, 0x5F], ZI::Load(ZIL::RegisterA, ZIL::RegisterR));
        self.decoder.register([0xED, 0x4F], ZI::Load(ZIL::RegisterA, ZIL::RegisterE));

        // OUT (C),r p.280
        self.decoder.register([0xED, 0x41], ZI::Out(ZIL::RegisterIndirectC, ZIL::RegisterB));
        self.decoder.register([0xED, 0x49], ZI::Out(ZIL::RegisterIndirectC, ZIL::RegisterC));
        self.decoder.register([0xED, 0x51], ZI::Out(ZIL::RegisterIndirectC, ZIL::RegisterD));
        self.decoder.register([0xED, 0x59], ZI::Out(ZIL::RegisterIndirectC, ZIL::RegisterE));
        self.decoder.register([0xED, 0x69], ZI::Out(ZIL::RegisterIndirectC, ZIL::RegisterH));
        self.decoder.register([0xED, 0x61], ZI::Out(ZIL::RegisterIndirectC, ZIL::RegisterL));
        self.decoder.register([0xED, 0x79], ZI::Out(ZIL::RegisterIndirectC, ZIL::RegisterA));

        // Z80 manual table 9 - block transfer group
        self.decoder.register([0xED, 0xA0], ZI::LoadIncrement);
        self.decoder.register([0xED, 0xB0], ZI::LoadIncrementRepeat);
        self.decoder.register([0xED, 0xA8], ZI::LoadDecrement);
        self.decoder.register([0xED, 0xB8], ZI::LoadDecrementRepeat);

        // Z80 manual table 8 - exchanges EX and EXX
        self.decoder.register([0x08], ZI::Exchange(ZIL::RegisterAF, ZIL::RegisterAFp));
        self.decoder.register([0xD9], ZI::ExchangeX);
        self.decoder.register([0xEB], ZI::Exchange(ZIL::RegisterDE, ZIL::RegisterHL));
        self.decoder.register([0xE3], ZI::Exchange(ZIL::RegisterIndirectSP, ZIL::RegisterHL));
        self.decoder.register([0xDD, 0xE3], ZI::Exchange(ZIL::RegisterIndirectSP, ZIL::RegisterIX));
        self.decoder.register([0xFD, 0xE3], ZI::Exchange(ZIL::RegisterIndirectSP, ZIL::RegisterIY));

        // Z80 manual table 7 - 16 bit load group
        for (a,b) in iproduct!(0..=0xff, 0..=0xff) {
            // BC
            self.decoder.register([0x01, a, b],
                ZI::Load16(ZIL::RegisterBC, ZIL::Immediate16(to_u16(a,b)))
            );
            self.decoder.register([0xED, 0x4B, a, b],
                ZI::Load16(ZIL::RegisterBC, ZIL::Indirect16(to_u16(a,b)))
            );

            // DE
            self.decoder.register([0x11, a, b],
                ZI::Load16(ZIL::RegisterDE, ZIL::Immediate16(to_u16(a,b)))
            );
            self.decoder.register([0xED, 0x5B, a, b],
                ZI::Load16(ZIL::RegisterDE, ZIL::Indirect16(to_u16(a,b)))
            );

            // HL
            self.decoder.register([0x21, a, b],
                ZI::Load16(ZIL::RegisterHL, ZIL::Immediate16(to_u16(a,b)))
            );
            self.decoder.register([0x2A, a, b],
                ZI::Load16(ZIL::RegisterHL, ZIL::Indirect16(to_u16(a,b)))
            );
        }

        // SP
        self.decoder.register([0xF9],       ZI::Load16(ZIL::RegisterSP, ZIL::RegisterHL));
        self.decoder.register([0xDD, 0xF9], ZI::Load16(ZIL::RegisterSP, ZIL::RegisterIX));
        self.decoder.register([0xFD, 0xF9], ZI::Load16(ZIL::RegisterSP, ZIL::RegisterIY));
        for (a,b) in iproduct!(0..=0xff, 0..=0xff) {
            self.decoder.register([0x31, a, b], 
                ZI::Load16(ZIL::RegisterSP, ZIL::Immediate16(to_u16(a,b)))
            );
        }

        for (a,b) in iproduct!(0..=0xff, 0..=0xff) {
            // IX
            self.decoder.register([0xDD, 0x21, a, b], 
                ZI::Load16(ZIL::RegisterIX, ZIL::Immediate16(to_u16(a,b)))
            );
            self.decoder.register([0xDD, 0x2A, a, b], 
                ZI::Load16(ZIL::RegisterIX, ZIL::Indirect16(to_u16(a,b)))
            );

            // IY
            self.decoder.register([0xFD, 0x21, a, b], 
                ZI::Load16(ZIL::RegisterIY, ZIL::Immediate16(to_u16(a,b)))
            );
            self.decoder.register([0xFD, 0x2A, a, b], 
                ZI::Load16(ZIL::RegisterIY, ZIL::Indirect16(to_u16(a,b)))
            );

            //
            self.decoder.register([0xED, 0x43, a, b],
                ZI::Load16(ZIL::Indirect16(to_u16(a,b)), ZIL::RegisterBC)
            );
            self.decoder.register([0xED, 0x53, a, b],
                ZI::Load16(ZIL::Indirect16(to_u16(a,b)), ZIL::RegisterDE)
            );
            self.decoder.register([0x22, a, b],
                ZI::Load16(ZIL::Indirect16(to_u16(a,b)), ZIL::RegisterHL)
            );
            self.decoder.register([0xED, 0x73, a, b],
                ZI::Load16(ZIL::Indirect16(to_u16(a,b)), ZIL::RegisterSP)
            );
            self.decoder.register([0xDD, 0x22, a, b],
                ZI::Load16(ZIL::Indirect16(to_u16(a,b)), ZIL::RegisterIX)
            );
            self.decoder.register([0xFD, 0x22, a, b],
                ZI::Load16(ZIL::Indirect16(to_u16(a,b)), ZIL::RegisterIY)
            );
        }

        // push / pop
        self.decoder.register([0xF5], ZI::Push(ZIL::RegisterAF));
        self.decoder.register([0xE5], ZI::Push(ZIL::RegisterHL));
        self.decoder.register([0xD5], ZI::Push(ZIL::RegisterDE));
        self.decoder.register([0xC5], ZI::Push(ZIL::RegisterBC));
        self.decoder.register([0xDD, 0xE5], ZI::Push(ZIL::RegisterIX));
        self.decoder.register([0xFD, 0xE5], ZI::Push(ZIL::RegisterIY));
        
        self.decoder.register([0xF1], ZI::Pop(ZIL::RegisterAF));
        self.decoder.register([0xE1], ZI::Pop(ZIL::RegisterHL));
        self.decoder.register([0xD1], ZI::Pop(ZIL::RegisterDE));
        self.decoder.register([0xC1], ZI::Pop(ZIL::RegisterBC));
        self.decoder.register([0xDD, 0xE1], ZI::Pop(ZIL::RegisterIX));
        self.decoder.register([0xFD, 0xE1], ZI::Pop(ZIL::RegisterIX));

        // Z80 manual table 11 - 8 bit arithmetic and logic
        // Add
        self.decoder.register([0x87], ZI::Add(ZIL::RegisterA));
        self.decoder.register([0x80], ZI::Add(ZIL::RegisterB));
        self.decoder.register([0x81], ZI::Add(ZIL::RegisterC));
        self.decoder.register([0x82], ZI::Add(ZIL::RegisterD));
        self.decoder.register([0x83], ZI::Add(ZIL::RegisterE));
        self.decoder.register([0x84], ZI::Add(ZIL::RegisterH));
        self.decoder.register([0x85], ZI::Add(ZIL::RegisterL));
        self.decoder.register([0x86], ZI::Add(ZIL::RegisterIndirectHL));
        for b in 0..=0xff {
            self.decoder.register([0xDD, 0x86, b], ZI::Add(ZIL::IndexedIX(b)));
            self.decoder.register([0xFD, 0x86, b], ZI::Add(ZIL::IndexedIY(b)));
            self.decoder.register([0xC6, b], ZI::Add(ZIL::Immediate(b)));
        }

        // AddCarry
        self.decoder.register([0x8F], ZI::AddCarry(ZIL::RegisterA));
        self.decoder.register([0x88], ZI::AddCarry(ZIL::RegisterB));
        self.decoder.register([0x89], ZI::AddCarry(ZIL::RegisterC));
        self.decoder.register([0x8A], ZI::AddCarry(ZIL::RegisterD));
        self.decoder.register([0x8B], ZI::AddCarry(ZIL::RegisterE));
        self.decoder.register([0x8C], ZI::AddCarry(ZIL::RegisterH));
        self.decoder.register([0x8D], ZI::AddCarry(ZIL::RegisterL));
        self.decoder.register([0x8E], ZI::AddCarry(ZIL::RegisterIndirectHL));
        for b in 0..=0xff {
            self.decoder.register([0xDD, 0x8E, b], ZI::AddCarry(ZIL::IndexedIX(b)));
            self.decoder.register([0xFD, 0x8E, b], ZI::AddCarry(ZIL::IndexedIY(b)));
            self.decoder.register([0xCE, b], ZI::AddCarry(ZIL::Immediate(b)));
        }

        // Sub
        self.decoder.register([0x97], ZI::Sub(ZIL::RegisterA));
        self.decoder.register([0x90], ZI::Sub(ZIL::RegisterB));
        self.decoder.register([0x91], ZI::Sub(ZIL::RegisterC));
        self.decoder.register([0x92], ZI::Sub(ZIL::RegisterD));
        self.decoder.register([0x93], ZI::Sub(ZIL::RegisterE));
        self.decoder.register([0x94], ZI::Sub(ZIL::RegisterH));
        self.decoder.register([0x95], ZI::Sub(ZIL::RegisterL));
        self.decoder.register([0x96], ZI::Sub(ZIL::RegisterIndirectHL));
        for b in 0..=0xff {
            self.decoder.register([0xDD, 0x96, b], ZI::Sub(ZIL::IndexedIX(b)));
            self.decoder.register([0xFD, 0x96, b], ZI::Sub(ZIL::IndexedIY(b)));
            self.decoder.register([0xD6, b], ZI::Sub(ZIL::Immediate(b)));
        }

        // SubCarry
        self.decoder.register([0x9F], ZI::SubCarry(ZIL::RegisterA));
        self.decoder.register([0x98], ZI::SubCarry(ZIL::RegisterB));
        self.decoder.register([0x99], ZI::SubCarry(ZIL::RegisterC));
        self.decoder.register([0x9A], ZI::SubCarry(ZIL::RegisterD));
        self.decoder.register([0x9B], ZI::SubCarry(ZIL::RegisterE));
        self.decoder.register([0x9C], ZI::SubCarry(ZIL::RegisterH));
        self.decoder.register([0x9D], ZI::SubCarry(ZIL::RegisterL));
        self.decoder.register([0x9E], ZI::SubCarry(ZIL::RegisterIndirectHL));
        for b in 0..=0xff {
            self.decoder.register([0xDD, 0x9E, b], ZI::SubCarry(ZIL::IndexedIX(b)));
            self.decoder.register([0xFD, 0x9E, b], ZI::SubCarry(ZIL::IndexedIY(b)));
            self.decoder.register([0xDE, b], ZI::SubCarry(ZIL::Immediate(b)));
        }

        // And
        self.decoder.register([0xA7], ZI::And(ZIL::RegisterA));
        self.decoder.register([0xA0], ZI::And(ZIL::RegisterB));
        self.decoder.register([0xA1], ZI::And(ZIL::RegisterC));
        self.decoder.register([0xA2], ZI::And(ZIL::RegisterD));
        self.decoder.register([0xA3], ZI::And(ZIL::RegisterE));
        self.decoder.register([0xA4], ZI::And(ZIL::RegisterH));
        self.decoder.register([0xA5], ZI::And(ZIL::RegisterL));
        self.decoder.register([0xA6], ZI::And(ZIL::RegisterIndirectHL));
        for b in 0..=0xff {
            self.decoder.register([0xDD, 0xA6, b], ZI::And(ZIL::IndexedIX(b)));
            self.decoder.register([0xFD, 0xA6, b], ZI::And(ZIL::IndexedIY(b)));
            self.decoder.register([0xE6, b], ZI::And(ZIL::Immediate(b)));
        }

        // Xor
        self.decoder.register([0xAF], ZI::Xor(ZIL::RegisterA));
        self.decoder.register([0xA8], ZI::Xor(ZIL::RegisterB));
        self.decoder.register([0xA9], ZI::Xor(ZIL::RegisterC));
        self.decoder.register([0xAA], ZI::Xor(ZIL::RegisterD));
        self.decoder.register([0xAB], ZI::Xor(ZIL::RegisterE));
        self.decoder.register([0xAC], ZI::Xor(ZIL::RegisterH));
        self.decoder.register([0xAD], ZI::Xor(ZIL::RegisterL));
        self.decoder.register([0xAE], ZI::Xor(ZIL::RegisterIndirectHL));
        for b in 0..=0xff {
            self.decoder.register([0xDD, 0xAE, b], ZI::Xor(ZIL::IndexedIX(b)));
            self.decoder.register([0xFD, 0xAE, b], ZI::Xor(ZIL::IndexedIY(b)));
            self.decoder.register([0xEE, b], ZI::Xor(ZIL::Immediate(b)));
        }

        // Or
        self.decoder.register([0xB7], ZI::Or(ZIL::RegisterA));
        self.decoder.register([0xB0], ZI::Or(ZIL::RegisterB));
        self.decoder.register([0xB1], ZI::Or(ZIL::RegisterC));
        self.decoder.register([0xB2], ZI::Or(ZIL::RegisterD));
        self.decoder.register([0xB3], ZI::Or(ZIL::RegisterE));
        self.decoder.register([0xB4], ZI::Or(ZIL::RegisterH));
        self.decoder.register([0xB5], ZI::Or(ZIL::RegisterL));
        self.decoder.register([0xB6], ZI::Or(ZIL::RegisterIndirectHL));
        for b in 0..=0xff {
            self.decoder.register([0xDD, 0xB6, b], ZI::Or(ZIL::IndexedIX(b)));
            self.decoder.register([0xFD, 0xB6, b], ZI::Or(ZIL::IndexedIY(b)));
            self.decoder.register([0xF6, b], ZI::Or(ZIL::Immediate(b)));
        }

        // Xor
        self.decoder.register([0xAF], ZI::Xor(ZIL::RegisterA));
        self.decoder.register([0xA8], ZI::Xor(ZIL::RegisterB));
        self.decoder.register([0xA9], ZI::Xor(ZIL::RegisterC));
        self.decoder.register([0xAA], ZI::Xor(ZIL::RegisterD));
        self.decoder.register([0xAB], ZI::Xor(ZIL::RegisterE));
        self.decoder.register([0xAC], ZI::Xor(ZIL::RegisterH));
        self.decoder.register([0xAD], ZI::Xor(ZIL::RegisterL));
        self.decoder.register([0xAE], ZI::Xor(ZIL::RegisterIndirectHL));
        for b in 0..=0xff {
            self.decoder.register([0xDD, 0xAE, b], ZI::Xor(ZIL::IndexedIX(b)));
            self.decoder.register([0xFD, 0xAE, b], ZI::Xor(ZIL::IndexedIY(b)));
            self.decoder.register([0xEE, b], ZI::Xor(ZIL::Immediate(b)));
        }

        // Compare
        self.decoder.register([0xBF], ZI::Compare(ZIL::RegisterA));
        self.decoder.register([0xB8], ZI::Compare(ZIL::RegisterB));
        self.decoder.register([0xB9], ZI::Compare(ZIL::RegisterC));
        self.decoder.register([0xBA], ZI::Compare(ZIL::RegisterD));
        self.decoder.register([0xBB], ZI::Compare(ZIL::RegisterE));
        self.decoder.register([0xBC], ZI::Compare(ZIL::RegisterH));
        self.decoder.register([0xBD], ZI::Compare(ZIL::RegisterL));
        self.decoder.register([0xBE], ZI::Compare(ZIL::RegisterIndirectHL));
        for b in 0..=0xff {
            self.decoder.register([0xDD, 0xBE, b], ZI::Compare(ZIL::IndexedIX(b)));
            self.decoder.register([0xFD, 0xBE, b], ZI::Compare(ZIL::IndexedIY(b)));
            self.decoder.register([0xFE, b], ZI::Compare(ZIL::Immediate(b)));
        }

        // Increment
        self.decoder.register([0x3C], ZI::Increment(ZIL::RegisterA));
        self.decoder.register([0x04], ZI::Increment(ZIL::RegisterB));
        self.decoder.register([0x0C], ZI::Increment(ZIL::RegisterC));
        self.decoder.register([0x14], ZI::Increment(ZIL::RegisterD));
        self.decoder.register([0x1C], ZI::Increment(ZIL::RegisterE));
        self.decoder.register([0x24], ZI::Increment(ZIL::RegisterH));
        self.decoder.register([0x2C], ZI::Increment(ZIL::RegisterL));
        self.decoder.register([0x34], ZI::Increment(ZIL::RegisterIndirectHL));
        for b in 0..=0xff {
            self.decoder.register([0xDD, 0x34, b], ZI::Increment(ZIL::IndexedIX(b)));
            self.decoder.register([0xFD, 0x34, b], ZI::Increment(ZIL::IndexedIY(b)));
        }

        // Decrement
        self.decoder.register([0x3D], ZI::Decrement(ZIL::RegisterA));
        self.decoder.register([0x05], ZI::Decrement(ZIL::RegisterB));
        self.decoder.register([0x0D], ZI::Decrement(ZIL::RegisterC));
        self.decoder.register([0x15], ZI::Decrement(ZIL::RegisterD));
        self.decoder.register([0x1D], ZI::Decrement(ZIL::RegisterE));
        self.decoder.register([0x25], ZI::Decrement(ZIL::RegisterH));
        self.decoder.register([0x2D], ZI::Decrement(ZIL::RegisterL));
        self.decoder.register([0x35], ZI::Decrement(ZIL::RegisterIndirectHL));
        for b in 0..=0xff {
            self.decoder.register([0xDD, 0x35, b], ZI::Decrement(ZIL::IndexedIX(b)));
            self.decoder.register([0xFD, 0x35, b], ZI::Decrement(ZIL::IndexedIY(b)));
        }

        // Z80 manual table 15 - jump, call and return group
        // jump
        for (a,b) in iproduct!(0..=0xff, 0..=0xff) {
            self.decoder.register([0xC3, a, b], 
                    ZI::JumpImmediate(ZJC::Unconditionnal, ZIL::Immediate16(to_u16(a,b))));
            self.decoder.register([0xDA, a, b], 
                    ZI::JumpImmediate(ZJC::Carry, ZIL::Immediate16(to_u16(a,b))));
            self.decoder.register([0xD2, a, b], 
                    ZI::JumpImmediate(ZJC::NonCarry, ZIL::Immediate16(to_u16(a,b))));
            self.decoder.register([0xCA, a, b], 
                    ZI::JumpImmediate(ZJC::Zero, ZIL::Immediate16(to_u16(a,b))));
            self.decoder.register([0xC2, a, b], 
                    ZI::JumpImmediate(ZJC::NonZero, ZIL::Immediate16(to_u16(a,b))));
            self.decoder.register([0xEA, a, b], 
                    ZI::JumpImmediate(ZJC::ParityEven, ZIL::Immediate16(to_u16(a,b))));
            self.decoder.register([0xE2, a, b], 
                    ZI::JumpImmediate(ZJC::ParityOdd, ZIL::Immediate16(to_u16(a,b))));
            self.decoder.register([0xFA, a, b], 
                    ZI::JumpImmediate(ZJC::SignNegative, ZIL::Immediate16(to_u16(a,b))));
            self.decoder.register([0xF2, a, b], 
                    ZI::JumpImmediate(ZJC::SignPositive, ZIL::Immediate16(to_u16(a,b))));
        }

        // jump relative
        for b in 0..=0xff {
            self.decoder.register([0x18, b], 
                    ZI::JumpRelative(ZJC::Unconditionnal, ZIL::Immediate(2 + b)));
            self.decoder.register([0x38, b], 
                    ZI::JumpRelative(ZJC::Carry, ZIL::Immediate(2 + b)));
            self.decoder.register([0x30, b], 
                    ZI::JumpRelative(ZJC::NonCarry, ZIL::Immediate(2 + b)));
            self.decoder.register([0x28, b], 
                    ZI::JumpRelative(ZJC::Zero, ZIL::Immediate(2 + b)));
            self.decoder.register([0x20, b], 
                    ZI::JumpRelative(ZJC::NonZero, ZIL::Immediate(2 + b)));
        }

        // jump indirect
        self.decoder.register([0xE9], ZI::JumpImmediate(ZJC::Unconditionnal, ZIL::RegisterHL));
        self.decoder.register([0xDD, 0xE9], ZI::JumpImmediate(ZJC::Unconditionnal, ZIL::RegisterIX));
        self.decoder.register([0xFD, 0xE9], ZI::JumpImmediate(ZJC::Unconditionnal, ZIL::RegisterIY));

        // call
        for (a,b) in iproduct!(0..=0xff, 0..=0xff) {
            self.decoder.register([0xCD, a, b], 
                    ZI::Call(ZJC::Unconditionnal, ZIL::Immediate16(to_u16(a,b))));
            self.decoder.register([0xDC, a, b], 
                    ZI::Call(ZJC::Carry, ZIL::Immediate16(to_u16(a,b))));
            self.decoder.register([0xD4, a, b], 
                    ZI::Call(ZJC::NonCarry, ZIL::Immediate16(to_u16(a,b))));
            self.decoder.register([0xCC, a, b], 
                    ZI::Call(ZJC::Zero, ZIL::Immediate16(to_u16(a,b))));
            self.decoder.register([0xC4, a, b], 
                    ZI::Call(ZJC::NonZero, ZIL::Immediate16(to_u16(a,b))));
            self.decoder.register([0xEC, a, b], 
                    ZI::Call(ZJC::ParityEven, ZIL::Immediate16(to_u16(a,b))));
            self.decoder.register([0xE4, a, b], 
                    ZI::Call(ZJC::ParityOdd, ZIL::Immediate16(to_u16(a,b))));
            self.decoder.register([0xFC, a, b], 
                    ZI::Call(ZJC::SignNegative, ZIL::Immediate16(to_u16(a,b))));
            self.decoder.register([0xF4, a, b], 
                    ZI::Call(ZJC::SignPositive, ZIL::Immediate16(to_u16(a,b))));
        }

        // decrement jump if non-zero
        for b in 0..=0xff {
            self.decoder.register([0x10, b], ZI::DecrementJumpNZ(ZIL::Immediate(2 + b)));
        }
        
        // return
        self.decoder.register([0xC9], ZI::Return(ZJC::Unconditionnal));
        self.decoder.register([0xD8], ZI::Return(ZJC::Carry));
        self.decoder.register([0xD0], ZI::Return(ZJC::NonCarry));
        self.decoder.register([0xC8], ZI::Return(ZJC::Zero));
        self.decoder.register([0xC0], ZI::Return(ZJC::NonZero));
        self.decoder.register([0xE8], ZI::Return(ZJC::ParityEven));
        self.decoder.register([0xE0], ZI::Return(ZJC::ParityOdd));
        self.decoder.register([0xF8], ZI::Return(ZJC::SignNegative));
        self.decoder.register([0xF0], ZI::Return(ZJC::SignPositive));

        // return from interrupt
        self.decoder.register([0xED, 0x4D], ZI::ReturnInterrupt);

        // Z80 manual table 17, restart group
        self.decoder.register([0xC7], ZI::Restart(0x00));
        self.decoder.register([0xCF], ZI::Restart(0x08));
        self.decoder.register([0xD7], ZI::Restart(0x10));
        self.decoder.register([0xDF], ZI::Restart(0x18));
        self.decoder.register([0xE7], ZI::Restart(0x20));
        self.decoder.register([0xEF], ZI::Restart(0x28));
        self.decoder.register([0xF7], ZI::Restart(0x30));
        self.decoder.register([0xFF], ZI::Restart(0x38));

        // Z80 rotate and shift, specialized register A opcodes
        self.decoder.register([0x07], ZI::RotateLeftCarryA);
        self.decoder.register([0x0F], ZI::RotateRightCarryA);
        self.decoder.register([0x17], ZI::RotateLeftA);
        self.decoder.register([0x1F], ZI::RotateRightA);
        
        self.decoder.register([0xED, 0x67], ZI::RotateRightDecimal);
        
        // Z80 manual table 14, bit operations
        for b in 0..=0xff {
            if let Some(ins) = self.decode_CB_bit_manipulation(b, None) {
                self.decoder.register([0xCB, b], ins);
            }
        }
        for (a,b) in iproduct!(0..=0xff, 0..=0xff) {
            if let Some(ins) = self.decode_CB_bit_manipulation(b, Some(ZIL::IndexedIX(a))) {
                self.decoder.register([0xDD, 0xCB, a, b], ins);
            }
            if let Some(ins) = self.decode_CB_bit_manipulation(b, Some(ZIL::IndexedIY(a))) {
                self.decoder.register([0xFD, 0xCB, a, b], ins);
            }
        }

        // Z80 manual table 12, general-purpose AF operation
        self.decoder.register([0x27], ZI::DecimalAdujstAccumulator);
        self.decoder.register([0x2F], ZI::ComplementAccumulator);
        self.decoder.register([0xED, 0x44], ZI::NegateAccumulator);
        self.decoder.register([0x3F], ZI::ComplementCarryFlag);
        self.decoder.register([0x37], ZI::SetCarryFlag);

        // Z80 manual table 19, 8bit arithmetic and logic
        for b in 0..=0xff {
            self.decoder.register([0xD3, b], ZI::Out(ZIL::Indirect(b), ZIL::RegisterA));
            self.decoder.register([0xDB, b], ZI::In(ZIL::RegisterA, ZIL::Indirect(b)));
        }

        self.decoder.register([0xED, 0xA3], ZI::OutIncrement);
        self.decoder.register([0xED, 0xB3], ZI::OutIncrementRepeat);
        self.decoder.register([0xED, 0xAB], ZI::OutDecrement);
        self.decoder.register([0xED, 0xBB], ZI::OutDecrementRepeat);
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn internal() {
        let zdec = Z80InstructionDecoder::new();
    }


}
