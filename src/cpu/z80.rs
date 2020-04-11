
use crate::cpu::Z80InstructionDecoder;
use crate::system::SystemBus;
use crate::memory::Rom;

use crate::cpu::Z80Instruction;
use Z80Instruction as ZI;

use crate::cpu::Z80InstructionLocation;
use Z80InstructionLocation as ZIL;

use crate::cpu::Z80JumpCondition;
use Z80JumpCondition as ZJC;

use std::ops;
use std::convert::From;
use std::mem;

use std::time::{Duration, Instant};

bitflags! {
    struct Z80StatusFlags: u8 {
        const S =   0b1000_0000;
        const Z =   0b0100_0000;
        // unused     0010_0000
        const H =   0b0001_0000;
        // unused     0000_1000
        const PV =  0b0000_0100;
        const N =   0b0000_0010;
        const C =   0b0000_0001;
    }
}

use Z80StatusFlags as ZSF;

impl Z80StatusFlags {

    pub fn to_string(&self) -> String {
        format!("{}|{}|{}|{}|{}|{}",
            if self.contains(ZSF::S) {"S"} else {" "},
            if self.contains(ZSF::Z) {"Z"} else {" "},
            if self.contains(ZSF::H) {"H"} else {" "},
            if self.contains(ZSF::PV){"PV"} else {"  "},
            if self.contains(ZSF::N) {"N"} else {" "},
            if self.contains(ZSF::C) {"C"} else {" "}
        )
    }
}

struct Z80Registers {
    // accumulator
    a: u8,
    // flags
    flags: Z80StatusFlags,
    // general purpose registers
    b: u8,
    d: u8,
    h: u8,
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
            flags: Z80StatusFlags::empty(),
            a: 0, b: 0, d:0, h:0,
            c: 0, e:0, l:0,
            iv: 0, mr: 0, sp:0,
            ix:0, iy: 0,
            pc: 0,
        }
    }

    pub fn set_AF(&mut self, w:u16) {
        self.a = (w >> 8) as u8;
        self.flags.bits = w as u8;
    }

    pub fn set_BC(&mut self, w:u16) {
        self.b = (w >> 8) as u8;
        self.c = w as u8;
    }

    pub fn set_DE(&mut self, w:u16) {
        self.d = (w >> 8) as u8;
        self.e = w as u8;
    }

    pub fn set_HL(&mut self, w:u16) {
        self.h = (w >> 8) as u8;
        self.l = w as u8;
    }

    pub fn AF(&self) -> u16 {
        let a = self.a as u16;
        let f = self.flags.bits as u16;
        (a << 8) | f
    }

    pub fn BC(&self) -> u16 {
        let b = self.b as u16;
        let c = self.c as u16;
        (b << 8) | c
    }

    pub fn DE(&self) -> u16 {
        let d = self.d as u16;
        let e = self.e as u16;
        (d << 8) | e
    }

    pub fn HL(&self) -> u16 {
        let h = self.h as u16;
        let l = self.l as u16;
        (h << 8) | l
    }

    pub fn to_string(&self) -> String {
        format!("A {:02x}   |B {:02x} |C {:02x} |D {:02x} |E {:02x} |H {:02x} |L {:02x}\n\
                 AF {:04x} |BC {:04x}    |DE {:04x}    |HL {:04x}\n\
                 PC {:04x} |SP {:04x}    |IX {:04x}    |IY {:04x}\n\
                {}",
                self.a, self.b , self.c , self.d,
                         self.e , self.h , self.l,
                self.AF(), self.BC(), self.DE(), self.HL(),
                self.pc, self.sp, self.ix, self.iy,
                self.flags.to_string())
    }
}

pub struct Z80 {

    // system bus
    pub bus: SystemBus,

    // CPU interruption enabled flag
    interrupt_enabled: bool,
    // CPU interruption mode
    interrupt_mode: u8,

    // CPU registers
    registers: Z80Registers,
    // CPU alternate registers '
    alternate_registers: Z80Registers,
    // CPU intruction decoder
    decoder: Z80InstructionDecoder,

    // last decoded instruction address
    last_decoded_address: u16,
    // last decoded instruction
    last_decoded_instruction: Z80Instruction,

    // breakpoint address
    breakpoint_addresses: Vec<u16>,

    // number of cycles
    ncycles: u64,
}

impl Z80 {
    pub fn new(rom:Rom) -> Z80 {
        Z80 {
            bus: SystemBus::new(rom),
            
            interrupt_enabled: false,
            interrupt_mode: 0,

            registers: Z80Registers::new(),
            alternate_registers: Z80Registers::new(),

            decoder: Z80InstructionDecoder::new(),

            last_decoded_address: 0,
            last_decoded_instruction: Z80Instruction::NOP,


            breakpoint_addresses: Vec::new(),

            ncycles: 0,
        }
    }

    pub fn reset(&mut self) {
        self.registers.pc = 0;
    }

    pub fn cycles(&self) -> u64 {
        self.ncycles
    }

    /// Generate an hardware interrupt on CPU
    pub fn interrupt(&mut self) {

        // nothing to do if interrupts are disabled
        if !self.interrupt_enabled {
            return
        }

        // only Z80 mode 1 is implemented
        if self.interrupt_mode != 1 {
            panic!("IRQ with mode other than 1 detected");
        }

        // decrement stack pointer
        self.registers.sp -= 2;
        // copy current contents of the Program Counter on top of the external memory stack
        self.bus.cpu_write_u16(self.registers.sp, self.registers.pc);
        // jump to IRQ address
        self.registers.pc = 0x0038;
    }

    /// Return last decoded instruction dissassembly debug string
    pub fn dissassembly_debug_string(&self) -> String {
      format!("{:04x}: {:16}",
          self.last_decoded_address,
          self.last_decoded_instruction.to_string())
    }

    /// Return cpu registers debug string
    pub fn registers_debug_string(&self) -> String {
        
        let i = if self.interrupt_enabled { "I" } else { "_" };
        format!("IRQ {}\n {}",
            i,
            self.registers.to_string()
        )
    }

    /// Set CPU PC breakpoint
    pub fn set_breakpoint(&mut self, addr:u16) {
        self.breakpoint_addresses.push(addr);
    }

    /// Step CPU by one instruction, return true on breakpoint
    pub fn step(&mut self) -> bool {

        self.last_decoded_address = self.registers.pc;

        loop {

            // fetch one byte from program counter + displacement e
            let opb = self.bus.cpu_read(self.registers.pc);
            // increment PC
            self.registers.pc += 1;

            // feed byte to instruction decoder
            match self.decoder.push(opb) {
                Some(ins) => {
                    
                    /*
                    println!("{:04x}: {:16}",
                          self.last_decoded_address,
                          ins.to_string());
                    */
                    // execute decoded instruction
                    let tstates = self.execute_instruction(ins);
                    
                    // store instruction
                    self.last_decoded_instruction = ins;

                    // increment cycles counter
                    self.ncycles += tstates as u64;

                    break;
                },

                None => {
                    // nothing to do
                },
            };

        }//loop
 
        // test breakpoint
        self.breakpoint_addresses.contains(&self.registers.pc)
    }

    fn add_i8_to_u8(base:u8, displacement:i8) -> u8 {
        let mut za :i16 = base as i16;
        za += displacement as i16;
        // do not check for overflow
        za as u8
    }

    fn add_signed_u8_to_u16(base:u16, displacement:u8) -> u16 {
        Z80::add_i8_to_u16(base, displacement as i8)
    }

    fn add_i8_to_u16(base:u16, displacement:i8) -> u16 {
        // Indexed Addressing   p.36
        // displacement is a signed number
        let mut za :i32 = base as i32;
        za += displacement as i32;
        // do not check for overflow
        za as u16
    }

    /// Return true if bit parity is even, false if parity is odd
    fn compute_u8_parity(byte:u8) -> bool {
        // Parity/Overflow Flag     p.67
        //
        // The number of 1 bits in a byte are counted.
        // If the total is Odd, ODD parity is flagged (i.e., P = 0).
        // If the total is even, even parity is flagged (i.e., P = 1).
        //

        let mut parity = false;
        let mut v:u8 = byte;
        
        // slow and naive method
        // https://graphics.stanford.edu/~seander/bithacks.html
        while v != 0 {
            parity = !parity;
            v = v & (v-1);
        }
        
        !parity
    }

    fn read_AND_operand(&self,
                                op:Z80InstructionLocation) -> u8 {
        match op {
            ZIL::Immediate(b) => { b },
            ZIL::RegisterA => { self.registers.a },
            ZIL::RegisterB => { self.registers.b },
            ZIL::RegisterC => { self.registers.c },
            ZIL::RegisterD => { self.registers.d },
            ZIL::RegisterE => { self.registers.e },
            ZIL::RegisterH => { self.registers.h },
            ZIL::RegisterL => { self.registers.l },
            ZIL::RegisterIndirectHL => {
                // fetch byte on bus at address pointed by register HL
                self.bus.cpu_read(self.registers.HL())
            },
            ZIL::IndexedIX(d) => {
                self.bus.cpu_read(Z80::add_signed_u8_to_u16(self.registers.ix, d))
            },
            ZIL::IndexedIY(d) => {
                self.bus.cpu_read(Z80::add_signed_u8_to_u16(self.registers.iy, d))
            },
            _ => { panic!("unhandled operand: {}", op.to_string()) }
        }
    }

    fn write_AND_operand(&mut self,
                            op:Z80InstructionLocation,
                            byte: u8) {
        match op {
            ZIL::RegisterA => { self.registers.a = byte },
            ZIL::RegisterB => { self.registers.b = byte },
            ZIL::RegisterC => { self.registers.c = byte },
            ZIL::RegisterD => { self.registers.d = byte },
            ZIL::RegisterE => { self.registers.e = byte },
            ZIL::RegisterH => { self.registers.h = byte },
            ZIL::RegisterL => { self.registers.l = byte },
            ZIL::RegisterIndirectHL => {
                // fetch byte on bus at address pointed by register HL
                self.bus.cpu_write(self.registers.HL(), byte)
            },
            ZIL::IndexedIX(d) => {
                self.bus.cpu_write(
                    Z80::add_signed_u8_to_u16(self.registers.ix, d),
                    byte
                )
            },
            ZIL::IndexedIY(d) => {
                self.bus.cpu_write(
                    Z80::add_signed_u8_to_u16(self.registers.iy, d),
                    byte
                )
            },
            _ => { panic!("unhandled operand: {}", op.to_string()) }
        }
    }

    fn read_EX_operand(&self, op:Z80InstructionLocation) -> u16 {
        match op {
            ZIL::RegisterDE => { self.registers.DE() },
            ZIL::RegisterHL => { self.registers.HL() },
            ZIL::RegisterIX => { self.registers.ix },
            ZIL::RegisterIY => { self.registers.iy },
            ZIL::RegisterAF => { self.registers.AF() },
            ZIL::RegisterAFp => { self.alternate_registers.AF() },
            ZIL::RegisterIndirectSP => {
                // fetch byte on bus at address pointed by register SP
                self.bus.cpu_read_u16(self.registers.sp)
            },
            _ => { panic!("unhandled operand: {}", op.to_string()) }
        }
    }

    fn write_EX_operand(&mut self,
                            op:Z80InstructionLocation,
                            word: u16) {
        match op {
            ZIL::RegisterDE => { self.registers.set_DE(word) },
            ZIL::RegisterHL => { self.registers.set_HL(word) },
            ZIL::RegisterIX => { self.registers.ix = word },
            ZIL::RegisterIY => { self.registers.iy = word },
            ZIL::RegisterAF => { self.registers.set_AF(word) },
            ZIL::RegisterAFp => { self.alternate_registers.set_AF(word) },
            ZIL::RegisterIndirectSP => {
                // fetch byte on bus at address pointed by register SP
                self.bus.cpu_write_u16(self.registers.sp, word)
            },
            _ => { panic!("unhandled operand: {}", op.to_string()) }
        }
    }

    fn unpack_LD_operand(&self,
                                op:Z80InstructionLocation) -> u8 {
        match op {
            ZIL::Immediate(b) => { b },
            ZIL::Indirect16(addr) => { self.bus.cpu_read(addr) },
            ZIL::RegisterA => { self.registers.a },
            ZIL::RegisterB => { self.registers.b },
            ZIL::RegisterC => { self.registers.c },
            ZIL::RegisterD => { self.registers.d },
            ZIL::RegisterE => { self.registers.e },
            ZIL::RegisterH => { self.registers.h },
            ZIL::RegisterL => { self.registers.l },
            ZIL::RegisterR => {
                println!("register R not implemented");
                0
            },
            ZIL::RegisterIndirectBC => {
                // fetch byte on bus at address pointed by register BC
                self.bus.cpu_read(self.registers.BC())
            },
            ZIL::RegisterIndirectDE => {
                // fetch byte on bus at address pointed by register BC
                self.bus.cpu_read(self.registers.DE())
            },
            ZIL::RegisterIndirectHL => {
                // fetch byte on bus at address pointed by register HL
                self.bus.cpu_read(self.registers.HL())
            },
            ZIL::IndexedIX(d) => {
                self.bus.cpu_read(Z80::add_signed_u8_to_u16(self.registers.ix, d))
            },
            ZIL::IndexedIY(d) => {
                self.bus.cpu_read(Z80::add_signed_u8_to_u16(self.registers.iy, d))
            },
            _ => { panic!("unhandled operand: {}", op.to_string()) }
        }
    }

    /// Perform a OUTI instruction
    fn out_increment(&mut self) {
        // OUTI     p.309
        
        // (C) <- (HL)
        // B  <- B - 1
        // HL <- HL + 1
        // Z flag is set if B becomes zero after decrement

        let byte = self.bus.cpu_read(self.registers.HL());
        self.bus.io_write(self.registers.c, byte);

        self.registers.set_HL(Z80::add_i8_to_u16(self.registers.HL(), 1));
        self.registers.b = Z80::add_i8_to_u8(self.registers.b, -1);

        // update flags
        // add/sub
        self.registers.flags.set(ZSF::N, true);
        // zero
        self.registers.flags.set(ZSF::Z, self.registers.b == 0);
    }

    /// Perform a LDI / LDD instruction incrementing by e
    fn load_increment(&mut self, e:i8) {
        // LDI     p.130
        // LDD     p.134
        
        // (DE) <- (HL)
        let byte = self.bus.cpu_read(self.registers.HL());
        self.bus.cpu_write(self.registers.DE(), byte);
        
        // DE <- DE + e
        self.registers.set_DE(Z80::add_i8_to_u16(self.registers.DE(),e));
        // HL <- HL + e
        self.registers.set_HL(Z80::add_i8_to_u16(self.registers.HL(),e));
        // BC <- BC - 1
        self.registers.set_BC(Z80::add_i8_to_u16(self.registers.BC(),-1));

        // change flags
        self.registers.flags.set(ZSF::H, false);
        self.registers.flags.set(ZSF::PV, self.registers.BC() != 0);
        self.registers.flags.set(ZSF::N, false);
    }

    /// Perform an DEC instruction decrementing by e
    fn decrement8(&mut self,
                    op: Z80InstructionLocation,
                    e: u8) {
    
        self.registers.flags.toggle(ZSF::C);
        self.increment8(op, !(e as u8));
        self.registers.flags.toggle(ZSF::C);
    }

    /// Perform an INC instruction incrementing by e
    fn increment8(&mut self,
                    op: Z80InstructionLocation,
                    e: u8) {
        // INC r    p.165
        // ...
        // DEC r    p.170
        // ...
        
        let temp = match op {
            ZIL::RegisterA => { self.registers.a },
            ZIL::RegisterB => { self.registers.b },
            ZIL::RegisterC => { self.registers.c },
            ZIL::RegisterD => { self.registers.d },
            ZIL::RegisterE => { self.registers.e },
            ZIL::RegisterH => { self.registers.h },
            ZIL::RegisterL => { self.registers.l },
            ZIL::RegisterIndirectHL => {
                // fetch byte on bus at address pointed by register HL
                self.bus.cpu_read(self.registers.HL())
            },
            ZIL::IndexedIX(d) => {
                // fetch byte on bus at address pointed by register IX + d
                let addr = Z80::add_signed_u8_to_u16(self.registers.ix, d);
                self.bus.cpu_read(addr)
            },
            ZIL::IndexedIY(d) => {
                // fetch byte on bus at address pointed by register IY + d
                let addr = Z80::add_signed_u8_to_u16(self.registers.iy, d);
                self.bus.cpu_read(addr)
            },
            _ => { panic!("unhandled operand: {}", op.to_string()) }
        };

        // increment register
        let temp = Z80::add8_with_carry(temp, e, &mut self.registers.flags);
        
        match op {
            ZIL::RegisterA => { self.registers.a = temp }
            ZIL::RegisterB => { self.registers.b = temp }
            ZIL::RegisterC => { self.registers.c = temp }
            ZIL::RegisterD => { self.registers.d = temp }
            ZIL::RegisterE => { self.registers.e = temp }
            ZIL::RegisterH => { self.registers.h = temp }
            ZIL::RegisterL => { self.registers.l = temp }
            ZIL::RegisterIndirectHL => {
                self.bus.cpu_write(self.registers.HL(), temp);
            },
            ZIL::IndexedIX(d) => {
                // fetch byte on bus at address pointed by register IX + d
                // and write incremented value
                let addr = Z80::add_signed_u8_to_u16(self.registers.ix, d);
                self.bus.cpu_write(addr, temp);
            },
            ZIL::IndexedIY(d) => {
                // fetch byte on bus at address pointed by register IY + d
                // and write incremented value
                let addr = Z80::add_signed_u8_to_u16(self.registers.iy, d);
                self.bus.cpu_write(addr, temp);
            },
            _ => { panic!("unhandled operand: {}", op.to_string()) }
        }
       
    }

    /// Perform an INC / DEC instruction incrementing by e
    fn increment16(&mut self,
                    op: Z80InstructionLocation,
                    e: i8) {
        // INC ss   p.198
        // ...
        // DEC ss   p.201
        // ...

        let temp = match op {
            ZIL::RegisterBC => { self.registers.BC() }
            ZIL::RegisterDE => { self.registers.DE() }
            ZIL::RegisterHL => { self.registers.HL() }
            ZIL::RegisterIX => { self.registers.ix }
            ZIL::RegisterIY => { self.registers.iy }
            ZIL::RegisterSP => { self.registers.sp }
            _ => { panic!("unhandled operand: {}", op.to_string()) }
        };

        // increment register
        let mut temp = temp as i32;
        temp += e as i32;
        let temp = temp as u16;

        // no flags altered for 16 bits INC/DEC

        match op {
            ZIL::RegisterBC => {
                self.registers.set_BC(temp)
            },
            ZIL::RegisterDE => {
                self.registers.set_DE(temp)
            },
            ZIL::RegisterHL => {
                self.registers.set_HL(temp)
            },
            ZIL::RegisterSP => {
                self.registers.sp = temp
            },
            ZIL::RegisterIX => {
                self.registers.ix = temp
            },
            ZIL::RegisterIY => {
                self.registers.iy = temp
            },
            _ => { panic!("unhandled operand: {}", op.to_string()) }
        }
    }

    fn test_jump_condition(&self, condition: Z80JumpCondition) -> bool {
        match condition {
            ZJC::Unconditionnal => { true },
            ZJC::Carry          => {  self.registers.flags.contains(ZSF::C) },
            ZJC::NonCarry       => { !self.registers.flags.contains(ZSF::C) },
            ZJC::Zero           => {  self.registers.flags.contains(ZSF::Z) },
            ZJC::NonZero        => { !self.registers.flags.contains(ZSF::Z) },
            ZJC::ParityEven     => {  self.registers.flags.contains(ZSF::PV) },
            ZJC::ParityOdd      => { !self.registers.flags.contains(ZSF::PV) },
            ZJC::SignNegative   => {  self.registers.flags.contains(ZSF::S) },
            ZJC::SignPositive   => { !self.registers.flags.contains(ZSF::S) },
        }
    }

    fn add16_with_carry(a: u16, b:u16, flags: &mut Z80StatusFlags) -> u16 {

        // based on
        // https://stackoverflow.com/questions/8034566/overflow-and-carry-flags-on-z80
 
        let mut carry_out = false;

        // 1111 11
        // 5432 1098 7654 3210
        // half carry is carry from bit 11 to bit 12
        let half_carry_out = ((a & 0x0fff) + (b & 0x0fff)) & 0x1000 != 0;

        let acc =
            if flags.contains(ZSF::C) {
                // with carry
                carry_out = (a >= 0xffff - b);
                a + b + 1
            }
            else {
                // without carry
                carry_out = (a > 0xffff - b);
                a + b
        };
    
        // compute overflow by sign comparison
        let mut carry_ins = ((a ^ b) ^ 0x8000) & 0x8000 != 0;
        if carry_ins {
            // if addend signs are the same
            // overflow if the sum sign differs from the sign of either addends
            carry_ins = ((acc ^ a) & 0x8000) != 0;
        }

        // update flags
        flags.set(ZSF::C, carry_out);
        flags.set(ZSF::H, half_carry_out);
        flags.set(ZSF::PV, carry_ins);

        flags.set(ZSF::S, acc & 0x8000 != 0);
        flags.set(ZSF::Z, acc == 0);

        acc
    }

    fn add8_with_carry(a: u8, b:u8, flags: &mut Z80StatusFlags) -> u8 {

        // based on
        // https://stackoverflow.com/questions/8034566/overflow-and-carry-flags-on-z80
 
        let mut carry_out = false;

        // half carry is carry from bit 3 to bit 4
        let half_carry_out = ((a & 0x0f) + (b & 0x0f)) & 0x10 != 0;

        let acc =
            if flags.contains(ZSF::C) {
                // with carry
                carry_out = (a >= 0xff - b);
                a + b + 1
            }
            else {
                // without carry
                carry_out = (a > 0xff - b);
                a + b
        };
    
        // compute overflow by sign comparison
        let mut carry_ins = ((a ^ b) ^ 0x80) & 0x80 != 0;
        if carry_ins {
            // if addend signs are the same
            // overflow if the sum sign differs from the sign of either addends
            carry_ins = ((acc ^ a) & 0x80) != 0;
        }

        // update flags
        flags.set(ZSF::C, carry_out);
        flags.set(ZSF::H, half_carry_out);
        flags.set(ZSF::PV, carry_ins);

        flags.set(ZSF::S, acc & 0x80 != 0);
        flags.set(ZSF::Z, acc == 0);

        acc
    }

    /** Execute given instruction and return number of T states taken to execute */
    pub fn execute_instruction(&mut self,
                                ins: Z80Instruction) -> u8 {

        match ins {
            ZI::NOP => {
                // nothing to do

                4 // T-states
            },

            ZI::Halt => {
                // HALT     p.173
                
                // Suspends CPU operation unitl a subsequent interrupt or reset is received
                
                // HACK
                self.registers.pc -= 1;
                // HACK
                
                4 // T-states
            },

            ZI::DisableInt => {
                // DI p.182
                self.interrupt_enabled = false;

                4 // T-states
            },
            ZI::EnableInt => {
                // EI p.183
                self.interrupt_enabled = true;

                4 // T-states
            },
            ZI::SetINTMode0 => {
                // IM p.184
                self.interrupt_mode = 0;

                8 // T-states
            },
            ZI::SetINTMode1 => {
                // IM p.185
                self.interrupt_mode = 1;

                8 // T-states
            },
            ZI::SetINTMode2 => {
                // IM p.186
                self.interrupt_mode = 2;

                8 // T-states
            },
            
            ZI::JumpImmediate(condition, opaddr) => {
                // JP nn    p.262
                // JP *, nn p.263
                // JP (HL)  p.275
                // JP (IX)  p.276
                // JP (IY)  p.277
                // ...

                // if jump condition is satisfied
                if self.test_jump_condition(condition) {

                    let addr = match opaddr {
                        ZIL::Immediate16(w) => { w },
                        ZIL::RegisterHL => {
                            // fetch byte on bus at address
                            // pointed by register HL
                            self.registers.HL()
                        },
                        ZIL::RegisterIX => {
                            self.registers.ix
                        },
                        ZIL::RegisterIY => {
                            self.registers.iy
                        },
                        _ => { panic!("unhandled operand: {}",
                                        opaddr.to_string()) }
                    };
                    
                    // set PC register
                    self.registers.pc = addr;

                }
                
                // T-states
                match opaddr {
                    ZIL::RegisterHL => { 4 },
                    ZIL::RegisterIX => { 8 },
                    ZIL::RegisterIY => { 8 },
                    _ => { 10 },
                }
            },

            ZI::JumpRelative(condition, opaddr) => {
                // JR *, e     p.265
                
                // if jump condition is satisfied
                if self.test_jump_condition(condition) {
                    
                    let e = match opaddr {
                        ZIL::Immediate(b) => { b },
                        _ => { panic!("unhandled operand: {}",
                                        opaddr.to_string()) }
                    };

                    // increment pc by e signed
                    self.registers.pc = 
                        Z80::add_signed_u8_to_u16(self.registers.pc, e-2);

                    // T-states
                    12
                }
                else {
                    // T-states
                    7
                }
            },

            ZI::DecrementJumpNZ(opaddr) => {
                // DJNZ,e   p.278

                // B <- B-1
                // if B = 0, continue
                // if B != 0, PC <- PC + e

                let e = match opaddr {
                    ZIL::Immediate(b) => { b },
                    _ => { panic!("unhandled operand: {}",
                                    opaddr.to_string()) }
                };

                // decrement register B
                self.registers.b = Z80::add_i8_to_u8(self.registers.b, -1);

                if self.registers.b != 0 {
                    // Increment PC by e signed
                    self.registers.pc =
                        Z80::add_signed_u8_to_u16(self.registers.pc, e-2);

                    // T-states
                    13
                }
                else {
                    // T-states
                    8
                }

            },

            ZI::Call(condition, op) => {
                // CALL nn      p.281
                // CALL cc,nn   p.283
                
                // if condition true,
                // (SP - 1) <- PCH
                // (SP - 2) <- PCL
                // PC <- nn
                
                if self.test_jump_condition(condition) {

                    let addr = match op {
                        ZIL::Immediate16(w) => { w },
                        _ => { panic!("unhandled operand: {}",
                                        op.to_string()) }
                    };

                    // decrement stack pointer
                    self.registers.sp -= 2;
                    // copy current contents of the Program Counter on top of the external memory stack
                    self.bus.cpu_write_u16(self.registers.sp, self.registers.pc);

                    // jump to nn
                    self.registers.pc = addr;

                    // T-states
                    17
                }
                else {
                    // T-states
                    10
                }
            },

            ZI::Return(condition) => {
                // RET          p.285
                // RET cc       p.286

                // if condition true,
                // PCL <- (SP)
                // PCH <- (SP+1)
                
                if self.test_jump_condition(condition) {
                    // copy from top of stack to PC
                    let addr = self.bus.cpu_read_u16(self.registers.sp);
                    // increment stack pointer
                    self.registers.sp += 2;
                    // jump to address
                    self.registers.pc = addr;

                    // T-states
                    match condition {
                        ZJC::Unconditionnal => { 10 },
                        _ => { 11 },
                    }
                }
                else {
                    // T-states
                    5
                }
            },

            ZI::Restart(addr) => {
                // RST p    p.292
    
                // (SP-1) <- PCH
                // (SP-2) <- PCL
                // PCH <- 0
                // PCL <- p
                
                // decrement stack pointer
                self.registers.sp -= 2;
                // copy current contents of the Program Counter on top of the external memory stack
                self.bus.cpu_write_u16(self.registers.sp, self.registers.pc);
                // jump to address
                self.registers.pc = addr;

                // T-states
                11
            },

            ZI::Push(op) => {
                // PUSH qq      p.115
                // PUSH IX      p.117
                // PUSH IY      p.118
                
                let word = match op {
                    ZIL::RegisterBC => { self.registers.BC() }
                    ZIL::RegisterDE => { self.registers.DE() }
                    ZIL::RegisterHL => { self.registers.HL() }
                    ZIL::RegisterAF => { self.registers.AF() }
                    ZIL::RegisterIX => { self.registers.ix }
                    ZIL::RegisterIY => { self.registers.iy }
                  _ => { panic!("unhandled operand: {}", op.to_string()) }
                };

                // decrement stack pointer
                self.registers.sp -= 2;
                // copy fetched word on top of the external memory stack
                self.bus.cpu_write_u16(self.registers.sp, word);
                // jump to address

                // T-states
                match op {
                    ZIL::RegisterIX => { 15 },
                    ZIL::RegisterIY => { 15 },
                    _ => { 11 },
                }
            },

            ZI::Pop(op) => {
                // POP qq       p.119
                // POP IX       p.121
                // POP IY       p.122

                // qqH <- (SP+1)
                // qqL <- (SP)
                
                // copy from top of stack
                let word = self.bus.cpu_read_u16(self.registers.sp);
                // increment stack pointer
                self.registers.sp += 2;
                // copy to register
                match op {
                    ZIL::RegisterBC => { self.registers.set_BC(word) }
                    ZIL::RegisterDE => { self.registers.set_DE(word) }
                    ZIL::RegisterHL => { self.registers.set_HL(word) }
                    ZIL::RegisterAF => { self.registers.set_AF(word) }
                    ZIL::RegisterIX => { self.registers.ix = word }
                    ZIL::RegisterIY => { self.registers.iy = word }
                  _ => { panic!("unhandled operand: {}", op.to_string()) }
                };

                // T-states
                match op {
                    ZIL::RegisterIX => { 14 },
                    ZIL::RegisterIY => { 14 },
                    _ => { 10 },
                }
            },

            ZI::Exchange(opa, opb) => {
                // EX DE, HL        p.124
                // EX AF, AF'       p.125
                // EX (SP), HL      p.127
                // EX (SP), IX      p.128
                // EX (SP), IY      p.129

                // exchange both operands values
                let wa = self.read_EX_operand(opa);
                let wb = self.read_EX_operand(opb);
                self.write_EX_operand(opa, wb);
                self.write_EX_operand(opb, wa);

                // T-states
                match opa {
                    ZIL::RegisterIndirectSP => { 19 },
                    _ => { 4 },
                }
            },

            ZI::ExchangeX => {
                // EXX      p.126
                // BC <-> BC'
                // DE <-> DE'
                // HL <-> HL'
                
                let r = &mut self.registers;
                let ar = &mut self.alternate_registers;
                mem::swap(&mut r.b, &mut ar.b);
                mem::swap(&mut r.c, &mut ar.c);
                mem::swap(&mut r.d, &mut ar.d);
                mem::swap(&mut r.e, &mut ar.e);
                mem::swap(&mut r.h, &mut ar.h);
                mem::swap(&mut r.l, &mut ar.l);

                // T-states
                4
            },

            ZI::Add(opv) => {
                // ADD A,r  p.145

                // unpack value to compare from operand
                let value = self.read_AND_operand(opv);

                // ignore carry
                self.registers.flags.remove(ZSF::C);
                // perform addition
                self.registers.a =
                    Z80::add8_with_carry(self.registers.a, value, &mut self.registers.flags);

                
                self.registers.flags.set(ZSF::N, false);

                // T-states
                match opv {
                    ZIL::RegisterIndirectHL => { 7 },
                    ZIL::IndexedIX(_) => { 19 },
                    ZIL::IndexedIY(_) => { 19 },
                    _ => { 4 },
                }
            },

            ZI::AddCarry(opv) => {
                // ADC A,s  p.146
                
                // unpack value to compare from operand
                let value = self.read_AND_operand(opv);

                // perform addition
                self.registers.a =
                    Z80::add8_with_carry(self.registers.a, value, &mut self.registers.flags);

                self.registers.flags.set(ZSF::N, false);

                // T-states
                match opv {
                    ZIL::RegisterIndirectHL => { 7 },
                    ZIL::IndexedIX(_) => { 19 },
                    ZIL::IndexedIY(_) => { 19 },
                    _ => { 4 },
                }
            },

            ZI::Sub(opv) => {
                // SUB s    p.153
                
                // unpack value to compare from operand
                let value = self.read_AND_operand(opv);

                // SUB can be performed with ADD:
                // a - b - c = a + ~b + 1 - c = a + ~b + !c
                // toggle carry (Sub does not take carry into account)
                self.registers.flags.insert(ZSF::C);
                // perform subtraction
                self.registers.a =
                    Z80::add8_with_carry(self.registers.a, !value, &mut self.registers.flags);
                // toggle carries
                self.registers.flags.toggle(ZSF::C);
                self.registers.flags.toggle(ZSF::H);

                self.registers.flags.set(ZSF::N, true);

                // T-states
                match opv {
                    ZIL::RegisterIndirectHL => { 7 },
                    ZIL::IndexedIX(_) => { 19 },
                    ZIL::IndexedIY(_) => { 19 },
                    _ => { 4 },
                }
            },

            ZI::SubCarry(opv) => {
                // SBC s    p.150
                
                // unpack value to compare from operand
                let value = self.read_AND_operand(opv);

                // SUB can be performed with ADD:
                // a - b - c = a + ~b + 1 - c = a + ~b + !c
                // toggle carry
                self.registers.flags.toggle(ZSF::C);
                // perform subtraction
                self.registers.a =
                    Z80::add8_with_carry(self.registers.a, !value, &mut self.registers.flags);
                // toggle carries
                self.registers.flags.toggle(ZSF::C);
                self.registers.flags.toggle(ZSF::H);

                self.registers.flags.set(ZSF::N, true);

                // T-states
                match opv {
                    ZIL::RegisterIndirectHL => { 7 },
                    ZIL::IndexedIX(_) => { 19 },
                    ZIL::IndexedIY(_) => { 19 },
                    _ => { 4 },
                }
            },

            ZI::Compare(opv) => {
                // CP s     p.163
                
                // NDJD: CP seems to behave like SUB except it
                // does not update accumulator with result
                // only flags are updated

                // unpack value to compare from operand
                let value = self.read_AND_operand(opv);

                // SUB can be performed with ADD:
                // a - b - c = a + ~b + 1 - c = a + ~b + !c
                // toggle carry (Sub does not take carry into account)
                self.registers.flags.insert(ZSF::C);
                // perform subtraction without setting accumulator
                Z80::add8_with_carry(self.registers.a, !value, &mut self.registers.flags);
                // toggle carry
                self.registers.flags.toggle(ZSF::C);
                self.registers.flags.toggle(ZSF::H);

                self.registers.flags.set(ZSF::N, true);

                // T-states
                match opv {
                    ZIL::RegisterIndirectHL => { 7 },
                    ZIL::IndexedIX(_) => { 19 },
                    ZIL::IndexedIY(_) => { 19 },
                    _ => { 4 },
                }
            },

            ZI::Add16(oplhs, oprhs) => {
                // ADD HL, ss   p.188
                // ADD IX, pp
                // ADD IY, rr

                // unpack rhs operand
                let rhs = match oprhs {
                    ZIL::RegisterBC => { self.registers.BC() }
                    ZIL::RegisterDE => { self.registers.DE() }
                    ZIL::RegisterHL => { self.registers.HL() }
                    ZIL::RegisterSP => { self.registers.sp }
                    ZIL::RegisterIX => { self.registers.ix }
                    ZIL::RegisterIY => { self.registers.iy }
                  _ => { panic!("unhandled operand: {}", oprhs.to_string()) }
                };

                // unpack lhs operand
                let lhs = match oplhs {
                    ZIL::RegisterHL => { self.registers.HL() }
                    ZIL::RegisterIX => { self.registers.ix }
                    ZIL::RegisterIY => { self.registers.iy }
                  _ => { panic!("unhandled operand: {}", oplhs.to_string()) }
                };
                
                // S, Z and P/V flags are preserved by ADD
                let flag_s = self.registers.flags.contains(ZSF::S);
                let flag_z = self.registers.flags.contains(ZSF::Z);
                let flag_pv = self.registers.flags.contains(ZSF::PV);

                // clear CARRY flag
                self.registers.flags.set(ZSF::C, false);
                let temp = Z80::add16_with_carry(lhs, rhs, &mut self.registers.flags);

                // assign value to lhs operand
                let lhs = match oplhs {
                    ZIL::RegisterHL => { self.registers.set_HL(temp) }
                    ZIL::RegisterIX => { self.registers.ix = temp }
                    ZIL::RegisterIY => { self.registers.iy = temp }
                  _ => { panic!("unhandled operand: {}", oplhs.to_string()) }
                };

                // update status flags
                // add/sub
                self.registers.flags.set(ZSF::N, false);

                // restore flags
                self.registers.flags.set(ZSF::S, flag_s);
                self.registers.flags.set(ZSF::Z, flag_z);
                self.registers.flags.set(ZSF::PV, flag_pv);

                // T-states
                match oplhs {
                    ZIL::RegisterHL => { 11 },
                    ZIL::RegisterIX => { 15 },
                    ZIL::RegisterIY => { 15 },
                    _ => { 0 },
                }
            },

            ZI::Add16Carry(oplhs, oprhs) => {
                // ADC HL, ss   p.180

                // unpack rhs operand
                let rhs = match oprhs {
                    ZIL::RegisterBC => { self.registers.BC() }
                    ZIL::RegisterDE => { self.registers.DE() }
                    ZIL::RegisterHL => { self.registers.HL() }
                    ZIL::RegisterSP => { self.registers.sp }
                  _ => { panic!("unhandled operand: {}", oprhs.to_string()) }
                };

                // unpack lhs operand
                let lhs = match oplhs {
                    ZIL::RegisterHL => { self.registers.HL() }
                  _ => { panic!("unhandled operand: {}", oplhs.to_string()) }
                };
                
                let temp = Z80::add16_with_carry(lhs, rhs, &mut self.registers.flags);

                // assign value to lhs operand
                let lhs = match oplhs {
                    ZIL::RegisterHL => { self.registers.set_HL(temp) }
                    ZIL::RegisterIX => { self.registers.ix = temp }
                    ZIL::RegisterIY => { self.registers.iy = temp }
                  _ => { panic!("unhandled operand: {}", oplhs.to_string()) }
                };

                // update status flags
                // add/sub
                self.registers.flags.set(ZSF::N, false);

                // T-states
                15
            },

            ZI::Sub16Carry(oplhs, oprhs) => {
                // SBC HL, ss   p.192
                
                // HL <- HL - ss - CY

                // unpack rhs operand
                let rhs = match oprhs {
                    ZIL::RegisterBC => { self.registers.BC() }
                    ZIL::RegisterDE => { self.registers.DE() }
                    ZIL::RegisterHL => { self.registers.HL() }
                    ZIL::RegisterSP => { self.registers.sp }
                  _ => { panic!("unhandled operand: {}", oprhs.to_string()) }
                };

                let lhs = self.registers.HL();
                
                self.registers.flags.toggle(ZSF::C);
                let temp = Z80::add16_with_carry(lhs, !rhs, &mut self.registers.flags);
                self.registers.flags.toggle(ZSF::C);
                self.registers.flags.toggle(ZSF::H);

                // assign result as u16 
                self.registers.set_HL(temp);

                // update status flags
                // add/sub
                self.registers.flags.set(ZSF::N, true);

                // T-states
                15
            },
            
            ZI::ComplementAccumulator => {
                // CPL      p.168
                //
                // A <- ~A
                
                // bitwise not on register A
                self.registers.a = !self.registers.a;

                self.registers.flags.set(ZSF::H, true);
                self.registers.flags.set(ZSF::N, true);

                // T-states
                4
            },

            ZI::ComplementCarryFlag => {
                // CCF      p.170
                //
                // CY <- /CY

                // status register carry flag is inverted
                self.registers.flags.toggle(ZSF::C);

                // T-states
                4
            },

            ZI::SetCarryFlag => {
                // SCF      p.171
                //
                // CY <- 1

                // status register carry flag is set
                self.registers.flags.insert(ZSF::C);

                // T-states
                4
            },

            ZI::DecimalAdujstAccumulator => {
                // DAA      p.167
                //
                // adjust accumulator for BCD addition and substratction operation
                
                // (from http://z80-heaven.wikidot.com/instructions-set:daa)
                // When this instruction is executed, the A register is BCD 
                // corrected using the contents of the flags. The exact 
                // process is the following: if the least significant four bits
                // of A contain a non-BCD digit (i. e. it is greater than 9)
                // or the H flag is set, then $06 is added to the register.
                // Then the four most significant bits are checked.
                // If this more significant digit also happens to be 
                // greater than 9 or the C flag is set, then $60 is added.

                let a = &mut self.registers.a;

                // least significant bits (0-3)
                if ((*a & 0x0f) > 9) || self.registers.flags.contains(ZSF::H) {
                    *a += 0x06;
                }

                // most significant bits (7-4)
                if((*a & 0xf0) > 0x90) || self.registers.flags.contains(ZSF::C) {
                    *a += 0x60;

                    // If the second addition was needed, the C flag is set after execution,
                    // otherwise it is reset. 
                    self.registers.flags.set(ZSF::C, true);
                }
                
                // The N flag is preserved, P/V is parity and the others are altered by definition.
                self.registers.flags.set(ZSF::PV, Z80::compute_u8_parity(*a));
                self.registers.flags.set(ZSF::Z, *a == 0);
                self.registers.flags.set(ZSF::S, *a & 0x80 != 0);

                // T-states
                4
            },

            ZI::NegateAccumulator => {
                // NEG      p.176
            
                // A <- 0 - A

                // P/V is set if accumulator was 80h before operation
                self.registers.flags.set(ZSF::PV, self.registers.a == 0x80);
                // C is set if accumulator was NOT 00h before operation
                self.registers.flags.set(ZSF::C, self.registers.a != 0x00);

                // negate accumulator
                let mut temp = self.registers.a as i8;
                temp = -temp;
                self.registers.a = temp as u8;

                // update status flags
                // add/sub
                self.registers.flags.set(ZSF::N, true);
                // sign
                self.registers.flags.set(ZSF::S, temp < 0);
                // zero
                self.registers.flags.set(ZSF::Z, temp == 0);
                // half-carry
                self.registers.flags.set(ZSF::H, false);

                // T-states
                4
            },

            ZI::Increment(op) => {
                // INC r    p.165
                // ...

                // preserve carry flag
                let carry = self.registers.flags.contains(ZSF::C);

                self.registers.flags.set(ZSF::C, false);
                self.increment8(op, 1);

                // add/sub 
                self.registers.flags.set(ZSF::N, false);
                // recover carry flag
                self.registers.flags.set(ZSF::C, carry);

                // T-states
                match op {
                    ZIL::RegisterIndirectHL => { 11 },
                    ZIL::IndexedIX(_) => { 23 },
                    ZIL::IndexedIY(_) => { 23 },
                    _ => { 4 },
                }
            },

            ZI::Increment16(op) => {
                // INC ss   p.198
                // ...
                self.increment16(op, 1);

                // T-states
                match op {
                    ZIL::RegisterIX => { 10 },
                    ZIL::RegisterIY => { 10 },
                    _ => { 6 },
                }
            },

            ZI::Decrement(op) => {
                // DEC r    p.170
                // ...
                
                // preserve carry flag
                let carry = self.registers.flags.contains(ZSF::C);

                self.registers.flags.set(ZSF::C, false);
                self.decrement8(op, 1);

                // add/sub 
                self.registers.flags.set(ZSF::N, false);
                // recover carry flag
                self.registers.flags.set(ZSF::C, carry);

                // T-states
                match op {
                    ZIL::RegisterIndirectHL => { 11 },
                    ZIL::IndexedIX(_) => { 23 },
                    ZIL::IndexedIY(_) => { 23 },
                    _ => { 4 },
                }
            },

            ZI::Decrement16(op) => {
                // DEC ss   p.201
                // ...
                self.increment16(op, -1);

                // T-states
                match op {
                    ZIL::RegisterIX => { 10 },
                    ZIL::RegisterIY => { 10 },
                    _ => { 6 },
                }
            },

            ZI::And(opv) => {
                // AND s     p.157
                
                // unpack value to compare from operand
                let value = self.read_AND_operand(opv);

                let r = self.registers.a & value;
                self.registers.a = r;

                // update status flags
                // add/sub
                self.registers.flags.set(ZSF::N, false);
                // sign
                self.registers.flags.set(ZSF::S, (r as i8) < 0);
                // zero
                self.registers.flags.set(ZSF::Z, r == 0);
                // overflow
                self.registers.flags.set(ZSF::PV, Z80::compute_u8_parity(r));
                // carry
                self.registers.flags.set(ZSF::C, false);
                // half-carry
                self.registers.flags.set(ZSF::H, true);

                // T-states
                match opv {
                    ZIL::RegisterIndirectHL => { 7 },
                    ZIL::IndexedIX(_) => { 19 },
                    ZIL::IndexedIY(_) => { 19 },
                    _ => { 4 },
                }
            },

            ZI::Or(opv) => {
                // OR s     p.159
                
                // unpack value to compare from operand
                let value = self.read_AND_operand(opv);

                let r = self.registers.a | value;
                self.registers.a = r;

                // update status flags
                // add/sub
                self.registers.flags.set(ZSF::N, false);
                // sign
                self.registers.flags.set(ZSF::S, (r as i8) < 0);
                // zero
                self.registers.flags.set(ZSF::Z, r == 0);
                // overflow
                self.registers.flags.set(ZSF::PV, Z80::compute_u8_parity(r));
                // carry
                self.registers.flags.set(ZSF::C, false);
                // half-carry
                self.registers.flags.set(ZSF::H, false);
                
                // T-states
                match opv {
                    ZIL::RegisterIndirectHL => { 7 },
                    ZIL::IndexedIX(_) => { 19 },
                    ZIL::IndexedIY(_) => { 19 },
                    _ => { 4 },
                }
            },

            ZI::Xor(opv) => {
                // XOR s     p.161
                
                // unpack value to compare from operand
                let value = self.read_AND_operand(opv);

                let r = self.registers.a ^ value;
                self.registers.a = r;

                // update status flags
                // add/sub
                self.registers.flags.set(ZSF::N, false);
                // sign
                self.registers.flags.set(ZSF::S, (r as i8) < 0);
                // zero
                self.registers.flags.set(ZSF::Z, r == 0);
                // overflow
                self.registers.flags.set(ZSF::PV, Z80::compute_u8_parity(r));
                // carry
                self.registers.flags.set(ZSF::C, false);
                // half-carry
                self.registers.flags.set(ZSF::H, false);

                // T-states
                match opv {
                    ZIL::RegisterIndirectHL => { 7 },
                    ZIL::IndexedIX(_) => { 19 },
                    ZIL::IndexedIY(_) => { 19 },
                    _ => { 4 },
                }
            },

            ZI::Bit(b, op) => {
                // BIT b,r          p.243
                // BIT b,(HL)       p.244
                // BIT b,(IX+d)     p.246
                // BIT b,(IY+d)     p.248

                // test bit b an set Z flag accordingly

                let byte = self.read_AND_operand(op);
                
                self.registers.flags.set(ZSF::Z, byte & (1<<b) == 0);
                self.registers.flags.set(ZSF::N, false);
                self.registers.flags.set(ZSF::H, true);

                // T-states
                match op {
                    ZIL::RegisterIndirectHL => { 12 },
                    ZIL::IndexedIX(_) => { 20 },
                    ZIL::IndexedIY(_) => { 20 },
                    _ => { 8 },
                }
            },

            ZI::Set(b, op) => {
                // SET b,r          p.251
                // SET b,(HL)       p.253
                // SET b,(IX+d)     p.255
                // SET b,(IY+d)     p.257

                // set bit b
                let mut byte = self.read_AND_operand(op);
                byte |= (1<<b);
                self.write_AND_operand(op, byte);

                // no condition bits are affected by this operation
                
                // T-states
                match op {
                    ZIL::RegisterIndirectHL => { 15 },
                    ZIL::IndexedIX(_) => { 23 },
                    ZIL::IndexedIY(_) => { 23 },
                    _ => { 8 },
                }
            },

            ZI::Reset(b, op) => {
                // RES b,m      p.259

                // clear bit b 
                let mut byte = self.read_AND_operand(op);
                byte &= !(1<<b);
                self.write_AND_operand(op, byte);

                // no condition bits are affected by this operation
                
                // T-states
                match op {
                    ZIL::RegisterIndirectHL => { 15 },
                    ZIL::IndexedIX(_) => { 23 },
                    ZIL::IndexedIY(_) => { 23 },
                    _ => { 8 },
                }
            }

            ZI::ShiftLeftArithmetic(opv) => {
                // SLA  p.230
                //

                // read byte pointed by operand
                let mut byte = self.read_AND_operand(opv);

                // store shifted bit in carry
                self.registers.flags.set(ZSF::C, (byte & 0x80) != 0);
                // shift left
                byte = byte << 1;

                // write byte back
                self.write_AND_operand(opv,byte);

                // update flags
                // add/sub
                self.registers.flags.set(ZSF::N, false);
                // sign
                self.registers.flags.set(ZSF::S, (byte as i8) < 0);
                // zero
                self.registers.flags.set(ZSF::Z, byte == 0);
                // overflow
                self.registers.flags.set(ZSF::PV, Z80::compute_u8_parity(byte));
                // half-carry
                self.registers.flags.set(ZSF::H, false);

                // T-states
                match opv {
                    ZIL::RegisterIndirectHL => { 15 },
                    ZIL::IndexedIX(_) => { 23 },
                    ZIL::IndexedIY(_) => { 23 },
                    _ => { 8 },
                }
            },

            ZI::ShiftRightArithmetic(opv) => {
                // SRA  p.233
                //

                // read byte pointed by operand
                let mut byte = self.read_AND_operand(opv);

                // bit 7 will be preserved by this shift
                let bit7 = byte & 0x80;
                // store shifted bit in carry
                self.registers.flags.set(ZSF::C, (byte & 0x01) != 0);
                // shift right
                byte = byte >> 1;
                // restore bit 7
                byte = byte | bit7;

                // write byte back
                self.write_AND_operand(opv,byte);

                // update flags
                // add/sub
                self.registers.flags.set(ZSF::N, false);
                // sign
                self.registers.flags.set(ZSF::S, (byte as i8) < 0);
                // zero
                self.registers.flags.set(ZSF::Z, byte == 0);
                // overflow
                self.registers.flags.set(ZSF::PV, Z80::compute_u8_parity(byte));
                // half-carry
                self.registers.flags.set(ZSF::H, false);

                // T-states
                match opv {
                    ZIL::RegisterIndirectHL => { 15 },
                    ZIL::IndexedIX(_) => { 23 },
                    ZIL::IndexedIY(_) => { 23 },
                    _ => { 8 },
                }
            },

            ZI::ShiftRightLogic(opv) => {
                // SRL  p.236
                //

                // read byte pointed by operand
                let mut byte = self.read_AND_operand(opv);

                // store shifted bit in carry
                self.registers.flags.set(ZSF::C, (byte & 0x01) != 0);
                // shift right
                byte = byte >> 1;

                // write byte back
                self.write_AND_operand(opv,byte);

                // update flags
                // add/sub
                self.registers.flags.set(ZSF::N, false);
                // sign
                self.registers.flags.set(ZSF::S, (byte as i8) < 0);
                // zero
                self.registers.flags.set(ZSF::Z, byte == 0);
                // overflow
                self.registers.flags.set(ZSF::PV, Z80::compute_u8_parity(byte));
                // half-carry
                self.registers.flags.set(ZSF::H, false);

                // T-states
                match opv {
                    ZIL::RegisterIndirectHL => { 15 },
                    ZIL::IndexedIX(_) => { 23 },
                    ZIL::IndexedIY(_) => { 23 },
                    _ => { 8 },
                }
            },

            ZI::RotateLeftCarry(opv) => {
                // RLC  p.213
                // ...

                // read byte pointed by operand
                let mut byte = self.read_AND_operand(opv);

                // bit 7 will be shoved back to bit 0 after shift
                let bit7 = byte & 0x80;
                // store rotated bit in carry
                self.registers.flags.set(ZSF::C, bit7 != 0);
                // shift left
                byte = byte << 1;
                // shove rotated bit in position 0
                byte = byte | if bit7 == 0 { 0x00 } else { 0x01 };

                // write byte back
                self.write_AND_operand(opv,byte);

                // update flags
                // add/sub
                self.registers.flags.set(ZSF::N, false);
                // sign
                self.registers.flags.set(ZSF::S, (byte as i8) < 0);
                // zero
                self.registers.flags.set(ZSF::Z, byte == 0);
                // overflow
                self.registers.flags.set(ZSF::PV, Z80::compute_u8_parity(byte));
                // half-carry
                self.registers.flags.set(ZSF::H, false);

                // T-states
                match opv {
                    ZIL::RegisterIndirectHL => { 15 },
                    ZIL::IndexedIX(_) => { 23 },
                    ZIL::IndexedIY(_) => { 23 },
                    _ => { 8 },
                }
            }

            ZI::RotateLeft(opv) => {
                // RL  p.221
                // ...

                // read byte pointed by operand
                let mut byte = self.read_AND_operand(opv);

                // carry will be pushed to bit 0
                let carry = self.registers.flags.contains(ZSF::C);
                // bit 7 will be pushed to carry after shift
                self.registers.flags.set(ZSF::C, (byte & 0x80) != 0);
                // shift left
                byte = byte << 1;
                // shove rotated bit in position 0
                byte = byte | if carry { 0x01 } else { 0x00 };

                // write byte back
                self.write_AND_operand(opv,byte);

                // update flags
                // add/sub
                self.registers.flags.set(ZSF::N, false);
                // sign
                self.registers.flags.set(ZSF::S, (byte as i8) < 0);
                // zero
                self.registers.flags.set(ZSF::Z, byte == 0);
                // overflow
                self.registers.flags.set(ZSF::PV, Z80::compute_u8_parity(byte));
                // half-carry
                self.registers.flags.set(ZSF::H, false);

                // T-states
                match opv {
                    ZIL::RegisterIndirectHL => { 15 },
                    ZIL::IndexedIX(_) => { 23 },
                    ZIL::IndexedIY(_) => { 23 },
                    _ => { 8 },
                }
            },

            ZI::RotateRightCarry(opv) => {
                // RRC  p.224
                // ...

                // read byte pointed by operand
                let mut byte = self.read_AND_operand(opv);

                // bit 0 will be shoved back to bit 7 after shift
                let bit0 = byte & 0x01;
                // store rotated bit in carry
                self.registers.flags.set(ZSF::C, bit0 != 0);
                // shift right
                byte = byte >> 1;
                // shove rotated bit in position 7
                byte = byte | if bit0 == 0 { 0x00 } else { 0x80 };

                // write byte back
                self.write_AND_operand(opv,byte);

                // update flags
                // add/sub
                self.registers.flags.set(ZSF::N, false);
                // sign
                self.registers.flags.set(ZSF::S, (byte as i8) < 0);
                // zero
                self.registers.flags.set(ZSF::Z, byte == 0);
                // overflow
                self.registers.flags.set(ZSF::PV, Z80::compute_u8_parity(byte));
                // half-carry
                self.registers.flags.set(ZSF::H, false);

                // T-states
                match opv {
                    ZIL::RegisterIndirectHL => { 15 },
                    ZIL::IndexedIX(_) => { 23 },
                    ZIL::IndexedIY(_) => { 23 },
                    _ => { 8 },
                }
            }

            ZI::RotateRight(opv) => {
                // RR  p.227
                // ...

                // read byte pointed by operand
                let mut byte = self.read_AND_operand(opv);

                // carry will be pushed to bit 7
                let carry = self.registers.flags.contains(ZSF::C);
                // bit 0 will be pushed to carry after shift
                self.registers.flags.set(ZSF::C, (byte & 0x01) != 0);
                // shift right
                byte = byte >> 1;
                // shove rotated bit in position 7
                byte = byte | if carry { 0x80 } else { 0x00 };

                // write byte back
                self.write_AND_operand(opv,byte);

                // update flags
                // add/sub
                self.registers.flags.set(ZSF::N, false);
                // sign
                self.registers.flags.set(ZSF::S, (byte as i8) < 0);
                // zero
                self.registers.flags.set(ZSF::Z, byte == 0);
                // overflow
                self.registers.flags.set(ZSF::PV, Z80::compute_u8_parity(byte));
                // half-carry
                self.registers.flags.set(ZSF::H, false);

                // T-states
                match opv {
                    ZIL::RegisterIndirectHL => { 15 },
                    ZIL::IndexedIX(_) => { 23 },
                    ZIL::IndexedIY(_) => { 23 },
                    _ => { 8 },
                }
            },

            ZI::LoadIncrement => {
                self.load_increment(1);

                // T-states
                16
            },

            ZI::LoadDecrement => {
                self.load_increment(-1);

                // T-states
                16
            },

            ZI::LoadIncrementRepeat => {
                // LDIR     p.132

                let mut states = 0;

                // execute LDI while BC != 0
                loop {
                    self.load_increment(1);
                    if(self.registers.BC() == 0) {
                        states += 16;
                        break;
                    }
                    states += 21;
                }

                // T-states
                states
            },

            ZI::LoadDecrementRepeat => {
                // LDDR     p.132

                let mut states = 0;

                // execute LDR while BC != 0
                loop {
                    self.load_increment(-1);
                    if(self.registers.BC() == 0) {
                        states += 16;
                        break;
                    }
                    states += 21;
                }
                
                // T-states
                states
            },

            ZI::Load(dst, src) => {
                // LD *,*   p.71 (8 bits load operations)
                
                // unpack value to load
                let value = self.unpack_LD_operand(src);

                match dst {
                    ZIL::Indirect16(addr) => { self.bus.cpu_write(addr,value) },
                    ZIL::RegisterA => { self.registers.a = value},
                    ZIL::RegisterB => { self.registers.b = value},
                    ZIL::RegisterC => { self.registers.c = value},
                    ZIL::RegisterD => { self.registers.d = value},
                    ZIL::RegisterE => { self.registers.e = value},
                    ZIL::RegisterH => { self.registers.h = value},
                    ZIL::RegisterL => { self.registers.l = value},
                    ZIL::RegisterR => {
                        println!("register R not implemented");
                    },
                    ZIL::RegisterIndirectHL => {
                        // write byte at address pointed by register HL
                        self.bus.cpu_write(self.registers.HL(), value)
                    },
                    ZIL::RegisterIndirectDE => {
                        // write byte at address pointed by register DE
                        self.bus.cpu_write(self.registers.DE(), value)
                    },
                    ZIL::RegisterIndirectBC => {
                        // write byte at address pointed by register BC
                        self.bus.cpu_write(self.registers.BC(), value)
                    },
                    ZIL::IndexedIX(d) => {
                        self.bus.cpu_write(
                            Z80::add_signed_u8_to_u16(self.registers.ix, d),
                            value
                        )
                    },
                    ZIL::IndexedIY(d) => {
                        self.bus.cpu_write(
                            Z80::add_signed_u8_to_u16(self.registers.iy, d),
                            value
                        )
                    },
                    _ => { panic!("unhandled operand: {}", dst.to_string()) }
                }

                // T-states
                match dst {
                    ZIL::RegisterA => match src {
                        ZIL::Indirect16(_) => { 13 },
                        ZIL::RegisterIndirectBC => { 7 },
                        _ => { 7 },
                    },
                    ZIL::Indirect16(_) => { 13 },
                    ZIL::RegisterIndirectBC => { 7 },
                    ZIL::RegisterIndirectDE => { 7 },
                    _ => match src {
                        ZIL::RegisterIndirectHL => { 7 },
                        ZIL::IndexedIX(_) => { 19 },
                        ZIL::IndexedIY(_) => { 19 },
                        _ => { 4 },
                    }
                }
            },

            ZI::Load16(dst, src) => {
                // LD *,*   p.99 (16 bits load operations)

                // unpack value to load
                let value = match src {
                    ZIL::Immediate16(w)     => { w },
                    ZIL::Indirect16(addr)   => { self.bus.cpu_read_u16(addr) },
                    ZIL::RegisterBC => { self.registers.BC() },
                    ZIL::RegisterDE => { self.registers.DE() },
                    ZIL::RegisterHL => { self.registers.HL() },
                    ZIL::RegisterSP => { self.registers.sp },
                    ZIL::RegisterIX => { self.registers.ix },
                    ZIL::RegisterIY => { self.registers.iy },

                    _ => { panic!("unhandled operand: {}", src.to_string()) }
                };

                match dst {
                    ZIL::Indirect16(addr) => { self.bus.cpu_write_u16(addr, value) },
                    ZIL::RegisterBC => { self.registers.set_BC(value) },
                    ZIL::RegisterDE => { self.registers.set_DE(value) },
                    ZIL::RegisterHL => { self.registers.set_HL(value) },
                    ZIL::RegisterSP => { self.registers.sp = value },
                    ZIL::RegisterIX => { self.registers.ix = value },
                    ZIL::RegisterIY => { self.registers.iy = value },
                    _ => { panic!("unhandled operand: {}", dst.to_string()) }
                }

                // T-states
                match dst {
                    ZIL::Indirect16(_) => { 20 },
                    ZIL::RegisterIX => match src {
                        ZIL::Indirect16(_) => { 20 },
                        _ => { 14 },
                    },
                    ZIL::RegisterIY => match src {
                        ZIL::Indirect16(_) => { 20 },
                        _ => { 14 },
                    },
                    ZIL::RegisterSP => match src {
                        ZIL::RegisterHL => { 6 },
                        _ => { 10 },
                    },

                    _ => { 10 },
                }
            },

            ZI::In(dst,src) => {
                // IN A,(n)     p.295
                // IN r,(C)     p.295

                let addr = match src {
                    ZIL::Indirect(b) => b,
                    ZIL::RegisterA => self.registers.a,
                    _ => { panic!("unhandled operand: {}", src.to_string()) }
                };

                // fetch byte on IO bus
                let byte = self.bus.io_read(addr);

                // store byte
                match dst {
                    ZIL::RegisterA => { self.registers.a = byte; },
                    ZIL::RegisterB => { self.registers.b = byte; },
                    ZIL::RegisterC => { self.registers.c = byte; },
                    ZIL::RegisterD => { self.registers.d = byte; },
                    ZIL::RegisterE => { self.registers.e = byte; },
                    ZIL::RegisterH => { self.registers.h = byte; },
                    ZIL::RegisterL => { self.registers.l = byte; },
                    _ => { panic!("unhandled operand: {}", dst.to_string()) }
                }

                // T-states
                match dst {
                    ZIL::RegisterA => { 11 },
                    _ => { 12 },
                }
            },

            ZI::Out(dst,src) => {
                // OUT (n), A   p.306
                // OUT (C), A   p.307

                let addr = match dst {
                    ZIL::Indirect(b) => b,
                    ZIL::RegisterIndirectA => self.bus.io_read(self.registers.a),
                    ZIL::RegisterIndirectB => self.bus.io_read(self.registers.b),
                    ZIL::RegisterIndirectC => self.bus.io_read(self.registers.c),
                    ZIL::RegisterIndirectD => self.bus.io_read(self.registers.d),
                    ZIL::RegisterIndirectE => self.bus.io_read(self.registers.e),
                    ZIL::RegisterIndirectH => self.bus.io_read(self.registers.h),
                    ZIL::RegisterIndirectL => self.bus.io_read(self.registers.l),
                    _ => { panic!("unhandled operand: {}", dst.to_string()) }
                };
                
                let byte = match src {
                    ZIL::RegisterA => { self.registers.a },
                    _ => { panic!("unhandled operand: {}", src.to_string()) }
                };

                // write byte on IO bus
                self.bus.io_write(addr, byte);

                // T-states
                match dst {
                    ZIL::RegisterA => { 11 },
                    _ => { 12 },
                }
            },

            ZI::OutIncrement => {
                // OUTI     p.309
                
                self.out_increment();

                // T-states
                16
            },

            ZI::OutIncrementRepeat => {
                // OTIR    p.311
                
                let mut states = 0;

                loop {
                    self.out_increment();
                    if(self.registers.b == 0) {
                        states += 16;
                        break;
                    }
                    states += 21;
                }

                // T-states
                states
            },

            _ => {
                panic!("unhandled instruction: {:?}", ins);
            },
        }
    }


}
