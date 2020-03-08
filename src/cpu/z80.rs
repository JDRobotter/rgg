
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
        format!("A {:02x} |B {:02x} |D {:02x} |H {:02x} \
                          |C {:02x} |E {:02x} |L {:02x} \
                |PC {:04x} |SP {:04x} |IX {:04x} |IY {:04x} \
                {}",
                self.a, self.b , self.d , self.h,
                         self.c , self.e , self.l,
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
    // CPU intruction decoder
    decoder: Z80InstructionDecoder,
}

impl Z80 {
    pub fn new(rom:Rom) -> Z80 {
        Z80 {
            bus: SystemBus::new(rom),
            
            interrupt_enabled: false,
            interrupt_mode: 0,

            registers: Z80Registers::new(),
            decoder: Z80InstructionDecoder::new(),
        }
    }

    /// Generate an hardware interrupt on CPU
    pub fn interrupt(&mut self) {

        // nothing to do if interrupts are disabled
        if !self.interrupt_enabled {
            return
        }

    }

    pub fn step(&mut self) {

        let base_pc = self.registers.pc;

        loop {
            // fetch one byte from program counter + displacement e
            let opb = self.bus.cpu_read(self.registers.pc);
            // increment PC
            self.registers.pc += 1;

            // feed byte to instruction decoder
            match self.decoder.push(opb) {
                Some(ins) => {

                    // execute decoded instruction
                    self.execute_instruction(ins);

                    // show decoded instruction and registers state
                    println!("{:04x}: {:16} {}",
                        base_pc,
                        ins.to_string(),
                        self.registers.to_string());

                    break;
                },

                None => {
                    // nothing to do
                },
            };

        }//loop
    }

    pub fn debug_cpu_registers(&mut self) {
        println!("{}", self.registers.to_string())
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
        
        parity
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

    /// Perform an INC / DEC instruction incrementing by e
    fn increment(&mut self,
                    op: Z80InstructionLocation,
                    e: i8) {
        // INC r    p.165
        // ...
        // INC ss   p.198
        // ...
        // DEC r    p.170
        // ...
        // DEC ss   p.201
        // ...
        match(op) {
            ZIL::RegisterA => { 
                self.registers.a = Z80::add_i8_to_u8(self.registers.a, e) 
            },
            ZIL::RegisterB => {
                self.registers.b = Z80::add_i8_to_u8(self.registers.b, e)
            },
            ZIL::RegisterC => {
                self.registers.c = Z80::add_i8_to_u8(self.registers.c, e)
            },
            ZIL::RegisterD => {
                self.registers.d = Z80::add_i8_to_u8(self.registers.d, e)
            },
            ZIL::RegisterE => {
                self.registers.e = Z80::add_i8_to_u8(self.registers.e, e)
            },
            ZIL::RegisterH => {
                self.registers.h = Z80::add_i8_to_u8(self.registers.h, e)
            },
            ZIL::RegisterL => {
                self.registers.l = Z80::add_i8_to_u8(self.registers.l, e)
            },
            ZIL::RegisterBC => {
                self.registers.set_BC(Z80::add_i8_to_u16(self.registers.BC(), e))
            }
            ZIL::RegisterDE => {
                self.registers.set_DE(Z80::add_i8_to_u16(self.registers.DE(), e))
            }
            ZIL::RegisterHL => {
                self.registers.set_HL(Z80::add_i8_to_u16(self.registers.HL(), e))
            }
            ZIL::RegisterSP => {
                self.registers.sp = Z80::add_i8_to_u16(self.registers.sp, e)
            }
            ZIL::RegisterIndirectHL => {
                // fetch byte on bus at address pointed by register HL
                // and write incremented version
                let byte = self.bus.cpu_read(self.registers.HL());
                self.bus.cpu_write(self.registers.HL(), Z80::add_i8_to_u8(byte, e));
            },
            ZIL::IndexedIX(d) => {
                // fetch byte on bus at address pointed by register IX + d
                // and write incremented value
                let addr = Z80::add_signed_u8_to_u16(self.registers.ix, d);
                let byte = self.bus.cpu_read(addr);
                self.bus.cpu_write(addr, Z80::add_i8_to_u8(byte, e));
            },
            ZIL::IndexedIY(d) => {
                // fetch byte on bus at address pointed by register IY + d
                // and write incremented value
                let addr = Z80::add_signed_u8_to_u16(self.registers.ix, d);
                let byte = self.bus.cpu_read(addr);
                self.bus.cpu_write(addr, Z80::add_i8_to_u8(byte, e));
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

    pub fn execute_instruction(&mut self,
                                ins: Z80Instruction) {

        match ins {
            ZI::NOP => {
                // nothing to do
            },

            ZI::DisableInt => {
                // DI p.182
                self.interrupt_enabled = false;
            },
            ZI::EnableInt => {
                // EI p.183
                self.interrupt_enabled = true;
            },
            ZI::SetINTMode0 => {
                // IM p.184
                self.interrupt_mode = 0;
            },
            ZI::SetINTMode1 => {
                // IM p.185
                self.interrupt_mode = 1;
            },
            ZI::SetINTMode2 => {
                // IM p.186
                self.interrupt_mode = 2;
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

            }

            ZI::Add(opv) => {
                // ADD A,r  p.145

                // unpack value to compare from operand
                let value = self.read_AND_operand(opv);
                
                // use an i16 temporary accumulator
                let mut temp :i16 = self.registers.a as i8 as i16;
                // perform addition
                temp += value as i8 as i16;
                // assign result to 8 bit register
                self.registers.a = temp as u8;
                
                // update status flags
                // add/sub
                self.registers.flags.set(ZSF::N, false);
                // sign
                self.registers.flags.set(ZSF::S, temp < 0);
                // zero
                self.registers.flags.set(ZSF::Z, temp == 0);
                // overflow
                self.registers.flags.set(ZSF::PV, (temp < -128) || (127 < temp));
                // carry
                self.registers.flags.set(ZSF::C, temp > 127);
                // half-carry
                self.registers.flags.set(ZSF::H, false);
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
                
                // use an i32 temporary accumulator
                let mut temp :i32 = lhs as i16 as i32;
                // perform addition
                temp += rhs as i16 as i32;

                // assign result as u16 
                let utemp = temp as u16;
                // assign value to lhs operand
                let lhs = match oplhs {
                    ZIL::RegisterHL => { self.registers.set_HL(utemp) }
                    ZIL::RegisterIX => { self.registers.ix = utemp }
                    ZIL::RegisterIY => { self.registers.iy = utemp }
                  _ => { panic!("unhandled operand: {}", oplhs.to_string()) }
                };

                // update status flags
                // add/sub
                self.registers.flags.set(ZSF::N, false);
                // sign
                self.registers.flags.set(ZSF::S, temp < 0);
                // zero
                self.registers.flags.set(ZSF::Z, temp == 0);
                // overflow
                self.registers.flags.set(ZSF::PV, (temp < -128) || (127 < temp));
                // carry
                self.registers.flags.set(ZSF::C, temp > 127);
                // half-carry
                self.registers.flags.set(ZSF::H, false);


            },

            ZI::Sub(opv) => {
                // SUB s    p.153
                
                // unpack value to compare from operand
                let value = self.read_AND_operand(opv);
                
                // use an i16 temporary accumulator
                let mut temp :i16 = self.registers.a as i8 as i16;
                // perform substraction
                temp -= value as i8 as i16;
                // assign result to 8 bit register
                self.registers.a = temp as u8;

                // update status flags
                // add/sub
                self.registers.flags.set(ZSF::N, false);
                // sign
                self.registers.flags.set(ZSF::S, temp < 0);
                // zero
                self.registers.flags.set(ZSF::Z, temp == 0);
                // overflow
                self.registers.flags.set(ZSF::PV, (temp < -128) || (127 < temp));
                // carry
                self.registers.flags.set(ZSF::C, temp < -128);
                // half-carry
                self.registers.flags.set(ZSF::H, false);

            },

            ZI::Increment(op) => {
                // INC r    p.165
                // ...
                // INC ss   p.198
                // ...
                self.increment(op, 1)
            },

            ZI::Decrement(op) => {
                // DEC r    p.170
                // ...
                // DEC ss   p.201
                // ...
                self.increment(op, -1)
            },

            ZI::Compare(opv) => {
                // CP s     p.163
                
                // unpack value to compare from operand
                let value = self.read_AND_operand(opv);

                // NDJD: CP seems to behave like SUB except it
                // does not update accumulator with result
                // only flags are updated

                // use an i16 temporary accumulator
                let mut temp :i16 = self.registers.a as i8 as i16;
                // perform substraction
                temp -= value as i8 as i16;

                // update status flags
                // add/sub
                self.registers.flags.set(ZSF::N, true);
                // sign
                self.registers.flags.set(ZSF::S, temp < 0);
                // zero
                self.registers.flags.set(ZSF::Z, temp == 0);
                // overflow
                self.registers.flags.set(ZSF::PV, (temp < -128) || (127 < temp));
                // carry
                self.registers.flags.set(ZSF::C, temp < -128);
                // half-carry
                self.registers.flags.set(ZSF::H, false);
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
                self.registers.flags.set(ZSF::PV, false);
                // carry
                self.registers.flags.set(ZSF::C, false);
                // half-carry
                self.registers.flags.set(ZSF::H, false);
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
                self.registers.flags.set(ZSF::PV, false);
                // carry
                self.registers.flags.set(ZSF::C, false);
                // half-carry
                self.registers.flags.set(ZSF::H, false);
            },

            ZI::Bit(b, op) => {
                // BIT b,r          p.243
                // BIT b,(HL)       p.244
                // BIT b,(IX+d)     p.246
                // BIT b,(IY+d)     p.248

                // test bit b an set Z flag accordingly

                let byte = self.read_AND_operand(op);

                self.registers.flags.set(ZSF::Z, byte & (1<<b) != 0);
                self.registers.flags.set(ZSF::N, false);
                self.registers.flags.set(ZSF::H, true);

            },

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
                byte = byte & bit7;

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
                byte = byte & if bit7 == 0 { 0x00 } else { 0x01 };

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
                byte = byte & if carry { 0x01 } else { 0x00 };

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
                byte = byte & if bit0 == 0 { 0x00 } else { 0x80 };

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
                byte = byte & if carry { 0x80 } else { 0x00 };

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
            },

            ZI::LoadIncrement => {
                self.load_increment(1)
            },

            ZI::LoadDecrement => {
                self.load_increment(-1)
            },

            ZI::LoadIncrementRepeat => {
                // LDIR     p.132

                // execute LDI while BC != 0
                loop {
                    self.load_increment(1);
                    if(self.registers.BC() == 0) {
                        break;
                    }
                }
            },

            ZI::LoadDecrementRepeat => {
                // LDDR     p.132

                // execute LDR while BC != 0
                loop {
                    self.load_increment(-1);
                    if(self.registers.BC() == 0) {
                        break;
                    }
                }
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
            },

            ZI::Out(dst,src) => {
                // OUT (n), A   p.306
                // OUT (C), A   p.307

                let addr = match dst {
                    ZIL::Indirect(b) => b,
                    _ => { panic!("unhandled operand: {}", dst.to_string()) }
                };
                
                let byte = match src {
                    ZIL::RegisterA => { self.registers.a },
                    _ => { panic!("unhandled operand: {}", src.to_string()) }
                };

                // write byte on IO bus
                self.bus.io_write(addr, byte);
            },

            _ => {
                panic!("unhandled instruction: {:?}", ins);
            },
        }
    }


}