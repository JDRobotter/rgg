
use crate::cpu::Z80InstructionDecoder;
use crate::system::SystemBus;

use crate::cpu::Z80Instruction;
use Z80Instruction as ZI;

use crate::cpu::Z80InstructionLocation;
use Z80InstructionLocation as ZIL;

use crate::cpu::Z80JumpCondition;
use Z80JumpCondition as ZJC;

use std::ops;
use std::convert::From;

struct Z80StatusFlags {
    s:  bool, // sign flag
    z:  bool, // zero flag
    h:  bool, // half carry flag
    pv: bool, // parity / overflow flag
    n:  bool, // add / substract
    c:  bool, // carry flag
}

impl Z80StatusFlags {
    pub fn new() -> Z80StatusFlags {
        Z80StatusFlags {
            s: false,
            z: false,
            h: false,
            pv: false,
            n: false,
            c: false,
        }
    }

    pub fn to_string(&self) -> String {
        format!("{} |{} |{} |{} |{} |{}",
            if self.s {"S"} else {" "},
            if self.z {"Z"} else {" "},
            if self.h {"H"} else {" "},
            if self.pv {"PV"} else {"  "},
            if self.n {"N"} else {" "},
            if self.c {"C"} else {" "}
        )
    }
}

struct Z80ProgramCounter {

    // program counter value
    value: u16,
    // auto-increment flag
    auto_increment: bool,

}

impl Z80ProgramCounter {

    pub fn new() -> Z80ProgramCounter {
        Z80ProgramCounter{
            value: 0,
            auto_increment: false,
        }
    }

    pub fn increment(&mut self, e: i8) {
        self.value = Z80::add_i8_to_u16(self.value, e);
    }

    /// force PC value and clear auto-increment flag
    pub fn force(&mut self, value: u16) {
        self.value = value;
        self.auto_increment = false;
    }

    /// return PC high byte
    pub fn high(&self) -> u8 {
        (self.value >> 8) as u8
    }

    /// return PC low byte
    pub fn low(&self) -> u8 {
        self.value as u8
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
    pc: Z80ProgramCounter,
    // stack pointer
    sp: u16,
    // index registers
    ix: u16,
    iy: u16,
}

impl Z80Registers {
    pub fn new() -> Z80Registers {
        Z80Registers {
            flags: Z80StatusFlags::new(),
            a: 0, b: 0, d:0, h:0,
            c: 0, e:0, l:0,
            iv: 0, mr: 0, sp:0,
            ix:0, iy: 0,
            pc: Z80ProgramCounter::new(),

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
                          |C {:02x} |E {:02x} |L {:02x}\n\
                PC {:04x} |SP {:04x} |IX {:04x} |IY {:04x}\n\
                {}",
                self.a, self.b , self.d , self.h,
                         self.c , self.e , self.l,
                self.pc.value, self.sp, self.ix, self.iy,
                self.flags.to_string())
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

    pub fn step(&mut self, bus:&mut SystemBus) {

        let mut e: u16 = 0;

        loop {
            // fetch one byte from program counter + displacement e
            let opb = bus.cpu_read(self.registers.pc.value + e);

            // increment displacement
            e += 1;

            // feed byte to instruction decoder
            match self.decoder.push(opb) {
                Some(ins) => {

                    // show decoded instruction
                    println!("{:04x}: {}",
                        self.registers.pc.value,
                        ins.to_string());

                    // set auto increment flag (will be set by PC jumps)
                    self.registers.pc.auto_increment = true;

                    // execute decoded instruction
                    self.execute_instruction(bus, ins);

                    // show CPU registers state
                    self.debug_cpu_registers();

                    // if PC auto-increment flag is set (no jump as occured)
                    // increment PC to next instruction
                    if self.registers.pc.auto_increment {
                        self.registers.pc.value += e;
                    }

                    // reset displacement
                    e = 0;

                    break;
                },
                None => {},
            }
        }
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

    fn unpack_AND_operand(&self,
                                bus:&SystemBus,
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
                bus.cpu_read(self.registers.HL())
            },
            ZIL::IndexedIX(d) => {
                bus.cpu_read(Z80::add_signed_u8_to_u16(self.registers.ix, d))
            },
            ZIL::IndexedIY(d) => {
                bus.cpu_read(Z80::add_signed_u8_to_u16(self.registers.iy, d))
            },
            _ => { panic!("unhandled operand: {}", op.to_string()) }
        }
    }

    fn unpack_LD_operand(&self,
                                bus:&SystemBus,
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
            ZIL::RegisterIndirectBC => {
                // fetch byte on bus at address pointed by register BC
                bus.cpu_read(self.registers.BC())
            },
            ZIL::RegisterIndirectDE => {
                // fetch byte on bus at address pointed by register BC
                bus.cpu_read(self.registers.DE())
            },
            ZIL::RegisterIndirectHL => {
                // fetch byte on bus at address pointed by register HL
                bus.cpu_read(self.registers.HL())
            },
            ZIL::IndexedIX(d) => {
                bus.cpu_read(Z80::add_signed_u8_to_u16(self.registers.ix, d))
            },
            ZIL::IndexedIY(d) => {
                bus.cpu_read(Z80::add_signed_u8_to_u16(self.registers.iy, d))
            },
            _ => { panic!("unhandled operand: {}", op.to_string()) }
        }
    }

    /// Perform a LDI / LDD instruction incrementing by e
    fn load_increment(&mut self, bus:&mut SystemBus, e:i8) {
        // LDI     p.130
        // LDD     p.134
        
        // (DE) <- (HL)
        let byte = bus.cpu_read(self.registers.HL());
        bus.cpu_write(self.registers.DE(), byte);
        
        // DE <- DE + e
        self.registers.set_DE(Z80::add_i8_to_u16(self.registers.DE(),e));
        // HL <- HL + e
        self.registers.set_HL(Z80::add_i8_to_u16(self.registers.HL(),e));
        // BC <- BC - 1
        self.registers.set_BC(Z80::add_i8_to_u16(self.registers.BC(),-1));

        // change flags
        self.registers.flags.h = false;
        self.registers.flags.pv = self.registers.BC() != 0;
        self.registers.flags.n = false;
    }

    /// Perform an INC / DEC instruction incrementing by e
    fn increment(&mut self,
                    bus:&mut SystemBus,
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
                let byte = bus.cpu_read(self.registers.HL());
                bus.cpu_write(self.registers.HL(), Z80::add_i8_to_u8(byte, e));
            },
            ZIL::IndexedIX(d) => {
                // fetch byte on bus at address pointed by register IX + d
                // and write incremented value
                let addr = Z80::add_signed_u8_to_u16(self.registers.ix, d);
                let byte = bus.cpu_read(addr);
                bus.cpu_write(addr, Z80::add_i8_to_u8(byte, e));
            },
            ZIL::IndexedIY(d) => {
                // fetch byte on bus at address pointed by register IY + d
                // and write incremented value
                let addr = Z80::add_signed_u8_to_u16(self.registers.ix, d);
                let byte = bus.cpu_read(addr);
                bus.cpu_write(addr, Z80::add_i8_to_u8(byte, e));
            },
            _ => { panic!("unhandled operand: {}", op.to_string()) }
        }
    }


    fn test_jump_condition(&self, condition: Z80JumpCondition) -> bool {
        match condition {
            ZJC::Unconditionnal => { true },
            ZJC::Carry          => { self.registers.flags.c },
            ZJC::NonCarry       => { !self.registers.flags.c },
            ZJC::Zero           => { self.registers.flags.z },
            ZJC::NonZero        => { !self.registers.flags.z },
            ZJC::ParityEven     => { self.registers.flags.pv },
            ZJC::ParityOdd      => { !self.registers.flags.pv },
            ZJC::SignNegative   => { self.registers.flags.s },
            ZJC::SignPositive   => { !self.registers.flags.s },
        }
    }

    pub fn execute_instruction(&mut self,
                                bus:&mut SystemBus,
                                ins: Z80Instruction) {

        match ins {
            ZI::DisableInt => {
                // XXX not properly handled
            },
            ZI::EnableInt => {
                // XXX not properly handled
            },
            ZI::SetINTMode0 => {
                // XXX not properly handled
            },
            ZI::SetINTMode1 => {
                // XXX not properly handled
            },
            ZI::SetINTMode2 => {
                // XXX not properly handled
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
                    self.registers.pc.force(addr);

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
                    self.registers.pc.force(
                        Z80::add_signed_u8_to_u16(self.registers.pc.value, e)
                    );
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
                    self.registers.pc.force(
                        Z80::add_signed_u8_to_u16(self.registers.pc.value,e)
                    );
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

                    // copy current contents of the Program Counter on top of the external memory stack
                    bus.cpu_write(self.registers.sp - 1, self.registers.pc.high());
                    bus.cpu_write(self.registers.sp - 2, self.registers.pc.low());
                    
                    // decrement stack pointer
                    self.registers.sp -= 2;

                    // jump to nn
                    self.registers.pc.force(addr);
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
                    let hi = bus.cpu_read(self.registers.sp) as u16;
                    let lo = bus.cpu_read(self.registers.sp+1) as u16;
                    let addr:u16 = (hi << 8) | lo;

                    // increment stack pointer
                    self.registers.sp += 2;

                    // jump to address
                    self.registers.pc.force(addr);
                }
            },

            ZI::Add(opv) => {
                // ADD A,r  p.145

                // unpack value to compare from operand
                let value = self.unpack_AND_operand(bus, opv);
                
                // use an i16 temporary accumulator
                let mut temp :i16 = self.registers.a as i8 as i16;
                // perform addition
                temp += value as i8 as i16;
                // assign result to 8 bit register
                self.registers.a = temp as u8;
                
                // update status flags
                // add/sub
                self.registers.flags.n = false;
                // sign
                self.registers.flags.s = temp < 0;
                // zero
                self.registers.flags.z = temp == 0;
                // overflow
                self.registers.flags.pv = (temp < -128) || (127 < temp);
                // carry
                self.registers.flags.c = temp > 127;
                // half-carry
                self.registers.flags.h = false;

            },

            ZI::Sub(opv) => {
                // SUB s    p.153
                
                // unpack value to compare from operand
                let value = self.unpack_AND_operand(bus, opv);
                
                // use an i16 temporary accumulator
                let mut temp :i16 = self.registers.a as i8 as i16;
                // perform substraction
                temp -= value as i8 as i16;
                // assign result to 8 bit register
                self.registers.a = temp as u8;

                // update status flags
                // add/sub
                self.registers.flags.n = true;
                // sign
                self.registers.flags.s = temp < 0;
                // zero
                self.registers.flags.z = temp == 0;
                // overflow
                self.registers.flags.pv = (temp < -128) || (127 < temp);
                // carry
                self.registers.flags.c = temp < -128;
                // half-carry
                self.registers.flags.h = false;

            },

            ZI::Increment(op) => {
                // INC r    p.165
                // ...
                // INC ss   p.198
                // ...
                self.increment(bus, op, 1)
            },

            ZI::Decrement(op) => {
                // DEC r    p.170
                // ...
                // DEC ss   p.201
                // ...
                self.increment(bus, op, -1)
            },

            ZI::Compare(opv) => {
                // CP s     p.163
                
                // unpack value to compare from operand
                let value = self.unpack_AND_operand(bus, opv);

                // NDJD: CP seems to behave like SUB except it
                // does not update accumulator with result
                // only flags are updated

                // use an i16 temporary accumulator
                let mut temp :i16 = self.registers.a as i8 as i16;
                // perform substraction
                temp -= value as i8 as i16;

                // update status flags
                // add/sub
                self.registers.flags.n = true;
                // sign
                self.registers.flags.s = temp < 0;
                // zero
                self.registers.flags.z = temp == 0;
                // overflow
                self.registers.flags.pv = (temp < -128) || (127 < temp);
                // carry
                self.registers.flags.c = temp < -128;
                // half-carry
                self.registers.flags.h = false;
            },

            ZI::Or(opv) => {
                // OR s     p.159
                
                // unpack value to compare from operand
                let value = self.unpack_AND_operand(bus, opv);

                // NDJD: CP seems to behave like SUB except it
                // does not update accumulator with result
                // only flags are updated

                let r = self.registers.a | value;
                self.registers.a = r;

                // update status flags
                // add/sub
                self.registers.flags.n = false;
                // sign
                self.registers.flags.s = (r as i8) < 0;
                // zero
                self.registers.flags.z = r == 0;
                // overflow
                self.registers.flags.pv = false;
                // carry
                self.registers.flags.c = false;
                // half-carry
                self.registers.flags.h = false;
            },



            ZI::LoadIncrement => {
                self.load_increment(bus, 1)
            },

            ZI::LoadDecrement => {
                self.load_increment(bus, -1)
            },

            ZI::LoadIncrementRepeat => {
                // LDIR     p.132

                // execute LDI while BC != 0
                loop {
                    self.load_increment(bus, 1);
                    if(self.registers.BC() == 0) {
                        break;
                    }
                }
            },

            ZI::LoadDecrementRepeat => {
                // LDDR     p.132

                // execute LDR while BC != 0
                loop {
                    self.load_increment(bus, -1);
                    if(self.registers.BC() == 0) {
                        break;
                    }
                }
            },

            ZI::Load(dst, src) => {
                // LD *,*   p.71 (8 bits load operations)
                
                // unpack value to load
                let value = self.unpack_LD_operand(bus, src);

                match dst {
                    ZIL::Indirect16(addr) => { bus.cpu_write(addr,value) },
                    ZIL::RegisterA => { self.registers.a = value},
                    ZIL::RegisterB => { self.registers.b = value},
                    ZIL::RegisterC => { self.registers.c = value},
                    ZIL::RegisterD => { self.registers.d = value},
                    ZIL::RegisterE => { self.registers.e = value},
                    ZIL::RegisterH => { self.registers.h = value},
                    ZIL::RegisterL => { self.registers.l = value},
                    ZIL::RegisterIndirectHL => {
                        // write byte at address pointed by register HL
                        bus.cpu_write(self.registers.HL(), value)
                    },
                    ZIL::RegisterIndirectDE => {
                        // write byte at address pointed by register DE
                        bus.cpu_write(self.registers.DE(), value)
                    },
                    ZIL::RegisterIndirectBC => {
                        // write byte at address pointed by register BC
                        bus.cpu_write(self.registers.BC(), value)
                    },
                    ZIL::IndexedIX(d) => {
                        bus.cpu_write(
                            Z80::add_signed_u8_to_u16(self.registers.ix, d),
                            value
                        )
                    },
                    ZIL::IndexedIY(d) => {
                        bus.cpu_write(
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
                    ZIL::Indirect16(addr)   => { bus.cpu_read_u16(addr) },
                    ZIL::RegisterBC => { self.registers.BC() },
                    ZIL::RegisterDE => { self.registers.DE() },
                    ZIL::RegisterHL => { self.registers.HL() },
                    ZIL::RegisterSP => { self.registers.sp },
                    ZIL::RegisterIX => { self.registers.ix },
                    ZIL::RegisterIY => { self.registers.iy },

                    _ => { panic!("unhandled operand: {}", src.to_string()) }
                };

                match dst {
                    ZIL::Indirect16(addr) => { bus.cpu_write_u16(addr, value) },
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
                let byte = bus.io_read(addr);

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
                bus.io_write(addr, byte);
            },

            _ => {
                panic!("unhandled instruction");
            },
        }
    }


}
