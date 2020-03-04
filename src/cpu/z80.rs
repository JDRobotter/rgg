
use crate::cpu::Z80InstructionDecoder;
use crate::system::SystemBus;

use crate::cpu::Z80Instruction;
use Z80Instruction as ZI;

use crate::cpu::Z80InstructionLocation;
use Z80InstructionLocation as ZIL;

use crate::cpu::Z80JumpCondition;
use Z80JumpCondition as ZJC;

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
            flags: Z80StatusFlags::new(),
            a: 0, b: 0, d:0, h:0,
            c: 0, e:0, l:0,
            iv: 0, mr: 0, pc: 0, sp:0,
            ix:0, iy: 0
        }
    }

    pub fn HL(&self) -> u16 {
        let h = self.h as u16;
        let l = self.l as u16;
        (h << 8) | l
    }

    pub fn to_string(&self) -> String {
        format!("A {:02x} |B {:02x} |D {:02x} |H {:02x} \
                          |C {:02x} |E {:02x} |L {:02x}\n\
                PC {:04x} |SP {:04x} |IX {:04x} |IY {:04x}",
                self.a, self.b , self.d , self.h,
                         self.c , self.e , self.l,
                self.pc, self.sp, self.ix, self.iy)
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

        let mut current_instruction_address : Option<u16> = None;

        loop {
            // fetch one byte from program counter
            let opb = bus.cpu_read(self.registers.pc);
            // store instruction address (first opcode byte address)
            if current_instruction_address.is_none() {
                current_instruction_address = Some(self.registers.pc);
            }

            // increment PC
            self.registers.pc += 1;
            // feed byte to instruction decoder
            match self.decoder.push(opb) {
                Some(ins) => {

                    // show decoded instruction
                    println!("{:04x}: {}",
                        current_instruction_address.or(Some(0)).unwrap(),
                        ins.to_string());

                    // execute decoded instruction
                    self.execute_instruction(bus, ins);

                    // show CPU registers state
                    self.debug_cpu_registers();

                    break;
                },
                None => {},
            }
        }
    }

    pub fn debug_cpu_registers(&mut self) {
        println!("{}", self.registers.to_string())
    }

    fn add_i8_to_u16(base:u16, displacement:u8) -> u16 {
        // Indexed Addressing   p.36
        // displacement is a signed number
        let mut za :i32 = base as i32;
        za += displacement as i8 as i32;
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
                bus.cpu_read(Z80::add_i8_to_u16(self.registers.ix, d))
            },
            ZIL::IndexedIY(d) => {
                bus.cpu_read(Z80::add_i8_to_u16(self.registers.iy, d))
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
            
            ZI::JumpRelative(condition, opaddr) => {
                // JR e     p.265
                // ...
                
                // if jump condition is satisfied
                if self.test_jump_condition(condition) {
                    
                    let jump = match opaddr {
                        ZIL::Immediate(b) => { b },
                        ZIL::RegisterIndirectHL => {
                            // fetch byte on bus at address
                            // pointed by register HL
                            bus.cpu_read(self.registers.HL())
                        },
                        ZIL::RegisterIndirectIX => {
                            bus.cpu_read(self.registers.ix)
                        },
                        ZIL::RegisterIndirectIY => {
                            bus.cpu_read(self.registers.iy)
                        },
                        _ => { panic!("unhandled operand: {}",
                                        opaddr.to_string()) }
                    };

                    // increment PC by jump
                    self.registers.pc = Z80::add_i8_to_u16(self.registers.pc, jump);


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
                // perform addition
                temp += value as i8 as i16;
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

            ZI::Compare(opv) => {
                // CP s     p.163
                
                // unpack value to compare from operand
                let value = self.unpack_AND_operand(bus, opv);
 

                // NDJD: CP seems to behave like SUB except it
                // does not update accumulator with result
                // only flags are updated

                // use an i16 temporary accumulator
                let mut temp :i16 = self.registers.a as i8 as i16;
                // perform addition
                temp += value as i8 as i16;

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
                    _ => { panic!("unhandled operand") }
                }
            }

            _ => {
                panic!("unhandled instruction");
            },
        }
    }


}
