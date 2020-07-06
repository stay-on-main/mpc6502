// http://dendy.migera.ru/nes/g11.html
// http://www.obelisk.me.uk/6502/reference.html
// https://wiki.nesdev.com/w/index.php/CPU_addressing_modes
// http://nesdev.com/6502.txt
// https://skilldrick.github.io/easy6502/
// https://stackoverflow.com/questions/29193303/6502-emulation-proper-way-to-implement-adc-and-sbc
// https://gamedev.ru/pages/l/forum/?id=152486&page=6
// https://tech-geek.ru/how-to-make-nes-emulator/
// https://forums.nesdev.com/viewtopic.php?t=19153
// https://wiki.nesdev.com/w/index.php/Programming_with_unofficial_opcodes
mod bus;
use bus::{Bus};

mod nes;
use nes::{Rom};

use std::clone::Clone;

enum StatusBit {
    Carry = 1 << 0,
    Zero = 1 << 1,
    Interrupt = 1 << 2,
    Decimal = 1 << 3,
    Break = 1 << 4,
    Overflow = 1 << 6,
    Sign = 1 << 7,
}

pub struct Cpu {
    a: u8,
    pc: u16,
    s: u8,
    x: u8,
    y: u8,
    p: u8,
    bus: Bus,
}

#[derive(Clone, Copy)]
enum Operand {
    Mem(u16),
    A,
    X,
    Y,
    S,
}

#[derive(Clone)]
enum Adressing {
    A,
    X,
    Y,
    S,
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Implied,
    Relative,
    IndirectX,
    IndirectY,
}

impl Cpu {
    fn set_bit(&mut self, bit: StatusBit, val: u8) {
        match bit {
            StatusBit::Carry => {
                if val != 0 {
                    self.p |= StatusBit::Carry as u8;
                } else {
                    self.p &= !(StatusBit::Carry as u8);
                }
            },
            StatusBit::Sign => {
                if val & 0x80 != 0 {
                    self.p |= StatusBit::Sign as u8;
                } else {
                    self.p &= !(StatusBit::Sign as u8);
                }
            },
            StatusBit::Zero => {
                if val == 0 {
                    self.p |= StatusBit::Zero as u8;
                } else {
                    self.p &= !(StatusBit::Zero as u8);
                }
            },
            StatusBit::Overflow => {
                if val != 0 {
                    self.p |= StatusBit::Overflow as u8;
                } else {
                    self.p &= !(StatusBit::Overflow as u8);
                }
            },
            _ => todo!(),
        }
    }

    fn bit(&self, bit: StatusBit) -> bool {
        (self.p & (bit as u8)) != 0
    }
    
    fn alu_add(&mut self, a: u8, b: u8) -> u8 {
        let mut temp = (a as u16) + (b as u16);
        
        if self.bit(StatusBit::Carry) {
            temp += 1;
        }

        self.set_bit(StatusBit::Zero, temp as u8);
        self.set_bit(StatusBit::Sign, temp as u8);
        // The overflow flag is set when
        // the sign of the addends is the same and
        // differs from the sign of the sum
        self.set_bit(StatusBit::Overflow, !(a ^ b) & (a ^ (temp as u8)) & 0x80);
        self.set_bit(StatusBit::Carry, if temp > 0xFF {1} else {0});
        temp as u8
    }

    fn alu_sub(&mut self, a: u8, b: u8) -> u8 {
        let temp = (a as i32) - (b as i32) - (if self.bit(StatusBit::Carry) {0} else {1});
        self.set_bit(StatusBit::Sign, temp as u8);
        self.set_bit(StatusBit::Zero, temp as u8);

        self.set_bit(StatusBit::Overflow, ((a ^ (temp as u8)) & 0x80) & ((a ^ b) & 0x80));
        self.set_bit(StatusBit::Carry, if a >= b {1} else {0});
        temp as u8
    }

    fn alu_and(&mut self, a: u8) {
        self.a &= a;
        self.set_bit(StatusBit::Sign, self.a);
        self.set_bit(StatusBit::Zero, self.a);
        //self.bus.clk();
    }

    fn alu_or(&mut self, a: u8) {
        self.a |= a;
        self.set_bit(StatusBit::Sign, self.a);
        self.set_bit(StatusBit::Zero, self.a);
        //self.bus.clk();
    }

    fn alu_xor(&mut self, a: u8) {
        self.a ^= a;
        self.set_bit(StatusBit::Sign, self.a);
        self.set_bit(StatusBit::Zero, self.a);
        //self.bus.clk();
    }

    fn alu_sl(&mut self, a: u8) -> u8 {
        self.set_bit(StatusBit::Carry, a & 0x80);
        let a = a << 1;
        self.set_bit(StatusBit::Sign, a);
        self.set_bit(StatusBit::Zero, a);
        //self.bus.clk();
        a
    }

    fn alu_sr(&mut self, a: u8) -> u8 {
        self.set_bit(StatusBit::Carry, a & 0x01);
        let a = a >> 1;
        self.set_bit(StatusBit::Sign, a);
        self.set_bit(StatusBit::Zero, a);
        //self.bus.clk();
        a
    }

    fn alu_rl(&mut self, a: u8) -> u8 {
        let res = a << 1;
        let res = res | if self.bit(StatusBit::Carry) {1} else {0};
        self.set_bit(StatusBit::Carry, a & 0x80);
        self.set_bit(StatusBit::Sign, res);
        self.set_bit(StatusBit::Zero, res);
        //self.bus.clk();
        res
    }

    fn alu_rr(&mut self, a: u8) -> u8 {
        let res = a >> 1;
        let res = res | if self.bit(StatusBit::Carry) {0x80} else {0};
        self.set_bit(StatusBit::Carry, a & 0x01);
        self.set_bit(StatusBit::Sign, res);
        self.set_bit(StatusBit::Zero, res);
        //self.bus.clk();
        res
    }

    fn alu_add_nc(&mut self, a: u8, b: u8) -> u8 {
        let n = ((a as u16) + (b as u16)) as u8;
        self.set_bit(StatusBit::Sign, n);
        self.set_bit(StatusBit::Zero, n);
        //self.bus.clk();
        n
    }

    fn alu_sub_nb(&mut self, a: u8, b: u8) -> u8 {
        let n = ((a as i16) - (b as i16)) as u8;
        self.set_bit(StatusBit::Sign, n);
        self.set_bit(StatusBit::Zero, n);
        //self.bus.clk();
        n
    }
    
    fn fetch_u8(&mut self) -> u8 {
        let n = self.bus.read_u8(self.pc);
        self.pc += 1;
        self.bus.clk();
        n
    }

    fn stack_push(&mut self, val: u8) {
        self.bus.write_u8(0x0100 + (self.s as u16), val);
        self.s -= 1;
        self.bus.clk();
    }

    fn stack_pop(&mut self) -> u8 {
        self.s += 1;
        self.bus.clk();
        self.bus.read_u8(0x0100 + (self.s as u16))
    }

    fn fetch_u16(&mut self) -> u16 {
        let l = self.fetch_u8() as u16;
        let h = self.fetch_u8() as u16;
        (h << 8) | l
    }

    fn page_crossing(&mut self, addr: u16) {
        if (addr >> 8) != (self.pc >> 8) {
            // additional clock on page change
            self.bus.clk();
        }
    }

    fn get_operand(&mut self, addressing: Adressing) -> Operand {
        match addressing {
            Adressing::A => Operand::A,
            Adressing::X => Operand::X,
            Adressing::Y => Operand::Y,
            Adressing::S => Operand::S,
            Adressing::Immediate => {
                let addr = self.pc;
                self.pc += 1;
                Operand::Mem(addr)
            },
            Adressing::ZeroPage => {
                let addr = self.fetch_u8();
                Operand::Mem(addr as u16)
            },
            Adressing::ZeroPageX => {
                let addr = self.fetch_u8();
                let addr = addr.wrapping_add(self.x);
                self.bus.clk();
                Operand::Mem(addr as u16)
            },
            Adressing::ZeroPageY => {
                let addr = self.fetch_u8();
                let addr = addr.wrapping_add(self.y);
                self.bus.clk();
                Operand::Mem(addr as u16)
            },
            Adressing::Absolute => {
                let addr = self.fetch_u16();
                Operand::Mem(addr)
            },
            Adressing::AbsoluteX => {
                let addr = self.fetch_u16() + self.x as u16;
                self.page_crossing(addr);
                Operand::Mem(addr)
            },
            Adressing::AbsoluteY => {
                let addr = self.fetch_u16() + self.y as u16;
                self.page_crossing(addr);
                Operand::Mem(addr)
            },
            Adressing::IndirectX => {
                let zaddr = self.fetch_u8();
                let zaddr = zaddr.wrapping_add(self.x);
                let l = self.bus.read_u8(zaddr as u16) as u16;
                self.bus.clk();
                let zaddr = zaddr.wrapping_add(1);
                let h = self.bus.read_u8(zaddr as u16) as u16;
                self.bus.clk();
                let addr = (h << 8) | l;
                self.bus.clk();
                Operand::Mem(addr)
            },
            Adressing::IndirectY => {
                let addr = self.fetch_u8();
                let l = self.bus.read_u8(addr as u16) as u16;
                self.bus.clk();
                let h = self.bus.read_u8(addr.wrapping_add(1) as u16) as u16;
                let addr = ((h << 8) | l);
                let addr_y =  addr.wrapping_add(self.y as u16);
                if addr >> 8 != addr_y >> 8 {
                    self.bus.clk();
                }
                self.bus.clk();
                //self.page_crossing(addr);
                //let addr = addr.wrapping_add(self.y as u16);
                //self.bus.clk();
                
                Operand::Mem(addr_y)
            },
            Adressing::Relative => {
                let addr = self.fetch_u8() as i8;
                Operand::Mem(((self.pc as i32) + (addr as i32)) as u16)
            }
            _ => { todo!(); }
            //Adressing::Implied,
            //Adressing::Relative,
        }
    }

    fn read(&mut self, operand: Operand) -> u8 {
        match operand {
            Operand::Mem(addr) => {
                self.bus.clk();
                self.bus.read_u8(addr)
            }
            Operand::A => self.a,
            Operand::X => self.x,
            Operand::Y => self.y,
            Operand::S => self.s,
        }
    }

    fn write(&mut self, operand: Operand, val: u8) {
        match operand {
            Operand::Mem(addr) => {
                self.bus.clk();
                self.bus.write_u8(addr, val)
            },
            Operand::A => self.a = val,
            Operand::X => self.x = val,
            Operand::Y => self.y = val,
            Operand::S => self.s = val,
        }
    }

    fn inc(&mut self, addressing: Adressing) {
        let operand = self.get_operand(addressing);
        let val = self.read(operand);
        let val = self.alu_add_nc(val, 1);
        self.bus.clk();
        self.write(operand, val);
    }

    fn dec(&mut self, addressing: Adressing) {
        let operand = self.get_operand(addressing);
        let val = self.read(operand);
        let val = self.alu_sub_nb(val, 1);
        self.bus.clk();
        self.write(operand, val);
    }

    fn adc(&mut self, addressing: Adressing) {
        let operand = self.get_operand(addressing);
        let val = self.read(operand);
        self.a = self.alu_add(self.a, val);
    }

    fn and(&mut self, addressing: Adressing) {
        let operand = self.get_operand(addressing);
        let val = self.read(operand);
        self.alu_and(val);
    }

    fn asl(&mut self, addressing: Adressing) {
        let operand = self.get_operand(addressing);
        let val = self.read(operand);
        let val = self.alu_sl(val);
        self.bus.clk();
        self.write(operand, val);
    }
    
    fn eor(&mut self, addressing: Adressing) {
        let operand = self.get_operand(addressing);
        let val = self.read(operand);
        self.alu_xor(val);
    }

    fn lsr(&mut self, addressing: Adressing) {
        let operand = self.get_operand(addressing);
        let val = self.read(operand);
        let val = self.alu_sr(val);
        self.bus.clk();
        self.write(operand, val);
    }

    fn cmp(&mut self, addressing_1: Adressing, addressing_2: Adressing) {
        let operand_1 = self.get_operand(addressing_1);
        let a = self.read(operand_1);
        let operand_2 = self.get_operand(addressing_2);
        let b = self.read(operand_2);
        
        let src = a as i32 - (b as i8) as i32;
        self.set_bit(StatusBit::Carry, if a >= b {1} else {0});
        self.set_bit(StatusBit::Sign, src as u8);
        self.set_bit(StatusBit::Zero, src as u8);
    }

    fn mov(&mut self, dst: Adressing, src: Adressing) {
        let src = self.get_operand(src);
        let src_val = self.read(src);
        let dst = self.get_operand(dst);
        self.write(dst, src_val);
    }

    fn branch(&mut self, addressing: Adressing, condition: bool) {
        if let Operand::Mem(addr) = self.get_operand(addressing) {
            if condition {
                self.bus.clk();
                self.page_crossing(addr);
                self.pc = addr;
            }
        } else {
            panic!();
        }
    }

    fn ora(&mut self, addressing: Adressing) {
        let operand = self.get_operand(addressing);
        let val = self.read(operand);
        self.alu_or(val);
    }

    fn sbc(&mut self, addressing: Adressing) {
        let operand = self.get_operand(addressing);
        let val = self.read(operand);
        self.a = self.alu_sub(self.a, val);
    }

    fn nop(&mut self) {
        self.bus.clk();
    }

    fn lax(&mut self, src: Adressing) {
        // LAX - Shortcut for LDA value then TAX
        self.mov(Adressing::A, src);
        self.x = self.alu_add_nc(self.a, 0);
    }

    fn transfer(&mut self, dst: Adressing, src: Adressing) {
        let src = self.get_operand(src);
        let src_val = self.read(src);
        let src_val = self.alu_add_nc(src_val, 0);
        let dst = self.get_operand(dst);
        self.write(dst, src_val);
    }

    fn transfer_reg_to_reg(&mut self, dst: Adressing, src: Adressing) {
        let src = self.get_operand(src);
        let src_val = self.read(src);
        let src_val = self.alu_add_nc(src_val, 0);
        self.bus.clk();
        let dst = self.get_operand(dst);
        self.write(dst, src_val);
    }

    fn isc(&mut self, src: Adressing) {
        // ISC - Equivalent to INC value then SBC value,
        // except supporting more addressing modes.
        todo!();
    }

    fn dcp(&mut self, src: Adressing) {
        todo!();
    }

    fn ign(&mut self, src: Adressing) {
        let src = self.get_operand(src);
        let _ = self.read(src);
    }

    fn clear_flag(&mut self, flag: StatusBit) {
        self.p &= !(flag as u8);
        self.bus.clk();
    }

    fn set_flag(&mut self, flag: StatusBit) {
        self.p |= flag as u8;
        self.bus.clk();
    }

    fn skb(&mut self, src: Adressing) {
        let src = self.get_operand(src);
        let _ = self.read(src);
    }

    fn sax(&mut self, dst: Adressing) {
        let dst = self.get_operand(dst);
        // SAX - Stores the bitwise AND of A and X.
        // As with STA and STX, no flags are affected.
        self.write(dst, self.a & self.x);
    }

    fn slo(&mut self, src: Adressing) {
        // SLO - Equivalent to ASL value then ORA value,
        // except supporting more addressing modes.
        // LDA #0 followed by SLO is an efficient way
        // to shift a variable while also loading it in A.
        let src = self.get_operand(src);
        let src_val = self.read(src);
        let src_val = self.alu_sl(src_val);
        self.write(src, src_val);
        self.alu_or(src_val);
    }

    fn rra(&mut self, src: Adressing) {
        // RRA - Equivalent to ROR value then ADC value,
        // except supporting more addressing modes.
        // Essentially this computes A + value / 2,
        // where value is 9-bit and the division is rounded up.
        let src = self.get_operand(src);
        let src_val = self.read(src);
        let src_val = self.alu_rr(src_val);
        self.write(src, src_val);
        self.a = self.alu_add(self.a, src_val);
    }

    fn rla(&mut self, src: Adressing) {
        // RLA - Equivalent to ROL value then AND value,
        // except supporting more addressing modes.
        let src = self.get_operand(src);
        let src_val = self.read(src);
        let src_val = self.alu_rl(src_val);
        self.write(src, src_val);
        self.alu_and(src_val);
    }

    fn sre(&mut self, src: Adressing) {
        // SRE - Equivalent to LSR value then EOR value,
        // except supporting more addressing modes.
        let src = self.get_operand(src);
        let src_val = self.read(src);
        let src_val = self.alu_sl(src_val);
        self.write(src, src_val);
        self.alu_xor(src_val);
    }

    fn ror(&mut self, addressing: Adressing) {
        let operand = self.get_operand(addressing);
        let val = self.read(operand);
        let val = self.alu_rr(val);
        self.bus.clk();
        self.write(operand, val);
    }

    fn rol(&mut self, addressing: Adressing) {
        let operand = self.get_operand(addressing);
        let val = self.read(operand);
        let val = self.alu_rl(val);
        self.bus.clk();
        self.write(operand, val);
    }

    fn brk(&mut self) {
        // BRK - Force Interrupt
        todo!();
        /*
        print!("BRK");
        self.pc += 1;
        self.stack_push((self.pc >> 8) as u8);
        self.stack_push(self.pc as u8);
        self.set_break(1);
        self.stack_push(self.s);
        self.set_interrupt(1);
        self.pc = self.bus.read_u16(0xFFFE);
        */
    }

    fn tsx(&mut self) {
        // TSX - Transfer Stack Pointer to X
        //print!("TSX");
        /*
        self.x = self.s;
        self.set_sign(self.s);
        self.set_zero(self.s);
        */
        //*/
        //todo!();
    }

    fn txs(&mut self) {
        // TXS - Transfer X to Stack Pointer
        //todo!();
        //print!("TXS");
        self.s = self.x;
        self.bus.clk();
    }

    fn rti(&mut self) {
        // RTI - Return from Interrupt
        self.bus.clk();
        self.p = self.stack_pop() | (1 << 5);
        let l = self.stack_pop() as u16;
        let h = self.stack_pop() as u16;
        self.pc = (h << 8) | l;
        self.bus.clk();
    }

    fn rts(&mut self) {
        // RTS - Return from Subroutine
        //print!("RTS");
        self.bus.clk();
        let l = self.stack_pop() as u16;
        let h = self.stack_pop() as u16;
        self.pc = (h << 8) | l;
        self.bus.clk();

        self.pc += 1;
        self.bus.clk();
    }

    fn pha(&mut self) {
        // PHA - Push Accumulator
        self.bus.clk();
        self.stack_push(self.a);
    }

    fn php(&mut self) {
        // PHP - Push Processor Status
        //print!("PHP");
        self.bus.clk();
        self.stack_push(self.p | StatusBit::Break as u8);
    }

    fn pla(&mut self) {
        // PLA - Pull Accumulator
        self.bus.clk();
        self.a = self.stack_pop();
        self.set_bit(StatusBit::Sign, self.a);
        self.set_bit(StatusBit::Zero, self.a);
        self.bus.clk();
    }

    fn plp(&mut self) {
        self.bus.clk();
        self.p = self.stack_pop();
        self.p &= !(StatusBit::Break as u8);
        self.p |= 1 << 5;
        self.bus.clk();
        /*
        print!("PLP");
        // PLP - Pull Processor Status
        self.p = self.stack_pop();
        self.p &= !BREAK_BIT;
        self.p |= 1 << 5;
        */
    }

    fn jmp(&mut self, src: Adressing) {
        // SRE - Equivalent to LSR value then EOR value,
        // except supporting more addressing modes.
        match self.get_operand(src) {
            Operand::Mem(addr) => {
                self.pc = addr;
            }
            _ => panic!(),
        }

        // JMP - Jump
        //print!("JMP");
        //let addr = addr(self);
        //self.pc = addr;// self.bus.read_u16(addr);
    }

    fn jsr(&mut self, src: Adressing) {
        // JSR - Jump to Subroutine
        match self.get_operand(src) {
            Operand::Mem(addr) => {
                self.pc -= 1;
                self.stack_push((self.pc >> 8) as u8);
                self.stack_push(self.pc as u8);
                self.pc = addr;
                self.bus.clk();
            },
            _ => panic!(),
        }
        /*
        print!("JSR");
        let addr = addr(self);
        self.pc -= 1;
        self.stack_push((self.pc >> 8) as u8);
        self.stack_push(self.pc as u8);
        self.pc = addr;//self.bus.read_u16(addr);
        */
    }

    fn bit_test(&mut self,  addressing: Adressing) {
        let src = self.get_operand(addressing);
        let src_val = self.read(src);
        self.set_bit(StatusBit::Sign, src_val);
        self.set_bit(StatusBit::Overflow, 0x40 & src_val);
        self.set_bit(StatusBit::Zero, src_val & self.a);
    }

    pub fn clock(&mut self) {
        print!("{:02X}  ", self.pc);
        //self.print_state();
                print!("A:{:02X} ", self.a);
        print!("X:{:02X} ", self.x);
        print!("Y:{:02X} ", self.y);
        print!("P:{:02X} ", self.p);
        print!("SP:{:02X}  ", self.s);

        println!(" CYC:{}", self.bus.clk);

        let instruction = self.fetch_u8();
        //print!("read instruction {:2x}", instruction);
        match instruction {
            // ADC - Add with Carry
            0x69 => self.adc(Adressing::Immediate),
            0x65 => self.adc(Adressing::ZeroPage),
            0x75 => self.adc(Adressing::ZeroPageX),
            0x6D => self.adc(Adressing::Absolute),
            0x7D => self.adc(Adressing::AbsoluteX),
            0x79 => self.adc(Adressing::AbsoluteY),
            0x61 => self.adc(Adressing::IndirectX),
            0x71 => self.adc(Adressing::IndirectY),
            // AND - Logical AND
            0x29 => self.and(Adressing::Immediate),
            0x25 => self.and(Adressing::ZeroPage),
            0x35 => self.and(Adressing::ZeroPageX),
            0x2D => self.and(Adressing::Absolute),
            0x3D => self.and(Adressing::AbsoluteX),
            0x39 => self.and(Adressing::AbsoluteY),
            0x21 => self.and(Adressing::IndirectX),
            0x31 => self.and(Adressing::IndirectY),
            // ASL - Arithmetic Shift Left
            0x0A => self.asl(Adressing::A),
            0x06 => self.asl(Adressing::ZeroPage),
            0x16 => self.asl(Adressing::ZeroPageX),
            0x0E => self.asl(Adressing::Absolute),
            0x1E => self.asl(Adressing::AbsoluteX),
            // BCC - Branch if Carry Clear
            0x90 => self.branch(Adressing::Relative, !self.bit(StatusBit::Carry)),
            // BCS - Branch if Carry Set
            0xB0 => self.branch(Adressing::Relative, self.bit(StatusBit::Carry)),
            // BEQ - Branch if Equal
            0xF0 => self.branch(Adressing::Relative, self.bit(StatusBit::Zero)),
            // BIT - Bit Test
            0x24 => self.bit_test(Adressing::ZeroPage),
            0x2C => self.bit_test(Adressing::Absolute),
            // BMI - Branch if Minus
            0x30 => self.branch(Adressing::Relative, self.bit(StatusBit::Sign)),
            // BNE - Branch if Not Equal
            0xD0 => self.branch(Adressing::Relative, !self.bit(StatusBit::Zero)),
            // BPL - Branch if Positive
            0x10 => self.branch(Adressing::Relative, !self.bit(StatusBit::Sign)),
            // BRK - Force Interrupt
            0x00 => self.brk(),
            // BVC - Branch if Overflow Clear
            0x50 => self.branch(Adressing::Relative, !self.bit(StatusBit::Overflow)),
            // BVS - Branch if Overflow Set
            0x70 => self.branch(Adressing::Relative, self.bit(StatusBit::Overflow)),
            // CLC - Clear Carry Flag
            0x18 => self.clear_flag(StatusBit::Carry),
            // CLD - Clear Decimal Mode
            0xD8 => self.clear_flag(StatusBit::Decimal),
            // CLI - Clear Interrupt Disable
            0x58 => self.clear_flag(StatusBit::Interrupt),
            // CLV - Clear Overflow Flag
            0xB8 => self.clear_flag(StatusBit::Overflow),
            // CMP - Compare
            0xC9 => self.cmp(Adressing::A, Adressing::Immediate),
            0xC5 => self.cmp(Adressing::A, Adressing::ZeroPage),
            0xD5 => self.cmp(Adressing::A, Adressing::ZeroPageX),
            0xCD => self.cmp(Adressing::A, Adressing::Absolute),
            0xDD => self.cmp(Adressing::A, Adressing::AbsoluteX),
            0xD9 => self.cmp(Adressing::A, Adressing::AbsoluteY),
            0xC1 => self.cmp(Adressing::A, Adressing::IndirectX),
            0xD1 => self.cmp(Adressing::A, Adressing::IndirectY),
            // CPX - Compare X Register
            0xE0 => self.cmp(Adressing::X, Adressing::Immediate),
            0xE4 => self.cmp(Adressing::X, Adressing::ZeroPage),
            0xEC => self.cmp(Adressing::X, Adressing::Absolute),
            // CPY - Compare Y Register
            0xC0 => self.cmp(Adressing::Y, Adressing::Immediate),
            0xC4 => self.cmp(Adressing::Y, Adressing::ZeroPage),
            0xCC => self.cmp(Adressing::Y, Adressing::Absolute),
            // DCP
            0xC3 => self.dcp(Adressing::IndirectX),
            0xC7 => self.dcp(Adressing::ZeroPage),
            0xCF => self.dcp(Adressing::Absolute),
            0xD3 => self.dcp(Adressing::IndirectY),
            0xD7 => self.dcp(Adressing::ZeroPageX),
            0xDB => self.dcp(Adressing::AbsoluteY),
            0xDF => self.dcp(Adressing::AbsoluteX),
            // DEC - Decrement Memory
            0xC6 => self.dec(Adressing::ZeroPage),
            0xD6 => self.dec(Adressing::ZeroPageX),
            0xCE => self.dec(Adressing::Absolute),
            0xDE => self.dec(Adressing::AbsoluteX),
            // DEX - Decrement X Register
            0xCA => self.dec(Adressing::X),
            // DEY - Decrement Y Register
            0x88 => self.dec(Adressing::Y),
            // EOR - Exclusive OR
            0x49 => self.eor(Adressing::Immediate),
            0x45 => self.eor(Adressing::ZeroPage),
            0x55 => self.eor(Adressing::ZeroPageX),
            0x4D => self.eor(Adressing::Absolute),
            0x5D => self.eor(Adressing::AbsoluteX),
            0x59 => self.eor(Adressing::AbsoluteY),
            0x41 => self.eor(Adressing::IndirectX),
            0x51 => self.eor(Adressing::IndirectY),
            // IGN
            0x04 | 0x44 | 0x64 => self.ign(Adressing::ZeroPage),
            0x0C => self.ign(Adressing::Absolute),
            0x14 | 0x34 | 0x54 | 0x74 | 0xD4 | 0xF4 => self.ign(Adressing::ZeroPageX),
            0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => self.ign(Adressing::AbsoluteX),
            // INC - Increment Memory
            0xE6 => self.inc(Adressing::ZeroPage),
            0xF6 => self.inc(Adressing::ZeroPageX),
            0xEE => self.inc(Adressing::Absolute),
            0xFE => self.inc(Adressing::AbsoluteX),
            // INX - Increment X Register
            0xE8 => self.inc(Adressing::X),
            // INY - Increment Y Register
            0xC8 => self.inc(Adressing::Y),
            // ISC
            0xE3 => self.isc(Adressing::IndirectX),
            0xE7 => self.isc(Adressing::ZeroPage),
            0xEF => self.isc(Adressing::Absolute),
            0xF3 => self.isc(Adressing::IndirectY),
            0xF7 => self.isc(Adressing::ZeroPageX),
            0xFB => self.isc(Adressing::AbsoluteY),
            0xFF => self.isc(Adressing::AbsoluteX),
            // JMP - Jump
            0x4C => self.jmp(Adressing::Absolute),
            0x6C => self.jmp(Adressing::Implied),
            // JSR - Jump to Subroutine
            0x20 => self.jsr(Adressing::Absolute),
            // LAX
            0xA3 => self.lax(Adressing::IndirectX),
            0xA7 => self.lax(Adressing::ZeroPage),
            0xAF => self.lax(Adressing::Absolute),
            0xB3 => self.lax(Adressing::IndirectY),
            0xB7 => self.lax(Adressing::ZeroPageY),
            0xBF => self.lax(Adressing::AbsoluteY),
            // LDA - Load Accumulator
            0xA9 => self.transfer(Adressing::A, Adressing::Immediate),
            0xA5 => self.transfer(Adressing::A, Adressing::ZeroPage),
            0xB5 => self.transfer(Adressing::A, Adressing::ZeroPageX),
            0xAD => self.transfer(Adressing::A, Adressing::Absolute),
            0xBD => self.transfer(Adressing::A, Adressing::AbsoluteX),
            0xB9 => self.transfer(Adressing::A, Adressing::AbsoluteY),
            0xA1 => self.transfer(Adressing::A, Adressing::IndirectX),
            0xB1 => self.transfer(Adressing::A, Adressing::IndirectY),
            // LDX - Load X Register
            0xA2 => self.transfer(Adressing::X, Adressing::Immediate),
            0xA6 => self.transfer(Adressing::X, Adressing::ZeroPage),
            0xB6 => self.transfer(Adressing::X, Adressing::ZeroPageY),
            0xAE => self.transfer(Adressing::X, Adressing::Absolute),
            0xBE => self.transfer(Adressing::X, Adressing::AbsoluteY),
            // LDY - Load Y Register
            0xA0 => self.transfer(Adressing::Y, Adressing::Immediate),
            0xA4 => self.transfer(Adressing::Y, Adressing::ZeroPage),
            0xB4 => self.transfer(Adressing::Y, Adressing::ZeroPageX),
            0xAC => self.transfer(Adressing::Y, Adressing::Absolute),
            0xBC => self.transfer(Adressing::Y, Adressing::AbsoluteX),
            // LSR - Logical Shift Right
            0x4A => self.lsr(Adressing::A),
            0x46 => self.lsr(Adressing::ZeroPage),
            0x56 => self.lsr(Adressing::ZeroPageX),
            0x4E => self.lsr(Adressing::Absolute),
            0x5E => self.lsr(Adressing::AbsoluteX),
            // NOP - No Operation
            0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xEA | 0xFA => self.nop(),
            // ORA - Logical Inclusive OR
            0x09 => self.ora(Adressing::Immediate),
            0x05 => self.ora(Adressing::ZeroPage),
            0x15 => self.ora(Adressing::ZeroPageX),
            0x0D => self.ora(Adressing::Absolute),
            0x1D => self.ora(Adressing::AbsoluteX),
            0x19 => self.ora(Adressing::AbsoluteY),
            0x01 => self.ora(Adressing::IndirectX),
            0x11 => self.ora(Adressing::IndirectY),
            // PHA - Push Accumulator
            0x48 => self.pha(),
            // PHP - Push Processor Status
            0x08 => self.php(),
            // PLA - Pull Accumulator
            0x68 => self.pla(),
            // PLP - Pull Processor Status
            0x28 => self.plp(),
            // RLA
            0x23 => self.rla(Adressing::IndirectX),
            0x27 => self.rla(Adressing::ZeroPage),
            0x2F => self.rla(Adressing::Absolute),
            0x33 => self.rla(Adressing::IndirectY),
            0x37 => self.rla(Adressing::ZeroPageX),
            0x3B => self.rla(Adressing::AbsoluteY),
            0x3F => self.rla(Adressing::AbsoluteX),
            // ROL - Rotate Left
            0x2A => self.rol(Adressing::A),
            0x26 => self.rol(Adressing::ZeroPage),
            0x36 => self.rol(Adressing::ZeroPageX),
            0x2E => self.rol(Adressing::Absolute),
            0x3E => self.rol(Adressing::AbsoluteX),
            // ROR - Rotate Right
            0x6A => self.ror(Adressing::A),
            0x66 => self.ror(Adressing::ZeroPage),
            0x76 => self.ror(Adressing::ZeroPageX),
            0x6E => self.ror(Adressing::Absolute),
            0x7E => self.ror(Adressing::AbsoluteX),
            // RRA
            0x63 => self.rra(Adressing::IndirectX),
            0x67 => self.rra(Adressing::ZeroPage),
            0x6F => self.rra(Adressing::Absolute),
            0x73 => self.rra(Adressing::IndirectY),
            0x77 => self.rra(Adressing::ZeroPageX),
            0x7B => self.rra(Adressing::AbsoluteY),
            0x7F => self.rra(Adressing::AbsoluteX),
            // RTI - Return from Interrupt
            0x40 => self.rti(),
            // RTS - Return from Subroutine
            0x60 => self.rts(),
            // SAX
            0x83 => self.sax(Adressing::IndirectX),
            0x87 => self.sax(Adressing::ZeroPage),
            0x8F => self.sax(Adressing::Absolute),
            0x97 => self.sax(Adressing::ZeroPageY),
            // SBC - Subtract with Carry
            0xE9 | 0xEB => self.sbc(Adressing::Immediate),
            0xE5 => self.sbc(Adressing::ZeroPage),
            0xF5 => self.sbc(Adressing::ZeroPageX),
            0xED => self.sbc(Adressing::Absolute),
            0xFD => self.sbc(Adressing::AbsoluteX),
            0xF9 => self.sbc(Adressing::AbsoluteY),
            0xE1 => self.sbc(Adressing::IndirectX),
            0xF1 => self.sbc(Adressing::IndirectY),
            // SEC - Set Carry Flag
            0x38 => self.set_flag(StatusBit::Carry),
            // SED - Set Decimal Flag
            0xF8 => self.set_flag(StatusBit::Decimal),
            // SEI - Set Interrupt Disable
            0x78 => self.set_flag(StatusBit::Interrupt),
            // SLO
            0x03 => self.slo(Adressing::IndirectX),
            0x07 => self.slo(Adressing::ZeroPage),
            0x0F => self.slo(Adressing::Absolute),
            0x13 => self.slo(Adressing::IndirectY),
            0x17 => self.slo(Adressing::ZeroPageX),
            0x1B => self.slo(Adressing::AbsoluteY),
            0x1F => self.slo(Adressing::AbsoluteX),
            // SRE
            0x43 => self.sre(Adressing::IndirectX),
            0x47 => self.sre(Adressing::ZeroPage),
            0x4f => self.sre(Adressing::Absolute),
            0x53 => self.sre(Adressing::IndirectY),
            0x57 => self.sre(Adressing::ZeroPageX),
            0x5B => self.sre(Adressing::AbsoluteY),
            0x5F => self.sre(Adressing::AbsoluteX),
            // STA - Store Accumulator
            0x85 => self.mov(Adressing::ZeroPage, Adressing::A),
            0x95 => self.mov(Adressing::ZeroPageX, Adressing::A),
            0x8D => self.mov(Adressing::Absolute, Adressing::A),
            0x9D => self.mov(Adressing::AbsoluteX, Adressing::A),
            0x99 => self.mov(Adressing::AbsoluteY, Adressing::A),
            0x81 => self.mov(Adressing::IndirectX, Adressing::A),
            0x91 => self.mov(Adressing::IndirectY, Adressing::A),
            // STX - Store X Register
            0x86 => self.mov(Adressing::ZeroPage, Adressing::X),
            0x96 => self.mov(Adressing::ZeroPageY, Adressing::X),
            0x8E => self.mov(Adressing::Absolute, Adressing::X),
            // STY - Store Y Register
            0x84 => self.mov(Adressing::ZeroPage, Adressing::Y),
            0x94 => self.mov(Adressing::ZeroPageX, Adressing::Y),
            0x8C => self.mov(Adressing::Absolute, Adressing::Y),
            // SKB
            0x80 | 0x82 | 0x89 | 0xC2 | 0xE2 => self.skb(Adressing::Immediate),
            // TAX - Transfer Accumulator to X
            0xAA => self.transfer_reg_to_reg(Adressing::X, Adressing::A),
            // TAY - Transfer Accumulator to Y
            0xA8 => self.transfer_reg_to_reg(Adressing::Y, Adressing::A),
            // TSX - Transfer Stack Pointer to X
            0xBA => self.transfer_reg_to_reg(Adressing::X, Adressing::S),// self.tsx(),
            // TXA - Transfer X to Accumulator
            0x8A => self.transfer_reg_to_reg(Adressing::A, Adressing::X),
            // TXS - Transfer X to Stack Pointer
            0x9A => self.txs(),
            // TYA - Transfer Y to Accumulator
            0x98 => self.transfer_reg_to_reg(Adressing::A, Adressing::Y),
            _ => {},
        }

        //println!(" clk: {}", self.bus.clk);
    }

    pub fn new(bus: Bus) -> Self {
        let start_addr = 0xC000;//bus.read_u16(0xFFFC);
        //print!("start: 0x{:x}", start_addr);
        Self {
            a: 0,
            pc: start_addr,
            s: 0xFD,
            x: 0,
            y: 0,
            p: 0x24,
            bus,
        }
    }
}

fn main() {
    let bus = Bus::new();
    let mut cpu = Cpu::new(bus);

    //for _ in 0..8991 {
    for _ in 0..8991 {
        cpu.clock();
    }
}