// http://dendy.migera.ru/nes/g11.html
// http://www.obelisk.me.uk/6502/reference.html
// https://wiki.nesdev.com/w/index.php/CPU_addressing_modes
// http://nesdev.com/6502.txt
// https://skilldrick.github.io/easy6502/
// https://stackoverflow.com/questions/29193303/6502-emulation-proper-way-to-implement-adc-and-sbc
// https://wiki.nesdev.com/w/index.php/Emulator_tests
//mod bus;
use super::bus::{Bus};

const CARRY_BIT: u8 = 1 << 0;
const ZERO_BIT: u8 = 1 << 1;
const INTERRUPT_BIT: u8 = 1 << 2;
const DECIMAL_BIT: u8 = 1 << 3;
const BREAK_BIT: u8 = 1 << 4;
const OVERFLOW_BIT: u8 = 1 << 6;
const SIGN_BIT: u8 = 1 << 7;

type OpAddrFn = fn (&mut Cpu) -> u16;

pub struct Cpu {
    a: u8,
    pc: u16,
    s: u8,
    x: u8,
    y: u8,
    p: u8,
    bus: Bus,
    clk: u32,
}

impl Cpu {
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
            clk: 0,
        }
    }

    fn accumulator(&mut self, instruction: fn(&mut Cpu)) {
        instruction(self);
    }

    fn immediate(&mut self) -> u16 {
        let pc = self.pc;
        self.pc += 1;
        print!(" #${:02X}", self.bus.read_u8(pc));
        pc
    }

    fn zero_page(&mut self) -> u16 {
        let addr = self.fetch_u8() as u16;
        print!(" ${:02X}", addr);
        addr
    }

    fn zero_page_x(&mut self) -> u16 {
        let addr = self.fetch_u8() as u16;
        print!(" ${:02X},X", addr);
        (addr + self.x as u16) & 0xff
    }

    fn zero_page_y(&mut self) -> u16 {
        let addr = self.fetch_u8() as u16;
        print!(" ${:02X},Y", addr);
        (addr + self.y as u16) & 0xff
    }

    fn absolute(&mut self) -> u16 {
        let addr = self.fetch_u16();
        print!(" ${:04X}", addr);
        addr
    }

    fn absolute_x(&mut self) -> u16 {
        let addr = self.fetch_u16();
        print!(" ${:04X},X", addr);
        ((addr as u32) + (self.x as u32)) as u16
    }

    fn absolute_y(&mut self) -> u16 {
        let addr = self.fetch_u16();
        print!(" ${:04X},Y", addr);
        ((addr as u32) + (self.y as u32)) as u16
    }

    fn relative(&mut self) -> u16 {
        let offset = self.fetch_u8() as i8;
        let addr = (self.pc as i32 + offset as i32) as u16;
        print!(" ${:X}", addr);
        addr
    }

    fn implicit(&mut self) {
        //print!();
    }

    fn indirect(&mut self) -> u16 {
        let addr = self.fetch_u16();
        print!(" (${:04X})", addr);
        // http://forums.nesdev.com/viewtopic.php?t=1279
        // On the page barrier (xxFF) the least significant byte is ignored.
        let l = self.bus.read_u8(addr) as u16;
        let h = self.bus.read_u8((addr & 0xff00) + ((addr + 1) & 0xff)) as u16;
        let addr = (h << 8) | l;//self.bus.read_u16(addr);
        print!(" = {:04X}", addr);
        addr
    }

    fn indirect_x(&mut self) -> u16 {
        let zaddr = self.fetch_u8() as u16;
        let l = self.bus.read_u8((zaddr + self.x as u16) & 0xff) as u16;
        let h = self.bus.read_u8((zaddr + self.x as u16 + 1) & 0xff) as u16;
        let addr = h << 8 | l;
        print!(" (${:X},X) @ {:X} = {:04X}", zaddr, (self.x as u16 + zaddr) as u8, addr);
        addr
    }

    fn indirect_y(&mut self) -> u16 {
        let addr = self.fetch_u8() as u16;
        let l = self.bus.read_u8(addr & 0xff) as u16;
        let h = self.bus.read_u8((addr + 1) & 0xff) as u16;
        let addr = (h << 8) | l;
        (addr as u32 + self.y as u32) as u16
    }

    fn fetch_u8(&mut self) -> u8 {
        let byte = self.bus.read_u8(self.pc);
        self.pc += 1;
        byte
    }

    fn fetch_u16(&mut self) -> u16 {
        let data = self.bus.read_u16(self.pc);
        self.pc += 2;
        data
    }

    fn stack_push(&mut self, val: u8) {
        self.bus.write_u8(0x0100 + (self.s as u16), val);
        self.s -= 1;
    }

    fn stack_pop(&mut self) -> u8 {
        self.s += 1;
        self.bus.read_u8(0x0100 + (self.s as u16))
    }

    fn set_sign(&mut self, val: u8) {
        if (val & (1 << 7)) != 0 {
            self.p |= SIGN_BIT;
        } else {
            self.p &= !SIGN_BIT;
        }
    }

    fn if_sign(&self) -> bool {
        (self.p & SIGN_BIT) != 0
    }

    fn set_zero(&mut self, val: u8) {
        if val == 0 {
            self.p |= ZERO_BIT;
        } else {
            self.p &= !ZERO_BIT;
        }
    }

    fn if_zero(&self) -> bool {
        (self.p & ZERO_BIT) != 0
    }

    fn set_carry(&mut self, val: u8) {
        if val != 0 {
            self.p |= CARRY_BIT;
        } else {
            self.p &= !CARRY_BIT;
        }
    }

    fn if_carry(&self) -> bool {
        (self.p & CARRY_BIT) != 0
    }

    fn set_overflow(&mut self, val: u8) {
        if val != 0 {
            self.p |= OVERFLOW_BIT;
        } else {
            self.p &= !OVERFLOW_BIT;
        }
    }

    fn if_overflow(&self) -> bool {
        (self.p & OVERFLOW_BIT) != 0
    }

    fn set_interrupt(&mut self, val: u8) {
        if val != 0 {
            self.p |= INTERRUPT_BIT;
        } else {
            self.p &= !OVERFLOW_BIT;
        }
    }

    fn if_interrupt(&self) -> bool {
        (self.p & INTERRUPT_BIT) != 0
    }

    fn set_break(&mut self, val: u8) {
        if val != 0 {
            self.p |= BREAK_BIT;
        } else {
            self.p &= !BREAK_BIT;
        }
    }

    fn if_break(&self) -> bool {
        (self.p & BREAK_BIT) != 0
    }

    fn set_decimal(&mut self, val: u8) {
        if val != 0 {
            self.p |= DECIMAL_BIT;
        } else {
            self.p &= !DECIMAL_BIT;
        }
    }

    fn if_decimal(&self) -> bool {
        (self.p & DECIMAL_BIT) != 0
    }

    fn adc(&mut self, addr: OpAddrFn) {
        // ADC - Add with Carry
        print!("ADC");
        let addr = addr(self);
        let src = self.bus.read_u8(addr);
        let mut temp = (self.a as u16) + (src as u16);
        
        if self.if_carry() {
            temp += 1;
        }

        self.set_zero(temp as u8);
        /*
        if self.if_decimal() {
            if ((self.a & 0xf) + (src & 0xf) + (if self.if_carry() {1} else {0})) > 9 {
                temp += 6;
            }

            self.set_sign(temp as u8);
            self.set_overflow(!(self.a ^ src) & (self.a ^ (temp as u8)) & 0x80);
            
            if temp > 0x99 {
                temp += 96;
            }
            
            self.set_carry(if temp > 0x99 {1} else {0});
        } else {
        */
            self.set_sign(temp as u8);
            // The overflow flag is set when
            // the sign of the addends is the same and
            // differs from the sign of the sum
            self.set_overflow(!(self.a ^ src) & (self.a ^ (temp as u8)) & 0x80);
            self.set_carry(if temp > 0xFF {1} else {0});
        //}

        self.a = temp as u8;
    }

    fn and(&mut self, addr: OpAddrFn) {
        // AND - Logical AND
        print!("AND");
        let addr = addr(self);
        self.a &= self.bus.read_u8(addr);
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn asl(&mut self, addr: OpAddrFn) {
        // ASL - Arithmetic Shift Left
        print!("ASL");
        let addr = addr(self);
        let mut src = self.bus.read_u8(addr);
        self.set_carry(src & 0x80);
        src <<= 1;
        self.set_sign(src);
        self.set_zero(src);
        self.bus.write_u8(addr, src);
    }

    fn asl_acc(&mut self) {
        // ASL - Arithmetic Shift Left
        print!("ASL A");
        self.set_carry(self.a & 0x80);
        self.a <<= 1;
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn bcc(&mut self, addr: OpAddrFn) {
        // BCC - Branch if Carry Clear
        print!("BCC");
        let new_pc = addr(self);

        if self.if_carry() {
            return;
        }

        if (self.pc & 0xFF00) != (new_pc & 0xFF00) {
            self.clk += 2;
        } else {
            self.clk += 1;
        }
        
        self.pc = new_pc;
    }

    fn bcs(&mut self, addr: OpAddrFn) {
        // BCS - Branch if Carry Set
        print!("BCS");
        let new_pc = addr(self);

        if !self.if_carry() {
            return;
        }

        if (self.pc & 0xFF00) != (new_pc & 0xFF00) {
            self.clk += 2;
        } else {
            self.clk += 1;
        }
        
        self.pc = new_pc;
    }

    fn beq(&mut self, addr: OpAddrFn) {
        // BEQ - Branch if Equal
        print!("BEQ");
        let new_pc = addr(self);

        if !self.if_zero() {
            return;
        }

        if (self.pc & 0xFF00) != (new_pc & 0xFF00) {
            self.clk += 2;
        } else {
            self.clk += 1;
        }
        
        self.pc = new_pc;
    }

    fn bit(&mut self, addr: OpAddrFn) {
        // BIT - Bit Test
        print!("BIT");
        let addr = addr(self);
        let src = self.bus.read_u8(addr);
        self.set_sign(src);
        self.set_overflow(0x40 & src);
        self.set_zero(src & self.a);
    }

    fn bmi(&mut self, addr: OpAddrFn) {
        // BMI - Branch if Minus
        print!("BMI");
        let new_pc = addr(self);

        if !self.if_sign() {
            return;
        }
        
        if (self.pc & 0xFF00) != (new_pc & 0xFF00) {
            self.clk += 2;
        } else {
            self.clk += 1;
        }
        
        self.pc = new_pc;
    }

    fn bne(&mut self, addr: OpAddrFn) {
        // BNE - Branch if Not Equal
        print!("BNE");
        let new_pc = addr(self);

        if self.if_zero() {
            return;
        }
        
        if (self.pc & 0xFF00) != (new_pc & 0xFF00) {
            self.clk += 2;
        } else {
            self.clk += 1;
        }
        
        self.pc = new_pc;
    }

    fn bpl(&mut self, addr: OpAddrFn) {
        // BPL - Branch if Positive
        print!("BPL");
        let new_pc = addr(self);

        if self.if_sign() {
            return;
        }

        if (self.pc & 0xFF00) != (new_pc & 0xFF00) {
            self.clk += 2;
        } else {
            self.clk += 1;
        }
        
        self.pc = new_pc;
    }

    fn brk(&mut self) {
        // BRK - Force Interrupt
        print!("BRK");
        self.pc += 1;
        self.stack_push((self.pc >> 8) as u8);
        self.stack_push(self.pc as u8);
        self.set_break(1);
        self.stack_push(self.s);
        self.set_interrupt(1);
        self.pc = self.bus.read_u16(0xFFFE);
    }

    fn bvc(&mut self, addr: OpAddrFn) {
        // BVC - Branch if Overflow Clear
        print!("BVC");
        let new_pc = addr(self);

        if self.if_overflow() {
            return;
        }
        
        if (self.pc & 0xFF00) != (new_pc & 0xFF00) {
            self.clk += 2;
        } else {
            self.clk += 1;
        }
        
        self.pc = new_pc;
    }

    fn bvs(&mut self, addr: OpAddrFn) {
        // BVS - Branch if Overflow Set
        print!("BVS");
        let new_pc = addr(self);

        if !self.if_overflow() {
            return;
        }

        if (self.pc & 0xFF00) != (new_pc & 0xFF00) {
            self.clk += 2;
        } else {
            self.clk += 1;
        }
        
        self.pc = new_pc;
    }

    fn clc(&mut self) {
        // CLC - Clear Carry Flag
        print!("CLC");
        self.set_carry(0);
    }

    fn cld(&mut self) {
        // CLD - Clear Decimal Mode
        print!("CLD");
        self.set_decimal(0);
    }

    fn cli(&mut self) {
        // CLI - Clear Interrupt Disable
        print!("CLI");
        self.set_interrupt(0);
    }

    fn clv(&mut self) {
        // CLV - Clear Overflow Flag
        print!("CLV");
        self.set_overflow(0);
    }

    fn cmp(&mut self, addr: OpAddrFn) {
        // CMP - Compare
        print!("CMP");
        let addr = addr(self);
        let m = self.bus.read_u8(addr);
        let src = self.a as i32 - (m as i8) as i32;
        self.set_carry(if self.a >= m {1} else {0});
        self.set_sign(src as u8);
        self.set_zero(src as u8);
    }

    fn cpx(&mut self, addr: OpAddrFn) {
        // CPX - Compare X Register
        print!("CPX");
        let addr = addr(self);
        let m = self.bus.read_u8(addr);
        let src = self.x as i32 - (m as i8) as i32;
        self.set_carry(if self.x >= m {1} else {0});
        self.set_sign(src as u8);
        self.set_zero(src as u8);
    }

    fn cpy(&mut self, addr: OpAddrFn) {
        // CPY - Compare Y Register
        print!("CPY");
        let addr = addr(self);
        let m = self.bus.read_u8(addr);
        let src = self.y as i32 - (m as i8) as i32;
        self.set_carry(if self.y >= m {1} else {0});
        self.set_sign(src as u8);
        self.set_zero(src as u8);
    }

    fn dcp(&mut self, addr: OpAddrFn) {
        // DCP - Equivalent to DEC value then CMP value,
        // except supporting more addressing modes. 
        // LDA #$FF followed by DCP can be used to check 
        // if the decrement underflows, which is useful for multi-byte decrements.
        print!("DCP");
        let addr = addr(self);
        let m = self.bus.read_u8(addr);
        let m = ((m as i16) - 1) as u8;
        self.bus.write_u8(addr, m);
        let src = self.a as i32 - (m as i8) as i32;
        self.set_carry(if self.a >= m {1} else {0});
        self.set_sign(src as u8);
        self.set_zero(src as u8);
    }

    fn dec(&mut self, addr: OpAddrFn) {
        // DEC - Decrement Memory
        print!("DEC");
        let addr = addr(self);
        let src = self.bus.read_u8(addr);
        let src = ((src as i16) - 1) as u8;
        self.set_sign(src);
        self.set_zero(src);
        self.bus.write_u8(addr, src);
    }

    fn dex(&mut self) {
        // DEX - Decrement X Register
        print!("DEX");
        self.x = ((self.x as i16) - 1) as u8;
        self.set_sign(self.x);
        self.set_zero(self.x);
    }

    fn dey(&mut self) {
        // DEY - Decrement Y Register
        print!("DEY");
        self.y = ((self.y as i16) - 1) as u8;
        self.set_sign(self.y);
        self.set_zero(self.y);
    }

    fn eor(&mut self, addr: OpAddrFn) {
        // EOR - Exclusive OR
        // A,Z,N = A^M
        print!("EOR");
        let addr = addr(self);
        let src = self.bus.read_u8(addr);
        self.a ^= src;
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn ign(&mut self, addr: OpAddrFn) {
        // CPY - Compare Y Register
        print!("IGN");
        let _ = addr(self);
    }

    fn inc(&mut self, addr: OpAddrFn) {
        // INC - Increment Memory
        print!("INC");
        let addr = addr(self);
        let src = self.bus.read_u8(addr);
        let src = ((src as u16) + 1) as u8;
        self.set_sign(src);
        self.set_zero(src);
        self.bus.write_u8(addr, src);
    }

    fn inx(&mut self) {
        // INX - Increment X Register
        print!("INX");
        self.x = ((self.x as u16) + 1) as u8;
        self.set_sign(self.x);
        self.set_zero(self.x);
    }

    fn iny(&mut self) {
        // INY - Increment Y Register
        print!("INY");
        self.y = ((self.y as u16) + 1) as u8;
        self.set_sign(self.y);
        self.set_zero(self.y);
    }

    fn isc(&mut self, addr: OpAddrFn) {
        // ISC - Equivalent to INC value then SBC value, except supporting more addressing modes.
        print!("ISC");
        let addr = addr(self);
        let m = self.bus.read_u8(addr);
        let m = ((m as u16) + 1) as u8;
        self.bus.write_u8(addr, m);

        let temp = (self.a as i32) - (m as i32) - (if self.if_carry() {0} else {1});
        self.set_sign(temp as u8);
        self.set_zero(temp as u8);	/* Sign and Zero are invalid in decimal mode */

        self.set_overflow(((self.a ^ (temp as u8)) & 0x80) & ((self.a ^ m) & 0x80));
        self.set_carry(if self.a >= m {1} else {0});
        self.a = temp as u8;
    }

    fn jmp(&mut self, addr: OpAddrFn) {
        // JMP - Jump
        print!("JMP");
        let addr = addr(self);
        self.pc = addr;// self.bus.read_u16(addr);
    }

    fn jsr(&mut self, addr: OpAddrFn) {
        // JSR - Jump to Subroutine
        print!("JSR");
        let addr = addr(self);
        self.pc -= 1;
        self.stack_push((self.pc >> 8) as u8);
        self.stack_push(self.pc as u8);
        self.pc = addr;//self.bus.read_u16(addr);
    }

    fn lax(&mut self, addr: OpAddrFn) {
        // LAX - Shortcut for LDA value then TAX
        print!("LAX");
        let addr = addr(self);
        self.a = self.bus.read_u8(addr);
        self.set_sign(self.a);
        self.set_zero(self.a);
        self.x = self.a;
    }

    fn lda(&mut self, addr: OpAddrFn) {
        // LDA - Load Accumulator
        print!("LDA");
        let addr = addr(self);
        self.a = self.bus.read_u8(addr);
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn ldx(&mut self, addr: OpAddrFn) {
        // LDX - Load X Register
        print!("LDX");
        let addr = addr(self);
        self.x = self.bus.read_u8(addr);
        self.set_sign(self.x);
        self.set_zero(self.x);
    }

    fn ldy(&mut self, addr: OpAddrFn) {
        // LDY - Load Y Register
        print!("LDY");
        let addr = addr(self);
        self.y = self.bus.read_u8(addr);
        self.set_sign(self.y);
        self.set_zero(self.y);
    }

    fn lsr(&mut self, addr: OpAddrFn) {
        // LSR - Logical Shift Right
        print!("LSR");
        let addr = addr(self);
        let mut src = self.bus.read_u8(addr);
        self.set_carry(src & 0x01);
        src >>= 1;
        self.set_sign(src);
        self.set_zero(src);
        self.bus.write_u8(addr, src);
    }

    fn lsr_acc(&mut self) {
        // LSR - Logical Shift Right
        print!("LSR A");
        self.set_carry(self.a & 0x01);
        self.a >>= 1;
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn nop(&mut self) {
        print!("NOP");
        // NOP - No Operation
    }

    fn ora(&mut self, addr: OpAddrFn) {
        // ORA - Logical Inclusive OR
        print!("ORA");
        let addr = addr(self);
        self.a |= self.bus.read_u8(addr);
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn pha(&mut self) {
        // PHA - Push Accumulator
        print!("PHA");
        self.stack_push(self.a);
    }

    fn php(&mut self) {
        // PHP - Push Processor Status
        print!("PHP");
        self.stack_push(self.p | BREAK_BIT);
    }

    fn pla(&mut self) {
        // PLA - Pull Accumulator
        print!("PLA");
        self.a = self.stack_pop();
        //self.a |= BREAK_BIT | (1 << 5); // ?????
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn plp(&mut self) {
        print!("PLP");
        // PLP - Pull Processor Status
        self.p = self.stack_pop();
        self.p &= !BREAK_BIT;
        self.p |= 1 << 5;
    }

    fn rla(&mut self, addr: OpAddrFn) {
        // RLA - Equivalent to ROL value then AND value,
        // except supporting more addressing modes.
        // LDA #$FF followed by RLA is an efficient way
        // to rotate a variable while also loading it in A.
        print!("RLA");
        let addr = addr(self);
        let src = self.bus.read_u8(addr);
        let mut res = src << 1;

        if self.if_carry() {
            res |= 0x01;
        }

        self.bus.write_u8(addr, res);
        self.set_carry(src & 0x80);
        self.a &= res;
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn rol(&mut self, addr: OpAddrFn) {
        // ROL - Rotate Left
        print!("ROL");
        let addr = addr(self);
        let src = self.bus.read_u8(addr);
        let mut res = src << 1;

        if self.if_carry() {
            res |= 0x01;
        }

        self.set_carry(src & 0x80);
        self.set_sign(res);
        self.set_zero(res);
        self.bus.write_u8(addr, res);
    }

    fn rol_acc(&mut self) {
        // ROL - Rotate Left
        print!("ROL");
        let src = self.a;
        let mut res = src << 1;

        if self.if_carry() {
            res |= 0x01;
        }

        self.set_carry(src & 0x80);
        self.set_sign(res);
        self.set_zero(res);
        self.a = res;
    }

    fn ror(&mut self, addr: OpAddrFn) {
        // ROR - Rotate Right
        print!("ROR");
        let addr = addr(self);
        let src = self.bus.read_u8(addr);
        let mut res = src >> 1;

        if self.if_carry() {
            res |= 0x80;
        }

        self.set_carry(src & 0x01);
        self.set_sign(res);
        self.set_zero(res);
        self.bus.write_u8(addr, res);
    }

    fn ror_acc(&mut self) {
        // ROR - Rotate Right
        print!("ROR");
        let src = self.a;
        let mut res = src >> 1;

        if self.if_carry() {
            res |= 0x80;
        }

        self.set_carry(src & 0x01);
        self.set_sign(res);
        self.set_zero(res);
        self.a = res;
    }

    fn rra(&mut self, addr: OpAddrFn) {
        // RRA - Equivalent to ROR value then ADC value,
        // except supporting more addressing modes.
        // Essentially this computes A + value / 2,
        // where value is 9-bit and the division is rounded up.
        print!("RRA");
        let addr = addr(self);
        let src = self.bus.read_u8(addr);
        let mut res = src >> 1;

        if self.if_carry() {
            res |= 0x80;
        }

        self.set_carry(src & 0x01);
        self.bus.write_u8(addr, res);

        let mut temp = (self.a as u16) + (res as u16);
        
        if self.if_carry() {
            temp += 1;
        }

        self.set_zero(temp as u8);
        self.set_sign(temp as u8);

        self.set_overflow(!(self.a ^ res) & (self.a ^ (temp as u8)) & 0x80);
        self.set_carry(if temp > 0xFF {1} else {0});

        self.a = temp as u8;
    }

    fn rti(&mut self) {
        // RTI - Return from Interrupt
        print!("RTI");
        self.p = self.stack_pop() | (1 << 5);
        let l = self.stack_pop() as u16;
        let h = self.stack_pop() as u16;
        self.pc = (h << 8) | l;
    }

    fn rts(&mut self) {
        // RTS - Return from Subroutine
        print!("RTS");
        let l = self.stack_pop() as u16;
        let h = self.stack_pop() as u16;
        self.pc = ((h << 8) | l) + 1;
    }

    fn sax(&mut self, addr: OpAddrFn) {
        // SAX - Stores the bitwise AND of A and X. As with STA and STX, no flags are affected.
        print!("SAX");
        let addr = addr(self);
        self.bus.write_u8(addr, self.a & self.x);
    }

    fn sbc(&mut self, addr: OpAddrFn) {
        // SBC - Subtract with Carry
        print!("SBC");
        let addr = addr(self);
        let src = self.bus.read_u8(addr);
        let temp = (self.a as i32) - (src as i32) - (if self.if_carry() {0} else {1});
        self.set_sign(temp as u8);
        self.set_zero(temp as u8);	/* Sign and Zero are invalid in decimal mode */

        self.set_overflow(((self.a ^ (temp as u8)) & 0x80) & ((self.a ^ src) & 0x80));
        /*
        if self.if_decimal() {
            if  ((self.a & 0xf) - (if self.if_carry() {0} else {1})) < (src & 0xf) {
                temp -= 6;
            }

            if temp > 0x99 {
                temp -= 0x60;
            }
        }
        */
        self.set_carry(if self.a >= src {1} else {0});
        self.a = temp as u8;
    }

    fn sec(&mut self) {
        // SEC - Set Carry Flag
        print!("SEC");
        self.set_carry(1);
    }

    fn sed(&mut self) {
        // SED - Set Decimal Flag
        print!("SED");
        self.set_decimal(1);
    }

    fn sei(&mut self) {
        print!("SEI");
        // SEI - Set Interrupt Disable
        self.set_interrupt(1);
    }

    fn slo(&mut self, addr: OpAddrFn) {
        // SLO - Equivalent to ASL value then ORA value,
        // except supporting more addressing modes.
        // LDA #0 followed by SLO is an efficient way
        // to shift a variable while also loading it in A.
        print!("SLO");
        let addr = addr(self);
        let m = self.bus.read_u8(addr);
        self.set_carry(m & (1 << 7));
        let m = m << 1;
        self.bus.write_u8(addr, m);
        self.a |= m;
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn sre(&mut self, addr: OpAddrFn) {
        // SRE - Equivalent to LSR value then EOR value,
        // except supporting more addressing modes.
        // LDA #0 followed by SRE is an efficient way
        // to shift a variable while also loading it in A.
        print!("SRE");
        let addr = addr(self);
        let mut src = self.bus.read_u8(addr);
        self.set_carry(src & 0x01);
        src >>= 1;
        self.bus.write_u8(addr, src);

        self.a ^= src;
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn sta(&mut self, addr: OpAddrFn) {
        // STA - Store Accumulator
        print!("STA");
        let addr = addr(self);
        self.bus.write_u8(addr, self.a);
    }

    fn stx(&mut self, addr: OpAddrFn) {
        // STX - Store X Register
        print!("STX");
        let addr = addr(self);
        self.bus.write_u8(addr, self.x);
    }

    fn sty(&mut self, addr: OpAddrFn) {
        // STY - Store Y Register
        print!("STY");
        let addr = addr(self);
        self.bus.write_u8(addr, self.y);
    }

    fn skb(&mut self, addr: OpAddrFn) {
        // STY - Store Y Register
        print!("SKB");
        let addr = addr(self);
        let _ = self.bus.read_u8(addr);
        //self.set_sign(src);
        //self.set_zero(src);
        //self.set_overflow(src);
    }

    fn tax(&mut self) {
        // TAX - Transfer Accumulator to X
        print!("TAX");
        self.x = self.a;
        self.set_sign(self.x);
        self.set_zero(self.x);
    }

    fn tay(&mut self) {
        // TAY - Transfer Accumulator to Y
        print!("TAY");
        self.y = self.a;
        self.set_sign(self.y);
        self.set_zero(self.y);
    }

    fn tsx(&mut self) {
        // TSX - Transfer Stack Pointer to X
        print!("TSX");
        self.x = self.s;
        self.set_sign(self.s);
        self.set_zero(self.s);
    }

    fn txa(&mut self) {
        // TXA - Transfer X to Accumulator
        print!("TXA");
        self.a = self.x;
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn txs(&mut self) {
        // TXS - Transfer X to Stack Pointer
        print!("TXS");
        self.s = self.x;
    }

    fn tya(&mut self) {
        // TYA - Transfer Y to Accumulator
        print!("TYA");
        self.a = self.y;
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn print_state(&self) {
        print!("A:{:02X} ", self.a);
        print!("X:{:02X} ", self.x);
        print!("Y:{:02X} ", self.y);
        print!("P:{:02X} ", self.p);
        print!("SP:{:02X}  ", self.s);
        //print!("PPU:  0, 21 CYC:7");
        
        //P:24 SP:FD PPU:  0, 21 CYC:7
    }

    pub fn clock(&mut self) {
        /*
        if self.clk != 0 {
            self.clk -= 1;
            return;
        }
        */
        print!("{:02X}  ", self.pc);
        self.print_state();

        let instruction = self.fetch_u8();
        //print!("read instruction {:2x}", instruction);
        match instruction {
            // ADC - Add with Carry
            0x69 => self.adc(Cpu::immediate),
            0x65 => self.adc(Cpu::zero_page),
            0x75 => self.adc(Cpu::zero_page_x),
            0x6D => self.adc(Cpu::absolute),
            0x7D => self.adc(Cpu::absolute_x),
            0x79 => self.adc(Cpu::absolute_y),
            0x61 => self.adc(Cpu::indirect_x),
            0x71 => self.adc(Cpu::indirect_y),
            // AND - Logical AND
            0x29 => self.and(Cpu::immediate),
            0x25 => self.and(Cpu::zero_page),
            0x35 => self.and(Cpu::zero_page_x),
            0x2D => self.and(Cpu::absolute),
            0x3D => self.and(Cpu::absolute_x),
            0x39 => self.and(Cpu::absolute_y),
            0x21 => self.and(Cpu::indirect_x),
            0x31 => self.and(Cpu::indirect_y),
            // ASL - Arithmetic Shift Left
            0x0A => self.asl_acc(),
            0x06 => self.asl(Cpu::zero_page),
            0x16 => self.asl(Cpu::zero_page_x),
            0x0E => self.asl(Cpu::absolute),
            0x1E => self.asl(Cpu::absolute_x),
            // BCC - Branch if Carry Clear
            0x90 => self.bcc(Cpu::relative),
            // BCS - Branch if Carry Set
            0xB0 => self.bcs(Cpu::relative),
            // BEQ - Branch if Equal
            0xF0 => self.beq(Cpu::relative),
            // BIT - Bit Test
            0x24 => self.bit(Cpu::zero_page),
            0x2C => self.bit(Cpu::absolute),
            // BMI - Branch if Minus
            0x30 => self.bmi(Cpu::relative),
            // BNE - Branch if Not Equal
            0xD0 => self.bne(Cpu::relative),
            // BPL - Branch if Positive
            0x10 => self.bpl(Cpu::relative),
            // BRK - Force Interrupt
            0x00 => self.brk(),
            // BVC - Branch if Overflow Clear
            0x50 => self. bvc(Cpu::relative),
            // BVS - Branch if Overflow Set
            0x70 => self.bvs(Cpu::relative),
            // CLC - Clear Carry Flag
            0x18 => self.clc(),
            // CLD - Clear Decimal Mode
            0xD8 => self.cld(),
            // CLI - Clear Interrupt Disable
            0x58 => self.cli(),
            // CLV - Clear Overflow Flag
            0xB8 => self.clv(),
            // CMP - Compare
            0xC9 => self.cmp(Cpu::immediate),
            0xC5 => self.cmp(Cpu::zero_page),
            0xD5 => self.cmp(Cpu::zero_page_x),
            0xCD => self.cmp(Cpu::absolute),
            0xDD => self.cmp(Cpu::absolute_x),
            0xD9 => self.cmp(Cpu::absolute_y),
            0xC1 => self.cmp(Cpu::indirect_x),
            0xD1 => self.cmp(Cpu::indirect_y),
            // CPX - Compare X Register
            0xE0 => self.cpx(Cpu::immediate),
            0xE4 => self.cpx(Cpu::zero_page),
            0xEC => self.cpx(Cpu::absolute),
            // CPY - Compare Y Register
            0xC0 => self.cpy(Cpu::immediate),
            0xC4 => self.cpy(Cpu::zero_page),
            0xCC => self.cpy(Cpu::absolute),
            // DCP
            0xC3 => self.dcp(Cpu::indirect_x), // DCP (d,X) ($C3 dd; 8 cycles)
            0xC7 => self.dcp(Cpu::zero_page), // DCP d ($C7 dd; 5 cycles)
            0xCF => self.dcp(Cpu::absolute), // DCP a ($CF aa aa; 6 cycles)
            0xD3 => self.dcp(Cpu::indirect_y), // DCP (d),Y ($D3 dd; 8 cycles)
            0xD7 => self.dcp(Cpu::zero_page_x), // DCP d,X ($D7 dd; 6 cycles)
            0xDB => self.dcp(Cpu::absolute_y), // DCP a,Y ($DB aa aa; 7 cycles)
            0xDF => self.dcp(Cpu::absolute_x), // DCP a,X ($DF aa aa; 7 cycles)
            // DEC - Decrement Memory
            0xC6 => self.dec(Cpu::zero_page),
            0xD6 => self.dec(Cpu::zero_page_x),
            0xCE => self.dec(Cpu::absolute),
            0xDE => self.dec(Cpu::absolute_x),
            // DEX - Decrement X Register
            0xCA => self.dex(),
            // DEY - Decrement Y Register
            0x88 => self.dey(),
            // EOR - Exclusive OR
            0x49 => self.eor(Cpu::immediate),
            0x45 => self.eor(Cpu::zero_page),
            0x55 => self.eor(Cpu::zero_page_x),
            0x4D => self.eor(Cpu::absolute),
            0x5D => self.eor(Cpu::absolute_x),
            0x59 => self.eor(Cpu::absolute_y),
            0x41 => self.eor(Cpu::indirect_x),
            0x51 => self.eor(Cpu::indirect_y),
            // IGN
            0x04 | 0x44 | 0x64 => self.ign(Cpu::zero_page),
            0x0C => self.ign(Cpu::absolute),
            0x14 | 0x34 | 0x54 | 0x74 | 0xD4 | 0xF4 => self.ign(Cpu::zero_page_x),
            0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => self.ign(Cpu::absolute_x),
            // INC - Increment Memory
            0xE6 => self.inc(Cpu::zero_page),
            0xF6 => self.inc(Cpu::zero_page_x),
            0xEE => self.inc(Cpu::absolute),
            0xFE => self.inc(Cpu::absolute_x),
            // INX - Increment X Register
            0xE8 => self.inx(),
            // INY - Increment Y Register
            0xC8 => self.iny(),
            // ISC
            0xE3 => self.isc(Cpu::indirect_x), // ISC (d,X) ($E3 dd; 8 cycles)
            0xE7 => self.isc(Cpu::zero_page), // ISC d ($E7 dd; 5 cycles)
            0xEF => self.isc(Cpu::absolute),  // ISC a ($EF aa aa; 6 cycles)
            0xF3 => self.isc(Cpu::indirect_y), // ISC (d),Y ($F3 dd; 8 cycles)
            0xF7 => self.isc(Cpu::zero_page_x), // ISC d,X ($F7 dd; 6 cycles)
            0xFB => self.isc(Cpu::absolute_y), // ISC a,Y ($FB aa aa; 7 cycles)
            0xFF => self.isc(Cpu::absolute_x), // ISC a,X ($FF aa aa; 7 cycles)
            // JMP - Jump
            0x4C => self.jmp(Cpu::absolute),
            0x6C => self.jmp(Cpu::indirect),
            // JSR - Jump to Subroutine
            0x20 => self.jsr(Cpu::absolute),
            // LAX
            0xA3 => self.lax(Cpu::indirect_x),
            0xA7 => self.lax(Cpu::zero_page), //LAX d ($A7 dd; 3 cycles)
            0xAF => self.lax(Cpu::absolute), // LAX a ($AF aa aa; 4 cycles)
            0xB3 => self.lax(Cpu::indirect_y), // LAX (d),Y ($B3 dd; 5 cycles)
            0xB7 => self.lax(Cpu::zero_page_y), // LAX d,Y ($B7 dd; 4 cycles)
            0xBF => self.lax(Cpu::absolute_y), // LAX a,Y ($BF aa aa; 4 cycles)
            // LDA - Load Accumulator
            0xA9 => self.lda(Cpu::immediate),
            0xA5 => self.lda(Cpu::zero_page),
            0xB5 => self.lda(Cpu::zero_page_x),
            0xAD => self.lda(Cpu::absolute),
            0xBD => self.lda(Cpu::absolute_x),
            0xB9 => self.lda(Cpu::absolute_y),
            0xA1 => self.lda(Cpu::indirect_x),
            0xB1 => self.lda(Cpu::indirect_y),
            // LDX - Load X Register
            0xA2 => self.ldx(Cpu::immediate),
            0xA6 => self.ldx(Cpu::zero_page),
            0xB6 => self.ldx(Cpu::zero_page_y),
            0xAE => self.ldx(Cpu::absolute),
            0xBE => self.ldx(Cpu::absolute_y),
            // LDY - Load Y Register
            0xA0 => self.ldy(Cpu::immediate),
            0xA4 => self.ldy(Cpu::zero_page),
            0xB4 => self.ldy(Cpu::zero_page_x),
            0xAC => self.ldy(Cpu::absolute),
            0xBC => self.ldy(Cpu::absolute_x),
            // LSR - Logical Shift Right
            0x4A => self.lsr_acc(),
            0x46 => self.lsr(Cpu::zero_page),
            0x56 => self.lsr(Cpu::zero_page_x),
            0x4E => self.lsr(Cpu::absolute),
            0x5E => self.lsr(Cpu::absolute_x),
            // NOP - No Operation
            0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xEA | 0xFA => self.nop(),
            // ORA - Logical Inclusive OR
            0x09 => self.ora(Cpu::immediate),
            0x05 => self.ora(Cpu::zero_page),
            0x15 => self.ora(Cpu::zero_page_x),
            0x0D => self.ora(Cpu::absolute),
            0x1D => self.ora(Cpu::absolute_x),
            0x19 => self.ora(Cpu::absolute_y),
            0x01 => self.ora(Cpu::indirect_x),
            0x11 => self.ora(Cpu::indirect_y),
            // PHA - Push Accumulator
            0x48 => self.pha(),
            // PHP - Push Processor Status
            0x08 => self.php(),
            // PLA - Pull Accumulator
            0x68 => self.pla(),
            // PLP - Pull Processor Status
            0x28 => self.plp(),
            // RLA
            0x23 => self.rla(Cpu::indirect_x), // RLA (d,X) ($23 dd; 8 cycles)
            0x27 => self.rla(Cpu::zero_page), // RLA d ($27 dd; 5 cycles)
            0x2F => self.rla(Cpu::absolute), // RLA a ($2F aa aa; 6 cycles)
            0x33 => self.rla(Cpu::indirect_y), // RLA (d),Y ($33 dd; 8 cycles)
            0x37 => self.rla(Cpu::zero_page_x), // RLA d,X ($37 dd; 6 cycles)
            0x3B => self.rla(Cpu::absolute_y), // RLA a,Y ($3B aa aa; 7 cycles)
            0x3F => self.rla(Cpu::absolute_x), // RLA a,X ($3F aa aa; 7 cycles)
            // ROL - Rotate Left
            0x2A => self.rol_acc(),
            0x26 => self.rol(Cpu::zero_page),
            0x36 => self.rol(Cpu::zero_page_x),
            0x2E => self.rol(Cpu::absolute),
            0x3E => self.rol(Cpu::absolute_x),
            // ROR - Rotate Right
            0x6A => self.ror_acc(),
            0x66 => self.ror(Cpu::zero_page),
            0x76 => self.ror(Cpu::zero_page_x),
            0x6E => self.ror(Cpu::absolute),
            0x7E => self.ror(Cpu::absolute_x),
            // RRA
            0x63 => self.rra(Cpu::indirect_x), // RRA (d,X) ($63 dd; 8 cycles)
            0x67 => self.rra(Cpu::zero_page), // RRA d ($67 dd; 5 cycles)
            0x6F => self.rra(Cpu::absolute), // RRA a ($6F aa aa; 6 cycles)
            0x73 => self.rra(Cpu::indirect_y), // RRA (d),Y ($73 dd; 8 cycles)
            0x77 => self.rra(Cpu::zero_page_x), // RRA d,X ($77 dd; 6 cycles)
            0x7B => self.rra(Cpu::absolute_y), // RRA a,Y ($7B aa aa; 7 cycles)
            0x7F => self.rra(Cpu::absolute_x), // RRA a,X ($7F aa aa; 7 cycles)
            // RTI - Return from Interrupt
            0x40 => self.rti(),
            // RTS - Return from Subroutine
            0x60 => self.rts(),
            // SAX
            0x83 => self.sax(Cpu::indirect_x), // SAX (d,X) ($83 dd; 6 cycles)
            0x87 => self.sax(Cpu::zero_page), // SAX d ($87 dd; 3 cycles)
            0x8F => self.sax(Cpu::absolute), // SAX a ($8F aa aa; 4 cycles)
            0x97 => self.sax(Cpu::zero_page_y), // SAX d,Y ($97 dd; 4 cycles)
            // SBC - Subtract with Carry
            0xE9 | 0xEB => self.sbc(Cpu::immediate),
            0xE5 => self.sbc(Cpu::zero_page),
            0xF5 => self.sbc(Cpu::zero_page_x),
            0xED => self.sbc(Cpu::absolute),
            0xFD => self.sbc(Cpu::absolute_x),
            0xF9 => self.sbc(Cpu::absolute_y),
            0xE1 => self.sbc(Cpu::indirect_x),
            0xF1 => self.sbc(Cpu::indirect_y),
            // SEC - Set Carry Flag
            0x38 => self.sec(),
            // SED - Set Decimal Flag
            0xF8 => self.sed(),
            // SEI - Set Interrupt Disable
            0x78 => self.sei(),
            // SLO
            0x03 => self.slo(Cpu::indirect_x), // SLO (d,X) ($03 dd; 8 cycles)
            0x07 => self.slo(Cpu::zero_page), // SLO d ($07 dd; 5 cycles)
            0x0F => self.slo(Cpu::absolute), // SLO a ($0F aa aa; 6 cycles)
            0x13 => self.slo(Cpu::indirect_y), // SLO (d),Y ($13 dd; 8 cycles)
            0x17 => self.slo(Cpu::zero_page_x), // SLO d,X ($17 dd; 6 cycles)
            0x1B => self.slo(Cpu::absolute_y), // SLO a,Y ($1B aa aa; 7 cycles)
            0x1F => self.slo(Cpu::absolute_x), // SLO a,X ($1F aa aa; 7 cycles)
            // SRE
            0x43 => self.sre(Cpu::indirect_x), // SRE (d,X) ($43 dd; 8 cycles)
            0x47 => self.sre(Cpu::zero_page), // SRE d ($47 dd; 5 cycles)
            0x4f => self.sre(Cpu::absolute), // SRE a ($4F aa aa; 6 cycles)
            0x53 => self.sre(Cpu::indirect_y), // SRE (d),Y ($53 dd; 8 cycles)
            0x57 => self.sre(Cpu::zero_page_x), // SRE d,X ($57 dd; 6 cycles)
            0x5B => self.sre(Cpu::absolute_y), // SRE a,Y ($5B aa aa; 7 cycles)
            0x5F => self.sre(Cpu::absolute_x), // SRE a,X ($5F aa aa; 7 cycles)
            // STA - Store Accumulator
            0x85 => self.sta(Cpu::zero_page),
            0x95 => self.sta(Cpu::zero_page_x),
            0x8D => self.sta(Cpu::absolute),
            0x9D => self.sta(Cpu::absolute_x),
            0x99 => self.sta(Cpu::absolute_y),
            0x81 => self.sta(Cpu::indirect_x),
            0x91 => self.sta(Cpu::indirect_y),
            // STX - Store X Register
            0x86 => self.stx(Cpu::zero_page),
            0x96 => self.stx(Cpu::zero_page_y),
            0x8E => self.stx(Cpu::absolute),
            // STY - Store Y Register
            0x84 => self.sty(Cpu::zero_page),
            0x94 => self.sty(Cpu::zero_page_x),
            0x8C => self.sty(Cpu::absolute),
            // SKB
            0x80 | 0x82 | 0x89 | 0xC2 | 0xE2 => self.skb(Cpu::immediate),
            // TAX - Transfer Accumulator to X
            0xAA => self.tax(),
            // TAY - Transfer Accumulator to Y
            0xA8 => self.tay(),
            // TSX - Transfer Stack Pointer to X
            0xBA => self.tsx(),
            // TXA - Transfer X to Accumulator
            0x8A => self.txa(),
            // TXS - Transfer X to Stack Pointer
            0x9A => self.txs(),
            // TYA - Transfer Y to Accumulator
            0x98 => self.tya(),
            _ => {},
        }

        println!();
    }

    pub fn reset(&mut self) {
        // set program counter to reset vector
        self.pc = 0xFFFC;
    }
}
