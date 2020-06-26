// http://dendy.migera.ru/nes/g11.html
// http://www.obelisk.me.uk/6502/reference.html
// https://wiki.nesdev.com/w/index.php/CPU_addressing_modes
// http://nesdev.com/6502.txt
// https://skilldrick.github.io/easy6502/
mod bus;
use bus::{Bus};

struct CpuFlagReg {
    c: bool,
    z: bool,
    i: bool,
    d: bool,
    b: bool,
    v: bool,
    n: bool,
}

struct Cpu {
    a: u8,
    pc: u16,
    s: u8,
    x: u8,
    y: u8,
    p: CpuFlagReg,
    bus: Bus,
    clk: u8,
}

fn bit(val: u8, bit: usize) -> bool {
    (val & (1 << bit)) != 0
}

impl Cpu {
    fn new() -> Self {
        todo!();
    }

    fn accumulator(&mut self, instruction: fn(&mut Cpu, u8)) {
        instruction(self, self.a);
    }

    fn immediate(&mut self, instruction: fn(&mut Cpu, u16)) {
        instruction(self, self.pc);
    }

    fn zero_page(&mut self, instruction: fn(&mut Cpu, u16)) {
        let addr = self.fetch_u8() as u16;
        instruction(self, addr);
    }

    fn zero_page_x(&mut self, instruction: fn(&mut Cpu, u16)) {
        let addr = (self.fetch_u8() + self.x) as u16;
        instruction(self, addr);
    }

    fn zero_page_y(&mut self, instruction: fn(&mut Cpu, u16)) {
        let addr = (self.fetch_u8() + self.y) as u16;
        instruction(self, addr);
    }

    fn absolute(&mut self, instruction: fn(&mut Cpu, u16)) {
        let addr = self.fetch_u16();
        instruction(self, addr);
    }

    fn absolute_x(&mut self, instruction: fn(&mut Cpu, u16)) {
        let addr = self.fetch_u16() + self.x as u16;
        instruction(self, addr);
    }

    fn absolute_y(&mut self, instruction: fn(&mut Cpu, u16)) {
        let addr = self.fetch_u16() + self.y as u16;
        instruction(self, addr);
    }

    fn relative(&mut self, instruction: fn(&mut Cpu, i8)) {
        let offset = self.fetch_u8() as i8;
        instruction(self, offset);
    }

    fn implicit(&mut self, instruction: fn(&mut Cpu)) {
        instruction(self);
    }

    fn indirect(&mut self, instruction: fn(&mut Cpu, u16)) {
        let addr = self.fetch_u16();
        instruction(self, self.bus.read_u16(addr as u16));
    }

    fn indirect_x(&mut self, instruction: fn(&mut Cpu, u16)) {
        let addr = self.fetch_u8() + self.x;
        instruction(self, self.bus.read_u16(addr as u16));
    }

    fn indirect_y(&mut self, instruction: fn(&mut Cpu, u16)) {
        let addr = self.fetch_u8();
        instruction(self, self.bus.read_u16(addr as u16) + (self.y as u16));
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
        self.p.n = (val & 0x80) != 0;
    }

    fn set_zero(&mut self, val: u8) {
        self.p.z = val == 0;
    }

    fn set_carry(&mut self, val: u8) {
        self.p.c = val != 0;
    }

    fn set_overflow(&mut self, val: u8) {
        self.p.n = val != 0;
    }

    fn set_interrupt(&mut self, val: u8) {
        self.p.i = val != 0;
    }

    fn set_break(&mut self, val: u8) {
        self.p.b = val != 0;
    }

    fn set_decimal(&mut self, val: u8) {
        self.p.c = val != 0;
    }


    fn adc(&mut self, addr: u16) {
        // ADC - Add with Carry
        let src = self.bus.read_u8(addr);
        let mut temp = (self.a as u16) + (src as u16);
        
        if self.if_carry() {
            temp += 1;
        }
        // This is not valid in decimal mode
        self.set_zero(temp as u8);

        if self.if_decimal() {
            if (((AC & 0xf) + (src & 0xf) + (IF_CARRY() ? 1 : 0)) > 9) 
                temp += 6;
                
            SET_SIGN(temp);
            SET_OVERFLOW(!((AC ^ src) & 0x80) && ((AC ^ temp) & 0x80));
       
            if (temp > 0x99) 
                temp += 96;
            
            SET_CARRY(temp > 0x99);
        } else {
            self.set_sign(temp as u8);
            SET_OVERFLOW(!((AC ^ src) & 0x80) && ((AC ^ temp) & 0x80));
            self.set_carry((temp >> 8) as u8);
        }
    }

    fn and(&mut self, addr: u16) {
        // AND - Logical AND
        self.a &= self.bus.read_u8(addr);
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn asl(&mut self, addr: u16) {
        // ASL - Arithmetic Shift Left
        let mut src = self.bus.read_u8(addr);
        self.set_carry(src & 0x80);
        src <<= 1;
        self.set_sign(src);
        self.set_zero(src);
        self.bus.write_u8(addr, src);
    }

    fn bcc(&mut self, offset: i8) {
        // BCC - Branch if Carry Clear
        if self.if_carry() {
            return;
        }

        let new_pc = ((self.pc as i32) + (offset as i32)) as u16;
        
        if (self.pc & 0xFF00) != (new_pc & 0xFF00) {
            self.clk += 2;
        } else {
            self.clk += 1;
        }
        
        self.pc = new_pc;
    }

    fn bcs(&mut self, offset: i8) {
        // BCS - Branch if Carry Set
        if !self.if_carry() {
            return;
        }

        let new_pc = ((self.pc as i32) + (offset as i32)) as u16;
        
        if (self.pc & 0xFF00) != (new_pc & 0xFF00) {
            self.clk += 2;
        } else {
            self.clk += 1;
        }
        
        self.pc = new_pc;
    }

    fn beq(&mut self, offset: i8) {
        // BEQ - Branch if Equal
        if self.if_zero() {
            return;
        }

        let new_pc = ((self.pc as i32) + (offset as i32)) as u16;
        
        if (self.pc & 0xFF00) != (new_pc & 0xFF00) {
            self.clk += 2;
        } else {
            self.clk += 1;
        }
        
        self.pc = new_pc;
    }

    fn bit(&mut self, addr: u16) {
        // BIT - Bit Test
        let src = self.bus.read_u8(addr);
        self.set_sign(src);
        self.set_verflow(0x40 & src);
        self.set_zero(src & self.a);
    }

    fn bmi(&mut self, offset: i8) {
        // BMI - Branch if Minus
        if self.if_sing() {
            return;
        }

        let new_pc = ((self.pc as i32) + (offset as i32)) as u16;
        
        if (self.pc & 0xFF00) != (new_pc & 0xFF00) {
            self.clk += 2;
        } else {
            self.clk += 1;
        }
        
        self.pc = new_pc;
    }

    fn bne(&mut self, offset: i8) {
        // BNE - Branch if Not Equal
        if !self.if_zero() {
            return;
        }

        let new_pc = ((self.pc as i32) + (offset as i32)) as u16;
        
        if (self.pc & 0xFF00) != (new_pc & 0xFF00) {
            self.clk += 2;
        } else {
            self.clk += 1;
        }
        
        self.pc = new_pc;
    }

    fn bpl(&mut self, offset: i8) {
        // BPL - Branch if Positive
        if !self.if_sign() {
            return;
        }

        let new_pc = ((self.pc as i32) + (offset as i32)) as u16;
        
        if (self.pc & 0xFF00) != (new_pc & 0xFF00) {
            self.clk += 2;
        } else {
            self.clk += 1;
        }
        
        self.pc = new_pc;
    }

    fn brk(&mut self) {
        // BRK - Force Interrupt
        self.pc += 1;
        self.stack_push((self.pc >> 8) as u8);
        self.stack_push(self.pc as u8);
        self.set_break(1);
        self.stack_push(self.s);
        self.set_interrupt(1);
        self.pc = self.bus.read_u16(0xFFFE);
    }

    fn bvc(&mut self, offset: i8) {
        // BVC - Branch if Overflow Clear
        if !self.if_overflow() {
            return;
        }

        let new_pc = ((self.pc as i32) + (offset as i32)) as u16;
        
        if (self.pc & 0xFF00) != (new_pc & 0xFF00) {
            self.clk += 2;
        } else {
            self.clk += 1;
        }
        
        self.pc = new_pc;
    }

    fn bvs(&mut self, offset: i8) {
        // BVS - Branch if Overflow Set
        if self.if_overflow() {
            return;
        }

        let new_pc = ((self.pc as i32) + (offset as i32)) as u16;
        
        if (self.pc & 0xFF00) != (new_pc & 0xFF00) {
            self.clk += 2;
        } else {
            self.clk += 1;
        }
        
        self.pc = new_pc;
    }

    fn clc(&mut self) {
        // CLC - Clear Carry Flag
        self.set_carry(0);
    }

    fn cld(&mut self) {
        // CLD - Clear Decimal Mode
        self.set_decimal(0);
    }

    fn cli(&mut self) {
        // CLI - Clear Interrupt Disable
        self.set_interrupt(0);
    }

    fn clv(&mut self) {
        // CLV - Clear Overflow Flag
        self.set_overflow(0);
    }

    fn cmp(&mut self, addr: u16) {
        // CMP - Compare
        let src = self.bus.read_u8(addr);
        self.set_carry(self.a >= src);
        let src = self.a - src;
        self.set_sign(src);
        self.set_zero(src);
    }

    fn cpx(&mut self, addr: u16) {
        // CPX - Compare X Register
        let src = self.bus.read_u8(addr);
        self.set_carry(self.x >= src);
        let src = self.x - src;
        self.set_sign(src);
        self.set_zero(src);
    }

    fn cpy(&mut self, addr: u16) {
        // CPY - Compare Y Register
        let src = self.bus.read_u8(addr);
        self.set_carry(self.y >= src);
        let src = self.y - src;
        self.set_sign(src);
        self.set_zero(src);
    }

    fn dec(&mut self, addr: u16) {
        // DEC - Decrement Memory
        let src = self.bus.read_u8(addr) - 1;
        self.set_sign(src);
        self.set_zero(src);
        self.bus.write_u8(addr, src);
    }

    fn dex(&mut self) {
        // DEX - Decrement X Register
        self.x -= 1;
        self.set_sign(self.x);
        self.set_zero(self.x);
    }

    fn dey(&mut self) {
        // DEY - Decrement Y Register
        self.y -= 1;
        self.set_sign(self.y);
        self.set_zero(self.y);
    }

    fn eor(&mut self, addr: u16) {
        // EOR - Exclusive OR
        // A,Z,N = A^M
        let src = self.bus.read_u8(addr);
        self.a ^= src;
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn inc(&mut self, addr: u16) {
        // INC - Increment Memory
        let src = self.bus.read_u8(addr) + 1;
        self.set_sign(src);
        self.set_zero(src);
        self.bus.write_u8(addr, src);
    }

    fn inx(&mut self) {
        // INX - Increment X Register
        self.x += 1;
        self.set_sign(self.x);
        self.set_zero(self.x);
    }

    fn iny(&mut self) {
        // INY - Increment Y Register
        self.y += 1;
        self.set_sign(self.y);
        self.set_zero(self.y);
    }

    fn jmp(&mut self, addr: u16) {
        // JMP - Jump
        self.pc = self.bus.read_u16(addr);
    }

    fn jsr(&mut self, addr: u16) {
        // JSR - Jump to Subroutine
        self.pc -= 1;
        self.stack_push((self.pc >> 8) as u8);
        self.stack_push(self.pc as u8);
        self.pc = self.bus.read_u16(addr);
    }

    fn lda(&mut self, addr: u16) {
        // LDA - Load Accumulator
        self.a = self.bus.read_u8(addr);
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn ldx(&mut self, addr: u16) {
        // LDX - Load X Register
        self.x = self.bus.read_u8(addr);
        self.set_sign(self.x);
        self.set_zero(self.x);
    }

    fn ldy(&mut self, addr: u16) {
        // LDY - Load Y Register
        self.y = self.bus.read_u8(addr);
        self.set_sign(self.y);
        self.set_zero(self.y);
    }

    fn lsr(&mut self, addr: u16) {
        // LSR - Logical Shift Right
        let src = self.bus.read_u8(addr);
        self.set_carry(src & 0x01);
        self.set_sign(src);
        self.set_zero(src);
        self.bus.write_u8(addr, src);
    }

    fn nop(&mut self) {
        // NOP - No Operation
    }

    fn ora(&mut self, addr: u16) {
        // ORA - Logical Inclusive OR
        self.a |= self.bus.read_u8(addr);
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn pha(&mut self) {
        // PHA - Push Accumulator
        self.stack_push(self.a);
    }

    fn php(&mut self) {
        // PHP - Push Processor Status
        todo!();
    }

    fn pla(&mut self) {
        // PLA - Pull Accumulator
        self.a = self.stack_pop();
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn plp(&mut self) {
        // PLP - Pull Processor Status
        todo!();
    }

    fn rol(&mut self, addr: u16) {
        // ROL - Rotate Left
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

    fn ror(&mut self, addr: u16) {
        // ROR - Rotate Right
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

    fn rti(&mut self) {
        // RTI - Return from Interrupt
        todo!();
    }

    fn rts(&mut self) {
        // RTS - Return from Subroutine
        let l = self.stack_pop() as u16;
        let h = self.stack_pop() as u16;
        self.pc = ((h << 8) | l) + 1;
    }

    fn sbc(&mut self, addr: u16) {
        // SBC - Subtract with Carry
        // A,Z,C,N = A-M-(1-C)
        todo!();
    }

    fn sec(&mut self) {
        // SEC - Set Carry Flag
        self.set_carry(1);
    }

    fn sed(&mut self) {
        // SED - Set Decimal Flag
        self.set_decimal(1);
    }

    fn sei(&mut self) {
        // SEI - Set Interrupt Disable
        self.set_interrupt(1);
    }

    fn sta(&mut self, addr: u16) {
        // STA - Store Accumulator
        self.bus.write_u8(addr, self.a);
    }

    fn stx(&mut self, addr: u16) {
        // STX - Store X Register
        self.bus.write_u8(addr, self.x);
    }

    fn sty(&mut self, addr: u16) {
        // STY - Store Y Register
        self.bus.write_u8(addr, self.y);
    }

    fn tax(&mut self) {
        // TAX - Transfer Accumulator to X
        self.x = self.a;
        self.set_sign(self.x);
        self.set_zero(self.x);
    }

    fn tay(&mut self) {
        // TAY - Transfer Accumulator to Y
        self.y = self.a;
        self.set_sign(self.y);
        self.set_zero(self.y);
    }

    fn tsx(&mut self) {
        // TSX - Transfer Stack Pointer to X
        self.x = self.s;
        self.set_sign(self.s);
        self.set_zero(self.s);
    }

    fn txa(&mut self) {
        // TXA - Transfer X to Accumulator
        self.a = self.x;
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn txs(&mut self) {
        // TXS - Transfer X to Stack Pointer
        self.s = self.x;
    }

    fn tya(&mut self) {
        // TYA - Transfer Y to Accumulator
        self.a = self.y;
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn clock(&mut self) {
        if self.clk != 0 {
            self.clk -= 1;
            return;
        }

        match self.fetch_u8() {
            // ADC - Add with Carry
            0x69 => self.immediate(Cpu::adc),
            0x65 => self.zero_page(Cpu::adc),
            0x75 => self.zero_page_x(Cpu::adc),
            0x6D => self.absolute(Cpu::adc),
            0x7D => self.absolute_x(Cpu::adc),
            0x79 => self.absolute_y(Cpu::adc),
            0x61 => self.indirect_x(Cpu::adc),
            0x71 => self.indirect_y(Cpu::adc),
            // AND - Logical AND
            0x29 => self.immediate(Cpu::and),
            0x25 => self.zero_page(Cpu::and),
            0x35 => self.zero_page_x(Cpu::and),
            0x2D => self.absolute(Cpu::and),
            0x3D => self.absolute_x(Cpu::and),
            0x39 => self.absolute_y(Cpu::and),
            0x21 => self.indirect_x(Cpu::and),
            0x31 => self.indirect_y(Cpu::and),
            // ASL - Arithmetic Shift Left
            0x0A => self.accumulator(Cpu::asl),
            0x06 => self.zero_page(Cpu::asl),
            0x16 => self.zero_page_x(Cpu::asl),
            0x0E => self.absolute(Cpu::asl),
            0x1E => self.absolute_x(Cpu::asl),
            // BCC - Branch if Carry Clear
            0x90 => self.relative(Cpu::bcc),
            // BCS - Branch if Carry Set
            0xB0 => self.relative(Cpu::bcs),
            // BEQ - Branch if Equal
            0xF0 => self.relative(Cpu::beq),
            // BIT - Bit Test
            0x24 => self.zero_page(Cpu::bit),
            0x2C => self.absolute(Cpu::bit),
            // BMI - Branch if Minus
            0x30 => self.relative(Cpu::bmi),
            // BNE - Branch if Not Equal
            0xD0 => self.relative(Cpu::bne),
            // BPL - Branch if Positive
            0x10 => self.relative(Cpu::bpl),
            // BRK - Force Interrupt
            0x00 => self.implicit(Cpu::brk),
            // BVC - Branch if Overflow Clear
            0x50 => self.relative(Cpu::bvc),
            // BVS - Branch if Overflow Set
            0x70 => self.relative(Cpu::bvs),
            // CLC - Clear Carry Flag
            0x18 => self.implicit(Cpu::clc),
            // CLD - Clear Decimal Mode
            0xD8 => self.implicit(Cpu::cld),
            // CLI - Clear Interrupt Disable
            0x58 => self.implicit(Cpu::cli),
            // CLV - Clear Overflow Flag
            0xB8 => self.implicit(Cpu::clv),
            // CMP - Compare
            0xC9 => self.immediate(Cpu::cmp),
            0xC5 => self.zero_page(Cpu::cmp),
            0xD5 => self.zero_page_x(Cpu::cmp),
            0xCD => self.absolute(Cpu::cmp),
            0xDD => self.absolute_x(Cpu::cmp),
            0xD9 => self.absolute_y(Cpu::cmp),
            0xC1 => self.indirect_x(Cpu::cmp),
            0xD1 => self.indirect_y(Cpu::cmp),
            // CPX - Compare X Register
            0xE0 => self.immediate(Cpu::cpx),
            0xE4 => self.zero_page(Cpu::cpx),
            0xEC => self.absolute(Cpu::cpx),
            // CPY - Compare Y Register
            0xC0 => self.immediate(Cpu::cpy),
            0xC4 => self.zero_page(Cpu::cpy),
            0xCC => self.absolute(Cpu::cpy),
            // DEC - Decrement Memory
            0xC6 => self.zero_page(Cpu::dec),
            0xD6 => self.zero_page_x(Cpu::dec),
            0xCE => self.absolute(Cpu::dec),
            0xDE => self.absolute_x(Cpu::dec),
            // DEX - Decrement X Register
            0xCA => self.implicit(Cpu::dex),
            // DEY - Decrement Y Register
            0x88 => self.implicit(Cpu::dey),
            // EOR - Exclusive OR
            0x49 => self.immediate(Cpu::eor),
            0x45 => self.zero_page(Cpu::eor),
            0x55 => self.zero_page_x(Cpu::eor),
            0x4D => self.absolute(Cpu::eor),
            0x5D => self.absolute_x(Cpu::eor),
            0x59 => self.absolute_y(Cpu::eor),
            0x41 => self.indirect_x(Cpu::eor),
            0x51 => self.indirect_y(Cpu::eor),
            // INC - Increment Memory
            0xE6 => self.zero_page(Cpu::inc),
            0xF6 => self.zero_page_x(Cpu::inc),
            0xEE => self.absolute(Cpu::inc),
            0xFE => self.absolute_x(Cpu::inc),
            // INX - Increment X Register
            0xE8 => self.implicit(Cpu::inx),
            // INY - Increment Y Register
            0xC8 => self.implicit(Cpu::iny),
            // JMP - Jump
            0x4C => self.absolute(Cpu::jmp),
            0x6C => self.indirect(Cpu::jmp),
            // JSR - Jump to Subroutine
            0x20 => self.absolute(Cpu::jsr),
            // LDA - Load Accumulator
            0xA9 => self.immediate(Cpu::lda),
            0xA5 => self.zero_page(Cpu::lda),
            0xB5 => self.zero_page_x(Cpu::lda),
            0xAD => self.absolute(Cpu::lda),
            0xBD => self.absolute_x(Cpu::lda),
            0xB9 => self.absolute_y(Cpu::lda),
            0xA1 => self.indirect_x(Cpu::lda),
            0xB1 => self.indirect_y(Cpu::lda),
            // LDX - Load X Register
            0xA2 => self.immediate(Cpu::ldx),
            0xA6 => self.zero_page(Cpu::ldx),
            0xB6 => self.zero_page_y(Cpu::ldx),
            0xAE => self.absolute(Cpu::ldx),
            0xBE => self.absolute_y(Cpu::ldx),
            // LDY - Load Y Register
            0xA0 => self.immediate(Cpu::ldy),
            0xA4 => self.zero_page(Cpu::ldy),
            0xB4 => self.zero_page_x(Cpu::ldy),
            0xAC => self.absolute(Cpu::ldy),
            0xBC => self.absolute_x(Cpu::ldy),
            // LSR - Logical Shift Right
            0x4A => self.accumulator(Cpu::lsr),
            0x46 => self.zero_page(Cpu::lsr),
            0x56 => self.zero_page_x(Cpu::lsr),
            0x4E => self.absolute(Cpu::lsr),
            0x5E => self.absolute_x(Cpu::lsr),
            // NOP - No Operation
            0xEA => self.implicit(Cpu::nop),
            // ORA - Logical Inclusive OR
            0x09 => self.immediate(Cpu::ora),
            0x05 => self.zero_page(Cpu::ora),
            0x15 => self.zero_page_x(Cpu::ora),
            0x0D => self.absolute(Cpu::ora),
            0x1D => self.absolute_x(Cpu::ora),
            0x19 => self.absolute_y(Cpu::ora),
            0x01 => self.indirect_x(Cpu::ora),
            0x11 => self.indirect_y(Cpu::ora),
            // PHA - Push Accumulator
            0x48 => self.implicit(Cpu::pha),
            // PHP - Push Processor Status
            0x08 => self.implicit(Cpu::php),
            // PLA - Pull Accumulator
            0x68 => self.implicit(Cpu::pla),
            // PLP - Pull Processor Status
            0x28 => self.implicit(Cpu::plp),
            // ROL - Rotate Left
            0x2A => self.accumulator(Cpu::rol),
            0x26 => self.zero_page(Cpu::rol),
            0x36 => self.zero_page_x(Cpu::rol),
            0x2E => self.absolute(Cpu::rol),
            0x3E => self.absolute_x(Cpu::rol),
            // ROR - Rotate Right
            0x6A => self.accumulator(Cpu::ror),
            0x66 => self.zero_page(Cpu::ror),
            0x76 => self.zero_page_x(Cpu::ror),
            0x6E => self.absolute(Cpu::ror),
            0x7E => self.absolute_x(Cpu::ror),
            // RTI - Return from Interrupt
            0x40 => self.implicit(Cpu::rti),
            // RTS - Return from Subroutine
            0x60 => self.implicit(Cpu::rts),
            // SBC - Subtract with Carry
            0xE9 => self.immediate(Cpu::sbc),
            0xE5 => self.zero_page(Cpu::sbc),
            0xF5 => self.zero_page_x(Cpu::sbc),
            0xED => self.absolute(Cpu::sbc),
            0xFD => self.absolute_x(Cpu::sbc),
            0xF9 => self.absolute_y(Cpu::sbc),
            0xE1 => self.indirect_x(Cpu::sbc),
            0xF1 => self.indirect_y(Cpu::sbc),
            // SEC - Set Carry Flag
            0x38 => self.implicit(Cpu::sec),
            // SED - Set Decimal Flag
            0xF8 => self.implicit(Cpu::sed),
            // SEI - Set Interrupt Disable
            0x78 => self.implicit(Cpu::sei),
            // STA - Store Accumulator
            0x85 => self.zero_page(Cpu::sta),
            0x95 => self.zero_page_x(Cpu::sta),
            0x8D => self.absolute(Cpu::sta),
            0x9D => self.absolute_x(Cpu::sta),
            0x99 => self.absolute_y(Cpu::sta),
            0x81 => self.indirect_x(Cpu::sta),
            0x91 => self.indirect_y(Cpu::sta),
            // STX - Store X Register
            0x86 => self.zero_page(Cpu::stx),
            0x96 => self.zero_page_y(Cpu::stx),
            0x8E => self.absolute(Cpu::stx),
            // STY - Store Y Register
            0x84 => self.zero_page(Cpu::sty),
            0x94 => self.zero_page_x(Cpu::sty),
            0x8C => self.absolute(Cpu::sty),
            // TAX - Transfer Accumulator to X
            0xAA => self.implicit(Cpu::tax),
            // TAY - Transfer Accumulator to Y
            0xA8 => self.implicit(Cpu::tay),
            // TSX - Transfer Stack Pointer to X
            0xBA => self.implicit(Cpu::tsx),
            // TXA - Transfer X to Accumulator
            0x8A => self.implicit(Cpu::txa),
            // TXS - Transfer X to Stack Pointer
            0x9A => self.implicit(Cpu::txs),
            // TYA - Transfer Y to Accumulator
            0x98 => self.implicit(Cpu::tya),
            _ => {},
        }
    }
}

fn main() {
    let mut cpu = Cpu::new();
    cpu.clock();
}
