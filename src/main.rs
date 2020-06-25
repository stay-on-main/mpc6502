// http://dendy.migera.ru/nes/g11.html
// http://www.obelisk.me.uk/6502/reference.html
// https://wiki.nesdev.com/w/index.php/CPU_addressing_modes
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

    fn adc(&mut self, addr: u16) {
        // ADC - Add with Carry
        // A,Z,C,N = A+M+C
        let m = self.bus.read_u8(addr);
        let mut res = (self.a as u16) + (m as u16);

        if self.p.c {
            res += 1;
        }
        
        self.a = res as u8;

        self.p.c = res > 0xff;
        self.p.n = (self.a & 0x80) != 0;
        self.p.z = self.a == 0;
        todo!(); // self.p.v 
    }

    fn and(&mut self, addr: u16) {
        // AND - Logical AND
        // A,Z,N = A&M
        self.a = self.a & self.bus.read_u8(addr);
        self.p.z = self.a == 0;
        self.p.n = bit(self.a, 7);
    }

    fn asl(&mut self, addr: u16) {
        // ASL - Arithmetic Shift Left
        // A,Z,C,N = M*2 or M,Z,C,N = M*2
        let m = self.bus.read_u8(addr);
        self.a = m << 1;
        self.p.z = self.a == 0;
        self.p.c = bit(m, 7);
        self.p.n = bit(self.a, 7);
    }

    fn bcc(&mut self, offset: i8) {
        // BCC - Branch if Carry Clear
        if self.p.c == false {
            self.pc = ((self.pc as i32) + (offset as i32)) as u16;
        }
    }

    fn bcs(&mut self, offset: i8) {
        // BCS - Branch if Carry Set
        if self.p.c == true {
            self.pc = ((self.pc as i32) + (offset as i32)) as u16;
        }
    }

    fn beq(&mut self, offset: i8) {
        // BEQ - Branch if Equal
        if self.p.z == true {
            self.pc = ((self.pc as i32) + (offset as i32)) as u16;
        }
    }

    fn bit(&mut self, addr: u16) {
        // BIT - Bit Test
        // A & M, N = M7, V = M6
        let res = self.a & self.bus.read_u8(addr);
        self.p.n = bit(self.a, 7);
        self.p.v = bit(self.a, 6);
    }

    fn bmi(&mut self, offset: i8) {
        // BMI - Branch if Minus
        if self.p.n == true {
            self.pc = ((self.pc as i32) + (offset as i32)) as u16;
        }
    }

    fn bne(&mut self, offset: i8) {
        // BNE - Branch if Not Equal
        if self.p.z == false {
            self.pc = ((self.pc as i32) + (offset as i32)) as u16;
        }
    }

    fn bpl(&mut self, offset: i8) {
        // BPL - Branch if Positive
        if self.p.n == false {
            self.pc = ((self.pc as i32) + (offset as i32)) as u16;
        }
    }

    fn brk(&mut self) {
        // BRK - Force Interrupt
        self.p.b = true;
    }

    fn bvc(&mut self, offset: i8) {
        // BVC - Branch if Overflow Clear
        if self.p.v == false {
            self.pc = ((self.pc as i32) + (offset as i32)) as u16;
        }
    }

    fn bvs(&mut self, offset: i8) {
        // BVS - Branch if Overflow Set
        if self.p.v == true {
            self.pc = ((self.pc as i32) + (offset as i32)) as u16;
        }
    }

    fn clc(&mut self) {
        // CLC - Clear Carry Flag
        self.p.c = false;
    }

    fn cld(&mut self) {
        // CLD - Clear Decimal Mode
        self.p.d = false;
    }

    fn cli(&mut self) {
        // CLI - Clear Interrupt Disable
        self.p.i = false;
    }

    fn clv(&mut self) {
        // CLV - Clear Overflow Flag
        self.p.v = false;
    }

    fn cmp(&mut self, addr: u16) {
        // CMP - Compare
        // Z,C,N = A-M
        let m = self.bus.read_u8(addr);
        self.p.z = self.a == m;
        self.p.c = self.a >= m;
        self.p.n = bit(self.a - m, 7);
    }

    fn cpx(&mut self, addr: u16) {
        // CPX - Compare X Register
        // Z,C,N = X-M
        let m = self.bus.read_u8(addr);
        self.p.z = self.x == m;
        self.p.c = self.x >= m;
        self.p.n = bit(self.x - m, 7);
    }

    fn cpy(&mut self, addr: u16) {
        // CPY - Compare Y Register
        // Z,C,N = Y-M
        let m = self.bus.read_u8(addr);
        self.p.z = self.y == m;
        self.p.c = self.y >= m;
        self.p.n = bit(self.y - m, 7);
    }

    fn dec(&mut self, addr: u16) {
        // DEC - Decrement Memory
        // M,Z,N = M-1
        let m = self.bus.read_u8(addr) - 1;
        self.bus.write_u8(addr, m);
        self.p.z = m == 0;
        self.p.n = bit(m, 7);
        
    }

    fn dex(&mut self) {
        // DEX - Decrement X Register
        // X,Z,N = X-1
        self.x -= 1;
        self.p.z = self.x == 0;
        self.p.n = bit(self.x, 7);
    }

    fn dey(&mut self) {
        // DEY - Decrement Y Register
        // Y,Z,N = Y-1
        self.y -= 1;
        self.p.z = self.y == 0;
        self.p.n = bit(self.y, 7);
    }

    fn eor(&mut self, addr: u16) {
        // EOR - Exclusive OR
        // A,Z,N = A^M
        let m = self.bus.read_u8(addr);
        self.a ^= m;
        self.p.z = self.a == 0;
        self.p.n = bit(self.a, 7);
    }

    fn inc(&mut self, addr: u16) {
        // INC - Increment Memory
        // M,Z,N = M+1
        let m = self.bus.read_u8(addr) + 1;
        self.bus.write_u8(addr, m);
        self.p.z = m == 0;
        self.p.n = bit(m, 7);
    }

    fn inx(&mut self) {
        // INX - Increment X Register
        // X,Z,N = X+1
        self.x += 1;
        self.p.z = self.x == 0;
        self.p.n = bit(self.x, 7);
    }

    fn iny(&mut self) {
        // INY - Increment Y Register
        // Y,Z,N = Y+1
        self.y += 1;
        self.p.z = self.y == 0;
        self.p.n = bit(self.y, 7);
    }

    fn jmp(&mut self, addr: u16) {
        // JMP - Jump
        self.pc = self.bus.read_u16(addr);
    }

    fn jsr(&mut self, addr: u16) {
        // JSR - Jump to Subroutine
        todo!();
        //self.jmp(addr);
    }

    fn lda(&mut self, addr: u16) {
        // LDA - Load Accumulator
        // A,Z,N = M
        self.a = self.bus.read_u8(addr);
        self.p.z = self.a == 0;
        self.p.n = bit(self.a, 7);
    }

    fn ldx(&mut self, addr: u16) {
        // LDX - Load X Register
        // X,Z,N = M
        self.x = self.bus.read_u8(addr);
        self.p.z = self.x == 0;
        self.p.n = bit(self.x, 7);
    }

    fn ldy(&mut self, addr: u16) {
        // LDY - Load Y Register
        // Y,Z,N = M
        self.y = self.bus.read_u8(addr);
        self.p.z = self.y == 0;
        self.p.n = bit(self.y, 7);
    }

    fn lsr(&mut self, addr: u16) {
        // LSR - Logical Shift Right
        // A,C,Z,N = A/2 or M,C,Z,N = M/2
        let m = self.bus.read_u8(addr);
        self.a = m >> 1;
        self.p.c = (m & 1) != 0;
        self.p.z = self.a == 0;
        self.p.n = bit(self.a, 7);
    }

    fn nop(&mut self) {
        // NOP - No Operation
    }

    fn ora(&mut self, addr: u16) {
        // ORA - Logical Inclusive OR
        // A,Z,N = A|M
        self.a = self.bus.read_u8(addr);
        self.p.z = self.a == 0;
        self.p.n = bit(self.a, 7);
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
        self.p.z = self.a == 0;
        self.p.n = bit(self.a, 7);
    }

    fn plp(&mut self) {
        // PLP - Pull Processor Status
        todo!();
    }

    fn rol(&mut self, addr: u16) {
        // ROL - Rotate Left
        todo!();
    }

    fn ror(&mut self, addr: u16) {
        // ROR - Rotate Right
        todo!();
    }

    fn rti(&mut self) {
        // RTI - Return from Interrupt
        todo!();
    }

    fn rts(&mut self) {
        // RTS - Return from Subroutine
        todo!();
    }

    fn sbc(&mut self, addr: u16) {
        // SBC - Subtract with Carry
        // A,Z,C,N = A-M-(1-C)
        todo!();
    }

    fn sec(&mut self) {
        // SEC - Set Carry Flag
        self.p.c = true;
    }

    fn sed(&mut self) {
        // SED - Set Decimal Flag
        self.p.d = true;
    }

    fn sei(&mut self) {
        // SEI - Set Interrupt Disable
        self.p.i = true;
    }

    fn sta(&mut self, addr: u16) {
        // STA - Store Accumulator
        // M = A
        self.bus.write_u8(addr, self.a);
    }

    fn stx(&mut self, addr: u16) {
        // STX - Store X Register
        // M = X
        self.bus.write_u8(addr, self.x);
    }

    fn sty(&mut self, addr: u16) {
        // STY - Store Y Register
        // M = Y
        self.bus.write_u8(addr, self.y);
    }

    fn tax(&mut self) {
        // TAX - Transfer Accumulator to X
        // X = A
        self.x = self.a;
        self.p.z = self.x == 0;
        self.p.n = bit(self.x, 7);
    }

    fn tay(&mut self) {
        // TAY - Transfer Accumulator to Y
        // Y = A;
        self.y = self.a;
        self.p.z = self.y == 0;
        self.p.n = bit(self.y, 7);
    }

    fn tsx(&mut self) {
        // TSX - Transfer Stack Pointer to X
        // X = S
        self.x = self.s;
        self.p.z = self.x == 0;
        self.p.n = bit(self.x, 7);
    }

    fn txa(&mut self) {
        // TXA - Transfer X to Accumulator
        // A = X
        self.a = self.x;
        self.p.z = self.a == 0;
        self.p.n = bit(self.a, 7);
    }

    fn txs(&mut self) {
        // TXS - Transfer X to Stack Pointer
        // S = X
        self.s = self.x;
    }

    fn tya(&mut self) {
        // TYA - Transfer Y to Accumulator
        // A = Y
        self.a = self.y;
        self.p.z = self.a == 0;
        self.p.n = bit(self.a, 7);
    }

    fn step(&mut self) {
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
    cpu.step();
}
