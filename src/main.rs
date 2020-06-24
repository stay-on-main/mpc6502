// http://dendy.migera.ru/nes/g11.html
// http://www.obelisk.me.uk/6502/reference.html
// https://wiki.nesdev.com/w/index.php/CPU_addressing_modes

struct Bus {

}

impl Bus {
    fn read(&self, addr: u16) -> u8 {
        todo!();
    }

    fn write(&mut self, addr: u16, val: u8) {
        todo!();
    }
}

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


impl Cpu {
    fn new() -> Self {
        todo!();
    }

    fn immediate(&mut self, instruction: fn(&mut Cpu, u8) -> u8) {
        let val = self.fetch_u8();
        instruction(self, val);
    }

    fn accumulator(&mut self, instruction: fn(&mut Cpu, u8) -> u8) {
        self.a = instruction(self, self.a);
    }

    fn zero_page(&mut self, instruction: fn(&mut Cpu, u8) -> u8) {
        let addr = self.fetch_u8() as u16;
        let val = self.bus.read(addr as u16);
        let val = instruction(self, val);
        self.bus.write(addr, val);
    }

    fn zero_page_x(&mut self, instruction: fn(&mut Cpu, u8) -> u8) {
        let addr = (self.fetch_u8() + self.x) as u16;
        let val = self.bus.read(addr as u16);
        let val = instruction(self, val);
        self.bus.write(addr, val);
    }

    fn absolute(&mut self, instruction: fn(&mut Cpu, u8) -> u8) {
        let addr = self.fetch_u16();
        let val = self.bus.read(addr as u16);
        let val = instruction(self, val);
        self.bus.write(addr, val);
    }

    fn absolute_x(&mut self, instruction: fn(&mut Cpu, u8) -> u8) {
        let addr = self.fetch_u16() + self.x as u16;
        let val = self.bus.read(addr as u16);
        let val = instruction(self, val);
        self.bus.write(addr, val);
    }

    fn absolute_y(&mut self, instruction: fn(&mut Cpu, u8) -> u8) {
        let addr = self.fetch_u16() + self.y as u16;
        let val = self.bus.read(addr as u16);
        let val = instruction(self, val);
        self.bus.write(addr, val);
    }

    fn relative(&mut self, instruction: fn(&mut Cpu, i8)) {
        let offset = self.fetch_u8() as i8;
        instruction(self, offset);
    }

    fn implicit(&mut self, instruction: fn(&mut Cpu)) {
        instruction(self);
    }

    fn indirect_x(&mut self, instruction: fn(&mut Cpu, u8) -> u8) {
        todo!();
    }

    fn indirect_y(&mut self, instruction: fn(&mut Cpu, u8) -> u8) {
        todo!();
    }

    fn fetch_u8(&mut self) -> u8 {
        let byte = self.bus.read(self.pc);
        self.pc += 1;
        byte
    }

    fn fetch_u16(&mut self) -> u16 {
        let l = self.bus.read(self.pc);
        let h = self.bus.read(self.pc);
        self.pc += 2;
        (l as u16) | ((h as u16) << 8)
    }

    fn adc(&mut self, data: u8) -> u8 {
        // ADC - Add with Carry
        todo!();
    }

    fn and(&mut self, data: u8) -> u8 {
        // AND - Logical AND
        todo!();
    }

    fn asl(&mut self, data: u8) -> u8 {
        // ASL - Arithmetic Shift Left
        todo!();
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

    fn bit(&mut self, data: u8) -> u8 {
        // BIT - Bit Test
        todo!();
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

    fn cmp(&mut self, data: u8) -> u8 {
        // CMP - Compare
        todo!();
    }

    fn cpx(&mut self, data: u8) -> u8 {
        // CPX - Compare X Register
        todo!();
    }

    fn cpy(&mut self, data: u8) -> u8 {
        // CPY - Compare Y Register
        todo!();
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
            0x11 => self.indirect_y(Cpu::and),
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
            _ => {},
        }
    }
}

fn main() {
    let mut cpu = Cpu::new();
    cpu.step();
}
