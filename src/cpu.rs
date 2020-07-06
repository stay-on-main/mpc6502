enum StatusBit {
    Carry = 1 << 0,
    Zero = 1 << 1,
    Interrupt = 1 << 2,
    Decimal = 1 << 3,
    Break = 1 << 4,
    Overflow = 1 << 5,
    Sign = 1 << 6,
}
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
}

impl Cpu {
    fn set_bit(&mut self, bit: StatusBit) {
        match bit {
            StatusBit::Carry => self.p 
        }
    }

    fn if_bit(bit: StatusBit) -> bool {

    }

    fn alu_add(&mut self, a: u8, b: u8) -> u8 {

    }

    fn alu_sub(&mut self, a: u8, b: u8) -> u8 {

    }

    fn alu_and(&mut self, a: u8) {
        self.a &= a;
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn alu_or(&mut self, a: u8) {
        self.a |= a;
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn alu_xor(&mut self, a: u8) {
        self.a ^= a;
        self.set_sign(self.a);
        self.set_zero(self.a);
    }

    fn alu_sl(&mut self, a: u8) -> u8 {
        self.set_carry(a & 0x80);
        let a = a << 1;
        self.set_sign(a);
        self.set_zero(a);
        a
    }

    fn alu_sr(&mut self, a: u8) -> u8 {
        self.set_carry(a & 0x01);
        let a = a >> 1;
        self.set_sign(a);
        self.set_zero(a);
        a
    }

    fn alu_rl(&mut self, a: u8) -> u8 {
        let res = (a << 1) | if self.if_carry() {1} else {0};
        self.set_carry(a & 0x80);
        self.set_sign(res);
        self.set_zero(res);
        res
    }

    fn alu_rr(&mut self, a: u8) -> u8 {
        let res = (a >> 1) | if self.if_carry() {0x80} else {0};
        self.set_carry(a & 0x01);
        self.set_sign(res);
        self.set_zero(res);
        res
    }

    fn alu_add_nc(&mut self, a: u8, b: u8) -> u8 {
        let n = ((a as u16) + (b as u16)) as u8;
        self.set_sign(n);
        self.set_zero(n);
        n
    }

    fn alu_sub_nb(&mut self, a: u8, b: u8) -> u8 {
        let src = ((a as i16) - (b as i16)) as u8;
        self.set_sign(src);
        self.set_zero(src);
        src
    }

    fn fetch_u8(&mut self) -> u8 {
        let n = self.bus.read(self.pc);
        self.pc += 1;
        self.bus.clk();
        n
    }

    fn addr_immediate(&mut self) -> u16 {
        let addr = self.pc;
        self.pc += 1;
        self.bus.clk();
        addr
    }

    fn addr_zero_page(&mut self) -> u16 {
        self.fetch_u8() as u16
    }

    fn addr_zero_page_x(&mut self) -> u16 {
        let addr = self.fetch_u8();
        let addr = addr.wrapping_add(self.x);
        self.bus.clk();
        addr
    }

    fn addr_zero_page_y(&mut self) -> u16 {
        let addr = self.fetch_u8();
        let addr = addr.wrapping_add(self.y);
        self.bus.clk();
        addr
    }

    fn addr_absolute(&mut self) -> u16 {
        let l = self.fetch_u8() as u16;
        let h = self.fetch_u8() as u16;
        let addr = (h << 8) | l;
        self.bus.clk();
        addr
    }

    
}


fn brk() {
    // 2
    self.fetch_u8();
    // 3
    self.stack_push(self.pcl);
    self.s -= 1;
    // 4
    self.stack_push(self.pch);
    self.s -= 1;
    // 5
    self.stack_push(self.p | (StatusBit::Break as u8);
    self.s -= 1;
    // 6
    self.pcl = self.bus.read(0xfffe);
    // 7
    self.pch = self.bus.read(0xffff);
}

fn rti() {
    // 2
    self.bus.read(self.pc);
    // 3
    self.s += 1;
    // 4
    self.p = self.stack_pull();
    self.s += 1;
    // 5
    self.pcl = self.stack_pull();
    self.s += 1;
    // 6
    self.pch = self.stack_pull();
}

fn rts() {
    // 2
    self.bus.read(self.pc);
    // 3
    self.s += 1;
    // 4
    self.pcl = self.stack_pull();
    self.s += 1;
    // 5
    self.pch = self.stack_pull();
    // 6
    self.pc += 1;
}

// pha, php
fn pha() {
    // 2
    self.bus.read(self.pc);
    // 3
    self.stack_push(self.a); // or self.p
    self.s -= 1;
}

// pla, plp

fn pla() {
    // 2
    self.bus.read(self.pc);
    // 3
    self.s += 1;
    // 4
    self.a = self.stack_pull();
}

fn jsr() {
    // 2
    let abl = self.fetch_u8();
    // 3
    self.s -= 1;
    // 4
    self.stack_push(self.pch);
    self.s -= 1;
    // 5
    self.stack_push(self.pcl);
    self.s -= 1;
    // 6
    self.pch = self.fetch_u8();
    self.pcl = abl;
}

fn addr_accumulator() {
    // 2
    self.bus.read(self.pc);
}

fn addr_implied() {
    // 2
    self.bus.read(self.pc);
}

fn addr_immediate() -> u8 {
    // 2
    self.fetch_u8()
}

fn absolute_jmp() {
    // 2
    let abl = self.fetch_u8();
    // 3
    self.pch = self.fetch_u8();
    self.pcl = abl;
}

// LDA, LDX, LDY, EOR, AND, ORA, ADC, SBC, CMP, BIT, LAX, NOP
fn absolute_read() {
    // 2
    let l = self.fetch_u8() as u16;
    // 3
    let h = self.fetch_u8() as u16;
    // 4
    let addr = (h << 8) | l;
    let val = self.bus.read(addr);
}

// ASL, LSR, ROL, ROR, INC, DEC, SLO, SRE, RLA, RRA, ISB, DCP
fn absolute_read_modify_write() {
    // 2
    let l = self.fetch_u8() as u16;
    // 3
    let h = self.fetch_u8() as u16;
    // 4
    let addr = (h << 8) | l;
    let val = self.bus.read(addr);
    // 5 
    // write the value back to effective address, and do the operation on it
    self.bus.write(addr, val);
    todo!(); //do the operation on 'val'
    // 6
    self.bus.write(addr, val);
}

// STA, STX, STY, SAX
fn absolute_write(reg_val: u8) {
    // 2
    let l = self.fetch_u8() as u16;
    // 3
    let h = self.fetch_u8() as u16;
    // 4
    let addr = (h << 8) | l;
    self.bus.write(addr, reg_val);
}

// LDA, LDX, LDY, EOR, AND, ORA, ADC, SBC, CMP, BIT, LAX, NOP
fn zero_page_read() -> u8{
    // 2
    let addr = self.fetch_u8() as u16;
    // 3
    self.bus.read(addr)
}

// ASL, LSR, ROL, ROR, INC, DEC, SLO, SRE, RLA, RRA, ISB, DCP
fn zero_page_read_modify_write() {
    // 2
    let addr = self.fetch_u8() as u16;
    // 3
    let val = self.bus.read(addr);
    // 4
    // write the value back to effective address, and do the operation on it
    self.bus.write(addr, val);
    todo!(); //do the operation on 'val'
    // 5
    self.bus.write(addr, val);
}

// STA, STX, STY, SAX
fn zero_page_write(reg_val: u8) {
    // 2
    let addr = self.fetch_u8() as u16;
    // 3
    self.bus.write(addr, reg_val);
}

// LDA, LDX, LDY, EOR, AND, ORA, ADC, SBC, CMP, BIT, LAX, NOP
fn zero_page_indexed_read(index: u8) -> u8 {
    // 2
    let addr = self.fetch_u8() as u16;
    // 3
    let addr = self.bus.read(addr);
    let addr = addr.wrapping_add(index);
    // 4
    self.bus.read(addr as u16)
}

// ASL, LSR, ROL, ROR, INC, DEC, SLO, SRE, RLA, RRA, ISB, DCP
fn zero_page_indexed_read_modify_write() {
    // 2
    let addr = self.fetch_u8() as u16;
    // 3
    let addr = self.bus.read(addr);
    let addr = addr.wrapping_add(self.x);
    // 4
    let val = self.bus.read(addr as u16)
    // 5
    // write the value back to effective address, and do the operation on it
    self.bus.write(addr, val);
    todo!(); //do the operation on 'val'
    // 6
    self.bus.write(addr, val);
}

// STA, STX, STY, SAX
fn zero_page_indexed_write(index: u8, reg_val: u8) {
    // 2
    let addr = self.fetch_u8() as u16;
    // 3
    let addr = self.bus.read(addr);
    let addr = addr.wrapping_add(index);
    // 4
    self.bus.write(addr as u16, reg_val);
}

// LDA, LDX, LDY, EOR, AND, ORA, ADC, SBC, CMP, BIT, LAX, LAE, SHS, NOP
fn absolute_indexed_read(index: u8) {
    // 2
    let l = self.fetch_u8() as u16;
    // 3
    let h = self.fetch_u8() as u16;
    let l = l.wrapping_add(index);
    // 4
    // The high byte of the effective address may be invalid
    // at this time, i.e. it may be smaller by $100.
    let addr = (h << 8) | l;
    let val = self.bus.read(addr);
    todo!(); // page boundary was crossed
    // 5
    let val = self.bus.read(addr);
}

// ASL, LSR, ROL, ROR, INC, DEC, SLO, SRE, RLA, RRA, ISB, DCP
fn absolute_indexed_read_modify_write() {
    // 2
    let l = self.fetch_u8() as u16;
    // 3
    let h = self.fetch_u8() as u16;
    let l = l.wrapping_add(self.x);
    // 4
    // The high byte of the effective address may be invalid
    // at this time, i.e. it may be smaller by $100.
    let addr = (h << 8) | l;
    let val = self.bus.read(addr);
    todo!(); // page boundary was crossed
    // 5
    let val = self.bus.read(addr);
    // 6
    // write the value back to effective address, and do the operation on it
    self.bus.write(addr, val);
    todo!(); //do the operation on 'val'
    // 7
    self.bus.write(addr, val);
}

// STA, STX, STY, SHA, SHX, SHY
fn absolute_indexed_write(index: u8) {
    // 2
    let l = self.fetch_u8() as u16;
    // 3
    let h = self.fetch_u8() as u16;
    let l = l.wrapping_add(index);
    // 4
    // The high byte of the effective address may be invalid
    // at this time, i.e. it may be smaller by $100.
    let addr = (h << 8) | l;
    let val = self.bus.read(addr);
    todo!(); // page boundary was crossed
    // 5
    self.bus.write(addr? val);
}

// BCC, BCS, BNE, BEQ, BPL, BMI, BVC, BVS
fn relative() {
    // 2
    let operand = self.fetch_u8();
    // 3
    self.bus.read(self.pc);

    if true {
        self.pcl += operand;
    } else {
        self.pc += 1;
    }
    // 4
    todo!();
}

// LDA, ORA, EOR, AND, ADC, CMP, SBC, LAX
fn indexed_x_read() -> u8 {
    // 2
    let addr = self.fetch_u8();
    // 3
    let pointer = self.bus.read(addr);
    let pointer = pointer.wrapping_add(self.x);
    // 4
    let l = self.bus.read(pointer as u16) as u16;
    // 5
    let h = self.bus.read(pointer.wrapping_add(1) as u16) as u16;
    // 6
    let addr = (h << 8) | l;
    self.bus.read(addr)
}

// SLO, SRE, RLA, RRA, ISB, DCP
fn indexed_x_read_modify_write() {
    // 2
    let addr = self.fetch_u8();
    // 3
    let pointer = self.bus.read(addr);
    let pointer = pointer.wrapping_add(self.x);
    // 4
    let l = self.bus.read(pointer as u16) as u16;
    // 5
    let h = self.bus.read(pointer.wrapping_add(1) as u16) as u16;
    // 6
    let addr = (h << 8) | l;
    let val = self.bus.read(addr);
    // 7
    // write the value back to effective address, and do the operation on it
    self.bus.write(addr, val);
    todo!(); //do the operation on 'val'
    // 8
    self.bus.write(addr, val);
}

// STA, SAX
fn indexed_x_write(reg_val: u8) {
    // 2
    let addr = self.fetch_u8();
    // 3
    let pointer = self.bus.read(addr);
    let pointer = pointer.wrapping_add(self.x);
    // 4
    let l = self.bus.read(pointer as u16) as u16;
    // 5
    let h = self.bus.read(pointer.wrapping_add(1) as u16) as u16;
    // 6
    let addr = (h << 8) | l;
    self.bus.write(addr, reg_val);
}