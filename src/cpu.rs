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

enum Operand {
    Mem(u16),
    A(u8),
    X(u8),
    Y(u8),
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

    fn addr_zero_page_y(&mut self) -> Operand {
        let addr = self.fetch_u8();
        let addr = addr.wrapping_add(self.y);
        self.bus.clk();
        Operand::Mem(addr)
    }
    /*
    fn addr_absolute(&mut self) -> u16 {
        let l = self.fetch_u8() as u16;
        let h = self.fetch_u8() as u16;
        let addr = (h << 8) | l;
        self.bus.clk();
        addr
    }
    */
    fn addr_absolute(&mut self) -> u16 {
        let l = self.fetch_u8() as u16;
        let h = self.fetch_u8() as u16;
        let addr = (h << 8) | l;
        self.bus.clk();
        addr
    }

    fn adc(&mut self) {
        let arg = self.addr_zero_page_y();
        arg.read();


        arg.set();
    }
}