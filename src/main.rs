// http://dendy.migera.ru/nes/g11.html
// http://www.obelisk.me.uk/6502/reference.html

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
    mem: [u8; 0xffff],
}

enum OpCode {
    Adc(fn (cpu: &mut Cpu) -> u16),
    And(fn (cpu: &mut Cpu) -> u16),
    Asl(fn (cpu: &mut Cpu) -> u16),
    Bcc(fn (cpu: &mut Cpu) -> i8),
    Bcs(fn (cpu: &mut Cpu) -> i8),
    Beq(fn (cpu: &mut Cpu) -> i8),
    Bmi(fn (cpu: &mut Cpu) -> i8),
    Bne(fn (cpu: &mut Cpu) -> i8),
    Bpl(fn (cpu: &mut Cpu) -> i8),
    Bvc(fn (cpu: &mut Cpu) -> i8),
    Bvs(fn (cpu: &mut Cpu) -> i8),
}

impl OpCode {
    fn run(&self, cpu: &mut Cpu) {
        match self {
            &Self::Adc(addresing_mode) => {
                // ADC - Add with Carry
                let addr = addresing_mode(cpu);
                todo!();
            },
            &Self::And(addresing_mode) => {
                // AND - Logical AND
                todo!();
            },
            &Self::Asl(addresing_mode) => {
                // ASL - Arithmetic Shift Left
                todo!();
            },
            &Self::Bcc(addresing_mode) => {
                // BCC - Branch if Carry Clear
                todo!();
            },
            &Self::Bcs(addresing_mode) => {
                // BCS - Branch if Carry Set
                todo!();
            },
            &Self::Beq(addresing_mode) => {
                todo!();
            },
            &Self::Bmi(addresing_mode) => {
                todo!();
            },
            &Self::Bne(addresing_mode) => {
                todo!();
            },
            &Self::Bpl(addresing_mode) => {
                todo!();
            },
            &Self::Bvc(addresing_mode) => {
                todo!();
            },
            &Self::Bvs(addresing_mode) => {
                todo!();
            },
            _ => {},
        }
    }
}

const CPU_OP_CODE: [OpCode; 29] = [
    OpCode::Adc(Cpu::immediate), // $69
    OpCode::Adc(Cpu::zero_page), // $65
    OpCode::Adc(Cpu::zero_page_x), // $75
    OpCode::Adc(Cpu::absolute), // $6D
    OpCode::Adc(Cpu::absolute_x), // $7D
    OpCode::Adc(Cpu::absolute_y), // $79
    OpCode::Adc(Cpu::indirect_x), // $61
    OpCode::Adc(Cpu::indirect_y), // $71

    OpCode::And(Cpu::immediate), // $29
    OpCode::And(Cpu::zero_page), // $25
    OpCode::And(Cpu::zero_page_x), // $35
    OpCode::And(Cpu::absolute), // $2D
    OpCode::And(Cpu::absolute_x), // $3D
    OpCode::And(Cpu::absolute_y), // $39
    OpCode::And(Cpu::indirect_x), // $21
    OpCode::And(Cpu::indirect_y), // $31

    OpCode::Asl(Cpu::accumulator), // $0A
    OpCode::Asl(Cpu::zero_page), // $06
    OpCode::Asl(Cpu::zero_page_x), // $16
    OpCode::Asl(Cpu::absolute), // $0E
    OpCode::Asl(Cpu::absolute_x), // $1E

    OpCode::Bcc(Cpu::relative), // $90
    OpCode::Bcs(Cpu::relative), // $b0

    OpCode::Beq(Cpu::relative), // $f0
    OpCode::Bmi(Cpu::relative), // $30
    OpCode::Bne(Cpu::relative), // $d0
    OpCode::Bpl(Cpu::relative), // $10
    OpCode::Bvc(Cpu::relative), // $50
    OpCode::Bvs(Cpu::relative), // $70
];

/*
enum Instruction {
    Implicit(fn (cpu: &mut Cpu6502, addr: u16)),
    Accumulator(fn (cpu: &mut Cpu6502, addr: u16)),
    Immediate(fn (cpu: &mut Cpu6502, addr: u16)),
    ZeroPage(fn (cpu: &mut Cpu6502, addr: u16)),
    ZeroPageX(fn (cpu: &mut Cpu6502, addr: u16)),
    ZeroPageY(fn (cpu: &mut Cpu6502, addr: u16)),
    Relative(fn (cpu: &mut Cpu6502, addr: u16)),
    Absolute(fn (cpu: &mut Cpu6502, addr: u16)),
    AbsoluteX(fn (cpu: &mut Cpu6502, addr: u16)),
    AbsoluteY(fn (cpu: &mut Cpu6502, addr: u16)),
    Indirect(fn (cpu: &mut Cpu6502, addr: u16)),
    IndirectX(fn (cpu: &mut Cpu6502, addr: u16)),
    IndirectY(fn (cpu: &mut Cpu6502, addr: u16)),
}

impl Instruction {
    fn run(&self, cpu: &mut Cpu6502) {
        match self {
            &Self::Immediate(imm) => {
                imm(cpu, 10);
            },
            &Self::ZeroPage(zp) => {
                zp(cpu, 10);
            },
            &Self::ZeroPageX(zpx) => {
                zpx(cpu, 10);
            },
            _ => {},
        }
    }
}

const CPU6502_INSTRUCTION: [Instruction; 21] = [
    Instruction::Adc(Cpu::immediate),
    // ADC - Add with Carry
    Instruction::Immediate(Cpu6502::adc), // $69
    Instruction::ZeroPage(Cpu6502::adc), // $65
    Instruction::ZeroPageX(Cpu6502::adc), // $75
    Instruction::Absolute(Cpu6502::adc), // $6D
    Instruction::AbsoluteX(Cpu6502::adc), // $7D
    Instruction::AbsoluteY(Cpu6502::adc), // $79
    Instruction::IndirectX(Cpu6502::adc), // $61
    Instruction::IndirectY(Cpu6502::adc), // $71
    // AND - Logical AND
    Instruction::Immediate(Cpu6502::and), // $29
    Instruction::ZeroPage(Cpu6502::and), // $25
    Instruction::ZeroPageX(Cpu6502::and), // $35
    Instruction::Absolute(Cpu6502::and), // $2D
    Instruction::AbsoluteX(Cpu6502::and), // $3D
    Instruction::AbsoluteY(Cpu6502::and), // $39
    Instruction::IndirectX(Cpu6502::and), // $21
    Instruction::IndirectY(Cpu6502::and), // $31
    // ASL - Arithmetic Shift Left
    Instruction::Accumulator(Cpu6502::asl), // $0A
    Instruction::ZeroPage(Cpu6502::asl), // $06
    Instruction::ZeroPageX(Cpu6502::asl), // $16
    Instruction::Absolute(Cpu6502::asl), // $0E
    Instruction::AbsoluteX(Cpu6502::asl), // $1E
    // 
];
*/
impl Cpu {
    fn new() -> Self {
        todo!();
    }

    fn execute(&mut self, byte: u8) {
        //let instruction = &CPU6502_INSTRUCTION[byte as usize];
        //instruction.run(self);
        todo!();
    }


    fn immediate(&mut self) -> u16 {
        todo!();
    }

    fn zero_page(&mut self) -> u16 {
        todo!();
    }

    fn zero_page_x(&mut self) -> u16 {
        todo!();
    }

    fn zero_page_y(&mut self) -> u16 {
        todo!();
    }

    fn absolute(&mut self) -> u16 {
        todo!();
    }

    fn absolute_x(&mut self) -> u16 {
        todo!();
    }

    fn absolute_y(&mut self) -> u16 {
        todo!();
    }

    fn indirect(&mut self) -> u16 {
        todo!();
    }

    fn indirect_x(&mut self) -> u16 {
        todo!();
    }

    fn indirect_y(&mut self) -> u16 {
        todo!();
    }

    fn relative(&mut self) -> i8 {
        todo!();
    }
    /*
    Implicit(fn (cpu: &mut Cpu6502, addr: u16)),
    Accumulator(fn (cpu: &mut Cpu6502, addr: u16)),
    */
}
/*
const ADC_IMM: u8   = 0x69;
const ADC_ZP: u8    = 0x65;
const ADC_ZPX: u8   = 0x75;
const ADC_ABS: u8   = 0x6D;
const ADC_ABS_X: u8 = 0x7d;
const ADC_ABS_Y: u8 = 0x79;
const ADC_IND_X: u8 = 0x61;
const ADC_IND_Y: u8 = 0x71;

const AND_IMM: u8   = 0x29;
const AND_ZP: u8    = 0x25;
const AND_ZPX: u8   = 0x35;
const AND_ABS: u8   = 0x2D;
const AND_ABS_X: u8 = 0x3d;
const AND_ABS_Y: u8 = 0x39;
const AND_IND_X: u8 = 0x21;
const AND_IND_Y: u8 = 0x31;

const ASL_ACC: u8   = 0x0a;
const ASL_ZP: u8    = 0x06;
const ASL_ZPX: u8   = 0x16;
const ASL_ABS: u8   = 0x0e;
const ASL_ABS_X: u8 = 0x1e;

const BCC_REL: u8 = 0x90;
const BCS_REL: u8 = 0xb0;
const BEQ_REL: u8 = 0xf0;
const BMI_REL: u8 = 0x30;
const BNE_REL: u8 = 0xd0;
const BPL_REL: u8 = 0x10;
const BVC_REL: u8 = 0x50;
const BVS_REL: u8 = 0x70;

const BIT_ZP: u8 = 0x24;
const BIT_ABS: u8 = 0x2c;

const BRK_IMPL: u8 = 0x00;

const CLC_IMPL: u8 = 0x18;
const CLD_IMPL: u8 = 0xd8;
const CLI_IMPL: u8 = 0x58;
const CLV_IMPL: u8 = 0xb8;

const CMP_IMM: u8 = 0xc9;
const CMP_ZP: u8 = 0xc5;
const CMP_ZPX: u8 = 0xd5;
const CMP_ABS: u8 = 0xcd;
const CMP_ABS_X: u8 = 0xdd;
const CMP_ABS_Y: u8 = 0xd9;
const CMP_IND_X: u8 = 0xc1;
const CMP_IND_Y: u8 = 0xd1;
const CPX_IMM: u8 = 0xe0;
const CPX_ZP: u8 = 0xe4;
const CPX_ABS: u8 = 0xec;
const CPY_IMM: u8 = 0xc0;
const CPY_ZP: u8 = 0xc4;
const CPY_ABS: u8 = 0xcc;

const DEC_ZP: u8 = 0xc6;
const DEC_ZPX: u8 = 0xd6;
const DEC_ABS: u8 = 0xce;
const DEC_ABS_X: u8 = 0xde;



impl Cpu6502 {
    fn adc(&mut self, arg: u8) {

    }

    fn and(&mut self, arg: u8) {

    }

    fn asl(&mut self, arg: u8) {

    }

    fn branch(&mut self, offset: i8, condition: bool) {
        if condition {
            self.pc = ((self.pc as i32) + (offset as i32)) as u16;
        }
    }

    fn bit(&mut self, arg: u8) {

    }

    fn brk(&mut self) {

    }

    fn clear(&mut self, bit: &mut bool) {

    }

    fn compare(&mut self, arg: u8, op: u8) {

    }

    fn dec(&mut self, arg: u8) {

    }

    fn fetch_u8(&mut self) -> u8 {
        let byte = self.mem[self.pc as usize];
        self.pc += 1;
        byte
    }

    fn fetch_u16(&mut self) -> u16 {
        let byte_l = self.fetch_u8() as u16;
        let byte_h = self.fetch_u8() as u16;
        (byte_h << 8) | byte_l
    }

    fn imm(&mut self) -> u8 {
        self.fetch_u8()
    }

    fn zp(&mut self) -> u8 {
        let arg = self.fetch_u8();
        self.mem[arg as usize]
    }

    fn zpx(&mut self) -> u8 {
        let x = self.fetch_u8();
        let addr = self.mem[x as usize] + self.x;
        self.mem[addr as usize]
    }

    fn abs(&mut self) -> u8 {
        let addr = self.fetch_u16();
        self.mem[addr as usize]
    }

    fn abs_x(&mut self) -> u8 {
        let addr = self.fetch_u16();
        let addr = addr + self.x as u16;
        self.mem[addr as usize]
    }

    fn abs_y(&mut self) -> u8 {
        let addr = self.fetch_u16();
        let addr = addr + self.x as u16;
        self.mem[addr as usize]
    }

    fn ind_x(&mut self) -> u8 {
        let arg = self.fetch_u8();
        let addr = self.mem[arg as usize] + self.x;
        let byte_l = self.mem[addr as usize];
        let byte_h = self.mem[addr as usize + 1];
        let addr = (byte_h << 8) | byte_l;
        self.mem[addr as usize]
    }

    fn ind_y(&mut self) -> u8 {
        let addr = self.fetch_u8();
        let byte_l = self.mem[addr as usize];
        let byte_h = self.mem[addr as usize + 1];
        let addr = (byte_h << 8) | byte_l;
        let addr = addr + self.y;
        self.mem[addr as usize]
    }

    fn acc(&self) -> u8 {
        self.a
    }

    fn rel(&mut self) -> i8 {
        self.fetch_u8() as i8
    }

    fn execute(&mut self) {
        match self.fetch_u8() {
            // ADC
            ADC_IMM => { let arg = self.imm(); self.adc(arg); },
            ADC_ZP => { let arg = self.zp(); self.adc(arg); },
            ADC_ZPX => { let arg = self.zpx(); self.adc(arg); },
            ADC_ABS => { let arg = self.abs(); self.adc(arg); },
            ADC_ABS_X => { let arg = self.abs_x(); self.adc(arg); },
            ADC_ABS_Y => { let arg = self.abs_y(); self.adc(arg); },
            ADC_IND_X => { let arg = self.ind_x(); self.adc(arg); },
            ADC_IND_Y => { let arg = self.ind_y(); self.adc(arg); },
            // AND
            AND_IMM => { let arg = self.imm(); self.and(arg); },
            AND_ZP => { let arg = self.zp(); self.and(arg); },
            AND_ZPX => { let arg = self.zpx(); self.and(arg); },
            AND_ABS => { let arg = self.abs(); self.and(arg); },
            AND_ABS_X => { let arg = self.abs_x(); self.and(arg); },
            AND_ABS_Y => { let arg = self.abs_y(); self.and(arg); },
            AND_IND_X => { let arg = self.ind_x(); self.and(arg); },
            AND_IND_Y => { let arg = self.ind_y(); self.and(arg); },
            // ASL
            ASL_ACC => { let arg = self.acc(); self.asl(arg); },
            ASL_ZP => { let arg = self.zp(); self.asl(arg); },
            ASL_ZPX => { let arg = self.zpx(); self.asl(arg); },
            ASL_ABS => { let arg = self.abs(); self.asl(arg); },
            ASL_ABS_X => { let arg = self.abs_x(); self.asl(arg); },
            // BRANCH
            BCC_REL => { let arg = self.rel(); self.branch(arg as i8, self.p.c == false); },
            BCS_REL => { let arg = self.rel(); self.branch(arg as i8, self.p.c == true); },
            BEQ_REL => { let arg = self.rel(); self.branch(arg as i8, self.p.z == true); },
            BMI_REL => { let arg = self.rel(); self.branch(arg as i8, self.p.n == true); },
            BNE_REL => { let arg = self.rel(); self.branch(arg as i8, self.p.z == false); },
            BPL_REL => { let arg = self.rel(); self.branch(arg as i8, self.p.n == false); },
            BVC_REL => { let arg = self.rel(); self.branch(arg as i8, self.p.v == false); },
            BVS_REL => { let arg = self.rel(); self.branch(arg as i8, self.p.v == true); },
            // BIT
            BIT_ZP => { let arg = self.zp(); self.bit(arg); },
            BIT_ABS => { let arg = self.abs(); self.bit(arg); },
            // BRK
            BRK_IMPL => { self.brk(); },
            // CLEAR
            CLC_IMPL => { self.p.c = false; },
            CLD_IMPL => { self.p.d = false; },
            CLI_IMPL => { self.p.i = false; },
            CLV_IMPL => { self.p.v = false; },
            // COMPARE
            CMP_IMM => { let arg = self.imm(); self.compare(self.a, arg); },
            CMP_ZP => { let arg = self.zp(); self.compare(self.a, arg); },
            CMP_ZPX => { let arg = self.zpx(); self.compare(self.a, arg); },
            CMP_ABS => { let arg = self.abs(); self.compare(self.a, arg); },
            CMP_ABS_X => { let arg = self.abs_x(); self.compare(self.a, arg); },
            CMP_ABS_Y => { let arg = self.abs_y(); self.compare(self.a, arg); },
            CMP_IND_X => { let arg = self.ind_x(); self.compare(self.a, arg); },
            CMP_IND_Y => { let arg = self.ind_y(); self.compare(self.a, arg); },
            CPX_IMM => { let arg = self.imm(); self.compare(self.x, arg); },
            CPX_ZP => { let arg = self.zp(); self.compare(self.x, arg); },
            CPX_ABS => { let arg = self.abs(); self.compare(self.x, arg); },
            CPY_IMM => { let arg = self.imm(); self.compare(self.y, arg); },
            CPY_ZP => { let arg = self.zp(); self.compare(self.y, arg); },
            CPY_ABS => { let arg = self.abs(); self.compare(self.y, arg); },
            // DEC
            DEC_ZP =>,
            DEC_ZPX =>,
            DEC_ABS =>,
            DEC_ABS_X =>,
            _ => {},
        }
    }
}
*/
fn main() {
    println!("Hello, world!");
}

