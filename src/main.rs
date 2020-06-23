// http://dendy.migera.ru/nes/g11.html
// http://www.obelisk.me.uk/6502/reference.html
// https://wiki.nesdev.com/w/index.php/CPU_addressing_modes

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
    Bit(fn (cpu: &mut Cpu) -> u16),
    Brk(fn (cpu: &mut Cpu) -> u16),
    Clc(fn (cpu: &mut Cpu) -> u16),
    Cld(fn (cpu: &mut Cpu) -> u16),
    Cli(fn (cpu: &mut Cpu) -> u16),
    Clv(fn (cpu: &mut Cpu) -> u16),
    Cmp(fn (cpu: &mut Cpu) -> u16),
    Cpx(fn (cpu: &mut Cpu) -> u16),
    Cpy(fn (cpu: &mut Cpu) -> u16),
    Dec(fn (cpu: &mut Cpu) -> u16),
    Dex(fn (cpu: &mut Cpu) -> u16),
    Dey(fn (cpu: &mut Cpu) -> u16),
    Eor(fn (cpu: &mut Cpu) -> u16),
    Inc(fn (cpu: &mut Cpu) -> u16),
    Inx(fn (cpu: &mut Cpu) -> u16),
    Iny(fn (cpu: &mut Cpu) -> u16),
    Jmp(fn (cpu: &mut Cpu) -> u16),
    Jsr(fn (cpu: &mut Cpu) -> u16),
    Lda(fn (cpu: &mut Cpu) -> u16),
    Ldx(fn (cpu: &mut Cpu) -> u16),
    Ldy(fn (cpu: &mut Cpu) -> u16),
    Lsr(fn (cpu: &mut Cpu) -> u16),
    Nop(fn (cpu: &mut Cpu) -> u16),
    Ora(fn (cpu: &mut Cpu) -> u16),
    Pha(fn (cpu: &mut Cpu) -> u16),
    Php(fn (cpu: &mut Cpu) -> u16),
    Pla(fn (cpu: &mut Cpu) -> u16),
    Plp(fn (cpu: &mut Cpu) -> u16),
    Rol(fn (cpu: &mut Cpu) -> u16),
    Ror(fn (cpu: &mut Cpu) -> u16),
    Rti(fn (cpu: &mut Cpu) -> u16),
    Rts(fn (cpu: &mut Cpu) -> u16),
    Sbc(fn (cpu: &mut Cpu) -> u16),
    Sec(fn (cpu: &mut Cpu) -> u16),
    Sed(fn (cpu: &mut Cpu) -> u16),
    Sei(fn (cpu: &mut Cpu) -> u16),
    Sta(fn (cpu: &mut Cpu) -> u16),
    Stx(fn (cpu: &mut Cpu) -> u16),
    Sty(fn (cpu: &mut Cpu) -> u16),
    Tax(fn (cpu: &mut Cpu) -> u16),
    Tay(fn (cpu: &mut Cpu) -> u16),
    Tsx(fn (cpu: &mut Cpu) -> u16),
    Txa(fn (cpu: &mut Cpu) -> u16),
    Txs(fn (cpu: &mut Cpu) -> u16),
    Tya(fn (cpu: &mut Cpu) -> u16),
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
            &Self::Bit(addresing_mode) => {
                todo!();
            },
            &Self::Brk(addresing_mode) => {
                // BRK - Force Interrupt
                todo!();
            },
            &Self::Clc(addresing_mode) => {
                todo!();
            },
            &Self::Cld(addresing_mode) => {
                todo!();
            },
            &Self::Cli(addresing_mode) => {
                todo!();
            },
            &Self::Clv(addresing_mode) => {
                todo!();
            },
            &Self::Cmp(addresing_mode) => {
                todo!();
            },
            &Self::Cpx(addresing_mode) => {
                todo!();
            },
            &Self::Cpy(addresing_mode) => {
                todo!();
            },
            &Self::Dec(addresing_mode) => {
                todo!();
            },
            &Self::Dex(addresing_mode) => {
                todo!();
            },
            &Self::Dey(addresing_mode) => {
                todo!();
            },
            &Self::Eor(addresing_mode) => {
                todo!();
            },
            &Self::Inc(addresing_mode) => {
                todo!();
            },
            &Self::Inx(addresing_mode) => {
                todo!();
            },
            &Self::Iny(addresing_mode) => {
                todo!();
            },
            &Self::Jmp(addresing_mode) => {
                todo!();
            },
            &Self::Jsr(addresing_mode) => {
                todo!();
            },
            &Self::Lda(addresing_mode) => {
                todo!();
            },
            &Self::Ldx(addresing_mode) => {
                todo!();
            },
            &Self::Ldy(addresing_mode) => {
                todo!();
            },
            &Self::Lsr(addresing_mode) => {
                todo!();
            },
            &Self::Nop(addresing_mode) => {
                todo!();
            },
            &Self::Ora(addresing_mode) => {
                todo!();
            },
            &Self::Pha(addresing_mode) => {
                todo!();
            },
            &Self::Php(addresing_mode) => {
                todo!();
            },
            &Self::Pla(addresing_mode) => {
                todo!();
            },
            &Self::Plp(addresing_mode) => {
                todo!();
            },
            &Self::Rol(addresing_mode) => {
                todo!();
            },
            &Self::Ror(addresing_mode) => {
                todo!();
            },
            &Self::Rti(addresing_mode) => {
                todo!();
            },
            &Self::Rts(addresing_mode) => {
                todo!();
            },
            &Self::Sbc(addresing_mode) => {
                todo!();
            },
            &Self::Sec(addresing_mode) => {
                todo!();
            },
            &Self::Sed(addresing_mode) => {
                todo!();
            },
            &Self::Sei(addresing_mode) => {
                todo!();
            },
            &Self::Sta(addresing_mode) => {
                todo!();
            },
            &Self::Stx(addresing_mode) => {
                todo!();
            },
            &Self::Sty(addresing_mode) => {
                todo!();
            },
            &Self::Tax(addresing_mode) => {
                todo!();
            },
            &Self::Tay(addresing_mode) => {
                todo!();
            },
            &Self::Tsx(addresing_mode) => {
                todo!();
            },
            &Self::Txa(addresing_mode) => {
                todo!();
            },
            &Self::Txs(addresing_mode) => {
                todo!();
            },
            &Self::Tya(addresing_mode) => {
                todo!();
            },
        }
    }
}

const CPU_OP_CODE: [OpCode; 96] = [
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
    OpCode::Bne(Cpu::relative), // $D0
    OpCode::Bpl(Cpu::relative), // $10
    OpCode::Bvc(Cpu::relative), // $50
    OpCode::Bvs(Cpu::relative), // $70

    OpCode::Bit(Cpu::zero_page), // $24
    OpCode::Bit(Cpu::absolute), // $2C

    OpCode::Brk(Cpu::implicit), // $00

    OpCode::Clc(Cpu::implicit), // $18

    OpCode::Cld(Cpu::implicit), // $D8

    OpCode::Cli(Cpu::implicit), // $58

    OpCode::Clv(Cpu::implicit), // $B8

    OpCode::Cmp(Cpu::immediate), // $C9
    OpCode::Cmp(Cpu::zero_page), // $C5
    OpCode::Cmp(Cpu::zero_page_x), // $D5
    OpCode::Cmp(Cpu::absolute), // $CD
    OpCode::Cmp(Cpu::absolute_x), // $DD
    OpCode::Cmp(Cpu::absolute_y), // $D9
    OpCode::Cmp(Cpu::indirect_x), // $C1
    OpCode::Cmp(Cpu::indirect_y), // $D1

    OpCode::Cpx(Cpu::immediate), // $E0
    OpCode::Cpx(Cpu::zero_page), // $E4
    OpCode::Cpx(Cpu::absolute), // $EC

    OpCode::Cpy(Cpu::immediate), // $C0
    OpCode::Cpy(Cpu::zero_page), // $C4
    OpCode::Cpy(Cpu::absolute), // $CC

    OpCode::Dec(Cpu::zero_page), // $C6
    OpCode::Dec(Cpu::zero_page_x), // $D6
    OpCode::Dec(Cpu::absolute), // $CE
    OpCode::Dec(Cpu::absolute_x), // $DE

    OpCode::Dex(Cpu::implicit), // $CA

    OpCode::Dey(Cpu::implicit), // $88

    OpCode::Eor(Cpu::immediate), // $49
    OpCode::Eor(Cpu::zero_page), // $45
    OpCode::Eor(Cpu::zero_page_x), // $55
    OpCode::Eor(Cpu::absolute), // $4D
    OpCode::Eor(Cpu::absolute_x), // $5D
    OpCode::Eor(Cpu::absolute_y), // $59
    OpCode::Eor(Cpu::indirect_x), // $41
    OpCode::Eor(Cpu::indirect_y), // $51

    OpCode::Inc(Cpu::zero_page), // $E6
    OpCode::Inc(Cpu::zero_page_x), // $F6
    OpCode::Inc(Cpu::absolute), // $EE
    OpCode::Inc(Cpu::absolute_x), // $FE

    OpCode::Inx(Cpu::implicit), // $E8

    OpCode::Iny(Cpu::implicit), // $C8

    OpCode::Jmp(Cpu::absolute), // $4C
    OpCode::Jmp(Cpu::indirect), // $6C

    OpCode::Jsr(Cpu::absolute), // $20

    OpCode::Lda(Cpu::immediate), // $A9
    OpCode::Lda(Cpu::zero_page), // $A5
    OpCode::Lda(Cpu::zero_page_x), // $B5
    OpCode::Lda(Cpu::absolute), // $AD
    OpCode::Lda(Cpu::absolute_x), // $BD
    OpCode::Lda(Cpu::absolute_y), // $B9
    OpCode::Lda(Cpu::indirect_x), // $A1
    OpCode::Lda(Cpu::indirect_y), // $B1

    OpCode::Ldx(Cpu::immediate), // $A2
    OpCode::Ldx(Cpu::zero_page), // $A6
    OpCode::Ldx(Cpu::zero_page_y), // $B6
    OpCode::Ldx(Cpu::absolute), // $AE
    OpCode::Ldx(Cpu::absolute_y), // $BE

    OpCode::Ldy(Cpu::immediate), // $A0
    OpCode::Ldy(Cpu::zero_page), // $A4
    OpCode::Ldy(Cpu::zero_page_x), // $B4
    OpCode::Ldy(Cpu::absolute), // $AC
    OpCode::Ldy(Cpu::absolute_x), // $BC

    OpCode::Lsr(Cpu::accumulator), // $4A
    OpCode::Lsr(Cpu::zero_page), // $46
    OpCode::Lsr(Cpu::zero_page_x), // $56
    OpCode::Lsr(Cpu::absolute), // $4E
    OpCode::Lsr(Cpu::absolute_x), // $5E
];

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

    fn implicit(&mut self) -> u16 {
        todo!();
    }

    fn accumulator(&mut self) -> u16 {
        todo!();
    }
}

fn main() {
    println!("Hello, world!");
}

