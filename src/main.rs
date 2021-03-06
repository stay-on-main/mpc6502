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

mod mpc6502;
use mpc6502::{Cpu};

mod nes;
use nes::{Rom};

fn main() {
    let bus = Bus::new();
    let mut cpu = Cpu::new(bus);

    for _ in 0..8991 {
        cpu.clock();
    }
}
