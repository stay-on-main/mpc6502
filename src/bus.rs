use super::nes::{Rom};

pub struct Bus {
    ram: [u8; 2048],
    rom: Rom,
}

impl Bus {
    pub fn read_u8(&self, addr: u16) -> u8 {
        if addr <= 0x1FFF {
            // RAM
            let addr = addr & 0x7FF;
            self.ram[addr as usize]
        }
        else if addr <= 0x3FFF {
            // Registers Video
            let addr = 0x2000 + addr & 0b111;
            0x0
        }
        else if addr <= 0x4017 {
            // Registers Audio & DMA & I/O
            todo!();
        }
        else if addr <= 0x4FFF {
            // Not used
            todo!();
        }
        else if addr <= 0x5FFF {
            // Expansion ROM\RAM (etc. in MMC5)
            todo!();
        }
        else if addr <= 0x7FFF {
            // SRAM (aka WRAM) (etc. in MMC3)
            todo!();
        }
        else {
            // PRG-ROM)
            self.rom.read(addr)
        }
    }

    pub fn write_u8(&mut self, addr: u16, val: u8) {
        if addr <= 0x1FFF {
            // RAM
            let addr = addr & 0x7FF;
            self.ram[addr as usize] = val;
        } else if addr <= 0x3FFF{
            //println!("write video ram");
        } else if addr <= 0x4017 {
            // Registers Audio & DMA & I/O
            //println!("write audio ram");
        }
        else if addr <= 0x4FFF {
            // Not used
            todo!();
        }
        else if addr <= 0x5FFF {
            // Expansion ROM\RAM (etc. in MMC5)
            todo!();
        }
        else if addr <= 0x7FFF {
            // SRAM (aka WRAM) (etc. in MMC3)
            todo!();
        }
        else {
            // PRG-ROM)
            todo!();
        }
    }

    pub fn read_u16(&self, addr: u16) -> u16 {
        let l = self.read_u8(addr);
        let h = self.read_u8(addr + 1);
        (l as u16) | ((h as u16) << 8)
    }

    pub fn new() -> Self {
        Self {
            rom: Rom::new("C:/github/mpc6502/nestest.nes"),
            ram: [0u8; 2048],
        }
    }
}
