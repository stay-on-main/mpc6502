pub struct Bus {

}

impl Bus {
    pub fn read_u8(&self, addr: u16) -> u8 {
        todo!();
    }

    pub fn write_u8(&mut self, addr: u16, val: u8) {
        todo!();
    }

    pub fn read_u16(&self, addr: u16) -> u16 {
        let l = self.read_u8(addr);
        let h = self.read_u8(addr + 1);
        (l as u16) | ((h as u16) << 8)
    }
}
