//                         N      E     S     
const NES_TAG: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];

pub enum Mirroring {
    Vertical,
    Horizontal,
    FourScreen,
}

pub struct Rom {
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub mapper: u8,
    pub mirroring: Mirroring
}

impl Rom {
    pub fn new(file: &Vec<u8>) -> Result<Rom, String> {
        if &file[0..4] != NES_TAG {
            return Err(String::from("Not in iNES format"));
        }

        let len_prg_rom = file[4] as usize * 16384;
        let len_chr_rom = file[5] as usize * 8192;
        
        let trainer = file[6] & 0b0000_0100 != 0;
        
        let prg_rom_start = 16 + if trainer {512} else {0};
        let chr_rom_start = prg_rom_start + len_prg_rom;

        //let has_battery_ram = file[6] & 0b0000_0010 != 0;
        
        let four_screen = file[6] & 0b0000_1000 != 0;
        let vert_mirror = file[6] & 0b0000_0001 != 0;
        let mirroring = match (four_screen, vert_mirror) {
            (true, _)       => Mirroring::FourScreen,
            (false, true)   => Mirroring::Vertical,
            (false, false)  => Mirroring::Horizontal,
        };

        // First 4 bits of F7 followed by First 4 bits of F6
        let mapper = (file[7] & 0b1111_0000) | (file[6] >> 4);
        if (file[7] >> 2) & 0b0000_0011 != 0 {
            return Err(String::from("NES2.0 format not supported"));
        }

        Ok(Rom { 
            prg_rom: file[prg_rom_start..(prg_rom_start + len_prg_rom)].to_vec(),
            chr_rom: file[chr_rom_start..(chr_rom_start + len_chr_rom)].to_vec(),
            mapper,
            mirroring
        })

    }
}