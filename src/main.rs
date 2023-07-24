use std::io::prelude::*;
use std::fs::File;
use rand::Rng;
use std::env;

const FONT_OFFSET: usize = 0x050;

struct CPU {
    delay_timer: u8,
    sound_timer: u8,

    position_in_memory: usize,

    memory: [u8; 0x1000],

    // CHIP-8 registers are 8bit
    registers: [u8; 16],

    // stack max size is 16 of 16 bit values (addr. in memory)
    stack: [u16; 16],
    // basically an index of the stack which shows our last location
    stack_pointer: usize,

    // the display is 64 by 32
    display: [[u8; 64]; 32],

    index_register: usize,
}

impl CPU {
    fn read_opcode(&self) -> u16 {
        let p = self.position_in_memory;

        let op_byte1 = self.memory[p] as u16;
        let op_byte2 = self.memory[p + 1] as u16;

        // shift left 8 times so we get 1st byte in the left group
        // then use OR to get the 2nd byte to the right
        (op_byte1 << 8) | op_byte2
    }

    fn run(&mut self, original: bool) {
        loop {
            let opcode = self.read_opcode();
            self.position_in_memory += 2;

            //let c = ((opcode & 0xf000) >> 12) as u8;
            let x = ((opcode & 0x0f00) >> 8) as u8;
            let y = ((opcode & 0x00f0) >> 4) as u8;
            let d = ((opcode & 0x000f) >> 0) as u8;

            let op_minor = (opcode & 0x000f) as u8;

            // take 3 rightmost nibbles
            let nnn = opcode & 0x0fff;
            // and 2 rightmost nibbles
            let kk = opcode & 0x00ff;

            match opcode {
                // finish exec opcode
                0x0000 => {
                    return;
                }
                // return opcode
                0x00ee => self.ret(),
                // clear the display
                0x00e0 => self.clr(),
                // jump opcodes
                0x1000..=0x1fff => self.jmp(nnn),
                // call opcode
                0x2000..=0x2fff => self.call(nnn),
                0x3000..=0x3fff => self.skip_eq(x, kk),
                0x4000..=0x4fff => self.skip_neq(x, kk),
                0x5000..=0x5fff => self.skip_eq_xy(x, y),
                0x6000..=0x6fff => self.set(x, kk),
                0x7000..=0x7fff => self.add(x, kk),
                // two operand opcodes
                0x8000..=0x8fff => {
                    match op_minor {
                        0 => self.set_xy(x, y),
                        1 => self.bin_or_xy(x, y),
                        2 => self.bin_and_xy(x, y),
                        3 => self.bin_xor_xy(x, y),
                        4 => self.add_xy(x, y),
                        5 => self.sub_xy(x, y, true),
                        6 => self.shift_xy(x, y, true, original),
                        7 => self.sub_xy(x, y, false),
                        0xe => self.shift_xy(x, y, false, original),
                        _ => todo!("opcode {:04x}", opcode),
                    }
                }
                0x9000..=0x9fff => self.skip_neq_xy(x, y),
                // set index register opcodes
                0xa000..=0xafff => self.set_idx(nnn),
                0xb000..=0xbfff => self.jmp_w_offset(x, nnn, original),
                0xc000..=0xcfff => self.rand(x, kk),
                0xd000..=0xdfff => self.to_display(x, y, d),
                0xe000..=0xefff => {
                    match kk {
                        0x00a1 => self.skip_not_pressed(x),
                        0x009e => self.skip_pressed(x),
                        _ => todo!("opcode {:04x}", opcode),
                    }
                }
                0xf000..=0xffff => {
                    match kk {
                        0x0007 => self.get_delay(x),
                        0x0015 => self.set_delay(x),
                        0x0018 => self.set_sound(x),
                        0x001e => self.add_to_idx(x),
                        0x000a => self.get_key(x),
                        0x0029 => self.get_font_char(x),
                        0x0033 => self.bin_dec_conversion(x),
                        0x0055 => self.store_reg_to_mem(x, original),
                        0x0065 => self.get_from_mem_to_reg(x, original),
                        _ => todo!("opcode {:04x}", opcode),
                    }
                }
                _ => todo!("opcode {:04x}", opcode),
            }
        }
    }

    fn store_reg_to_mem(&mut self, x: u8, original: bool) {
        if !original {
            for i in 0..x {
                self.memory[self.index_register + (i as usize)] = self.registers[i as usize];
            }
        } else {
            for i in 0..x {
                self.index_register += i as usize;
                self.memory[self.index_register] = self.registers[i as usize];
            }

            self.index_register += 1;
        }
    }

    fn get_from_mem_to_reg(&mut self, x: u8, original: bool) {
        if !original {
            for i in 0..x {
                self.registers[i as usize] = self.memory[self.index_register + (i as usize)];
            }
        } else {
            for i in 0..x {
                self.index_register += i as usize;
                self.registers[i as usize] = self.memory[self.index_register];
            }

            self.index_register += 1;
        }
    }

    fn get_key(&mut self, x: u8) {
        // if key is pressed -> put key val in Vx
        // if not - decrement PC, cause this operation blocks
        // until a key is pressed

        self.position_in_memory -= 2;

        // however, the delay timer and sound timer should still be decreased
        // even when blocked

        // on original int. it only register a key on *press and release*.
    }

    fn get_font_char(&mut self, x: u8) {
        // put an addr of hexademical char
        // from "vx" to idx register
        let char = self.registers[x as usize];

        // our fonts start at FONT_OFFSET and we need to offset from there
        self.index_register = FONT_OFFSET + (char as usize) * 5;
    }

    fn add_to_idx(&mut self, x: u8) {
        let arg = self.registers[x as usize];

        self.index_register += arg as usize;

        if self.index_register > 0x1000 {
            self.registers[0xf] = 1;
        }
    }

    fn bin_dec_conversion(&mut self, x: u8) {
        let arg = self.registers[x as usize];

        let a = arg / 100;
        let b = (arg % 100) / 10;
        let c = arg % 10;

        self.memory[self.index_register] = a;
        self.memory[self.index_register + 1] = b;
        self.memory[self.index_register + 2] = c;
    }

    fn set_delay(&mut self, x: u8) {
        let arg = self.registers[x as usize];

        self.delay_timer = arg;
    }

    fn get_delay(&mut self, x: u8) {
        self.registers[x as usize] = self.delay_timer;
    }

    fn set_sound(&mut self, x: u8) {
        let arg = self.registers[x as usize];

        self.sound_timer = arg;
    }

    fn skip_pressed(&mut self, x: u8) {
        // check vX register and see if the key equal to vX value is pressed
        // if it is -> skip (PC += 2)
        // valid keypad keys are [0 - F]
    }

    fn skip_not_pressed(&mut self, x: u8) {
        // check vX register and see if the key equal to vX value is pressed
        // if it is not -> skip (PC += 2)
    }

    // adds x and y register values and stores it in x register.
    fn add_xy(&mut self, x: u8, y: u8) {
        let arg1 = self.registers[x as usize];
        let arg2 = self.registers[y as usize];

        let (val, overflow) = arg1.overflowing_add(arg2);

        self.registers[x as usize] = val;

        self.registers[0xf] = 0;

        if overflow {
            self.registers[0xf] = 1;
        }
    }

    fn sub_xy(&mut self, x: u8, y: u8, order: bool) {
        let arg1 = self.registers[x as usize];
        let arg2 = self.registers[y as usize];

        let val: u8;
        let mut flipVf: bool = false;
        let overflow: bool;

        if order {
            (val, overflow) = arg1.overflowing_sub(arg2);
            if arg1 > arg2 && !overflow {
                flipVf = true;
            }
        } else {
            (val, overflow) = arg2.overflowing_sub(arg1);
            if arg2 > arg1 && !overflow {
                flipVf = true;
            }
        }

        self.registers[x as usize] = val;

        self.registers[0xf] = 0;

        if flipVf {
            self.registers[0xf] = 1;
        }
    }

    fn shift_xy(&mut self, x: u8, y: u8, order: bool, original: bool) {
        if original {
            self.registers[x as usize] = self.registers[y as usize];
        }

        let Vf: u8;

        if order {
            Vf = self.registers[x as usize] & 0b0000_0001;
            self.registers[x as usize] >>= 1;
        } else {
            // get the bit which is going to be shifted out
            Vf = self.registers[x as usize] & 0b1000_0000;
            self.registers[x as usize] <<= 1;
        }

        self.registers[0xf] = Vf;
    }

    fn jmp_w_offset(&mut self, x: u8, nnn: u16, original: bool) {
        if original {
            let V0 = self.registers[0x0];

            self.position_in_memory = (nnn as usize) + (V0 as usize);
        } else {
            let Vx = self.registers[x as usize];

            self.position_in_memory = (nnn as usize) + (Vx as usize);
        }
    }

    fn rand(&mut self, x: u8, kk: u16) {
        let random_number: u8 = rand::thread_rng().gen();

        self.registers[x as usize] = random_number & (kk as u8);
    }

    fn to_display(&mut self, x: u8, y: u8, n: u8) {
        let start_x = self.registers[x as usize].rem_euclid(64) as usize;
        let start_y = self.registers[y as usize].rem_euclid(32) as usize;

        self.registers[0xf] = 0;

        for row in 0..n as usize {
            if start_y + row >= 32 {
                continue;
            }

            let sprite_row = self.memory[self.index_register + row];

            let mut mask = 0b1000_0000;

            for col in 0..8 as usize {
                if start_x + col >= 64 {
                    continue;
                }

                let sprite_pixel = (sprite_row & mask) >> (7 - col);

                // we actually should clip all "offscreen" sprites
                // so if we are at the end of the row or at the end of the columns we should stop
                // let old_pixel =
                //     self.display[(start_y + row).rem_euclid(32)][(start_x + col).rem_euclid(64)];

                let old_pixel = self.display[start_y + row][start_x + col];

                let new_pixel = sprite_pixel ^ old_pixel;

                // self.display[(start_y + row).rem_euclid(32)][(start_x + col).rem_euclid(64)] =
                //     new_pixel;

                self.display[start_y + row][start_x + col] = new_pixel;

                mask >>= 1;
            }
        }

        self.redraw();
    }

    fn jmp(&mut self, nnn: u16) {
        self.position_in_memory = nnn as usize;
    }

    fn set_idx(&mut self, nnn: u16) {
        self.index_register = nnn as usize;
    }

    fn set(&mut self, x: u8, kk: u16) {
        self.registers[x as usize] = kk as u8;
    }

    fn set_xy(&mut self, x: u8, y: u8) {
        self.registers[x as usize] = self.registers[y as usize];
    }

    fn bin_or_xy(&mut self, x: u8, y: u8) {
        self.registers[x as usize] |= self.registers[y as usize];
    }

    fn bin_and_xy(&mut self, x: u8, y: u8) {
        self.registers[x as usize] &= self.registers[y as usize];
    }

    fn bin_xor_xy(&mut self, x: u8, y: u8) {
        self.registers[x as usize] ^= self.registers[y as usize];
    }

    fn add(&mut self, x: u8, kk: u16) {
        (self.registers[x as usize], _) = self.registers[x as usize].overflowing_add(kk as u8);
    }

    fn clr(&mut self) {
        self.display = [[0; 64]; 32];
        self.redraw();
    }

    fn skip_eq(&mut self, x: u8, kk: u16) {
        if self.registers[x as usize] == (kk as u8) {
            self.position_in_memory += 2;
        }
    }

    fn skip_eq_xy(&mut self, x: u8, y: u8) {
        if self.registers[x as usize] == self.registers[y as usize] {
            self.position_in_memory += 2;
        }
    }

    fn skip_neq_xy(&mut self, x: u8, y: u8) {
        if self.registers[x as usize] != self.registers[y as usize] {
            self.position_in_memory += 2;
        }
    }

    fn skip_neq(&mut self, x: u8, kk: u16) {
        if self.registers[x as usize] != (kk as u8) {
            self.position_in_memory += 2;
        }
    }

    fn call(&mut self, addr: u16) {
        let sp = self.stack_pointer;
        let stack = &mut self.stack;

        if sp > stack.len() {
            panic!("stack overflow!");
        }

        stack[sp] = self.position_in_memory as u16;
        self.stack_pointer += 1;
        self.position_in_memory = addr as usize;
    }

    fn ret(&mut self) {
        if self.stack_pointer == 0 {
            panic!("stack overflow");
        }

        self.stack_pointer -= 1;

        let call_addr = self.stack[self.stack_pointer];
        self.position_in_memory = call_addr as usize;
    }

    fn redraw(&self) {
        for row in self.display {
            for pixel in row {
                if pixel == 1 {
                    print!("X");
                } else {
                    print!(" ");
                }
            }
            println!("");
        }
        println!("frame end");
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[2];

    let mut f = File::open(file_path).unwrap();

    let mut buffer: [u8; 4096 - 0x200] = [0; 4096 - 0x200];

    f.read(&mut buffer).unwrap();

    let mut cpu = CPU {
        delay_timer: 0,
        sound_timer: 0,
        registers: [0; 16],
        memory: [0; 4096],
        position_in_memory: 0x200,
        stack: [0; 16],
        stack_pointer: 1,
        display: [[0; 64]; 32],
        index_register: 0x000,
    };

    let font = [
        0xf0,
        0x90,
        0x90,
        0x90,
        0xf0, // 0
        0x20,
        0x60,
        0x20,
        0x20,
        0x70, // 1
        0xf0,
        0x10,
        0xf0,
        0x80,
        0xf0, // 2
        0xf0,
        0x10,
        0xf0,
        0x10,
        0xf0, // 3
        0x90,
        0x90,
        0xf0,
        0x10,
        0x10, // 4
        0xf0,
        0x80,
        0xf0,
        0x10,
        0xf0, // 5
        0xf0,
        0x80,
        0xf0,
        0x90,
        0xf0, // 6
        0xf0,
        0x10,
        0x20,
        0x40,
        0x40, // 7
        0xf0,
        0x90,
        0xf0,
        0x90,
        0xf0, // 8
        0xf0,
        0x90,
        0xf0,
        0x10,
        0xf0, // 9
        0xf0,
        0x90,
        0xf0,
        0x90,
        0x90, // A
        0xe0,
        0x90,
        0xe0,
        0x90,
        0xe0, // B
        0xf0,
        0x80,
        0x80,
        0x80,
        0xf0, // C
        0xe0,
        0x90,
        0x90,
        0x90,
        0xe0, // D
        0xf0,
        0x80,
        0xf0,
        0x80,
        0xf0, // E
        0xf0,
        0x80,
        0xf0,
        0x80,
        0x80, // F
    ];

    // put fonts in the reserved memory
    // 16 chars 5 bytes each
    for i in FONT_OFFSET..FONT_OFFSET + 0xf * 5 {
        cpu.memory[i] = font[i - FONT_OFFSET];
    }

    // put our program in memory (after our reserved memory)
    for i in 0x200..4096 {
        cpu.memory[i] = buffer[i - 0x200];
    }

    // a switch between original VIP int. behavior and newer ones
    cpu.run(false);

    println!("{}", cpu.registers[0]);
}
