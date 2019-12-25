use std::env::args;
use std::io::{self, BufRead};
//use std::collections::HashMap;

pub mod file_help;
pub mod new_intcode_compy;

use file_help::read_ints_comma_sep;
use new_intcode_compy::{run_program_inst_cnt, OutputHandler};

#[derive(Debug)]
struct MD {
    command: String,
    out_pos: usize,
}
impl MD {
    fn new() -> Self {
        Self {
            command: "".to_string(),
            out_pos: 0,
        }
    }
}
impl Iterator for MD {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        if self.out_pos >= self.command.chars().count() {
            self.command = String::new();
            let stdin = io::stdin();
            stdin.lock().read_line(&mut self.command).unwrap();
            self.out_pos = 0;
        }
        let retval: i64 = self.command.chars().nth(self.out_pos)
                .expect("Unwrapping char") as i64;
        self.out_pos += 1;
        Some(retval)
    }
}
impl OutputHandler for MD {
    fn output(&mut self, val: i64) -> Result<(), &'static str> {
        if val > 256 {
            println!("Non ASCII {}", val);
            return Ok(());
        }
        let val_as_char = (val as u8) as char;
        print!("{}", val_as_char);
        Ok(())
    }
}

fn main() {
    let filename = args().nth(1).expect("Supply a filename!");
    let input_prog = read_ints_comma_sep(&filename).into_iter().nth(0).unwrap();

    let mut mdroid = MD::new();
    run_program_inst_cnt(0, &input_prog, &mut mdroid);
}
