use std::env::args;
//use std::collections::HashMap;

pub mod file_help;
pub mod new_intcode_compy;

use file_help::read_ints_comma_sep;
use new_intcode_compy::{run_program_inst_cnt, OutputHandler};

#[derive(Debug)]
struct SD {
    commands: String,
    out_pos: usize,
    ignore_output: bool,
    big_val: i64,
}
impl SD {
    fn new(commands: &String) -> Self {
        /*
        // 18843
        let commands = "NOT B J
NOT A T
OR T J
AND D J
WALK
";
        */
        /*
        // 20344 
        // Part 1 winner
        let commands = "NOT C J
AND D J
NOT A T
OR T J
WALK
";
        */
        /*
        // 48691
        let commands = "NOT C J
AND D J
OR H T
OR E T
AND T J
NOT A T
OR T J
RUN
";
*/
        /*
        // 771793
        // Part 2 winner
        let commands = "NOT C J
AND D J
OR H T
OR E T
AND T J
NOT B T
NOT T T
OR E T
NOT T T
OR T J
NOT A T
OR T J
RUN
";
*/
        Self {
            commands: commands.to_string(),
            out_pos: 0,
            ignore_output: false,
            big_val: 0,
        }
    }
}
impl Iterator for SD {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        let retval: Self::Item = self.commands.bytes().nth(self.out_pos)
                .unwrap() as i64;
        self.out_pos += 1;
        Some(retval)
    }
}
impl OutputHandler for SD {
    fn output(&mut self, val: i64) -> Result<(), &'static str> {
        if val > 256 {
            println!("Non ASCII {}", val);
            self.big_val = val;
            return Ok(());
        }
        if self.ignore_output {
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

    let p1_cmds = "NOT C J
AND D J
NOT A T
OR T J
WALK
";
    let p2_cmds = "NOT C J
AND D J
OR H T
OR E T
AND T J
NOT B T
NOT T T
OR E T
NOT T T
OR T J
NOT A T
OR T J
RUN
";

    let mut sdroid = SD::new(&p1_cmds.to_string());
    let out_inf = run_program_inst_cnt(0, &input_prog, &mut sdroid);

    println!("Part 1: {} Insts: {}", sdroid.big_val, out_inf.2);

    let mut sdroid = SD::new(&p2_cmds.to_string());
    let out_inf = run_program_inst_cnt(0, &input_prog, &mut sdroid);

    println!("Part 2: {} Insts: {}", sdroid.big_val, out_inf.2);
}
