use std::env::args;

pub mod file_help;
pub mod utils;
pub mod intcode_compy;

use file_help::read_ints_comma_sep;
use intcode_compy::{run_program, output_println};

fn main() {
    let filename: String = args().skip(1).take(1).collect();
    let input_prog: Vec<i64> = (&read_ints_comma_sep(&filename)[0]).to_vec();

    println!("Part 1:");
    let test_mode_inputs = vec![1];
    let mut tmi_iter = test_mode_inputs.into_iter();
    run_program(0, &input_prog, &mut tmi_iter, output_println);

    println!("Part 2:");
    let test_mode_inputs = vec![2];
    let mut tmi_iter = test_mode_inputs.into_iter();
    run_program(0, &input_prog, &mut tmi_iter, output_println);
}
