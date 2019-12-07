use std::env::args;

pub mod file_help;
pub mod utils;
pub mod intcode_compy;

use file_help::read_ints_comma_sep;
use utils::combo_ind_without_repl;
use intcode_compy::run_program;

fn run_thrusters(phase_sets: &Vec<i64>, prog: &Vec<i64>) -> i64 {
    let mut prev_output: i64 = 0;

    for phase in phase_sets {
        let input_vals: Vec<i64> = vec![phase.clone(), prev_output.clone()];
        println!("Invals {:?}", input_vals);
        let (_, out_vals) = run_program(prog, &input_vals);
        prev_output = out_vals[0];
    }
    return prev_output;
}

fn run_thrusters_2(phase_sets: &Vec<i64>, prog: &Vec<i64>) -> i64 {
    let mut prev_output: i64 = 0;

    for phase in phase_sets {
        let input_vals: Vec<i64> = vec![phase.clone()+5, prev_output.clone()];
        println!("Invals {:?}", input_vals);
        let (_, out_vals) = run_program(prog, &input_vals);
        prev_output = out_vals[0];
    }
    return prev_output;
}

fn main() {
    let filename: String = args().skip(1).take(1).collect();
    let input_prog: Vec<i64> = (&read_ints_comma_sep(&filename)[0]).to_vec();
    let phase_set_combos_usize = combo_ind_without_repl(5, 5);
    let phase_set_combos: Vec<Vec<i64>> = phase_set_combos_usize.iter().map(
            |combo| combo.iter().map(|phase| phase.clone() as i64).collect()).collect();

    let outputs: Vec<i64> = phase_set_combos.iter().map(|combo| run_thrusters(combo, &input_prog)).collect();
    let max_output: i64 = outputs.iter().fold(-1,
            |accum, i| if *i > accum { *i } else { accum }
        );

    println!("Outputs: {:?}", outputs);
    println!("Max: {}", max_output);
}
