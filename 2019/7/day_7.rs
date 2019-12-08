use std::env::args;
use std::thread;
use std::sync::mpsc;

pub mod file_help;
pub mod utils;
pub mod intcode_compy;

use file_help::read_ints_comma_sep;
use utils::combo_ind_without_repl;
use intcode_compy::run_program_thread;

/*
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
*/

fn run_thrusters_2(phase_sets: &Vec<i64>, prog: &Vec<i64>) -> i64 {
    let mut last_thread_output: i64 = 0;
    println!("Starting threads!");
    let threads: Vec<(thread::JoinHandle<()>, mpsc::Sender<i64>, mpsc::Receiver<i64>, mpsc::Receiver<bool>)> = phase_sets.iter().enumerate().map(|(ind, phase)| {
            let (handle, input_tx, output_rx, finished_rx) = run_program_thread(ind, prog);
            match input_tx.send((phase + 5).clone()) {
                Ok(_) => (),
                Err(err) => println!("Error sending phase {}", err),
            };
            (handle, input_tx, output_rx, finished_rx)
        }).collect();

    match threads[0].1.send(0) {
        Ok(_) => (),
        Err(err) => println!("Error sending initial {}", err),
    };

    'handle_threads: loop {
        for thread_ind in 0..threads.len() {
            let (_handle, _input_tx, output_rx, finished_rx) = &threads[thread_ind];
            let (_next_handle, next_input_tx, _next_output_rx, _) = &threads[(thread_ind + 1) % threads.len()];
            match output_rx.try_recv() {
                Ok(val) => {
                        match next_input_tx.send(val) {
                            Ok(_) => (),
                            Err(err) => println!("Error sending val {}", err),
                        };
                        if thread_ind == threads.len() - 1 {
                            println!("Saw output from last thread {}", val);
                            last_thread_output = val;
                        }
                    }
                Err(_) => (),
            };
            match finished_rx.try_recv() {
                Ok(val) => {
                        println!("Saw thread {} finished: {}", thread_ind, val);
                        if thread_ind == threads.len() - 1 {
                            println!("Saw last thread finished");
                            break 'handle_threads;
                        }
                    },
                Err(_) => (),
            }
        }
    }

    println!("Threads done.");
    return last_thread_output;
}

fn main() {
    let filename: String = args().skip(1).take(1).collect();
    let input_prog: Vec<i64> = (&read_ints_comma_sep(&filename)[0]).to_vec();
    let phase_set_combos_usize = combo_ind_without_repl(5, 5);
    let phase_set_combos: Vec<Vec<i64>> = phase_set_combos_usize.iter().map(
            |combo| combo.iter().map(|phase| phase.clone() as i64).collect()).collect();

    let outputs: Vec<i64> = phase_set_combos.iter().map(|combo| run_thrusters_2(combo, &input_prog)).collect();
    let max_output: i64 = outputs.iter().fold(-1,
            |accum, i| if *i > accum { *i } else { accum }
        );

    println!("Outputs: {:?}", outputs);
    println!("Max: {}", max_output);
}
