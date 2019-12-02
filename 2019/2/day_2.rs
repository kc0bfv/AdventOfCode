use std::env::args;

pub mod file_help;
use file_help::read_ints_comma_sep;

/*
fn print_state(init: &str, items: &Vec<i64>) {
    println!("{}", init);
    items.iter().map(|item| print!("{},", item)).count();
    println!("");
    ()
}
*/

fn do_4_op<F>(memory: &mut Vec<i64>, cur_pos: &mut usize, mut func: F )
    where F: FnMut(i64, i64) -> i64
{
    let pos_1: usize = memory[*cur_pos + 1] as usize;
    let pos_2: usize = memory[*cur_pos + 2] as usize;
    let out_pos: usize = memory[*cur_pos + 3] as usize;
    let val_1: i64 = memory[pos_1];
    let val_2: i64 = memory[pos_2];
    memory[out_pos] = func(val_1, val_2);
    *cur_pos += 4;
}

fn run_program(noun: i64, verb: i64, initial_memory: &Vec<i64>) -> Vec<i64> {
    let mut memory: Vec<i64> = initial_memory.to_vec();
    memory[1] = noun;
    memory[2] = verb;

    let mut cur_pos: usize = 0;
    while cur_pos < memory.len() {
        match memory[cur_pos] {
            1 => do_4_op(&mut memory, &mut cur_pos, std::ops::Add::add),
            2 => do_4_op(&mut memory, &mut cur_pos, std::ops::Mul::mul),
            99 => {
                    //cur_pos += 1;
                    break;
                },
            _ => panic!("Unknown opcode encountered!"),
        }
        //print_state("Current state", &memory);
        //println!("Now at position {}", cur_pos);
    }
    return memory;
}

fn main() {
    let filename: String = args().skip(1).take(1).collect();
    let input_vals: Vec<i64> = (&read_ints_comma_sep(&filename)[0]).to_vec();

    //print_state("Initial state", &input_vals);

    for noun in 0..100 {
        for verb in 0..100 {
            let out_mem = run_program(noun, verb, &input_vals);
            println!("Noun: {} Verb: {} Final: {}", noun, verb, out_mem[0]);
            if out_mem[0] == 19690720 {
                println!("Found!");
            }
        }
    }
}

