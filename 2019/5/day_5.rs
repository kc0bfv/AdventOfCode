use std::env::args;
use std::io::{self, Write};

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

fn get_int_from_stdin() -> i64 {
    let mut inval = String::new();
    io::stdin().read_line(&mut inval).unwrap();
    match inval.trim().parse::<i64>() {
        Ok(val) => val,
        Err(err) => panic!("Error on input: {}", err),
    }
}

#[derive(Debug)]
enum ParamModes {
    Pos, Imm
}

fn get_param_modes(opcode: i64, count: usize) -> Vec<ParamModes> {
    if opcode < 0 {
        panic!("Negative opcode found!");
    }
    let mode_sec = opcode / 100;
    let base: i64 = 10;
    let modes: Vec<ParamModes> = (0..count).map(|param_ind| 
            {
                let mode = (mode_sec /
                        base.checked_pow(param_ind as u32).unwrap()) % 10;
                match mode {
                    0 => ParamModes::Pos,
                    1 => ParamModes::Imm,
                    _ => panic!("Invalid mode! pos {} mode {} opcode {}",
                            count, mode, opcode),
                }
            }
        ).collect();
    //println!("Modes {:?}", modes);
    modes
}

fn do_4_op<F>(opcode: i64, memory: &mut Vec<i64>, cur_pos: &mut usize, mut func: F )
    where F: FnMut(i64, i64) -> i64
{
    let modes = get_param_modes(opcode, 2);
    let val_1: i64 = handle_read_param(memory, memory[*cur_pos + 1], &modes[0]);
    let val_2: i64 = handle_read_param(memory, memory[*cur_pos + 2], &modes[1]);
    let out_pos: usize = memory[*cur_pos + 3] as usize;
    //println!("4 op {} {} {}", val_1, val_2, out_pos);
    memory[out_pos] = func(val_1, val_2);
    *cur_pos += 4;
}

fn do_input(opcode: i64, memory: &mut Vec<i64>, cur_pos: &mut usize) {
    if opcode / 10 != 0 {
        panic!("Param modes not supported on input! {} {}", opcode, cur_pos);
    }
    print!("Input required! Provide it: ");
    io::stdout().flush().unwrap();
    let out_pos: usize = memory[*cur_pos + 1] as usize;
    memory[out_pos] = get_int_from_stdin();
    *cur_pos += 2;
}

fn handle_read_param(memory: &mut Vec<i64>, param: i64, mode: &ParamModes) -> i64 {
    match mode {
        ParamModes::Pos => memory[param as usize],
        ParamModes::Imm => param,
        //_ => panic!("Invalid parameter mode at read time!"),
    }
}

fn do_output(opcode: i64, memory: &mut Vec<i64>, cur_pos: &mut usize) {
    let modes = get_param_modes(opcode, 1);
    let out_val: i64 = handle_read_param(memory, memory[*cur_pos + 1], &modes[0]);
    println!("Output: {}", out_val);
    *cur_pos += 2;
}

fn jump_test<F>(opcode: i64, memory: &mut Vec<i64>, cur_pos: &mut usize,
        mut tester: F)
    where F: FnMut(i64) -> bool
{
    let modes = get_param_modes(opcode, 2);
    let cond: i64 = handle_read_param(memory, memory[*cur_pos + 1], &modes[0]);
    let dest: i64 = handle_read_param(memory, memory[*cur_pos + 2], &modes[1]);
    if tester(cond) {
        *cur_pos = dest as usize;
    } else {
        *cur_pos += 3;
    }
}

fn run_program(initial_memory: &Vec<i64>) -> Vec<i64> {
    let mut memory: Vec<i64> = initial_memory.to_vec();

    let mut cur_pos: usize = 0;
    while cur_pos < memory.len() {
        let opcode = memory[cur_pos];
        //println!("Execing {}", opcode);
        match opcode % 100 {
            1 => do_4_op(opcode, &mut memory, &mut cur_pos, std::ops::Add::add),
            2 => do_4_op(opcode, &mut memory, &mut cur_pos, std::ops::Mul::mul),
            3 => do_input(opcode, &mut memory, &mut cur_pos),
            4 => do_output(opcode, &mut memory, &mut cur_pos),
            5 => jump_test(opcode, &mut memory, &mut cur_pos, |val| val != 0),
            6 => jump_test(opcode, &mut memory, &mut cur_pos, |val| val == 0),
            7 => do_4_op(opcode, &mut memory, &mut cur_pos, |val_1, val_2| {
                    if val_1 < val_2 {
                        1
                    } else {
                        0
                    }
                }),
            8 => do_4_op(opcode, &mut memory, &mut cur_pos, |val_1, val_2| {
                    if val_1 == val_2 {
                        1
                    } else {
                        0
                    }
                }),
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

    run_program(&input_vals);
}

