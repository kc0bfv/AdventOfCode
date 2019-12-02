use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufRead};
use std::path::Path;

fn main() {
    let path = Path::new("1_input");

    let mut file = match File::open(&path) {
        Err(why) => panic!("Couldn't open {}: {}", path.display(),
                why.description()),
        Ok(file) => file,
    };

    let lines = io::BufReader::new(file).lines();
    let mut out_val = 0;
    for line_mbe in lines {
        if let Ok(line) = line_mbe {
            out_val += handle_one(line);
        }
    }
    println!("Initial fuel: {}", out_val);

    let mut cur_fuel = out_val;
    loop {
        cur_fuel = cur_fuel / 3 - 2; 

        if cur_fuel < 0 {
            break;
        }

        println!("Stage: {}", cur_fuel);
        out_val += cur_fuel;
    }

    println!("Final fuel: {}", out_val);
}

fn handle_one(line: String) -> i64 {
    let mass = line.parse::<i64>().unwrap();

    let fin_out = mass / 3 - 2;

    return fin_out;
}
