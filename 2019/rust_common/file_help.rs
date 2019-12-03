use std::error::Error;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

pub fn read_ints(filename: &str) -> Vec<i64> {
    let path = Path::new(filename);

    let file = match File::open(&path) {
        Err(why) => panic!("Couldn't open {}: {}", path.display(),
                why.description()),
        Ok(file) => file,
    };

    let lines = io::BufReader::new(file).lines();
    let in_vals: Vec<i64> = lines.map(|line_mbe| match line_mbe {
            Err(why) => panic!("Error on line! {}", why.description()),
            Ok(line) => line.parse::<i64>().unwrap(),
        }).collect();

    return in_vals;
}

pub fn read_ints_comma_sep(filename: &str) -> Vec<Vec<i64>> {
    let path = Path::new(filename);

    let file = match File::open(&path) {
        Err(why) => panic!("Couldn't open {}: {}", path.display(),
                why.description()),
        Ok(file) => file,
    };

    let lines = io::BufReader::new(file).lines();
    let in_vals: Vec<Vec<i64>> = lines.map(|line_mbe| match line_mbe {
            Err(why) => panic!("Error on line! {}", why.description()),
            Ok(line) => line.split(',').map(
                    |item| item.parse::<i64>().unwrap()
                ).collect(),
        }).collect();

    return in_vals;
}

pub fn read_strs_comma_sep(filename: &str) -> Vec<Vec<String>> {
    let path = Path::new(filename);

    let file = match File::open(&path) {
        Err(why) => panic!("Couldn't open {}: {}", path.display(),
                why.description()),
        Ok(file) => file,
    };

    let lines = io::BufReader::new(file).lines();
    let in_vals: Vec<Vec<String>> = lines.map(|line_mbe| match line_mbe {
            Err(why) => panic!("Error on line! {}", why.description()),
            Ok(line) => line.split(',').map(|item| String::from(item)).collect(),
        }).collect();

    return in_vals;
}
