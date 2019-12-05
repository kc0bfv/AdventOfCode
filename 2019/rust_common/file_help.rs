use std::error::Error;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

// Line-separated integers
pub fn read_ints_nl_sep(filename: &str) -> Vec<i64> {
    let lines_in = read_ints_comma_sep(filename);
    lines_in.iter().map(|item| item[0]).collect()
}

pub fn read_ints_comma_sep(filename: &str) -> Vec<Vec<i64>> {
    read_ints(filename, ',')
}

pub fn read_ints(filename: &str, sep: char) -> Vec<Vec<i64>> {
    let lines_strs = read_strs(filename, sep);
    lines_strs.iter().map(|line|
            line.iter().map(|str| str.parse::<i64>().unwrap()).collect()
        ).collect()
}

pub fn read_strs_comma_sep(filename: &str) -> Vec<Vec<String>> {
    read_strs(filename, ',')
}

pub fn read_strs(filename: &str, sep: char) -> Vec<Vec<String>> {
    let path = Path::new(filename);

    let file = match File::open(&path) {
        Err(why) => panic!("Couldn't open {}: {}", path.display(),
                why.description()),
        Ok(file) => file,
    };

    let lines = io::BufReader::new(file).lines();
    let in_vals: Vec<Vec<String>> = lines.map(|line_mbe| match line_mbe {
            Err(why) => panic!("Error on line! {}", why.description()),
            Ok(line) => line.split(sep).map(|item| String::from(item)).collect(),
        }).collect();

    return in_vals;
}
