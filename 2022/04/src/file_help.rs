use std::env::args;
use std::fs::File;
use std::path::Path;
use std::io::{BufReader, BufRead};

pub fn read_lines(filename: &str) -> Result<Vec<String>, std::io::Error>
{
    let path = Path::new(filename);
    let file = File::open(&path)?;
    BufReader::new(file)
        .lines()
        .map(|line| -> Result<_,_> { Ok(line?.trim().to_string()) })
        .collect()
}

pub fn get_cmd_arg() -> String
{
    return args().skip(1).take(1).collect();
}
