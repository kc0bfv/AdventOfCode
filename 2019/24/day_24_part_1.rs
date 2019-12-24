use std::env::args;
use std::fmt;
use std::collections::HashSet;

pub mod file_help;
pub mod utils;

use file_help::read_strs;
use utils::print_vec_of_vecs;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Spc {
    Bug,
    Emp,
}
impl fmt::Display for Spc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
                match self {
                    Self::Bug => "#",
                    Self::Emp => ".",
                }
            )
    }
}

fn parse_input(raw: Vec<Vec<String>>) -> Vec<Vec<Spc>>{
    let mut ret: Vec<Vec<Spc>> = vec![];
    for line in raw {
        let mut spcline: Vec<Spc> = vec![];
        for spot in line[0].chars() {
            match spot {
                '#' => spcline.push(Spc::Bug),
                '.' => spcline.push(Spc::Emp),
                _   => panic!("Invalid input at spot: {}", spot),
            }
        }
        ret.push(spcline);
    }
    return ret;
}

fn count_adjacent(input: &Vec<Vec<Spc>>) -> Vec<Vec<usize>> {
    let mut ret: Vec<Vec<usize>> = vec![];
    let (y_min, y_max): (usize, usize) = (0, input.len()-1);
    let (x_min, x_max): (usize, usize) = (0, input[0].len()-1);
    for (y, line) in input.iter().enumerate() {
        let mut retline: Vec<usize> = vec![];
        for (x, _) in line.iter().enumerate() {
            retline.push(vec![
                if y > y_min && input[y-1][x] == Spc::Bug { 1 } else { 0 },
                if y < y_max && input[y+1][x] == Spc::Bug { 1 } else { 0 },
                if x > x_min && input[y][x-1] == Spc::Bug { 1 } else { 0 },
                if x < x_max && input[y][x+1] == Spc::Bug { 1 } else { 0 },
            ].iter().sum());
        }
        ret.push(retline);
    }
    return ret;
}

fn apply_rules(input: &Vec<Vec<Spc>>, counts: Vec<Vec<usize>>)
    -> Vec<Vec<Spc>>
{
    input.iter().zip(counts.iter()).map(|(valline, countline)| {
            valline.iter().zip(countline.iter()).map(|(val, count)|
                    match val {
                        Spc::Bug if *count == 1                => Spc::Bug,
                        Spc::Bug                               => Spc::Emp,
                        Spc::Emp if *count == 1 || *count == 2 => Spc::Bug,
                        Spc::Emp                               => Spc::Emp,
                    }
                ).collect()
        }).collect()
}

fn do_round(input: &Vec<Vec<Spc>>) -> Vec<Vec<Spc>> {
    let adj_count = count_adjacent(input);
    apply_rules(input, adj_count)
}

fn biodiv_rating(input: &Vec<Vec<Spc>>) -> u32 {
    let mults: Vec<u32> = (0..25).scan(0, |acc, _| {
                *acc = if *acc == 0 { 1 } else { *acc * 2 };
                Some(*acc)
            })
            .collect();
    input.iter().flatten().zip(mults.iter()).map(|i| {
            if *i.0 == Spc::Bug { *i.1 } else { 0 }
        }).sum()
}

fn main() {
    let filename = args().nth(1).expect("Supply a filename!");
    let input_raw = read_strs(&filename, ',');

    let input_spc = parse_input(input_raw);

    let mut biodivs: HashSet<u32> = HashSet::new();
    let mut output = input_spc;
    let mut out_biodiv: u32 = biodiv_rating(&output);
    let mut round = 0;
    while !biodivs.contains(&out_biodiv) {
        round += 1;
        biodivs.insert(out_biodiv);
        output = do_round(&output);
        out_biodiv = biodiv_rating(&output);
        println!("{} {}", round, out_biodiv);
        print_vec_of_vecs(&output);
        println!("");
    }

    println!("{} {}", round, out_biodiv);
    print_vec_of_vecs(&output);
    println!("");
}
