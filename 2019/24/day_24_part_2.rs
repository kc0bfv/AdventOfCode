use std::env::args;
use std::fmt;
//use std::collections::HashSet;
use std::collections::VecDeque;

pub mod file_help;
pub mod utils;

use file_help::read_strs;
use utils::print_vec_of_vecs;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum Spc {
    Bug,
    Emp,
    Que,
}
impl fmt::Display for Spc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
                match self {
                    Self::Bug => "#",
                    Self::Emp => ".",
                    Self::Que => "?",
                }
            )
    }
}

fn parse_input(raw: Vec<Vec<String>>) -> Vec<Vec<Spc>>{
    let mut ret: Vec<Vec<Spc>> = vec![];
    for (y, line) in raw.iter().enumerate() {
        let mut spcline: Vec<Spc> = vec![];
        for (x, spot) in line[0].chars().enumerate() {
            if x == 2 && y == 2 {
                spcline.push(Spc::Que);
                continue;
            }
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

fn count_adjacent(input: &VecDeque<Vec<Vec<Spc>>>) -> Vec<Vec<Vec<usize>>> {
    let mut ret: Vec<Vec<Vec<usize>>> = vec![];
    let (d_min, d_max): (usize, usize) = (0, input.len()-1);
    for (d, depth) in input.iter().enumerate() {
        let mut retdepth: Vec<Vec<usize>> = vec![];
        for (y, line) in depth.iter().enumerate() {
            let mut retline: Vec<usize> = vec![];
            for (x, _) in line.iter().enumerate() {
                let mut adj: Vec<(usize, usize, usize)> = vec![];
                // Handle right and left, top and bottom edges and depth up
                if d < d_max {
                    if x == 4 {
                        adj.push((d+1, 3, 2));
                    } else if x == 0 {
                        adj.push((d+1, 1, 2));
                    }
                    if y == 4 {
                        adj.push((d+1, 2, 3));
                    } else if y == 0 {
                        adj.push((d+1, 2, 1));
                    }
                }
                // Handle depth down
                if d > d_min {
                    if x == 2 && y == 1 {
                        let iny = 0;
                        (0..5).for_each(|inx| adj.push((d-1, inx, iny)));
                    } else if x == 2 && y == 3 {
                        let iny = 4;
                        (0..5).for_each(|inx| adj.push((d-1, inx, iny)));
                    } else if x == 1 && y == 2 {
                        let inx = 0;
                        (0..5).for_each(|iny| adj.push((d-1, inx, iny)));
                    } else if x == 3 && y == 2 {
                        let inx = 4;
                        (0..5).for_each(|iny| adj.push((d-1, inx, iny)));
                    }
                }

                // Handle same depth
                if y > 0 && ! (y==3 && x==2) { adj.push((d, x, y-1)); }
                if y < 4 && ! (y==1 && x==2) { adj.push((d, x, y+1)); }
                if x > 0 && ! (x==3 && y==2) { adj.push((d, x-1, y)); }
                if x < 4 && ! (x==1 && y==2) { adj.push((d, x+1, y)); }

                // Count bugs in adjacents
                retline.push(adj.iter().map(|(chd, chx, chy)|
                        match input[*chd][*chy][*chx] {
                            Spc::Bug => 1,
                            Spc::Emp => 0,
                            Spc::Que => panic!("Checked center as adj"),
                        }
                    ).sum());
            }
            retdepth.push(retline);
        }
        ret.push(retdepth);
    }
    return ret;
}

fn apply_rules(input: &VecDeque<Vec<Vec<Spc>>>, counts: Vec<Vec<Vec<usize>>>)
    -> VecDeque<Vec<Vec<Spc>>>
{
    let mut ret: VecDeque<Vec<Vec<Spc>>> = VecDeque::new();

    for (d, valdepth) in input.iter().enumerate() {
        let countdepth = &counts[d];
        let newd: Vec<Vec<Spc>> = valdepth.iter().zip(countdepth.iter())
            .map(|(valline, countline)| {
                valline.iter().zip(countline.iter()).map(|(val, count)|
                        match val {
                            Spc::Bug if *count == 1                => Spc::Bug,
                            Spc::Bug                               => Spc::Emp,
                            Spc::Emp if *count == 1 || *count == 2 => Spc::Bug,
                            Spc::Emp                               => Spc::Emp,
                            Spc::Que                               => Spc::Que,
                        }
                    ).collect()
            }).collect();
        ret.push_back(newd);
    }
    return ret;
}

fn depth_is_empty(depth: &Vec<Vec<Spc>>) -> bool {
    depth.iter().all(|line| line.iter().all(|val| *val != Spc::Bug) )
}

fn do_round(input: &VecDeque<Vec<Vec<Spc>>>, ind_off: usize)
        -> (VecDeque<Vec<Vec<Spc>>>, usize)
{
    let blank_line: String = ".....".to_string();
    let blank_input: Vec<Vec<String>> = vec![vec![blank_line]; 5];
    let blank_depth = parse_input(blank_input);

    // Add some depths to expand into
    let mut in_formod = input.clone();
    let mut new_ind_off = ind_off + 1;
    in_formod.push_front(blank_depth.clone());
    in_formod.push_back(blank_depth);

    let adj_count = count_adjacent(&in_formod);
    let mut out_rules = apply_rules(&in_formod, adj_count);

    if depth_is_empty(&out_rules[0]) {
        new_ind_off -= 1;
        out_rules.pop_front();
    }
    if depth_is_empty(&out_rules[out_rules.len()-1]) {
        out_rules.pop_back();
    }

    return (out_rules, new_ind_off);
}

/*
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
*/

fn count_bugs(input: &VecDeque<Vec<Vec<Spc>>>) -> usize {
    input.iter().flat_map(|depth|
        depth.iter().flat_map(|line|
            line.iter().filter(|val| **val == Spc::Bug)
            )
        ).count()
}

fn print_vec_of_vec_of_vecs(input: &VecDeque<Vec<Vec<Spc>>>, ind_off: usize) {
    for (ind, vec) in input.iter().enumerate() {
        println!("Depth {}", (ind as i32) - (ind_off as i32));
        print_vec_of_vecs(vec);
    }
}

fn main() {
    let filename = args().nth(1).expect("Supply a filename!");
    let input_raw = read_strs(&filename, ',');

    let max_round = 200;

    let input_spc: VecDeque<Vec<Vec<Spc>>> = vec![parse_input(input_raw)]
            .into_iter().collect();

    let mut output = input_spc;
    let mut round = 0;
    let mut ind_off = 0;
    while round < max_round {
        round += 1;
        let round_out = do_round(&output, ind_off);
        output = round_out.0;
        ind_off = round_out.1;
        println!("{} {}", round, count_bugs(&output));
        print_vec_of_vec_of_vecs(&output, ind_off);
        println!("");
    }

    println!("{} {}", round, count_bugs(&output));
}
