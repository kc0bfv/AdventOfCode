use std::env::args;

pub mod file_help;

use file_help::read_strs;

struct PatternIter {
    repeat: usize,
    pattern_pos: usize,
    repeat_pos: usize,
}
impl PatternIter {
    fn new(repeat: usize) -> Self { 
        Self {
            repeat: repeat,
            pattern_pos: 0,
            repeat_pos: 0,
        }
    }
}
impl Iterator for PatternIter {
    type Item = i32;
    fn next(&mut self) -> Option<Self::Item> {
        let pattern: Vec<Self::Item> = vec![0, 1, 0, -1];

        if self.repeat_pos > self.repeat {
            self.pattern_pos += 1;
            self.pattern_pos %= pattern.len();
            self.repeat_pos = 0;
        }
        
        self.repeat_pos += 1;

        Some(pattern[self.pattern_pos].clone())
    }
}

fn run_muts(input_vals: &Vec<u32>, mut_count: usize) -> Vec<i32> {
    let mut cur_list: Vec<i32> = input_vals.iter().map(|val| *val as i32).collect();
    for _ind in 0..mut_count {
        let patt_iters = (0..cur_list.len()).map(|repeat| PatternIter::new(repeat));
        let new_list_signed: Vec<i32> = patt_iters
                .map(|mut iter| {
                        iter.next();
                        iter
                            .zip(cur_list.iter())
                            .map(|(uno, dos)| uno * *dos)
                            .sum::<i32>()
                            .abs() % 10
                    })
                .collect();
        cur_list = new_list_signed.iter().cloned().collect();
        //println!("Output {} {:?}", _ind+1, cur_list);
    }
    cur_list
}

fn part_2(input_vals: &Vec<u32>, mut_count: usize, offset: usize) -> Vec<i32> {
    if offset < input_vals.len()/2 { panic!("Offset expectation not met!"); }

    let mut cur_list: Vec<i32> = input_vals.iter().skip(offset).map(|val| *val as i32).collect();
    println!("len {} {}", input_vals.len(), offset);
    for _ind in 0..mut_count {
        println!("Ind {} {:?}", _ind, cur_list.len());
        let mut new_list: Vec<i32> = Vec::new();
        let mut last_sum: i32 = cur_list.iter().sum();
        new_list.push(last_sum % 10);
        for innerind in 0..(cur_list.len()-1) {
            last_sum -= cur_list[innerind];
            new_list.push(last_sum % 10);
        }

        cur_list = new_list.iter().cloned().collect();
    }
    return cur_list;
}

fn main() {
    let filename = args().skip(1).next().expect("Supply a filename!");
    let input_lines = read_strs(&filename, ' ');
    let input_nums: Vec<u32> = input_lines[0][0]
            .chars()
            .filter(|char| char.is_numeric())
            .map(|char| char.to_digit(10).unwrap())
            .collect();

    let part1 = run_muts(&input_nums, 100);
    println!("Part 1 {:?}", part1);

    let repeat_count: usize = 10000;
    let part2_input: Vec<u32> = (0..repeat_count).flat_map(|_| input_nums.iter().cloned()).collect();
    let part2_offset: usize = input_lines[0][0][..7].parse().expect("Invalid parse part 2");

    let part2 = part_2(&part2_input, 100, part2_offset);
    let ans: Vec<i32> = part2[..8].iter().cloned().collect();
    println!("Part 2 {:?}", ans);
}
