pub mod file_help;
use file_help::read_ints;

fn main() {
    let in_vals: Vec<i64> = read_ints("1_input");

    let handled_one = in_vals.iter().map(|&amt| handle_one(amt));
    let handled_two = in_vals.iter().map(|&amt| handle_two(amt));

    println!("First part {}", handled_one.sum::<i64>());
    println!("Second part {}", handled_two.sum::<i64>());
}

fn handle_one(mass: i64) -> i64 {
    return mass / 3 - 2;
}

struct PartTwoGen {
    curr: i64,
}

impl PartTwoGen {
    fn new(start_mass: i64) -> PartTwoGen {
        PartTwoGen { curr: start_mass }
    }
}

impl Iterator for PartTwoGen {
    type Item = i64;
    fn next(&mut self) -> Option<Self::Item> {
        let curr = self.curr;
        self.curr = self.curr / 3 - 2;
        return if curr > 0 { Some(curr) } else { None }
    }
}

fn handle_two(mass: i64) -> i64 {
    let p2 = PartTwoGen::new(mass);
    let vals: Vec<i64> = p2.collect();
    return vals[1..].iter().sum::<i64>();
}
