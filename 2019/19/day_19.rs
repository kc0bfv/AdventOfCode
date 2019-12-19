use std::env::args;
use std::collections::HashMap;

pub mod file_help;
pub mod new_intcode_compy;

use file_help::read_ints_comma_sep;
use new_intcode_compy::{run_program, OutputHandler};

#[derive(Debug,Hash,PartialEq,Eq,Clone)]
struct Position {
    x: i64,
    y: i64,
}
impl Position {
    fn new(x: i64, y: i64) -> Position {
        Position{ x, y }
    }
    /*
    fn manh_dist(&self, other: &Position) -> u64 {
        ((other.x - self.x).abs() + (other.y - self.y).abs()).try_into().unwrap()
    }
    fn diff(&self, other: &Position) -> (i64, i64) {
        (other.x - self.x, other.y - self.y)
    }
    fn add(&self, other: &Position) -> Position {
        Position::new(self.x + other.x, self.y + other.y)
    }
    fn in_vec<'a, T>(&self, vec: &'a Vec<Vec<T>>) -> &'a T {
        match vec.get(self.y as usize) {
            Some(vec_y) =>
                match vec_y.get(self.x as usize) {
                        Some(ref_val) => ref_val,
                        None => vec.get(0).unwrap().get(0).unwrap(),
                    },
            None => vec.get(0).unwrap().get(0).unwrap(),
        }
    }
    */
}

#[derive(Debug)]
struct Drone {
    view: HashMap<Position, char>,
    last_move: Position,
    trac_pt_cnt: usize,
    sent_x: bool,
    input_done: bool,
    part_1: bool,
    part_2_done: bool,

    // Part 2 edge mode state
    p2_edge_mode: bool,
    p2_edge_find_right: bool,
    p2_edge_find_just_start: bool,
    last_re: Position,
    last_le: Position,
}
impl Drone {
    fn new() -> Self {
        Self {
            view: HashMap::new(),
            last_move: Position::new( 0, -1),
            trac_pt_cnt: 0,
            sent_x: false,
            input_done: false,
            part_1: true,
            part_2_done: false,

            p2_edge_mode: true,
            p2_edge_find_right: true,
            p2_edge_find_just_start: true,
            last_re: Position::new(19, 39),
            last_le: Position::new(14, 39),
        }
    }

    fn print_map(&mut self, x_min: usize, x_max: usize, y_min: usize, y_max: usize) {
        for y in y_min..=y_max {
            for x in x_min..=x_max {
                match self.view.get(&Position::new(x as i64, y as i64)) {
                    Some(c) => print!("{}", c),
                    None => print!(" "),
                }
            }
            println!("");
        }
    }

    fn part_1_next(&mut self) -> Option<<Self as Iterator>::Item> {
        if ! self.sent_x {
            let next_y = self.last_move.y + 1;
            let next_x = self.last_move.x + (if next_y >= 50 { 1 } else { 0 });
            self.last_move = Position::new(next_x, next_y % 50);
        }

        if self.last_move.y == 49 && self.last_move.x == 49 {
            self.input_done = true;
        }
        if self.last_move.x >= 50 {
            None
        } else if ! self.sent_x {
            self.sent_x = true;
            //println!("Sending x {}", self.last_move.x);
            Some(self.last_move.x)
        } else {
            self.sent_x = false;
            //println!("Sending y {}", self.last_move.y);
            Some(self.last_move.y)
        }
    }

    fn part_2_next(&mut self) -> Option<<Self as Iterator>::Item> {
        if ! self.sent_x {
            let next_move = self.calc_next_part_2_edge();
            self.last_move = next_move;
        }
        if ! self.sent_x {
            self.sent_x = true;
            //println!("Sending x {}", self.last_move.x);
            Some(self.last_move.x)
        } else {
            self.sent_x = false;
            //println!("Sending y {}", self.last_move.y);
            Some(self.last_move.y)
        }
    }

    fn is_trac(&self, pos: &Position) -> bool {
        match self.view.get(pos) {
            Some(val) => *val == '#',
            None => panic!("Checked is trac with unexplored point"),
        }
    }

    fn is_explored(&self, pos: &Position) -> bool {
        match self.view.get(pos) {
            Some(_) => true,
            None => false,
        }
    }

    fn calc_next_part_2_edge(&mut self) -> Position {
        let box_size = 100;

        // Edge finding mode
        // Follow the left and right edges until they are further than 100 points apart
        // Edges will be the left/right-most tractor beam position at a y
        // Track these positions separately from next part
        if self.p2_edge_mode {
            // Find right edge mode
            if self.p2_edge_find_right {
                // Are we just starting in this mode?
                if self.p2_edge_find_just_start {
                    self.p2_edge_find_just_start = false;

                    let pos_below_last_re = Position::new(self.last_re.x,
                            self.last_re.y + 1);
                    return pos_below_last_re;
                }

                // Did we run off the tractor beam edge thus finding it?
                else if !self.is_trac(&self.last_move) {
                    // Save the right edge
                    self.last_re = Position::new(self.last_move.x - 1,
                            self.last_move.y);
                    // update state and re-run the calculator to find left edge
                    self.p2_edge_find_right = false;
                    self.p2_edge_find_just_start = true;
                    return self.calc_next_part_2_edge();
                }

                // Or are we still on a tractor beam part and thus looking for the edge
                else {
                    let next_x = self.last_move.x + 1;
                    return Position::new(next_x, self.last_move.y);
                }
            }
            // Find left edge mode
            else {
                // Are we just starting in this mode?
                if self.p2_edge_find_just_start {
                    self.p2_edge_find_just_start = false;

                    let pos_below_last_le = Position::new(self.last_le.x,
                            self.last_le.y + 1);
                    return pos_below_last_le;
                }

                // Did we find a trac beam thus finding the edge?
                else if self.is_trac(&self.last_move) {
                    // Save the left edge
                    self.last_le = Position::new(self.last_move.x,
                            self.last_move.y);

                    // Did we find a spot that's wide enough?
                    if (self.last_re.x - self.last_le.x) + 1 >= box_size {
                        // Allow moving into length finding mode
                        self.p2_edge_mode = false;

                        //println!("Found 100 width area {:?} {:?} {}", self.last_le, self.last_re, self.last_re.x - self.last_le.x);
                    } else {
                        // Stay in edge finding mode
                        self.p2_edge_mode = true;
                    }

                    // Reset the edge finding mode state
                    self.p2_edge_find_right = true;
                    self.p2_edge_find_just_start = true;

                    // Rerun the calculator
                    return self.calc_next_part_2_edge();
                }

                // Or are we off a tractor beam and thus looking for the edge
                else {
                    let next_x = self.last_move.x + 1;
                    return Position::new(next_x, self.last_move.y);
                }
            }
        }

        // Length finding mode
        // When edge finding mode worked, now track the left edge to make sure it stays >= 100 points apart from the old right edge's x for 100 points
        else {
            // Check the bottom left corner of the box
            // If it's in the tractor beam, and the top right is (it's self.last_re)
            // then the whole thing is
            let x_tst = self.last_re.x - (box_size - 1);
            let y_tst = self.last_le.y + (box_size - 1);
            let test_pos = Position::new(x_tst, y_tst);

            // If we don't know what's in that corner, explore it
            if !self.is_explored(&test_pos) {
                return test_pos;
            }
            // If bottom left corner isn't track, go back to edge finding
            if !self.is_trac(&test_pos) {
                // Jump to edge finding mode
                self.p2_edge_mode = true;

                // Rerun the calculator
                return self.calc_next_part_2_edge();
            }
            // If bottom left corner is track, we're done.
            else {
                println!("Part 2: {:?} {}", self.last_re, (self.last_re.x - (box_size - 1)) * 10000 + self.last_le.y);
                self.part_2_done = true;
                return test_pos;
            }
        }
    }
}
impl Iterator for Drone {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        match self.part_1 {
            true => self.part_1_next(),
            false => self.part_2_next(),
        }
    }
}
impl OutputHandler for Drone {
    fn output(&mut self, val: i64) -> Result<(), &'static str> {
        if self.sent_x {
            panic!("Strange state to receive output!");
        }
        //println!("Got output {}", val);
        let for_write: char = match val {
                0 => '.',
                1 => { self.trac_pt_cnt += 1; '#' },
                _ => panic!("Unexpected output! {}", val),
            };
        self.view.insert(self.last_move.clone(), for_write);

        Ok(())
    }
}

fn main() {
    let filename: String = args().skip(1).take(1).collect();
    let input_prog: Vec<i64> = (&read_ints_comma_sep(&filename)[0]).to_vec();

    let mut drone = Drone::new();
    // /*
    while !drone.input_done {
        run_program(0, &input_prog, &mut drone);
    }
    drone.print_map(0, 49, 0, 49);
    println!("Part 1: {}", drone.trac_pt_cnt);
    //*/

    drone.part_1 = false;
    while !drone.part_2_done {
        run_program(0, &input_prog, &mut drone);
    }
    //drone.print_map(0, (drone.last_re.x + 2) as usize, (drone.last_le.y + 0) as usize, (drone.last_le.y + 100 ) as usize);
    //println!("Part 2: ");
}
