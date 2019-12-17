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
    */
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
    fn turn_lookup(&self, other: &Position) -> &str {
        let rt_ret = "R";
        let lt_ret = "L";
        if self.y == 0 && other.x == 0 {
            if self.x == other.y { rt_ret }
            else { lt_ret }
        } else if self.x ==0 && other.y == 0 {
            if self.y == other.x { lt_ret }
            else { rt_ret }
        } else {
            panic!("Invalid turn! {:?} {:?}", self, other);
        }
        /*
        -1 0 -> 0 -1 - right
        -1 0 -> 0 1 - left
        1 0 -> 0 1 - right
        1 0 -> 0 -1 - left
        0 -1 -> 1 0 - right
        0 -1 -> -1 0 - left
        0 1 -> 1 0 - left
        0 1 -> -1 0 - right
        */
    }
}

#[derive(Debug)]
struct ASCII {
    view: Vec<Vec<char>>,
    scaf: Vec<Position>,
    moves: Vec<String>,
    robot: Position,
    cmd_pos: usize,
    map_lines: usize,
}
impl ASCII {
    fn new() -> Self {
        Self {
            view: vec![vec![]],
            scaf: vec![],
            moves: vec!["R".to_string()], // cheating
            robot: Position::new(0, 0),
            cmd_pos: 0,
            map_lines: 0,
        }
    }

    fn print_map(&mut self, highlight: Option<Position>) {
        for (y, line) in self.view.iter().enumerate() {
            for (x, c) in line.iter().enumerate() {
                if highlight != None
                        && highlight.as_ref().unwrap().x == x as i64
                        && highlight.as_ref().unwrap().y == y as i64
                {
                    print!("@");
                } else {
                    print!("{}", c);
                }
            }
            println!("");
        }
        println!("\n\n\n");
        self.map_lines = self.view.len();
    }

    fn interpret_map(&mut self) {
        if self.robot == Position::new(0, 0) {
            panic!("Robot still initialized!");
        }

        let surrounding_offsets: Vec<Position> = vec![
                Position::new(-1,  0),
                Position::new( 1,  0),
                Position::new( 0, -1),
                Position::new( 0,  1),
            ];
        let mut backwards_dir: HashMap<Position, Position> = HashMap::new();
        backwards_dir.insert(Position::new(-1,  0), Position::new( 1,  0));
        backwards_dir.insert(Position::new( 1,  0), Position::new(-1,  0));
        backwards_dir.insert(Position::new( 0, -1), Position::new( 0,  1));
        backwards_dir.insert(Position::new( 0,  1), Position::new( 0, -1));

        let mut cur_pos = self.robot.clone();
        let mut dir_cnt: u32 = 0;
        let mut cur_dir = 
            {
                let mut surr_scaf: Vec<Position> = surrounding_offsets.iter()
                    .filter(|dir| {
                            let in_dir = dir.add(&cur_pos);
                            (*in_dir.in_vec(&self.view)) == '#'
                        })
                    .cloned()
                    .collect();
                if surr_scaf.len() != 1 { panic!("Wrong surr scaff in init!"); }
                surr_scaf.remove(0)
            };
        loop {
            // Add each step to the scaffold vector
            self.scaf.push(cur_pos.clone());

            let bw_dir: Position = (*backwards_dir
                    .get(&cur_dir)
                    .expect("Did not find cur_dir in backwards!"))
                    .clone();
            let surr_scaf: Vec<Position> = surrounding_offsets.iter()
                .filter(|dir| {
                        let in_dir = dir.add(&cur_pos);
                        **dir != bw_dir && (*in_dir.in_vec(&self.view)) == '#'
                    })
                .cloned()
                .collect();

            if surr_scaf.len() == 0 {
                // No more scaffolding... done
                break;
            }

            if !surr_scaf.contains(&cur_dir) {
                // If we can't step in the same direction we've been going...
                // Figure out which way to turn
                if surr_scaf.len() != 1 {
                    self.print_map(Some(cur_pos.clone()));
                    panic!("Unexpected surr_scaf! {:?} {:?}", cur_pos, surr_scaf);
                }

                // Add a move to mark this turn
                self.moves.push(dir_cnt.to_string());
                self.moves.push(cur_dir.turn_lookup(
                                surr_scaf.get(0).unwrap()
                            ).to_string()
                        );

                cur_dir = surr_scaf[0].clone();
                dir_cnt = 0;
            }
            cur_pos = cur_pos.add(&cur_dir);
            dir_cnt += 1;
        }
        self.moves.push(dir_cnt.to_string());
    }

    fn find_dupe_scaf(&self) -> Vec<Position> {
        let mut seen: Vec<Position> = vec![];
        self.scaf
            .iter()
            .filter(|pos| {
                let in_seen = seen.contains(&pos);
                seen.push((*pos).clone());
                in_seen
            })
            .cloned()
            .collect()
    }
    fn calc_alignment(&self) -> i64 {
        self.find_dupe_scaf().iter().map(|pos| pos.x * pos.y).sum()
    }
}
impl Iterator for ASCII {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        let cmds = "C,A,C,A,B,C,A,B,C,B\nL,8,L,6,L,10,L,6\nR,6,L,8,L,10,R,6\nR,6,L,6,L,10\n";
        let retval: Self::Item;

        println!("Sending program! {} {}", self.cmd_pos, cmds.len());
        if self.cmd_pos < cmds.len() {
            retval = cmds.bytes().skip(self.cmd_pos).next().unwrap() as i64;
        } else if self.cmd_pos == cmds.len() {
            retval = 'y' as i64;
        } else if self.cmd_pos == cmds.len() +1 {
            retval = '\n' as i64;
        } else {
            panic!("Another input requested!");
        }
        self.cmd_pos += 1;
        return Some(retval);
        /*
        master="C,A,C,A,B,C,A,B,C,B"
        A="L,8,L,6,L,10,L,6"
        B="R,6,L,8,L,10,R,6"
        C="R,6,L,6,L,10"
        "C,A,C,A,B,C,A,B,C,B\nL,8,L,6,L,10,L,6\nR,6,L,8,L,10,R,6\nR,6,L,6,L,10"
        */
    }
}
impl OutputHandler for ASCII {
    fn output(&mut self, val: i64) -> Result<(), &'static str> {
        if self.map_lines > 0 && self.view.len() == self.map_lines {
            self.view = vec![];
            self.view.push(vec![]);
        }
        if val > 255 {
            println!("Big val! {}", val);
            return Ok(());
        }

        let last_line = self.view.len() - 1;
        let val_as_char = (val as u8) as char;
        // Store view
        match val {
            10 => self.view.push(Vec::new()),
            _ => self.view[last_line].push(val_as_char),
        }
        // Find robot
        match val_as_char {
            '^' | 'v' | '<' | '>' | 'X' => self.robot =
                    Position::new((self.view[last_line].len() - 1) as i64,
                            (self.view.len() - 1) as i64),
            _ => (),
        }
        if self.map_lines > 0 && self.view.len() == self.map_lines {
            self.print_map(None);
        }
        Ok(())
    }
}

fn main() {
    let filename: String = args().skip(1).take(1).collect();
    let input_prog: Vec<i64> = (&read_ints_comma_sep(&filename)[0]).to_vec();

    let mut ascii = ASCII::new();
    run_program(0, &input_prog, &mut ascii);
    ascii.print_map(None);
    ascii.interpret_map();
    println!("Part 1: {}", ascii.calc_alignment());
    println!("Moves: {:?}", ascii.moves);

    let mut part_2_input: Vec<i64> = vec![2];
    part_2_input.extend(input_prog[1..].iter().cloned());
    run_program(0, &part_2_input, &mut ascii);
}
