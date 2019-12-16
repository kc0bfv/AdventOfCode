use std::env::args;
use std::collections::HashMap;
use std::convert::TryInto;
//use std::io::{self, Write};

pub mod file_help;
pub mod new_intcode_compy;

use file_help::read_ints_comma_sep;
use new_intcode_compy::{run_program, OutputHandler};

#[derive(Debug, PartialEq, Eq)]
enum Movement { North = 1, South = 2, West = 3, East = 4 }

#[derive(Debug, PartialEq, Eq)]
enum Status{ HitWall = 0, Moved = 1, MovedFoundO2 = 2 }
impl Status {
    fn from_int(val: i64) -> Status {
        match val {
            0 => Status::HitWall,
            1 => Status::Moved,
            2 => Status::MovedFoundO2,
            _ => panic!("Invalid status conv {}", val),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum MapPos{ Wall, Blank, O2 }

#[derive(Debug,Hash,PartialEq,Eq,Clone)]
struct Position {
    x: i64,
    y: i64,
}
impl Position {
    fn new(x: i64, y: i64) -> Position {
        Position{ x, y }
    }
    fn manh_dist(&self, other: &Position) -> u64 {
        ((other.x - self.x).abs() + (other.y - self.y).abs()).try_into().unwrap()
    }
    fn diff(&self, other: &Position) -> (i64, i64) {
        (other.x - self.x, other.y - self.y)
    }
}

#[derive(Debug)]
struct RC {
    map: HashMap<Position, MapPos>,
    unexplored: Vec<Position>,
    cur_pos: Position,
    move_to_pos: Position,
    cur_dest: Option<Position>,
    o2_pos: Position,
}
impl RC {
    fn new() -> Self {
        let mut map: HashMap<Position, MapPos> = HashMap::new();
        map.insert(Position::new(0,0), MapPos::Blank);
        RC {
                map: map,
                unexplored: Self::get_surroundings(&Position::new(0,0)),
                cur_pos: Position::new(0,0),
                move_to_pos: Position::new(0,0),
                cur_dest: None,
                o2_pos: Position::new(0,0),
            }
    }

    fn fill_map_with_o2(&self) {
        let mut border: Vec<Position> = vec![self.o2_pos.clone()];
        let mut previously_filled: Vec<Position> = Vec::new();
        let mut iter_cnt: usize = 0;
        loop {
            if border.len() == 0 { println!("Iter count {}", iter_cnt); return; };

            let new_border: Vec<Position> = border.iter()
                .flat_map(|pos| Self::get_surroundings(pos))
                .filter(|pos| ! border.contains(pos))
                .filter(|pos| ! previously_filled.contains(pos))
                .filter(|pos| *(self.map.get(pos).unwrap()) == MapPos::Blank)
                .collect();

            previously_filled.extend(border);
            border = new_border.clone();

            iter_cnt += 1;
        }
    }

    fn print_map(&self) {
        let (max_extent, min_extent): (Position, Position) = self.map.keys()
            .fold((Position::new(0, 0), Position::new(0, 0)), |(max_accum, min_accum), cur|
                (Position::new(
                        if cur.x > max_accum.x { cur.x } else { max_accum.x },
                        if cur.y > max_accum.y { cur.y } else { max_accum.y },
                        ),
                    Position::new(
                        if cur.x < min_accum.x { cur.x } else { min_accum.x },
                        if cur.y < min_accum.y { cur.y } else { min_accum.y },
                        )));
        //println!("Extents {:?} {:?}", min_extent, max_extent);
        for y_pos in min_extent.y..(max_extent.y+1) {
            for x_pos in min_extent.x..(max_extent.x+1) {
                let cur_pos = Position::new(x_pos, y_pos);
                //println!("{:?}", cur_pos);
                if self.cur_pos == cur_pos {
                    print!("c");
                } else {
                    print!("{}",
                            match self.map.get(&cur_pos) {
                                Some(MapPos::Wall) => '#',
                                Some(MapPos::Blank) => ' ',
                                Some(MapPos::O2) => 'o',
                                None => '?',
                            }
                        );
                }
            }
            println!("");
        }
        println!("\n\n\n");
    }

    fn get_surroundings(pos: &Position) -> Vec<Position> {
        vec![
                Position::new(pos.x + 1, pos.y + 0),
                Position::new(pos.x - 1, pos.y + 0),
                Position::new(pos.x + 0, pos.y + 1),
                Position::new(pos.x + 0, pos.y - 1),
            ]
    }
    
    fn add_unvisited_near(&mut self) {
        let surroundings = Self::get_surroundings(&self.cur_pos);
        let to_extend: Vec<Position>;
        {
            let filted = surroundings.into_iter()
                    .filter(|pos| ! self.map.contains_key(pos))
                    .filter(|pos| ! self.unexplored.contains(pos));
            to_extend = filted.collect();
        }
        self.unexplored.extend(to_extend);
        //println!("Unexplored {:?}", self.unexplored);
    }

    fn find_closest_unvisited(&mut self) -> Option<Position> {
        {
            let unexp_filt: Vec<Position>;
            {
                unexp_filt = self.unexplored.iter().cloned().filter(|pos| ! self.map.contains_key(&pos)).collect();
            }
            self.unexplored = unexp_filt;
        }
        let output: Option<(usize, &Position)> = 
            self.unexplored.iter().enumerate().min_by(|(_, pos_1), (_, pos_2)|
                self.cur_pos.manh_dist(&pos_1).cmp(&self.cur_pos.manh_dist(&pos_2))
                );
        match output {
                Some((ind, _)) => Some(self.unexplored.remove(ind)),
                None => None,
            }
    }

    fn find_path_to(&self, dest: &Position) -> Vec<Position> {
        self.find_path_to_from(self.cur_pos.clone(), dest)
    }

    fn find_path_to_from(&self, src: Position, dest: &Position) -> Vec<Position> {
        //println!("DEST {:?} src {:?}", dest, src);
        let mut dist_map: HashMap<Position, u64> = HashMap::new();
        dist_map.insert(src.clone(), 0);
        let adj = Self::get_surroundings(&src);
        let mut open_set: Vec<(u64, Position)> = adj.into_iter().map(|pos| (1, pos)).collect();
        loop {
            //println!("Starting {}", open_set.len());
            // Find the minimum element from the open set and remove it
            let (ind, _) = open_set.iter().enumerate().min_by(|(_, (w1, _)), (_, (w2, _))| w1.cmp(w2)).expect("Open set had no more mins...");
            let (weight, next_pt): (u64, Position) = open_set.remove(ind);

            // If it's the destination, we're done
            if next_pt == *dest {
                dist_map.insert(next_pt.clone(), weight + 1);
                break;
            }

            // Make sure it's a valid place to move to
            if ! self.map.contains_key(&next_pt) {
                // If it's not the dest, it must be a place we've already seen
                //println!("Not already seen {:?}", next_pt);
                continue;
            } else if *self.map.get(&next_pt).unwrap() == MapPos::Wall {
                // Make sure it's not a wall...
                //println!("Wall {:?}", next_pt);
                continue;
            }

            dist_map.insert(next_pt.clone(), weight + 1);

            // Get all the surrounding ones that haven't been checked and add them to the open set with a new weight
            let nu_with_weight: Vec<(u64, Position)>;
            {
                let near = Self::get_surroundings(&next_pt);
                let near_unchecked = near.into_iter()
                    .filter(|pos| ! dist_map.contains_key(&pos))
                    ;
                    //.filter(|pos| self.map.contains_key(&pos))
                    //.filter(|pos| *self.map.get(&pos).unwrap() != MapPos::Wall)
                    //.filter(|pos| ! open_set.iter().any(|(_, in_os)| pos == in_os));
                nu_with_weight = near_unchecked.map(|pos| (weight + 1, pos)).collect();
                //println!("nu_ {:?}", nu_with_weight);
            }
            open_set.extend(nu_with_weight);
        }
        //println!("Finding");

        // Now find the path
        let mut path: Vec<Position> = vec![(*dest).clone()];
        let mut prev_pos: Position = (*dest).clone();
        loop {

            let near_prev = Self::get_surroundings(&prev_pos);
            let next_step_pos = near_prev.into_iter().filter(|pos| dist_map.contains_key(&pos));
            let next_step: Position = next_step_pos.min_by(|pos_1, pos_2|
                    dist_map.get(&pos_1).expect("FPEG1")
                            .cmp(dist_map.get(&pos_2).expect("FPEG2")))
                            .expect("Didn't find min next_step");
            prev_pos = next_step.clone();
            if next_step == src { break; }
            path.push(next_step);
        }
        path.reverse();
        path
    }

    fn get_move_dir(&self) -> Result<Movement, &'static str> {
        match self.cur_pos.diff(&self.move_to_pos) {
            (-1,  0) => Ok(Movement::West),
            ( 1,  0) => Ok(Movement::East),
            ( 0, -1) => Ok(Movement::North),
            ( 0,  1) => Ok(Movement::South),
            _        => Err("Invalid movement!"),
        }
    }
}
impl Iterator for RC {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        // If we don't have a cur_dest find the closest unexplored position and set it as cur_dest
        //println!("Starting next");
        if self.cur_dest == None {
            //println!("Finding new dest");
            let closest_unvis = self.find_closest_unvisited();
            match closest_unvis { 
                Some(val) => {
                        self.cur_dest = Some(val);
                    },
                None => {
                    println!("Dist to o2: {}", self.find_path_to_from(Position::new(0,0), &self.o2_pos).len());
                    self.fill_map_with_o2();
                    return None;
                }
            }
        }

        //println!("Cur {:?} dest {:?}", self.cur_pos, self.cur_dest.as_ref().unwrap());

        // Find the path to the cur_dest using only known spaces
        let path: Vec<Position> = self.find_path_to(self.cur_dest.as_ref().unwrap());

        // Try to take a step down that path
        self.move_to_pos = path[0].clone();
        //println!("Next {:?}", self.move_to_pos);

        if *(self.cur_dest.as_ref().unwrap()) == self.move_to_pos {
            //println!("Will get to dest...");
            self.cur_dest = None;
        }

        Some(self.get_move_dir().expect("Could not calculate move dir!") as i64)
    }
}
impl OutputHandler for RC {
    fn output(&mut self, val: i64) -> Result<(), &'static str> {
        match Status::from_int(val) {
            Status::HitWall => {
                    if self.map.contains_key(&self.move_to_pos) {
                        if *self.map.get(&self.move_to_pos).unwrap() != MapPos::Wall {
                            panic!("We hit a wall but we didn't think that pos was a wall");
                        } else {
                            panic!("We hit a wall that we already knew about... {:?}", self.move_to_pos);
                        }
                    }
                    self.map.insert(self.move_to_pos.clone(), MapPos::Wall);
                },
            Status::Moved => {
                    if self.map.contains_key(&self.move_to_pos) {
                        if *self.map.get(&self.move_to_pos).unwrap() != MapPos::Blank {
                            panic!("We hit a blank spot that we didn't think was blank");
                        }
                    } else {
                        self.map.insert(self.move_to_pos.clone(), MapPos::Blank);
                    }
                    self.cur_pos = self.move_to_pos.clone();
                },
            Status::MovedFoundO2 => {
                    if self.map.contains_key(&self.move_to_pos) {
                        if *self.map.get(&self.move_to_pos).unwrap() != MapPos::O2 {
                            panic!("We hit O2 that we didn't think was O2");
                        }
                    } else {
                        self.map.insert(self.move_to_pos.clone(), MapPos::O2);
                        self.o2_pos = self.move_to_pos.clone();
                        println!("Found O2! {:?}", self.move_to_pos);
                    }
                    self.cur_pos = self.move_to_pos.clone();
                },
        }
        self.add_unvisited_near();

        println!("Printing map");
        self.print_map();

        Ok(())
    }
}

fn main() {
    let filename: String = args().skip(1).take(1).collect();
    let input_prog: Vec<i64> = (&read_ints_comma_sep(&filename)[0]).to_vec();

    let mut rc = RC::new();
    run_program(0, &input_prog, &mut rc);
    println!("Part 1: {}", '?');
}
