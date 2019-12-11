use std::env::args;
use std::collections::HashMap;

pub mod file_help;
pub mod new_intcode_compy;

use file_help::read_ints_comma_sep;
use new_intcode_compy::{run_program, OutputHandler};

#[derive(Debug, PartialEq, Eq)]
enum PanelColor {
    Black,
    White,
}

#[derive(Debug)]
enum OutputState {
    PaintColor,
    Move,
}

#[derive(Debug)]
enum MoveDir {
    Up,
    Down,
    Left,
    Right,
}
impl MoveDir {
    fn get_rotated(&self, dir: i64) -> MoveDir {
        match dir {
            0 => match self {
                    Self::Up => MoveDir::Left,
                    Self::Left => MoveDir::Down,
                    Self::Down => MoveDir::Right,
                    Self::Right => MoveDir::Up,
                },
            1 => match self {
                    Self::Up => MoveDir::Right,
                    Self::Right => MoveDir::Down,
                    Self::Down => MoveDir::Left,
                    Self::Left => MoveDir::Up,
                },
            _ => panic!("Invalid turn direction"),
        }
    }
    fn pos_change(&self) -> (i64, i64) {
        match self {
            Self::Up => (0, -1),
            Self::Down => (0, 1),
            Self::Left => (-1, 0),
            Self::Right => (1, 0),
        }
    }
}

#[derive(Debug,Hash,PartialEq,Eq,Clone)]
struct Position {
    x: i64,
    y: i64,
}
impl Position {
    fn new(x: i64, y: i64) -> Position {
        Position{ x, y }
    }
}

#[derive(Debug)]
struct Robot {
    panels: HashMap<Position, PanelColor>,
    position: Position,
    point_dir: MoveDir,
    output_state: OutputState,
}
impl Robot {
    fn new(start_color: PanelColor) -> Robot {
        let mut panels = HashMap::new();
        panels.insert(Position::new(0, 0), start_color);
        Robot { panels,
                position: Position::new(0, 0),
                point_dir: MoveDir::Up,
                output_state: OutputState::PaintColor
            }
    }
    fn print_graph(&self) -> () {
        let (mut min_x, mut max_x, mut min_y, mut max_y) = (10000, -10000, 10000, -10000);
        self.panels.keys().for_each(|pos| {
                if pos.x < min_x { min_x = pos.x }
                if pos.x > max_x { max_x = pos.x }
                if pos.y < min_y { min_y = pos.y }
                if pos.y > max_y { max_y = pos.y }
            });
        for cur_y in min_y..max_y+1 {
            for cur_x in min_x..max_x {
                let cur_pos = Position::new(cur_x, cur_y);
                print!("{}",
                        if self.panels.contains_key(&cur_pos) &&
                            *self.panels.get(&cur_pos).unwrap() == PanelColor::White
                        {
                            "#"
                        } else {
                            " "
                        }
                    );
            }
            println!();
        }
    }
    fn graph_len(&self) -> usize {
        self.panels.len()
    }
}
impl Iterator for Robot {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        match self.panels.get(&self.position).unwrap() {
            PanelColor::Black => Some(0),
            PanelColor::White => Some(1),
        }
    }
}
impl OutputHandler for Robot {
    fn output(&mut self, val: i64) -> Result<(), &'static str> {
        match self.output_state {
            OutputState::PaintColor => {
                    self.panels.insert(self.position.clone(),
                            match val {
                                0 => PanelColor::Black,
                                1 => PanelColor::White,
                                _ => return Err("Invalid paint color specified"),
                            }
                        );
                    self.output_state = OutputState::Move;
                },
            OutputState::Move => {
                    self.point_dir = self.point_dir.get_rotated(val);
                    let (delta_x, delta_y) = self.point_dir.pos_change();
                    let (new_x, new_y) = (self.position.x + delta_x,
                            self.position.y + delta_y);
                    self.position = Position::new(new_x, new_y);
                    if !self.panels.contains_key(&self.position) {
                        self.panels.insert(self.position.clone(), PanelColor::Black);
                    }
                    self.output_state = OutputState::PaintColor;
                },
        }
        //self.print_graph();
        Ok(())
    }
}

fn main() {
    let filename: String = args().skip(1).take(1).collect();
    let input_prog: Vec<i64> = (&read_ints_comma_sep(&filename)[0]).to_vec();

    let mut robot = Robot::new(PanelColor::Black);
    run_program(0, &input_prog, &mut robot);
    robot.print_graph();
    println!("Part 1: {}", robot.graph_len());

    let mut robot = Robot::new(PanelColor::White);
    run_program(0, &input_prog, &mut robot);
    println!("Part 2");
    robot.print_graph();
}
