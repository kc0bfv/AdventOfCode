use std::env::args;
use std::collections::HashMap;
use std::io::{self, Write};

pub mod file_help;
pub mod new_intcode_compy;

use file_help::read_ints_comma_sep;
use new_intcode_compy::{run_program, OutputHandler};

#[derive(Debug, PartialEq, Eq)]
enum TileColor {
    Empty, Wall, Block, Paddle, Ball
}
impl TileColor {
    fn new(val: i64) -> TileColor {
        match val {
            0 => TileColor::Empty,
            1 => TileColor::Wall,
            2 => TileColor::Block,
            3 => TileColor::Paddle,
            4 => TileColor::Ball,
            _ => panic!("Invalid tile! {}", val),
        }
    }
}

#[derive(Debug)]
enum InputState {
    X,
    Y,
    T,
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
struct Game {
    tiles: HashMap<Position, TileColor>,
    input_state: InputState,
    temp_x: i64,
    temp_y: i64,
    score: i64,
    last_ball: Position,
    last_paddle: Position,
    paddle_move_needed: i64,
    human_player: bool,
}
impl Game {
    fn new(human_player: bool) -> Game {
        let tiles = HashMap::new();
        Game { tiles,
                input_state: InputState::X,
                temp_x: 0,
                temp_y: 0,
                score: 0,
                last_ball: Position::new(0, 0),
                last_paddle: Position::new(0, 0),
                paddle_move_needed: 0,
                human_player,
            }
    }

    fn count_tile(&self, tile: TileColor) -> usize {
        self.tiles.values().filter(|val| **val == tile).count()
    }

    fn print_game(&self) {
        println!("Score: {}", self.score);

        let extents = self.tiles.keys().fold(Position::new(0,0), |accum, cur|
                Position::new( if cur.x > accum.x { cur.x } else { accum.x },
                        if cur.y > accum.y { cur.y } else { accum.y } )
            );
        for y_pos in 0..extents.y {
            for x_pos in 0..extents.x {
                print!("{}",
                        match self.tiles.get(
                                &Position::new(x_pos, y_pos)
                            )
                        {
                            Some(TileColor::Wall)   => '#',
                            Some(TileColor::Block)  => '@',
                            Some(TileColor::Paddle) => '_',
                            Some(TileColor::Ball)   => '*',
                            Some(TileColor::Empty)  => ' ',
                            _                       => ' ',
                        }
                    )
            }
            println!("");
        }
    }
}
impl Iterator for Game {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        self.print_game();
        if ! self.human_player {
            Some(self.paddle_move_needed)
        } else {
            let mut inval = String::new();
            print!("a or s or d? ");
            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut inval).unwrap();
            match inval.chars().collect::<Vec<char>>()[0] {
                'a' => Some(-1),
                'd' => Some(1),
                _ => Some(0),
            }
        }
    }
}
impl OutputHandler for Game {
    fn output(&mut self, val: i64) -> Result<(), &'static str> {
        match self.input_state {
            InputState::X => {
                    self.temp_x = val;
                    self.input_state = InputState::Y;
                },
            InputState::Y => {
                    self.temp_y = val;
                    self.input_state = InputState::T;
                },
            InputState::T => {
                    if self.temp_x == -1 && self.temp_y == 0 {
                        self.score = val;
                    } else {
                        let pos = Position::new(self.temp_x.clone(), self.temp_y.clone());
                        let tile = TileColor::new(val);
                        if tile == TileColor::Paddle {
                            self.last_paddle = pos.clone();
                            self.paddle_move_needed = self.last_ball.x - self.last_paddle.x;
                        } else if tile == TileColor::Ball {
                            self.last_ball = pos.clone();
                            self.paddle_move_needed = self.last_ball.x - self.last_paddle.x;
                        }
                        self.tiles.insert(pos, tile);
                    }
                    self.input_state = InputState::X;
                },
        }
        Ok(())
    }
}

fn main() {
    let filename: String = args().skip(1).take(1).collect();
    let human_player: bool = args().len() > 2;
    let input_prog: Vec<i64> = (&read_ints_comma_sep(&filename)[0]).to_vec();

    let mut game = Game::new(false);
    run_program(0, &input_prog, &mut game);
    println!("Part 1: {}", game.count_tile(TileColor::Block));

    let mut quarters = input_prog.to_vec();
    quarters[0] = 2;
    let mut game = Game::new(human_player);
    run_program(0, &quarters, &mut game);
    println!("Part 2: {}", game.score);
}
