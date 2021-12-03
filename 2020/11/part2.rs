use std::io::BufRead;
use std::fmt::Display;
use std::cmp::PartialEq;
use std::convert::TryInto;

trait MapSpace {
    fn new(val: char) -> Self;
    fn react_to_neighbors(&self, neigh: Vec<Self>) -> Self where Self: Sized;
}

#[derive(Clone, PartialEq, Debug)]
enum Space {
    Floor,
    Empty,
    Occupied,
}

impl MapSpace for Space {
    fn new(val: char) -> Self {
        match val {
            'L' => Space::Empty,
            '#' => Space::Occupied,
            _   => Space::Floor,
        }
    }
    fn react_to_neighbors(&self, neigh: Vec<Self>) -> Self {
        let neigh_count: usize = neigh.iter().fold( 0, |acc, n| {
            match n {
                Space::Occupied => acc + 1,
                _ => acc,
            }
        });
        //println!("neigh_count {} {:?}", neigh_count, neigh);
        match self {
            Space::Empty if neigh_count == 0 => Space::Occupied,
            Space::Occupied if neigh_count >= 5 => Space::Empty,
            oth => oth.clone(),
        }
    }
}

impl std::fmt::Display for Space {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let outval = match self {
            Space::Floor => ".",
            Space::Empty => "L",
            Space::Occupied => "#",
        };
        write!(f, "{}", outval)
    }
}

struct Map<T> {
    grid: Vec<Vec<T>>,
}

impl<T: MapSpace + Clone + PartialEq> Map<T> {
    fn new(filename: String) -> Self {
        let path = std::path::Path::new(&filename);
        let file = std::fs::File::open(path).expect("File failed to open");
        let lines_maybe = std::io::BufReader::new(file).lines();
        let lines = lines_maybe.map( |line| line.expect("Line had an error"));
        let grid: Vec<Vec<T>> = lines.map(
            |line| line.chars().map(
                |char| T::new(char)
            ).collect()
        ).collect();

        Map {
            grid: grid,
        }
    }

    fn step_map(&self, ignore_entries: &Vec<T>) -> Self {
        let mut new_grid: Vec<Vec<T>> = Vec::new();

        for ind_outer in 0..self.grid.len() {
            let row_size = self.grid[ind_outer].len();
            let mut new_row: Vec<T> = Vec::new();
            for ind_inner in 0..row_size {
                let cur_space = &self.grid[ind_outer][ind_inner];
                let neigh = self.get_neighbors(ind_outer, ind_inner, ignore_entries);
                let new_space = cur_space.react_to_neighbors(neigh);
                new_row.push(new_space);
            }
            new_grid.push(new_row);
        }

        Map {
            grid: new_grid,
        }
    }

    fn get_neighbors(&self, ind_row: usize, ind_col: usize, ignore_entries: &Vec<T>)
        -> Vec<T>
    {
        let mut retval = Vec::new();
        let directions = vec![(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)];
        for (row_dir, col_dir) in directions.iter() {
            let mut keep_going = true;
            let mut count = 1;

            while keep_going {
                let ind_r: i32 = ind_row.try_into().expect("Row type conv fail");
                let ind_c: i32 = ind_col.try_into().expect("Col type conv fail");
                let row_i: i32 = (row_dir * count) + ind_r;
                let col_i: i32 = (col_dir * count) + ind_c;

                count += 1;

                if row_i < 0 || col_i < 0 {
                    break;
                }

                let row: usize = row_i.try_into().expect("Rowi type conv fail");
                let col: usize = col_i.try_into().expect("Coli type conv fail");

                if row >= self.grid.len() || col >= self.grid[row].len() {
                    break;
                }

                let item = self.grid[row][col].clone();
                if ignore_entries.iter().any(|i| *i == item) {
                    continue;
                }

                retval.push(item);
                keep_going = false;
            }
        }

        retval
    }

    fn count_diff(&self, other: &Self) -> usize {
        let mut retval = 0;
        if self.grid.len() != other.grid.len() {
            panic!("Grids are not same-sized");
        }
        for row in 0..self.grid.len() {
            if self.grid[row].len() != other.grid[row].len() {
                panic!("Grids are not same-sized");
            }
            for col in 0..self.grid[row].len() {
                if self.grid[row][col] != other.grid[row][col] {
                    retval += 1;
                }
            }
        }

        retval
    }

    fn count_em(&self, em: &T) -> usize {
        self.grid.iter().fold(0, |accout, row| {
            row.iter().fold(accout, |acc, item| {
                if item == em { acc + 1 }
                else { acc }
            })
        })
    }
}

impl<T: Display> std::fmt::Display for Map<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.grid.iter().for_each( |row| {
            row.iter().for_each( |item| {
                write!(f, "{}", item).expect("Error writing item");
            });
            write!(f, "\n").expect("Error writing newline");
        });
        Ok(())
    }
}

fn main() {
    let filename = std::env::args().nth(1).expect("Give a filename...");
    let mut map = Map::<Space>::new(filename);
    println!("Map: \n{}", map);

    let ignore_entries = vec![Space::Floor];

    let mut newmap = map.step_map(&ignore_entries);
    let mut iter = 1;

    while map.count_diff(&newmap) > 0 {
        println!("Map: {}\n{}", iter, newmap);
        iter += 1;
        map = newmap;
        newmap = map.step_map(&ignore_entries);
    }

    println!("Final Map: {}\n{}", iter, newmap);

    println!("Counted: {}", newmap.count_em(&Space::Occupied));
}
