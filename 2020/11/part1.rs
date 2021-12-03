use std::io::BufRead;
use std::fmt::Display;
use std::cmp::PartialEq;

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
            Space::Occupied if neigh_count >= 4 => Space::Empty,
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

    fn step_map(&self) -> Self {
        let mut new_grid: Vec<Vec<T>> = Vec::new();

        for ind_outer in 0..self.grid.len() {
            let row_size = self.grid[ind_outer].len();
            let mut new_row: Vec<T> = Vec::new();
            for ind_inner in 0..row_size {
                let cur_space = &self.grid[ind_outer][ind_inner];
                let neigh = self.get_neighbors(ind_outer, ind_inner);
                let new_space = cur_space.react_to_neighbors(neigh);
                new_row.push(new_space);
            }
            new_grid.push(new_row);
        }

        Map {
            grid: new_grid,
        }
    }

    fn get_neighbors(&self, ind_outer: usize, ind_inner: usize) -> Vec<T> {
        let mut retval = Vec::new();
        for out_off in 0..3 {
            for inn_off in 0..3 {
                //println!("p {} {} {} {}", ind_outer, out_off, ind_inner, inn_off);
                if ind_outer == 0 && out_off == 0 {}
                else if ind_inner == 0 && inn_off == 0 {}
                else if out_off == 1 && inn_off == 1 {}
                else if (ind_outer + out_off - 1 < self.grid.len()) &&
                    (ind_inner + inn_off - 1 < self.grid[ind_outer].len())
                {
                    //println!("a {} {} {} {}", ind_outer, ind_outer+out_off-1, ind_inner, ind_inner+inn_off-1);
                    let item = self.grid[ind_outer+out_off-1][ind_inner+inn_off-1].clone();
                    retval.push(item);
                }
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

    let mut newmap = map.step_map();
    let mut iter = 1;

    while map.count_diff(&newmap) > 0 {
        println!("Map: {}\n{}", iter, newmap);
        iter += 1;
        map = newmap;
        newmap = map.step_map();
    }

    println!("Final Map: {}\n{}", iter, newmap);

    println!("Counted: {}", newmap.count_em(&Space::Occupied));
}
