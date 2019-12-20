use std::env::args;
use std::collections::{HashMap, HashSet, BTreeSet};
use std::convert::TryInto;
//use std::cmp::Ordering;

pub mod file_help;

use file_help::read_strs;

#[derive(Debug, Clone)]
enum SpotType {
    Start,
    Wall,
    Space,
    Key(char),
    Door(char),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
struct Position {
    x: i64,
    y: i64,
}
impl Position {
    fn new(x: i64, y: i64) -> Self {
        Self { x, y }
    }
    fn get_surroundings(&self) -> Vec<Self> {
        vec![
            Position::new(self.x - 1, self.y + 0),
            Position::new(self.x + 1, self.y + 0),
            Position::new(self.x + 0, self.y - 1),
            Position::new(self.x + 0, self.y + 1),
            ]
    }
}

#[derive(Debug, Clone)]
struct MazeInt {
    maze: Vec<Vec<SpotType>>,
    keys: HashMap<char, Position>,
    key_blocks: HashMap<char, HashSet<char>>,
    doors: HashMap<char, Position>,
    start: Vec<Position>,
    got_keys: HashMap<char, Position>,
}
impl MazeInt {
    fn new(lines: Vec<Vec<String>>) -> Self {
        let mut maze: Vec<Vec<SpotType>> = vec![];
        let mut keys: HashMap<char, Position> = HashMap::new();
        let key_blocks: HashMap<char, HashSet<char>> = HashMap::new();
        let mut doors: HashMap<char, Position> = HashMap::new();
        let mut start: Vec<Position> = vec![];
        let got_keys: HashMap<char, Position> = HashMap::new();

        let mut maze_line: Vec<SpotType> = vec![];
        for line in lines {
            for spot in line[0].chars() {
                let cur_pos = Position::new(maze_line.len().try_into().unwrap(),
                        maze.len().try_into().unwrap());
                match spot {
                    '@' => {
                            start.push(cur_pos);
                            maze_line.push(SpotType::Start);
                        },
                    '#' => maze_line.push(SpotType::Wall),
                    '.' => maze_line.push(SpotType::Space),
                    'a'..='z' => {
                            keys.insert(spot, cur_pos);
                            maze_line.push(SpotType::Key(spot))
                        },
                    'A'..='Z' => {
                            doors.insert(spot, cur_pos);
                            maze_line.push(SpotType::Door(spot));
                        },
                    _ => {
                            panic!("Unexpected map char found: {}", spot);
                        },
                }
            }
            maze.push(maze_line);
            maze_line = vec![];
        }

        Self { maze, keys, key_blocks, doors, start, got_keys }
    }

    fn get_quad_for_dest(&self, dest: &Position) -> usize {
        if self.start.len() == 1 { return 0; }
        assert!(self.start.len() == 4);
        // Quad and start key map:
        // 0 1
        // 2 3
        if dest.y < (self.maze.len() / 2) as i64 {
            if dest.x < (self.maze[0].len() / 2) as i64 { 0 }
            else { 1 }
        } else {
            if dest.x < (self.maze[0].len() / 2) as i64 { 2 }
            else { 3 }
        }
    }
    fn update_pos_to_key(&self, cur_pos: &Vec<Position>, key: &char) -> Vec<Position> {
        let key_pos = match self.keys.get(key) {
            Some(val) => val,
            None => panic!("No start point for nonexistent key: {}", key),
        };
        let dest_quad = self.get_quad_for_dest(key_pos);
        cur_pos.iter().cloned().enumerate()
                .map(|(quad, pos)| if quad == dest_quad { key_pos.clone() } else { pos })
                .collect()
    }

    fn get_cur_pos_for_dest<'a>(&'a self, cur_pos: &'a Vec<Position>, dest: &'a Position)
        -> &'a Position
    {
        cur_pos.get(self.get_quad_for_dest(dest))
                .expect("Invalid unwrap in get_cur_pos_for_dest")
    }

    fn get_start_for_key(&self, key: char) -> &Position {
        let key_pos = match self.keys.get(&key) {
            Some(val) => val,
            None => panic!("No start point for nonexistent key: {}", key),
        };
        self.start.get(self.get_quad_for_dest(key_pos))
                .expect("Invalid unwrap in get_start_for_key")
    }

    fn init_key_blocks(&mut self,
            cache: &mut HashMap<(Position, Position),
                    Option<(Vec<Position>, HashSet<char>)>>
        )
    {
        if self.key_blocks.len() > 0 {
            return;
        }
        let initial_filt: Vec<(char, Position)> =
            (b'a'..=b'z').map(|c| c as char)
                .filter(|c| self.keys.contains_key(c))
                .map(|c| (c, self.keys.get(&c).unwrap().clone()))
                .collect();
        let key_chars: Vec<(char, Option<(Vec<Position>, HashSet<char>)>)> =
            initial_filt.into_iter()
                .map(|(c, dest)| (c, self.find_path_to(
                        &self.start.clone(), &dest, cache))
                        )
                .collect();
        key_chars.into_iter()
                .for_each(|(c, opt_val)| match opt_val {
                        Some((_, blocks)) => { self.key_blocks.insert(c, blocks); },
                        None => (),
                    });
                        
        ()
    }

    fn find_shortest(&mut self,
            cache: &mut HashMap<(Position, Position),
                    Option<(Vec<Position>, HashSet<char>)>>,
            cache_2: &mut HashMap<Vec<Position>, HashSet<(Vec<char>, usize, usize)>>,
        ) -> usize
    {
        self.init_key_blocks(cache);
        let path: Vec<char> = vec![];
        self.recurs_find(&self.start.clone(), 0, std::usize::MAX, cache, &path, cache_2)
    }

    fn recurs_find(&mut self, cur_pos: &Vec<Position>, in_path_len: usize, in_best_prev: usize,
            cache: &mut HashMap<(Position, Position),
                    Option<(Vec<Position>, HashSet<char>)>>,
            path: &Vec<char>,
            cache_2: &mut HashMap<Vec<Position>, HashSet<(Vec<char>, usize, usize)>>,
        ) -> usize
    {
        // No sense in continuing if we are already worse than previous best
        // TODO add in an estimate of min remaining here
        if cache_2.contains_key(cur_pos) {
            let mut keys_set: BTreeSet<char> = BTreeSet::new();
            self.got_keys.keys().for_each(|k| { keys_set.insert(k.clone()); });

            let pos_cache = cache_2.get(cur_pos).unwrap().clone();
            for (cache_key_vec, cache_dist_come, cache_dist_rem) in pos_cache {
                let mut cache_keys: BTreeSet<char> = BTreeSet::new();
                cache_key_vec.into_iter().for_each(|k| { cache_keys.insert(k); });

                if keys_set == cache_keys {
                    // If this cache has the same set of keys remaining, whatever
                    // its path was was optimal
                    // If we can't beat it, no sense in continuing
                    if in_path_len >= cache_dist_come {
                        return std::usize::MAX;
                    }
                    // Otherwise, we got a new best
                    let keys_vec: Vec<char> = keys_set.into_iter().collect();
                    cache_2.get_mut(cur_pos).unwrap().insert(
                            (keys_vec, in_path_len, cache_dist_rem));
                    return in_path_len + cache_dist_rem;
                }
                if keys_set.is_subset(&cache_keys) {
                    // If the cache has more keys than me, it's further along
                    // If it also has a better path length then there's no sense
                    // In continuing
                    if in_path_len >= cache_dist_come {
                        return std::usize::MAX;
                    }
                }
            }
        }
        if in_path_len >= in_best_prev {
            return std::usize::MAX;
        }

        let poss_keys: Vec<char> = (b'a'..=b'z')
                .map(|c| c as char)
                .filter(|c| self.keys.contains_key(c))
                .filter(|c| !self.got_keys.contains_key(c))
                .collect();

        //println!("cur_pos {:?} poss_keys {:?} got_keys {:?}", cur_pos, poss_keys, self.got_keys);

        if poss_keys.len() == 0 {
            return in_path_len; // no more keys to find!
        }

        let mut sorted_poss_keys = poss_keys.clone();
        sorted_poss_keys.sort_by(|uno, dos| {
                        let uno_pos = self.keys.get(uno).unwrap().clone();
                        let dos_pos = self.keys.get(dos).unwrap().clone();
                        let uno_dist = self.find_dist_to(cur_pos, &uno_pos, cache);
                        let dos_dist = self.find_dist_to(cur_pos, &dos_pos, cache);
                        uno_dist.cmp(&dos_dist)
                    });
        //println!("{:?}", sorted_poss_keys);

        let mut opts: Vec<usize> = vec![];
        //let mut best_in_recur: usize = std::usize::MAX;
        let mut best_in_recur: usize = in_best_prev;
        for poss_key in sorted_poss_keys {
            let key_blocks = self.key_blocks
                    .get(&poss_key)
                    .expect("Panic in recurs key blocks");
            // If we don't have all the blocking keys for this one, skip it
            if ! key_blocks
                    .iter()
                    .all(|val| self.got_keys.contains_key(
                            &val.to_lowercase()
                                .next()
                                .expect("Lowercase conversion error"))
                                )
            {
                //println!("Don't have all keys: {:?}", key_blocks);
                continue;
            }

            let dest = self.keys.get(&poss_key).unwrap().clone();
            let opt_path = self.find_path_to_no_blocks(cur_pos, &dest, cache);
            let path_len = in_path_len + match opt_path {
                    None => { /*println!("No path found");*/ continue; },
                    Some(vec) => vec.len(),
                };
            let mut self_mod: Self = self.clone();
            let new_pos: Vec<Position> = self.update_pos_to_key(&cur_pos, &poss_key);
            let poss_key_pos: Position = self.keys.get(&poss_key).unwrap().clone();
            self_mod.got_keys.insert(poss_key.clone(), poss_key_pos);
            let mut new_path = path.clone();
            new_path.push(poss_key.clone());
            
            //let best_to_pass = if in_best_prev < best_in_recur { in_best_prev }
            //        else { best_in_recur };
            //let path_best = self_mod.recurs_find(&new_pos, path_len, best_to_pass, cache);
            //println!("Path {:?}", new_path);
            let path_best = self_mod.recurs_find(&new_pos, path_len, best_in_recur, cache, &new_path, cache_2);
            opts.push(path_best);
            if path_best < best_in_recur {
                if in_path_len == 0 {
                    println!("Best so far: {} {}", best_in_recur, path_best);
                }
                println!("Best so far {}: {} {}", self.got_keys.len(), best_in_recur, path_best);
                best_in_recur = path_best;
            }
        }
        let best_val = match opts.iter().min() {
                None => panic!("No min value in recurs_find!"),
                Some(val) => *val,
            };

        {
            let mut keys_set: BTreeSet<char> = BTreeSet::new();
            self.got_keys.keys().for_each(|k| { keys_set.insert(k.clone()); });

            if cache_2.contains_key(cur_pos) {
                let pos_cache = cache_2.get(cur_pos).unwrap().clone();
                for item in pos_cache {
                    let (cache_key_vec, cache_dist_come, _cache_dist_rem) = item.clone();
                    let mut cache_keys: BTreeSet<char> = BTreeSet::new();
                    cache_key_vec.into_iter().for_each(|k| { cache_keys.insert(k); });

                    if keys_set.is_superset(&cache_keys) {
                        if in_path_len <= cache_dist_come {
                            cache_2.get_mut(cur_pos).unwrap().remove(&item);
                        } else {
                            //println!("Unexpected cache find...");
                            //println!("{:?} {} {} - {:?} {}", cache_keys, cache_dist_come, _cache_dist_rem, keys_set, in_path_len);
                        }
                    }
                }
            } else {
                cache_2.insert(cur_pos.clone(), HashSet::new());
            }
            let keys_vec: Vec<char> = keys_set.into_iter().collect();
            cache_2.get_mut(cur_pos).unwrap().insert(
                    (keys_vec, in_path_len, best_val - in_path_len));
        }
        return best_val
    }

    fn find_dist_to(&mut self, cur_pos: &Vec<Position>, dest: &Position,
            cache: &mut HashMap<(Position, Position),
                    Option<(Vec<Position>, HashSet<char>)>>
        ) -> usize
    {
        match self.find_path_to(cur_pos, dest, cache) {
            Some((path, _)) => path.len(),
            None => panic!("Cannot get path to find distance!")
        }
    }

    fn find_path_to(&mut self, in_cur_pos: &Vec<Position>, dest: &Position,
            cache: &mut HashMap<(Position, Position),
                    Option<(Vec<Position>, HashSet<char>)>>
        ) -> Option<(Vec<Position>, HashSet<char>)>
    {
        // Find the cur_pos for the actual dest
        let cur_pos: &Position = self.get_cur_pos_for_dest(in_cur_pos, dest);
        //println!("cur {:?} quad {:?} dest {:?}", in_cur_pos, cur_pos, dest);

        //println!("Finding...");
        let cache_key = (cur_pos.clone(), dest.clone());
        if cache.contains_key(&cache_key) {
            //println!("Cache hit");
            return cache.get(&cache_key).unwrap().clone();
        }
        //println!("Cache miss {:?} {:?}", cur_pos, dest);

        let mut visited: HashMap<Position, usize> = HashMap::new();
        let mut open: HashMap<Position, usize> = HashMap::new();
        open.insert(cur_pos.clone(), 0);
        loop {
            // Get the next node to visit - one w/min dist
            let min_pos: Position;
            {
                let opt_min = open.keys().min_by_key(|k| open.get(k).unwrap().clone());
                min_pos = match opt_min {
                        Some(val) => val.clone(),
                        None => {
                            // Actually I don't think this will happen
                            panic!("Failed to find route {:?} {:?}", cur_pos, dest);
                            //println!("Failed to find route");
                            // TODO cache here if reenabled
                            },
                    };
            }
            let min_dist = open.remove(&min_pos).expect("Removing min dist");

            // Add it to visited list
            visited.insert(min_pos.clone(), min_dist);

            if min_pos == *dest {
                // If we found it, done...
                break;
            }
            
            // Otherwise, add potential next stops to the list
            let next_stops: Vec<Position> = min_pos.get_surroundings().into_iter()
                    .filter(|pos| pos.x < self.maze[0].len().try_into().unwrap()
                            && pos.y < self.maze.len().try_into().unwrap())
                    .filter(|pos| match self.maze[pos.y as usize][pos.x as usize] {
                            SpotType::Wall => false,
                            _ => true,
                        })
                    .filter(|pos| !open.contains_key(pos))
                    .filter(|pos| !visited.contains_key(pos))
                    .collect();

            next_stops.into_iter().for_each(|pos| { open.insert(pos, min_dist + 1); });
        }

        // Calculate the path
        let mut path: Vec<Position> = vec![];
        let mut doors: HashSet<char> = HashSet::new();
        let mut path_cur: Position = dest.clone();
        while path_cur != *cur_pos {
            path.push(path_cur.clone());
            match self.maze[path_cur.y as usize][path_cur.x as usize] {
                SpotType::Door(val) => { doors.insert(val); },
                SpotType::Wall => panic!("Wall in pathfinding!"),
                _ => (),
            };
            path_cur = path_cur.get_surroundings().into_iter()
                .filter(|pos| visited.contains_key(pos))
                .min_by_key(|pos| visited.get(pos).unwrap())
                .expect("Unwrap min in calc path");
        }

        cache.insert(cache_key.clone(), Some((path, doors)));
        return cache.get(&cache_key).unwrap().clone();
    }

    fn find_path_to_no_blocks(&mut self, cur_pos: &Vec<Position>, dest: &Position,
            cache: &mut HashMap<(Position, Position),
                    Option<(Vec<Position>, HashSet<char>)>>
        ) -> Option<Vec<Position>>
    {
        let result = self.find_path_to(cur_pos, dest, cache);
        match result {
            Some((path, blocks)) =>
                    if blocks
                        .iter()
                        .all(|c_r| self.got_keys.contains_key(
                                &c_r.to_lowercase()
                                    .next()
                                    .expect("Lowercase conversion error")
                                    ))
                    {
                        Some(path)
                    } else { None },
            None => None,
        }
    }

    fn part_2_mut(&mut self) {
        let st = self.start[0].clone();
        let (st_x, st_y): (usize, usize) = (st.x as usize, st.y as usize);
        self.start = vec![];
        if let Some(line) = self.maze.get_mut(st_y-1) {
            if let Some(val) = line.get_mut(st_x-1) {
                *val = SpotType::Start;
                self.start.push(Position::new(st_x as i64 - 1, st_y as i64 - 1));
            }
            if let Some(val) = line.get_mut(st_x) {
                *val = SpotType::Wall;
            }
            if let Some(val) = line.get_mut(st_x+1) {
                *val = SpotType::Start;
                self.start.push(Position::new(st_x as i64 + 1, st_y as i64 - 1));
            }
        }
        if let Some(line) = self.maze.get_mut(st_y) {
            if let Some(val) = line.get_mut(st_x-1) {
                *val = SpotType::Wall;
            }
            if let Some(val) = line.get_mut(st_x) {
                *val = SpotType::Wall;
            }
            if let Some(val) = line.get_mut(st_x+1) {
                *val = SpotType::Wall;
            }
        }
        if let Some(line) = self.maze.get_mut(st_y+1) {
            if let Some(val) = line.get_mut(st_x-1) {
                *val = SpotType::Start;
                self.start.push(Position::new(st_x as i64 - 1, st_y as i64 + 1));
            }
            if let Some(val) = line.get_mut(st_x) {
                *val = SpotType::Wall;
            }
            if let Some(val) = line.get_mut(st_x+1) {
                *val = SpotType::Start;
                self.start.push(Position::new(st_x as i64 + 1, st_y as i64 + 1));
            }
        }
    }
}

fn main() {
    let filename = args().nth(1).expect("Supply a filename!");
    let input_lines = read_strs(&filename, ' ');

    let mut cache: HashMap<(Position, Position),
            Option<(Vec<Position>, HashSet<char>)>> = HashMap::new();
    let mut cache_2: HashMap<Vec<Position>, HashSet<(Vec<char>, usize, usize)>> =
            HashMap::new();
    
    let mut maze = MazeInt::new(input_lines.clone());
    println!("Part 1: {}", maze.find_shortest(&mut cache, &mut cache_2));

    let mut cache: HashMap<(Position, Position),
            Option<(Vec<Position>, HashSet<char>)>> = HashMap::new();
    let mut cache_2: HashMap<Vec<Position>, HashSet<(Vec<char>, usize, usize)>> =
            HashMap::new();
    
    let mut maze2 = MazeInt::new(input_lines);
    maze2.part_2_mut();
    println!("Part 2: {}", maze2.find_shortest(&mut cache, &mut cache_2));
}
