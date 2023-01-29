mod search;

use std::collections::HashSet;

use search::BreadthSearch;

type PosInd = i32;

#[derive(Debug, PartialEq, Eq, Hash)]
struct Position {
    row: PosInd,
    col: PosInd,
    steps: PosInd,
}

#[derive(Debug)]
struct BlizardState {
    start: (PosInd, PosInd),
    end: (PosInd, PosInd),
    right: HashSet<(PosInd, PosInd)>,
    left: HashSet<(PosInd, PosInd)>,
    up: HashSet<(PosInd, PosInd)>,
    down: HashSet<(PosInd, PosInd)>,
    valid: HashSet<(PosInd, PosInd)>,
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Row {} Col {} Steps {}", self.row, self.col, self.steps)
    }
}
impl std::fmt::Display for BlizardState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Start ({}, {}) End ({}, {})\n", 
            self.start.0, self.start.1,
            self.end.0, self.end.1,
        )?;
        write!(f, "Storm Counts: Right {} Left {} Up {} Down {}\n",
            self.right.len(), self.left.len(), self.up.len(), self.down.len(),
        )?;
        write!(f, "Valid count: {}", self.valid.len())
    }
}

#[derive(Debug)]
enum StormDir {
    Right,
    Left,
    Up,
    Down,
}

fn mod_w_neg(val: i32, mod_val: i32) -> i32 {
    // This is dumb but fine for this usecase
    let mut not_neg = val;
    while not_neg < 0 { not_neg += mod_val; }
    not_neg % mod_val
}

impl BlizardState {
    fn new(data: &str) -> Self {
        let pos_chr: Vec<_> = data
            .lines()
            .map(|line| line.trim())
            .enumerate()
            .map(|(row_no, line)| {
                line.chars()
                    .enumerate()
                    .map(move |(col_no, chr)| {
                        (
                            i32::try_from(row_no).expect("Enum row conv fail"),
                            i32::try_from(col_no).expect("Enum col conv fail"),
                            chr
                        )
                    })
            })
            .flatten()
            .collect();

        let line_cnt: i32 = data.lines().count().try_into().expect("Line count conv fail");
        let line_len: i32 = data.lines().next().expect("Failed to grab one line").len()
                    .try_into().expect("Line len conv fail");

        Self { // Row, Col - y, x
            start: (0, 1),
            end: (line_cnt-1, line_len - 2),
            right: Self::filter_map(&pos_chr, HashSet::from_iter(vec!['>'])),
            left: Self::filter_map(&pos_chr, HashSet::from_iter(vec!['<'])),
            up: Self::filter_map(&pos_chr, HashSet::from_iter(vec!['^'])),
            down: Self::filter_map(&pos_chr, HashSet::from_iter(vec!['v'])),
            valid: Self::filter_map(&pos_chr, HashSet::from_iter(vec!['<','>','^','v','.'])),
        }
    }

    fn filter_map(inmap: &Vec<(PosInd, PosInd, char)>, chr: HashSet<char>)
        -> HashSet<(PosInd, PosInd)>
    {
        HashSet::from_iter(
            inmap.iter()
                .filter(|(_, _, c)| chr.contains(c))
                .map(|(r, c, _)| (r.clone(), c.clone()))
        )
    }

    fn is_storm_intersect(&self, dir: StormDir, pos: &Position) -> bool {
        // Start and end positions have no storms
        if (pos.row == self.start.0 && pos.col == self.start.1) ||
            (pos.row == self.end.0 && pos.col == self.end.1) {
            return false;
        }

        // Grab the right storm lookup for our direction
        let storm_locs = match dir {
            StormDir::Left => &self.left,
            StormDir::Right => &self.right,
            StormDir::Up => &self.up,
            StormDir::Down => &self.down,
        };

        let storm_area_width = (self.end.1 - self.start.1) + 1;
        let storm_area_height = (self.end.0 - self.start.0) - 1;

        // Shifting a storm down by X steps is equivalent to shifting a current position
        // up by X steps.  So we can shift the current position and see if it lines up
        // with the original positions of any storms.
        let shifted_pos_row = match dir {
            StormDir::Down => 1 + mod_w_neg( (pos.row - 1) - pos.steps, storm_area_height ),
            StormDir::Up => 1 + mod_w_neg( (pos.row - 1) + pos.steps, storm_area_height ),
            StormDir::Left => pos.row,
            StormDir::Right => pos.row,
        };
        let shifted_pos_col = match dir {
            StormDir::Right => 1 + mod_w_neg( (pos.col - 1) - pos.steps, storm_area_width ),
            StormDir::Left => 1 + mod_w_neg( (pos.col - 1) + pos.steps, storm_area_width ),
            StormDir::Up => pos.col,
            StormDir::Down => pos.col,
        };

        let shifted_pos = (shifted_pos_row, shifted_pos_col);
        //println!("dir {:?} Pos {} Shift {},{}", dir, pos, shifted_pos_row, shifted_pos_col);

        // Return true if the directional storm lookup has our shifted position
        storm_locs.contains(&shifted_pos)
    }
}

impl BreadthSearch<Position> for BlizardState {
    fn is_complete<'a, B: 'a> (&self, collec: B) -> bool
        where B: IntoIterator<Item=&'a Position>, Position: 'a,
    {
        collec.into_iter().any(|pos| pos.row == self.end.0 && pos.col == self.end.1)
    }

    fn gen_children(&self, item: Position) -> Box<dyn Iterator<Item=Position>> {
        //println!("Pos: {}", item);

        let new_pos = vec![
            Position { row: item.row, col: item.col, steps: item.steps + 1 },
            Position { row: item.row - 1, col: item.col, steps: item.steps + 1 },
            Position { row: item.row + 1, col: item.col, steps: item.steps + 1 },
            Position { row: item.row, col: item.col - 1, steps: item.steps + 1 },
            Position { row: item.row, col: item.col + 1, steps: item.steps + 1 },
        ];

        let valid = new_pos.into_iter().filter(|pos| self.valid.contains(&(pos.row, pos.col)));
        let no_storm = valid
            .filter(|pos| !self.is_storm_intersect(StormDir::Right, pos))
            .filter(|pos| !self.is_storm_intersect(StormDir::Left, pos))
            .filter(|pos| !self.is_storm_intersect(StormDir::Up, pos))
            .filter(|pos| !self.is_storm_intersect(StormDir::Down, pos));

        Box::new(no_storm.collect::<Vec<_>>().into_iter())
    }

    fn find_result(&self, iter: &mut impl Iterator<Item=Position>)
        -> Result<Position, Box<dyn std::error::Error>>
    {
        iter
            .filter(|pos| pos.row == self.end.0 && pos.col == self.end.1)
            .min_by(|pos1, pos2| pos1.steps.cmp(&pos2.steps))
            .ok_or(Box::new(search::SearchError::NoResult))
    }
}

fn do_it(filename: &str) -> Result<(), Box<dyn std::error::Error>> {
    let data = std::fs::read_to_string(filename)?;
    let bliz_state = BlizardState::new(&data);

    println!("Bliz: \n{}", bliz_state);

    let init = vec![Position { row: bliz_state.start.0, col: bliz_state.start.1, steps: 0 }];

    let result = bliz_state.breadth(init)?;

    println!("Part 1: {}", result.steps);

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    do_it("samp")?;
    do_it("input.txt")?;
    Ok(())
}
