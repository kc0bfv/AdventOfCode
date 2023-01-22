use std::collections::HashSet;
use std::marker::Copy;

struct Map<T> {
    data: Vec<T>,
    row_len: usize,
    col_len: usize,
}


impl<T: Copy> Map<T> {
    fn new(rows: &Vec<Vec<T>>) -> Self {
        // Validate that all rows are same len...
        let row_len = rows
            .iter()
            .fold(Some(None), |prev, line| match prev {
                None => None,
                Some(None) => Some(Some(line.len())),
                Some(Some(prev_len)) => {
                    if prev_len == line.len() {
                        Some(Some(prev_len))
                    } else {
                        None
                    }
                }
            })
            .expect("Rows are not same length")
            .expect("Row count never initialized");

        let col_len = rows.len();

        let data = rows.iter().fold(Vec::new(), |mut accum, line| {
            accum.extend_from_slice(line);
            accum
        });
        Self {
            data,
            row_len,
            col_len,
        }
    }

    fn iter_rows(self: &Self) -> MapIter<T> {
        MapIter::new(self, 1, self.row_len, self.row_len, self.col_len)
    }

    fn iter_cols(self: &Self) -> MapIter<T> {
        MapIter::new(self, self.row_len, 1, self.col_len, self.row_len)
    }
}

struct MapIter<'a, T> {
    map: &'a Map<T>,
    in_line_step: usize,
    btwn_line_step: usize,
    line_len: usize,
    num_lines: usize,
    cur_pos: usize,
}

impl<'a, T> MapIter<'a, T> {
    fn new(
        map: &'a Map<T>,
        in_line_step: usize,
        btwn_line_step: usize,
        line_len: usize,
        num_lines: usize,
    ) -> Self {
        let cur_pos = 0;
        Self {
            map,
            in_line_step,
            btwn_line_step,
            line_len,
            num_lines,
            cur_pos,
        }
    }
}

impl<'a, T> Iterator for MapIter<'a, T> {
    type Item = LineIter<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur_pos >= self.btwn_line_step * self.num_lines {
            None
        } else {
            let ret = LineIter::new(self.map, self.cur_pos, self.in_line_step, self.line_len);
            self.cur_pos += self.btwn_line_step;
            Some(ret)
        }
    }
}

struct LineIter<'a, T> {
    map: &'a Map<T>,
    start: usize,
    step_size: usize,
    line_len: usize,
    cur_step: usize,
    cur_step_back: usize,
}

impl<'a, T> LineIter<'a, T> {
    fn new(map: &'a Map<T>, start: usize, step_size: usize, line_len: usize) -> Self {
        let cur_step = 0;
        let cur_step_back = 0;
        Self {
            map,
            start,
            step_size,
            line_len,
            cur_step,
            cur_step_back,
        }
    }
}

impl<'a, T: Copy> Iterator for LineIter<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur_step + self.cur_step_back >= self.line_len {
            None
        } else {
            let ret = self
                .map
                .data
                .get(self.start + (self.cur_step * self.step_size))
                .expect("Out of bounds unexpectedly");
            self.cur_step += 1;
            Some(*ret)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remain_len = self.line_len - (self.cur_step + self.cur_step_back);
        (remain_len, Some(remain_len))
    }
}

impl<'a, T: Copy> DoubleEndedIterator for LineIter<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.cur_step + self.cur_step_back >= self.line_len {
            None
        } else {
            let calc_step = self.line_len - 1 - self.cur_step_back;
            let ret = self
                .map
                .data
                .get(self.start + (calc_step * self.step_size))
                .expect("Out of bounds unexpectedly");
            self.cur_step_back += 1;
            Some(*ret)
        }
    }
}

impl<'a, T: Copy> ExactSizeIterator for LineIter<'a, T> {}

fn run_p1_direction(do_rows: bool, do_rev: bool, map: &Map<u32>) -> HashSet<(usize, usize)> {
    let mut visible: HashSet<(usize, usize)> = HashSet::new();
    let map_iter: Box<dyn Iterator<Item = _>> = if do_rows {
        Box::new(map.iter_rows().enumerate())
    } else {
        Box::new(map.iter_cols().enumerate())
    };
    visible = map_iter.fold(visible, |mut set, (uno_ind, row)| {
        let mut row_max = None;
        let to_fold: Box<dyn Iterator<Item = _>> = if do_rev {
            Box::new(row.enumerate().rev())
        } else {
            Box::new(row.enumerate())
        };
        set = to_fold.fold(set, |mut innerset, (dos_ind, val)| {
            if row_max.map_or(true, |m| val > m) {
                row_max = Some(val);
                innerset.insert(if do_rows {
                    (uno_ind, dos_ind)
                } else {
                    (dos_ind, uno_ind)
                });
            }
            innerset
        });
        set
    });
    visible
}

fn runit(filename: &str) -> Result<(), Box<dyn std::error::Error>> {
    let raw_content = std::fs::read_to_string(filename)?;
    let parsed_content: Vec<Vec<u32>> = raw_content
        .lines()
        .map(|line| {
            line.trim()
                .chars()
                .map(|c| c.to_digit(10).expect("Was not digit..."))
                .collect()
        })
        .collect();
    let map = Map::new(&parsed_content);

    let mut visible = run_p1_direction(true, false, &map);
    visible.extend(run_p1_direction(true, true, &map));
    visible.extend(run_p1_direction(false, false, &map));
    visible.extend(run_p1_direction(false, true, &map));

    println!("Part 1: {}", visible.len());

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    runit("samp")?;
    runit("input.txt")?;

    Ok(())
}

#[cfg(test)]
mod map_tests {
    use crate::Map;

    fn test_dat_setup() -> Map<u32> {
        let test_dat: Vec<Vec<u32>> = vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9]];
        Map::new(&test_dat)
    }

    #[test]
    fn double_ended_exhaustion() {
        let map = test_dat_setup();

        let mut iter = map.iter_rows();
        let mut row1 = iter.next().unwrap();
        let mut row2 = iter.next().unwrap();
        let mut row3 = iter.next().unwrap();
        assert!(iter.next().is_none());

        assert_eq!(1, row1.next().unwrap());
        assert_eq!(3, row1.next_back().unwrap());
        assert_eq!(2, row1.next_back().unwrap());
        assert!(row1.next().is_none());
        assert!(row1.next_back().is_none());

        assert_eq!(6, row2.next_back().unwrap());
        assert_eq!(4, row2.next().unwrap());
        assert_eq!(5, row2.next().unwrap());
        assert!(row2.next_back().is_none());
        assert!(row2.next().is_none());

        assert_eq!(7, row3.next().unwrap());
        assert_eq!(8, row3.next().unwrap());
        assert_eq!(9, row3.next().unwrap());
        assert!(row3.next_back().is_none());
        assert!(row3.next().is_none());
    }

    #[test]
    fn map_size() {
        let map = test_dat_setup();

        let mut iter = map.iter_cols();
        let mut col1 = iter.next().unwrap();

        for i in (1..4).rev() {
            assert_eq!((i, Some(i)), col1.size_hint());
            assert_eq!(i, col1.len());

            col1.next().unwrap();
        }

        for _ in 0..2 {
            assert_eq!((0, Some(0)), col1.size_hint());
            assert_eq!(0, col1.len());
            col1.next();
        }
    }
}
