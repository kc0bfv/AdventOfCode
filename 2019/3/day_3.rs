use std::env::args;
use std::cmp::Ordering;

pub mod file_help;
use file_help::read_strs_comma_sep;

#[derive(Debug, Eq, Clone, Copy)]
struct LinePoint {
    x: i64,
    y: i64,
    dist: i64,
    walk: i64,
}

impl LinePoint {
    fn new(x: i64, y: i64, walk: i64) -> LinePoint {
        LinePoint { x: x, y: y, dist: i64::abs(x) + i64::abs(y), walk: walk }
    }
    fn from_inbetween(prev: &LinePoint, dir_dist: &DirDist) -> Vec<LinePoint> {
        // TODO: don't love this...
        let (add_x, add_y, dist) = match dir_dist {
            DirDist::DirUp(dist) => ( 0,  1, dist),
            DirDist::DirDn(dist) => ( 0, -1, dist),
            DirDist::DirLf(dist) => (-1,  0, dist),
            DirDist::DirRt(dist) => ( 1,  0, dist),
        };
        let mut accum = *prev;
        (0..*dist).map(|_| {
                let new_pt = LinePoint::new(accum.x + add_x, accum.y + add_y,
                        accum.walk + 1);
                accum = new_pt.clone();
                new_pt
            }).collect()
    }
}

impl Ord for LinePoint {
    fn cmp(&self, other: &Self) -> Ordering {
        let dist_cmp = self.dist.cmp(&other.dist);
        let x_cmp = self.x.cmp(&other.x);
        let y_cmp = self.y.cmp(&other.y);
        if dist_cmp != Ordering::Equal {
            return dist_cmp;
        } else if x_cmp != Ordering::Equal {
            return x_cmp;
        } else {
            return y_cmp;
        }
    }
}

impl PartialOrd for LinePoint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for LinePoint {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

#[derive(Debug, Clone, Copy)]
enum DirDist {
    DirUp(i64),
    DirDn(i64),
    DirLf(i64),
    DirRt(i64),
}

impl DirDist {
    fn new(item: &String) -> DirDist {
        let dist = item[1..].parse().unwrap();
        match item.chars().next() {
            Some('U') => Self::DirUp(dist),
            Some('D') => Self::DirDn(dist),
            Some('L') => Self::DirLf(dist),
            Some('R') => Self::DirRt(dist),
            _   => panic!("Invalid direction! {}", item),
        }
    }
}

fn build_line(input_val: &Vec<String>) -> Vec<LinePoint> {
    let directions = input_val.iter().map(|item| DirDist::new(item));
    //println!("dirdist {:?}", directions);
    
    let mut prev_pt = LinePoint::new(0, 0, 0);
    let mut all_pts = Vec::<LinePoint>::new();

    for item in directions {
        let mut new_pts = LinePoint::from_inbetween(&prev_pt, &item);
        all_pts.append(&mut new_pts);
        prev_pt = *all_pts.last().unwrap();
    }
    return all_pts;
}

fn sort_line(input: &Vec<LinePoint>) -> Vec<LinePoint> {
    let mut to_sort = input.to_vec();
    to_sort.sort();
    to_sort
}

fn intersection_with_walk(line_1: &Vec<LinePoint>, line_2: &Vec<LinePoint>) 
        -> Vec<LinePoint>
{
    let mut line_1_iter = line_1.iter();
    let mut line_2_iter = line_2.iter();
    let mut line_1_pos = line_1_iter.next().unwrap();
    let mut line_2_pos = line_2_iter.next().unwrap();
    let mut intersected = Vec::<LinePoint>::new();
    loop {
        let mut adv_1 = false;
        let mut adv_2 = false;
        if line_1_pos == line_2_pos {
            let mut pt_copy = line_1_pos.clone();
            pt_copy.walk += line_2_pos.walk;
            intersected.push(pt_copy);
            adv_1 = true;
            adv_2 = true;
        } else if line_1_pos < line_2_pos {
            adv_1 = true;
        } else {
            adv_2 = true;
        }
        if adv_1 {
            line_1_pos = match line_1_iter.next() {
                Some(val) => val,
                None => break,
            };
        }
        if adv_2 {
            line_2_pos = match line_2_iter.next() {
                Some(val) => val,
                None => break,
            };
        }
    };
    return intersected;
}

fn walk_sort(pt_1: &LinePoint, pt_2: &LinePoint) -> Ordering {
    if pt_1.walk == pt_2.walk {
        return pt_1.cmp(pt_2);
    } else {
        return pt_1.walk.cmp(&pt_2.walk);
    }
}

fn main() {
    let filename: String = args().skip(1).take(1).collect();
    let input_vals: Vec<Vec<String>> = read_strs_comma_sep(&filename);

    //input_vals[0].iter().map(|val| println!("Value: {:?}", val)).count();
    //input_vals[1].iter().map(|val| println!("Value: {:?}", val)).count();

    let line_1_unsort = build_line(&input_vals[0]);
    //println!("line1 {:?}", line_1_unsort);
    let line_2_unsort = build_line(&input_vals[1]);
    //println!("line2 {:?}", line_2_unsort);

    let line_1 = sort_line(&line_1_unsort);
    //println!("line1 sorted {:?}", line_1);
    let line_2 = sort_line(&line_2_unsort);
    //println!("line2 sorted {:?}", line_2);

    let mut intersect = intersection_with_walk(&line_1, &line_2);

    println!("First intersection: {:?}", intersect[0]);

    intersect.sort_by(walk_sort);
    println!("Shortest walk: {:?}", intersect[0]);
}
