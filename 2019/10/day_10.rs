use std::env::args;
use std::cmp::Ordering;
use std::collections::HashMap;

pub mod file_help;

use file_help::read_strs;

#[derive(Debug, Clone)]
struct Point { x: i32, y: i32, }

#[derive(Debug, Clone)]
struct ViewOfAsteroid {
    source: Point,
    dest: Point,
    slope_x: i32,
    slope_y: i32,
    manh_dist: i32,
}

impl ViewOfAsteroid {
    fn new(source: Point, dest: Point) -> ViewOfAsteroid {
        let st_slope_x = dest.x - source.x;
        let st_slope_y = dest.y - source.y;
        let manh_dist = st_slope_x.abs() + st_slope_y.abs();
        let (slope_x, slope_y) = match (st_slope_x, st_slope_y) {
                (0, 0) => panic!("Source and dest were same!"),
                (_, 0) => (1 * st_slope_x.signum(), 0),
                (0, _) => (0, 1 * st_slope_y.signum()),
                (_, _) => {
                        let gcd = find_gcd(&st_slope_x, &st_slope_y);
                        (st_slope_x / gcd, st_slope_y / gcd)
                    },
            };
        ViewOfAsteroid { source, dest, slope_x, slope_y, manh_dist }
    }
}

fn find_gcd(in_uno: &i32, in_dos: &i32) -> i32 {
    if *in_uno == 0 || *in_dos == 0 {
        panic!("Cannot find gcd with a 0 value!");
    }
    let uno = in_uno.abs();
    let dos = in_dos.abs();
    (1..(uno+1))
            .filter(|val| uno % val == 0 )
            .filter(|val| dos % val == 0).max().unwrap()
}

fn repr_map(in_lines: Vec<Vec<String>>) -> Vec<Point> {
    in_lines.iter().enumerate().flat_map( |(row, line)| {
            let y = row.clone() as i32;
            line[0].chars().enumerate().filter_map(move |(col, char)| {
                    let x = col.clone() as i32;
                    match char {
                        '#' => Some(Point { x, y, }),
                        _ => None
                    }
                })
        }).collect()
}

fn build_view_from_asteroid(src_point: &Point, map: &Vec<Point>)
        -> Vec<ViewOfAsteroid>
{
    let views: Vec<ViewOfAsteroid> = map.iter().filter_map( |dst_point|
            if src_point.x == dst_point.x && src_point.y == dst_point.y {
                None
            } else {
                Some(ViewOfAsteroid::new(src_point.clone(), dst_point.clone()))
            }
        ).collect();
    //println!("{:?}", views);

    let mut hm_angles = HashMap::<(i32, i32), ViewOfAsteroid>::new();
    for view in views {
        let key: (i32, i32) = (view.slope_x, view.slope_y);
        if hm_angles.contains_key(&key) {
            let cur: &ViewOfAsteroid = hm_angles.get(&key).unwrap();
            if view.manh_dist < cur.manh_dist {
                hm_angles.insert(key, view);
            }
        } else {
            hm_angles.insert(key, view);
        }
    }

    let (_, out_vals): (Vec<(i32, i32)>, Vec<ViewOfAsteroid>) = 
            hm_angles.drain().unzip();
    //println!("outvals {} {:?}", out_vals.len(), out_vals);
    return out_vals;
}

// Return the (point, # seen)
fn build_all_views(map: &Vec<Point>) -> Vec<(&Point, Vec<ViewOfAsteroid>)> {
    map.iter().map( |point| {
            let view = build_view_from_asteroid(&point, &map);
            (point, view)
        }).collect()
}

fn get_quadrant(view: &ViewOfAsteroid) -> usize {
    if view.slope_x >= 0 && view.slope_y < 0 {
        return 0;
    } else if view.slope_x > 0 && view.slope_y >= 0 {
        return 1;
    } else if view.slope_x <= 0 && view.slope_y > 0 {
        return 2;
    } else if view.slope_x < 0 && view.slope_y <= 0 {
        return 3;
    } else {
        panic!("Invalid quadrant get.");
    }
}

fn view_compare(uno: &ViewOfAsteroid, dos: &ViewOfAsteroid) -> Ordering {
    /* Ordering needed - x and y are the slopes...
    x = 0 and greater   y = less than 0 - smaller abs(x / y) is earlier     quad 0
    x = greater than 0  y = 0 and greater - smaller abs(y / x) is earlier   quad 1
    x = 0 and less      y = greater than 0 - smaller abs(x/y) is earlier    quad 2
    x = less than 0     y = 0 and less - smaller abs(y / x) is earlier      quad 3
    */
    let uno_quadrant = get_quadrant(uno);
    let dos_quadrant = get_quadrant(dos);
    let result = uno_quadrant.cmp(&dos_quadrant);
    if result != Ordering::Equal {
        return result;
    }

    // Quadrants are the same...
    let (uno_slope, dos_slope) = 
        if uno_quadrant == 0 || uno_quadrant == 2 {
            ((uno.slope_x.abs() as f32) / (uno.slope_y.abs() as f32),
                    (dos.slope_x.abs() as f32) / (dos.slope_y.abs() as f32))
        } else {
            ((uno.slope_y.abs() as f32) / (uno.slope_x.abs() as f32),
                    (dos.slope_y.abs() as f32) / (dos.slope_x.abs() as f32))
        };
    return if uno_slope < dos_slope { Ordering::Less }
            else { Ordering::Greater };
}

fn shoot_order_views(views: &Vec<ViewOfAsteroid>) -> Vec<ViewOfAsteroid> {
    let mut to_sort: Vec<ViewOfAsteroid> = views.iter().cloned().collect();
    to_sort.sort_unstable_by(view_compare);
    return to_sort;
}

fn main() {
    let filename: String = args().skip(1).take(1).collect();
    let lines = read_strs(&filename, ' ');
    let map = repr_map(lines);

    //println!("{:?}", map);

    let views = build_all_views(&map);
    let (best_pt, best_view): &(&Point, Vec<ViewOfAsteroid>) =
            views.iter()
                .max_by(
                    |(_, uno_views), (_, dos_views)| {
                            let uno_count = uno_views.len();
                            let dos_count = dos_views.len();
                            uno_count.cmp(&dos_count)
                        }
                    )
                .unwrap();
    
    println!("Part 1: {} {:?}", best_view.len(), best_pt);

    let shoot_order = shoot_order_views(best_view);
    shoot_order.iter()
            .enumerate()
            .for_each(|(ind, view)| println!("Shoot {} {:?}", ind+1, view));
}
