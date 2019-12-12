use std::env::args;
use std::cmp::Ordering;

pub mod file_help;
use file_help::read_strs_comma_sep;

type Threespace = Vec<i64>;
//struct Threespace { x: i64, y: i64, z: i64 }
#[derive(Debug,Clone)]
struct Planet {
    pos: Threespace,
    vel: Threespace,
}
impl Planet {
    fn new(vals: Vec<i64>) -> Planet {
        let pos = vec![vals[0], vals[1], vals[2]];
        let vel = vec![0, 0, 0];
        Planet { 
                //pos: Threespace { x: vals[0], y: vals[1], z: vals[2] },
                //vel: Threespace { x: 0, y: 0, z: 0 },
                pos, vel
            }
    }
    fn apply_gravity(&mut self, other: &Planet) {
        for axis in 0..3 {
            self.vel[axis] += match self.pos[axis].cmp(&other.pos[axis]) {
                        Ordering::Less => 1,
                        Ordering::Greater => -1,
                        Ordering::Equal => 0
                    };
        }
    }
    fn apply_velocity(&mut self) {
        for axis in 0..3 {
            self.pos[axis] += self.vel[axis];
        }
    }
    fn calc_energy(&self) -> i64 {
        self.pos.iter().map(|val| val.abs()).sum::<i64>() * self.vel.iter().map(|val| val.abs()).sum::<i64>()
    }
}

fn strip_out_non_num(val: String) -> String {
    let needs_vec = vec!['0','1','2','3','4','5','6','7','8','9','-'];
    val.chars().filter(|char| needs_vec.contains(char)).collect()
}

// let's do the time step again!
fn move_planets(planets: &mut Vec<Planet>) -> () {
    //let planet_pw_inds = combo_ind_without_repl(planets.len(), 2);
    let planet_pw_inds = vec![  
            vec![0,1],
            vec![0,2],
            vec![0,3],
            vec![1,2],
            vec![1,3],
            vec![2,3],
        ];

    // apply gravity to change velocities
    for pl_pair in planet_pw_inds {
        let (pl_1_ind, pl_2_ind) = (pl_pair[0], pl_pair[1]);
        /*
        {
            println!("plpair bef {:?} {:?}", planets[pl_1_ind], planets[pl_2_ind]);
        }
        */
        {
            let pl_2: Planet = planets[pl_2_ind].clone();
            let pl_1: &mut Planet = &mut planets[pl_1_ind];
            pl_1.apply_gravity(&pl_2);
        }
        {
            let pl_1: Planet = planets[pl_1_ind].clone();
            let pl_2: &mut Planet = &mut planets[pl_2_ind];
            pl_2.apply_gravity(&pl_1);
        }
        /*
        {
            println!("plpair aft {:?} {:?}", planets[pl_1_ind], planets[pl_2_ind]);
        }
        */
    }

    // apply velocity to change positions
    for pl in planets {
        pl.apply_velocity();
        //println!("plvel {:?}", pl);
    }
}

fn main() {
    let filename: String = args().skip(1).take(1).collect();
    let timesteps: usize = args().skip(2).take(1).map(|val| val.parse().unwrap()).collect::<Vec<usize>>()[0];
    let input_strs: Vec<Vec<String>> = read_strs_comma_sep(&filename);
    let input_vals: Vec<Vec<i64>> = input_strs
            .into_iter()
            .map(|line|
                    line
                        .into_iter()
                        .map(|val| strip_out_non_num(val)
                                .parse::<i64>()
                                .unwrap()
                            )
                        .collect()
                )
            .collect();
    let mut input_planets: Vec<Planet> = input_vals
            .into_iter()
            .map(|plntvec| Planet::new(plntvec))
            .collect();

    for pl in input_planets.iter() {
        println!("In pl {:?}", pl);
    }
    for i in 0..timesteps {
        move_planets(&mut input_planets);
        for pl in input_planets.iter() {
            println!("{} pl {:?}", i, pl);
        }
        print!("{} Pos0 ", i);
        for pl in input_planets.iter() {
            print!("{} ", pl.pos[0]);
        }
        print!(" Pos1 ");
        for pl in input_planets.iter() {
            print!("{} ", pl.pos[1]);
        }
        print!(" Pos2 ");
        for pl in input_planets.iter() {
            print!("{} ", pl.pos[2]);
        }
        println!();
        print!("{} Vel0 ", i);
        for pl in input_planets.iter() {
            print!("{} ", pl.vel[0]);
        }
        print!(" Vel1 ");
        for pl in input_planets.iter() {
            print!("{} ", pl.vel[1]);
        }
        print!(" Vel2 ");
        for pl in input_planets.iter() {
            print!("{} ", pl.vel[2]);
        }
        println!();
    }
    for pl in input_planets.iter() {
        println!("Now pl {:?}", pl);
    }
    println!("Part 1: {}", input_planets.iter().map(|pl| pl.calc_energy()).sum::<i64>());
}
