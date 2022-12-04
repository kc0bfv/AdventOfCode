use std::error::Error;
use std::fmt;

pub mod file_help;

use file_help::{read_lines, get_cmd_arg};

#[derive(Debug, Clone)]
struct WasNone;

impl Error for WasNone {}

impl fmt::Display for WasNone {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Was None")
    }
}

struct Elf {
    st: u32,
    ed: u32,
}

impl Elf {
    fn new(instr: &str) -> Result<Elf, Box<dyn Error>> {
        let split: Vec<u32> = instr
            .split("-")
            .map(|item| { item.parse::<u32>() })
            .collect::<Result<_,_>>()?;
        let mut iter = split.into_iter();
        let st = iter.next().ok_or(WasNone)?;
        let ed = iter.next().ok_or(WasNone)?;

        Ok(Elf{ st, ed })
    }

    fn full_overlap(&self, other: &Elf) -> bool {
        self.st <= other.st && self.ed >= other.ed
    }

    fn no_overlap(&self, other: &Elf) -> bool {
        self.ed < other.st || self.st > other.ed
    }
}

struct Pair<A> {
    p0: A,
    p1: A,
}

impl<A> Pair<A> {
    fn new(mut vector: Vec<A>) -> Result<Pair<A>, Box<dyn Error>> {
        let mut it = vector.drain(..);
        let p0 = it.next().ok_or(WasNone)?;
        let p1 = it.next().ok_or(WasNone)?;
        Ok(Pair { p0, p1 })
    }
}

impl Pair<Elf> {
    fn full_overlap(&self) -> bool {
        self.p0.full_overlap(& self.p1) || self.p1.full_overlap(& self.p0)
    }
    fn part_overlap(&self) -> bool {
        ! self.p0.no_overlap(& self.p1)
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let lines = read_lines(&get_cmd_arg())?;
    let elf_pairs: Vec<Pair<Elf>> = lines
        .into_iter()
        .map(|line| {
            line
                .split(",")
                .map(|instr| { Elf::new(instr) })
                .collect::<Result<_,_>>()
                .and_then( Pair::new )
        })
        .collect::<Result<_,_>>()?;

    let full_overlaps = (&elf_pairs)
        .into_iter()
        .map( Pair::full_overlap )
        .fold( 0, |accum, val| { if val { accum + 1 } else { accum } } );

    let part_overlaps = (&elf_pairs)
        .into_iter()
        .map( Pair::part_overlap )
        .fold( 0, |accum, val| { if val { accum + 1 } else { accum } } );

    println!("Part 1: {full_overlaps}");
    println!("Part 2: {part_overlaps}");

    return Ok(());
}
