use std::env::args;
use std::convert::TryFrom;
use std::collections::HashMap;

pub mod file_help;

use file_help::read_strs;

#[derive(Debug,Copy,Clone)]
enum Shuffle {
    IntoNew,
    Cut(i64),
    DealInc(i64),
}

fn shuffle_parse(line: &str) -> Shuffle {
    let fifth = match line.chars().nth(5) { Some(val) => val, None => ' ', };
    match line[..3].as_ref() {
        "cut" => {
                let ind_str: String = line.chars().skip(4).collect();
                let ind: i64 = ind_str.parse().expect("Parsing cut ind.");
                Shuffle::Cut(ind)
            },
        "dea" if fifth == 'w' => {
                let ind_str: String = line.chars().skip(20).collect();
                let ind: i64 = ind_str.parse().expect("Parsing deal inc ind.");
                Shuffle::DealInc(ind)
            },
        "dea" if fifth == 'i' => Shuffle::IntoNew,
        _ => panic!("Unparsable line {}", line),
    }
}

/* Tools for summarizing a shuffle in a minimized way */
fn summarize(shufs: &Vec<Shuffle>, deck_size: i64) -> (i64, bool, i64) {
    let mut fw_bk = true;
    let mut rot = 0;
    let mut inc_deal = 1;
    shufs.iter().for_each(|shuf|
            match shuf {
                Shuffle::IntoNew => { fw_bk = !fw_bk;
                        rot = (deck_size + (-rot)) % deck_size;
                        rot += 1;
                    },
                Shuffle::Cut(val) => {
                        //let in_val = if fw_bk { *val } else { -(*val) };
                        let in_val = *val;
                        rot = (deck_size + (rot + in_val)) % deck_size;
                    },
                Shuffle::DealInc(val) => {
                        rot = (rot * val) % deck_size;
                        inc_deal = (inc_deal * val) % deck_size;
                    },
            }
        );
    (inc_deal, fw_bk, if fw_bk { rot } else { rot - 1 })
}

fn calc_rot(count: i64, in_deck: i64, rot: i64, inc_deal: i64) -> i64 {
    let mut reps = count as i128;
    let mut result = 0 as i128;
    let mut parts = (-inc_deal as i128, (in_deck + 1 + rot) as i128);
    let deck = in_deck as i128;

    while reps > 0 {
        //println!("Reps {} result {}", reps, result);
        if reps % 2 == 1 {
            result = (parts.0 * result + parts.1) % deck;
        }
        parts = ((parts.0 * parts.0) % deck, (parts.0 * parts.1 + parts.1) % deck);
        reps >>= 1;
    }
    return ((result - (if count % 2 == 1 { 1 } else { 0 })) % deck) as i64;
}

// Repeat a summary `count` times, and summarize that repeated shuffle
fn mult_summary(count: i64, (inc_deal, fw_bk, rot): (i64, bool, i64),
        deck_size: i64) -> (i64, bool, i64)
{
    let out_inc_deal = mod_exp(inc_deal, count, deck_size);
    
    let rot = calc_rot(count, deck_size, rot, inc_deal);

    let out_back = if count % 2 == 1 { fw_bk } else { true };

    /*
    d1r + (-inc_deal) * val

    Examining composition
    d1r - inc_deal * val
    (-inc_deal) * val + d1r
    a = -inc_deal   b = d1r

    d1r + (-inc_deal) * (d1r + (-inc_deal) * val)
    c(ax + b) + d
    cax + cb + d

    (d1r + (-inc_deal)*dlr) + (-inc_deal * -inc_deal) * val
    */

    (out_inc_deal, out_back, rot)
}
/* End - tools for summarizing a shuffle in a minimized way */

/* Utilities */
fn mod_exp(in_base: i64, in_exp: i64, in_modval: i64) -> i64 {
    if in_modval == 1 {
        return 0;
    }
    let modval: i128 = in_modval as i128;
    let test: i128 = (modval - 1) * (modval - 1);
    assert!(test > 0);

    let mut base: i128 = (in_base as i128) % (modval as i128);
    let mut result: i128 = 1;
    let mut exp = in_exp;
    while exp > 0 {
        if exp % 2 == 1 {
            result = (result * base) % modval;
        }
        exp >>= 1;
        base = (base * base) % modval;
    }
    return i64::try_from(result).expect("Overflow in mod_exp result");
}

fn mod_inverse(a: i64, modval: i64) -> i64 {
    // If deck_size is prime, then a ^ (modval - 2) yields the x such that
    // a * x = 1  % modval
    // TODO REALLY should make sure deck_size is prime, here...
    return mod_exp(a, modval - 2, modval);
}
/* End - utilities */

/* Work backwards through a reversed shuffle - for a card at pos in the output,
   return where it was in the input */
fn find_from(pos: i64, shuf: &Shuffle, deck_size: i64,
        cache: &mut HashMap<(usize, usize), Vec<i64>>)
    -> i64
{
    match shuf {
        Shuffle::IntoNew => ff_into_new(pos, deck_size),
        Shuffle::Cut(val) => ff_cut(pos, deck_size, *val),
        Shuffle::DealInc(val) => ff_deal_inc(pos, deck_size, *val, cache),
    }
}

fn ff_into_new(pos: i64, deck_size: i64) -> i64 {
    let retval = deck_size - 1 - pos;
    if retval < 0 { panic!("Invalid in ff_into_new"); }
    retval
}

fn ff_cut(pos: i64, deck_size: i64, val: i64) -> i64 {
    let retval = (deck_size + (pos + val)) % deck_size;
    if retval < 0 { panic!("Invalid in ff_cut {} {} {} {}", pos, val, deck_size + (pos + val), retval); }
    retval
}

fn ff_deal_inc(pos: i64, deck_size: i64, val: i64,
        _cache: &mut HashMap<(usize, usize), Vec<i64>>)
    -> i64
{
    // Needs to return the location the card at pos came from before a deal inc

    // Originally, output position was - val * in = out  % deck_size
    // Inverse format is               - a   * x  = 1    % mod
    // With a as val, mod inv yields x, or the input, which is what we need
    // But only the input that produces output of 1
    // We can then multiply that by the output (pos, here), in mod deck_size
    // to get the input that we really want.
    let mod_inv = mod_inverse(val, deck_size);
    return (((mod_inv as i128) * (pos as i128)) % (deck_size as i128)) as i64;

    /*
    // This is my old hack
    // This method worked well when deck_size was only like, 1 mil
    let reord: Vec<i64> = reorder_wraps_short(val, deck_size, _cache);
    let pos_in_diff_cycle: i64 = (pos % deck_size) % val;
    let mult = reord[pos_in_diff_cycle as usize];
    return (pos + (deck_size * mult)) / val;
    */
}
/* End work backwards through a reversed shuffle */


/* Straightforward shuffle */
fn do_shuffle(deck: Vec<usize>, shuf: &Shuffle) -> Vec<usize> {
    //println!("Now: {:?}", deck);
    match shuf {
        Shuffle::IntoNew => do_into_new(deck),
        Shuffle::Cut(val) => do_cut(deck, *val),
        Shuffle::DealInc(val) => do_deal_inc(deck, *val),
    }
}

fn do_into_new(deck: Vec<usize>) -> Vec<usize> {
    deck.into_iter().rev().collect()
}

fn do_cut(deck: Vec<usize>, val: i64) -> Vec<usize> {
   let deck_len = deck.len();
   let split_pt: usize = (if val < 0 { (deck_len as i64)+val } else { val }) as usize;
   deck.into_iter().cycle().skip(split_pt).take(deck_len).collect()
}

fn do_deal_inc(deck: Vec<usize>, val: i64) -> Vec<usize> {
    let mut out_vec: Vec<usize> = vec![0; deck.len()];
    let mut in_pos: usize = 0;
    let mut out_pos: i64 = 0;
    while in_pos < deck.len() {
        out_vec[out_pos as usize] = deck[in_pos];
        in_pos += 1;
        out_pos = (out_pos + val) % (deck.len() as i64);
        //println!("Out_pos {}", out_pos);
    }
    out_vec
}
/* End - straightforward shuffle */

fn main() {
    let filename = args().nth(1).expect("Supply a filename!");
    let in_count: u32 = args().nth(2).expect("Supply a card count!")
            .parse().expect("Supply a valid card count!");
    /*
    let p1_reps: u32 = args().nth(3).expect("Supply a p1 reps count!")
            .parse().expect("Supply a valid p1 reps count!");
    */
    let p1_reps: usize = 1;
    let input_raw = read_strs(&filename, ',');
    let input_lines: Vec<String> = input_raw.into_iter()
            .map(|sepd| sepd.into_iter().nth(0).unwrap())
            .collect();


    let shuffles: Vec<Shuffle> = input_lines.iter()
            .map(|line| shuffle_parse(line))
            .collect();
    //println!("Shuffles: {:?}", shuffles);


    let mut out_deck: Vec<usize> = (0..in_count)
            .map(|val| usize::try_from(val).expect("Error building deck"))
            .collect();
    for _ind in 0..p1_reps {
        out_deck = shuffles.iter()
                .fold(out_deck, |deck, shuf| do_shuffle(deck, shuf));
    }
    //println!("Deck: {:?}", out_deck);
    //println!("Card 2020: {:?}", out_deck[2020]);

    let part1_pos = out_deck.iter().enumerate().find(|(_, val)| **val == 2019);
    match part1_pos {
        Some((ind, _)) => println!("Part 1: {}", ind),
        None => println!("Couldn't find 2019"),
    };

    // PART 2
    let p2_deck = 119315717514047i64;
    let p2_reps = 101741582076661i64;
    let p2_out_pos = 2020;

    let summary = summarize(&shuffles, p2_deck);
    let mult_summ = mult_summary(p2_reps, summary, p2_deck);
    println!("Part 2 summary {:?}", summary);
    println!("Part 2 multiplied summ {:?}", mult_summ);

    let mut p2_shuff: Vec<Shuffle> = vec![Shuffle::DealInc(mult_summ.0)];
    if ! mult_summ.1 {
        p2_shuff.push(Shuffle::IntoNew);
    }
    p2_shuff.push(Shuffle::Cut(mult_summ.2));

    let mut cache: HashMap<(usize, usize), Vec<i64>> = HashMap::new();
    let shuf_rev: Vec<&Shuffle> = p2_shuff.iter().rev().collect();
    let mut orig_pos = p2_out_pos;

    orig_pos = shuf_rev.iter()
            .fold(orig_pos, |pos, shuf| find_from(pos, shuf, p2_deck, &mut cache));
    println!("Part 2: {}", orig_pos);

}
