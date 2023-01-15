use std::collections::HashSet;

fn find_marker(content: &Vec<char>, back_cnt: usize) -> Option<usize> {
    for ind in (back_cnt - 1)..content.len() {
        let set: HashSet<char> = HashSet::from_iter(
            content.get((ind - (back_cnt - 1))..ind+1)
                .expect("Slice 2 fail")
                .iter()
                .rev()
                .map(|ref_char| { *ref_char })
        );
        if set.len() == back_cnt {
            return Some(ind + 1);
        }
    }

    None
}

fn runit(filename: &str) -> Result<(), Box<dyn std::error::Error>> {
    println!("File: {}", filename);

    let content: Vec<char> = std::fs::read_to_string(filename)?.trim().chars().collect();

    println!("Part 1: {}", find_marker(&content, 4).expect("Failed to find for 4"));
    println!("Part 2: {}", find_marker(&content, 14).expect("Failed to find for 14"));

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    runit("samp")?;
    runit("samp2")?;
    runit("input.txt")?;

    Ok(())
}
