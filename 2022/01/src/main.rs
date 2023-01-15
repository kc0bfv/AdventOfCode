
type Elf = Vec<u32>;

fn gen_elves(filename: &str) -> Result<Vec<Elf>, std::io::Error> {
    Ok(std::fs::read_to_string(filename)?
        .lines()
        .fold(
            {
                let mut acc = Vec::new();
                acc.push(Elf::new());
                acc
            },
            |old_acc: Vec<Elf>, cur| {
                let mut acc = old_acc;
                let acc_len = acc.len();
                if cur.trim() == "" {
                    acc.push(Elf::new());
                } else {
                    acc.get_mut(acc_len - 1)
                        .expect("Failure in acc get_mut, unexpected")
                        .push(cur.parse().expect("Failed to parse"));
                }
                acc
            })
    )
}

fn main() -> Result<(), std::io::Error> {
    //let elves = gen_elves("samp")?;
    let elves = gen_elves("input.txt")?;

    /*
    println!("{}",
        elves.iter()
            .map( |lst| {
                    lst.iter().map(|item| {
                            item.to_string()
                        })
                    .collect::<Vec<String>>()
                    .join(",")
                })
            .collect::<Vec<String>>()
            .join("\n")
    );
    */

    let part_1: u32 = elves.iter().map( |lst| { lst.iter().sum() } ).max().expect("No max");
    println!("Part 1: {}", part_1);

    let mut p2_sort = elves.iter()
        .map( |lst| { lst.iter().sum() } )
        .collect::<Vec<u32>>();
    p2_sort.sort_unstable();

    let part_2: u32 = p2_sort.get(elves.len()-3..)
        .expect("Failed to get last 3")
        .iter()
        .sum();

    println!("Part 2: {}", part_2);

    Ok(())
}
