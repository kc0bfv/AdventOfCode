use std::fmt;

// Index combinations without replacement
pub fn combo_ind_without_repl(item_count: usize, output_amt: usize) -> Vec<Vec<usize>> {
    if output_amt == 1 {
        return (0..item_count).map(|item| vec![item]).collect();
    }
    let sub_vecs = combo_ind_without_repl(item_count, output_amt - 1);
    let mut ret_val: Vec<Vec<usize>> = vec![];
    
    for sub_vec in sub_vecs {
        for new_item in 0..item_count {
            if !sub_vec.contains(&new_item) {
                let mut new_vec: Vec<usize> = sub_vec.iter().cloned().collect();
                new_vec.push(new_item);
                ret_val.push(new_vec);
            }
        }
    }
    return ret_val;
}

pub fn print_vec_of_vecs<T>(input: &Vec<Vec<T>>)
    where T: fmt::Display,
{
    for line in input.iter() {
        for val in line.iter() {
            print!("{}", val);
        }
        println!("");
    }
}
