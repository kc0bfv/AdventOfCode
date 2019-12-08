use std::env::args;

pub mod file_help;
use file_help::read_digits;

fn build_layers(digits: Vec<u8>, wide: u32, height: u32) -> Vec<Vec<Vec<u8>>> {
    let mut digit_iter = digits.iter().cloned();

    let mut ret_val = Vec::<Vec<Vec<u8>>>::new();

    loop {
        let mut cur_layer = Vec::<Vec<u8>>::new();
        for _ in 0..height {
            let mut cur_row = Vec::<u8>::new();
            for _ in 0..wide {
                let next_dig = digit_iter.next();
                match next_dig {
                    None => return ret_val,
                    Some(val) => cur_row.push(val.clone()),
                }
            }
            cur_layer.push(cur_row);
        }
        ret_val.push(cur_layer);
    };
}

fn count_num(layer: &Vec<Vec<u8>>, num: u8) -> usize {
    layer.iter().flatten().filter(|item| **item == num).count()
}

fn get_visible(layers: &Vec<Vec<Vec<u8>>>) -> Vec<Vec<u8>> {
    let mut ret: Vec<Vec<u8>> = layers[0].to_vec();
    for layer in layers.iter().skip(1) {
        for row_ind in 0..layer.len() {
            for col_ind in 0..layer[0].len() {
                if ret[row_ind][col_ind] == 2 {
                    ret[row_ind][col_ind] = layer[row_ind][col_ind];
                }
            }
        }
    }
    return ret;
}

fn main() {
    let filename: String = args().skip(1).take(1).collect();
    let wide_height: Vec<String> = args().skip(2).take(2).collect();
    let wide = (wide_height[0]).parse::<u32>().unwrap();
    let height = (wide_height[1]).parse::<u32>().unwrap();
    let input_vals = (&read_digits(&filename)).to_vec();

    let layers = build_layers(input_vals, wide, height);
    let zeroes: Vec<(usize, usize, usize, usize)> = layers.iter()
            .enumerate()
            .map(|(ind, layer)|
                (ind, count_num(layer, 0), count_num(layer, 1), count_num(layer, 2)))
            .collect();
    let (ind, zeroes, ones, twos) = zeroes.iter()
            .min_by(|(_, zeroes_1, _, _), (_, zeroes_2, _, _)|
                    zeroes_1.cmp(zeroes_2)
                )
            .unwrap();
    println!("{} {} {} {} Multed {}", ind, zeroes, ones, twos, ones * twos);

    let visible = get_visible(&layers);
    for line in visible {
        println!("{:?}", line);
    }
}
