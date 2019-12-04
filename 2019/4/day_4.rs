fn test_criteria(val: &u32) -> bool {
    // Is 6 digit?  tested by input range
    // Is within range?  tested by input range

    // Separate out digits
    let dig_6 = val / 100000;
    let dig_5 = (val % 100000) / 10000;
    let dig_4 = (val % 10000) / 1000;
    let dig_3 = (val % 1000) / 100;
    let dig_2 = (val % 100) / 10;
    let dig_1 = val % 10;
    //println!("{} {} {} {} {} {}", dig_6, dig_5, dig_4, dig_3, dig_2, dig_1);

    // Two adjacent digits are same?
    if (dig_6 == dig_5) || (dig_5 == dig_4) || (dig_4 == dig_3)
            || (dig_3 == dig_2) || (dig_2 == dig_1) {
    } else {
        return false;
    }

    // Digits never decrease
    if (dig_6 > dig_5) || (dig_5 > dig_4) || (dig_4 > dig_3)
            || (dig_3 > dig_2) || (dig_2 > dig_1) {
        return false;
    } else {
    }

    // Two adjacent digits are same but not 3?
    if         (dig_6 == dig_5 && dig_5 != dig_4)
            || (dig_5 == dig_4 && dig_4 != dig_3 && dig_5 != dig_6)
            || (dig_4 == dig_3 && dig_3 != dig_2 && dig_4 != dig_5)
            || (dig_3 == dig_2 && dig_2 != dig_1 && dig_3 != dig_4)
            || (dig_2 == dig_1                   && dig_2 != dig_3)
    {
    } else {
        return false;
    }

    return true;
}

fn main() {
    let (st_pt, ed_pt): (u32, u32) = (178416, 676461);
    //let (st_pt, ed_pt): (u32, u32) = (100000, 200000);
    
    let meet_criteria: Vec<u32> = (st_pt..ed_pt+1).filter(
            |val| test_criteria(val)).collect();

    println!("Met crit {:?}", meet_criteria);
    println!("Met crit {}", meet_criteria.len());
}
