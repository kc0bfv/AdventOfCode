#!/usr/bin/env python3

def count_matches(winning, have):
    return sum(1 if val in winning else 0 for val in have)

def p1_count(winning, have):
    summed = count_matches(winning, have)
    return 0 if summed < 1 else 2 ** (summed - 1)

def main(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f]
    
    num_only = [line.split(":")[1] for line in lines]
    winning, have = zip(*(line.split("|") for line in num_only))
    winning_nums = [[int(val.strip()) for val in line.strip().split(" ") if val.strip() != ""] for line in winning]
    have_nums = [[int(val.strip()) for val in line.strip().split(" ") if val.strip() != ""] for line in have]

    paired = list(zip(winning_nums, have_nums))
    p1_results = [p1_count(win, have) for (win, have) in paired]
    print(f"Part 1: {sum(p1_results)}")
    
    card_counts = [1 for pair in paired]
    for ind, pair in enumerate(paired):
        for i in range(count_matches(*pair)):
            card_counts[ind+i+1] += card_counts[ind]

    print(f"Part 2: {sum(card_counts)}")

if __name__ == "__main__":
    main("samp01")
    main("input")