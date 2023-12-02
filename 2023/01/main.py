#!/usr/bin/env python3

def p2digit(substr):
    if substr[0].isdigit():
        return int(substr[0])
    
    digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    for ind, dig in enumerate(digits):
        if substr.startswith(dig):
            return ind + 1
    
    return None


def part1(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f]
    digits = ([int(i) for i in line if i.isdigit()] for line in lines)
    cal = [d[0]*10 + d[-1] for d in digits]
    print(f"Part 1 {sum(cal)}")

def part2(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f]
    digits = [[p2digit(line[ind:]) for ind, _ in enumerate(line) if p2digit(line[ind:]) is not None] for line in lines]
    cal = [d[0]*10 + d[-1] for d in digits]
    print(f"Part 2 {sum(cal)}")

if __name__ == "__main__":
    part1("test")
    part2("test2")
    part1("input")
    part2("input")