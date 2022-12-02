#!/usr/bin/env python3

def split_elves(lines):
    ret_elves = list()
    cur_elf = list()

    for line in lines:
        if line.strip() == "":
            ret_elves.append(cur_elf)
            cur_elf = list()
        else:
            cur_elf.append(int(line.strip()))

    ret_elves.append(cur_elf)
    return ret_elves


def main(infile):
    with open(infile, "r") as f:
        lines = f.readlines()

    elves = split_elves(lines)

    elf_sums = [sum(elf) for elf in elves]
    max_elf = max(elf_sums)

    print(f"Part 1 {max_elf}")

    top_three = sorted(elf_sums)[-3:]
    sum_top_three = sum(top_three)

    print(f"Part 2 {sum_top_three}")



if __name__ == "__main__":
    main("input.txt")
