#!/usr/bin/env python3

def do_propose(elves, elf, round_no):
    posns = [(x-1 + elf[0], y-1 + elf[1])
        for x in range(3)
        for y in range(3)
        if x != 1 or y != 1
    ]
    ind_map = (
        (0, 3, 5), # North
        (2, 4, 7), # South
        (0, 1, 2), # West
        (5, 6, 7), # East
    )
    if not any(pos in elves for pos in posns):
        return None

    for posind in range(len(ind_map)):
        check_dir = (posind + round_no) % len(ind_map)

        dir_posns = (posns[i] for i in ind_map[check_dir])
        if not any(pos in elves for pos in dir_posns):
            return posns[ind_map[check_dir][1]]

    return None

def do_round(elves, round_no):
    elf_props = [(elf, do_propose(elves, elf, round_no)) for elf in elves]
    
    # Find multiples
    seen = set()
    to_disallow = set()
    for elf, prop in elf_props:
        if prop in seen:
            to_disallow.add(prop)
        seen.add(prop)

    # Make moves
    new_elves = {elf if prop is None or prop in to_disallow else prop
        for elf, prop in elf_props}
    return new_elves

def find_extents(elves):
    min_x, max_x, min_y, max_y = None, None, None, None
    for x, y in elves:
        if min_x is None or x < min_x:
            min_x = x
        if max_x is None or x > max_x:
            max_x = x
        if min_y is None or y < min_y:
            min_y = y
        if max_y is None or y > max_y:
            max_y = y
    return min_x, max_x, min_y, max_y

def str_map(elves):
    min_x, max_x, min_y, max_y = find_extents(elves)
    return "\n".join(
        "".join(
            "#" if (x, y) in elves else "."
            for x in range(min_x, max_x+1)
        )
        for y in range(min_y, max_y+1)
    )

def main(filename):
    with open(filename, "r") as f:
        lines = [line.strip() for line in f]

    elves = set()
    for y, line in enumerate(lines):
        for x, char in enumerate(line):
            if char == "#":
                elves.add((x, y))

    for rnd in range(10):
        print(str_map(elves))
        print()
        elves = do_round(elves, rnd)

    print(str_map(elves))
    print()

    min_x, max_x, min_y, max_y = find_extents(elves)
    total_spaces = (max_x - min_x + 1) * (max_y - min_y + 1)
    
    print(f"Part 1: {total_spaces - len(elves)}")

    next_elves = elves
    elves = set()
    while elves != next_elves:
        elves = next_elves
        rnd += 1
        next_elves = do_round(elves, rnd)

        if rnd % 1000 == 0:
            print(rnd)

    print(f"Part 2: {rnd + 1}")

if __name__ == "__main__":
    main("samp")
    main("input.txt")
