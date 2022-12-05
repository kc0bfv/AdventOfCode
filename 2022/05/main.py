#!/usr/bin/env python3

# stack will be a list of letters.  stacks will be list of stacks

def parse_stacks(lines):
    stack_cnt = int(len(lines[0]) / 4)
    stacks = [list() for _ in range(stack_cnt)]

    stack_height = 0
    found_stack = False
    for ind in range(len(lines)):
        if not found_stack:
            stack_height += 1
            if lines[ind][0] == "[":
                found_stack = True
        else:
            if lines[ind][0] == "[":
                stack_height += 1
            else:
                break

    for row_ind in range(stack_height):
        for stack_ind in range(stack_cnt):
            box = lines[row_ind][(stack_ind * 4) + 1]
            if box != " ":
                stacks[stack_ind].insert(0, box)

    return stacks, stack_height

class Instr:
    def __init__(self, line):
        split = line.split(" ")
        self.cnt = int(split[1])
        self.src = int(split[3])
        self.dst = int(split[5])

    def do_move(self, stacks):
        for _ in range(self.cnt):
            stacks[self.dst-1].append(stacks[self.src-1].pop())
        return stacks

    def do_move_9001(self, stacks):
        stacks[self.dst-1] += stacks[self.src-1][-self.cnt:]
        stacks[self.src-1] = stacks[self.src-1][:-self.cnt]
        return stacks

def parse_instructions(lines, stack_height):
    return [Instr(line) for line in lines[stack_height+2:]]

def get_tops(stacks):
    retval = ""
    for stack in stacks:
        retval += stack[-1]
    return retval

def main(filename):
    with open(filename, "r") as f:
        lines = [l for l in f]

    orig_stacks, stack_height = parse_stacks(lines)
    instrs = parse_instructions(lines, stack_height)

    stacks = orig_stacks
    for instr in instrs:
        stacks = instr.do_move(stacks)

    print("Part 1 ", get_tops(stacks))

    orig_stacks, stack_height = parse_stacks(lines)

    stacks = orig_stacks
    for instr in instrs:
        stacks = instr.do_move_9001(stacks)

    print("Part 2 ", get_tops(stacks))

if __name__ == "__main__":
    main("input.txt")
