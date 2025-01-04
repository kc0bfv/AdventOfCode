#!/usr/bin/env python3

from collections import defaultdict
import sys

class Computer:
    def __init__(self, inlines):
        self.reg = dict()
        self.reg["A"] = int(inlines[0].split(":")[1].strip())
        self.reg["B"] = int(inlines[1].split(":")[1].strip())
        self.reg["C"] = int(inlines[2].split(":")[1].strip())
        self.prog = [int(val)
            for val in inlines[4].split(":")[1].strip().split(",")]

        self.output = list()
        self.ip = 0

    @property
    def outstr(self):
        return ",".join(str(val) for val in self.output)

    def step(self):
        if self.ip >= len(self.prog):
            return False

        instr, literal_oper = self.prog[self.ip], self.prog[self.ip + 1]
        
        combo_oper = literal_oper
        if literal_oper == 4:
            combo_oper = self.reg["A"]
        elif literal_oper == 5:
            combo_oper = self.reg["B"]
        elif literal_oper == 6:
            combo_oper = self.reg["C"]
        elif 0 <= literal_oper <= 3:
            pass
        else:
            raise RuntimeError(f"Invalid oper: {literal_oper}")

        self.ip += 2

        if instr == 0:
            self.reg["A"] = int(self.reg["A"]/(2 ** combo_oper))
        elif instr == 1:
            self.reg["B"] = self.reg["B"] ^ literal_oper
        elif instr == 2:
            self.reg["B"] = combo_oper % 8
        elif instr == 3:
            if self.reg["A"] != 0:
                self.ip = literal_oper
        elif instr == 4:
            self.reg["B"] = self.reg["B"] ^ self.reg["C"]
        elif instr == 5:
            self.output.append(combo_oper % 8)
        elif instr == 6:
            self.reg["B"] = int(self.reg["A"]/(2 ** combo_oper))
        elif instr == 7:
            self.reg["C"] = int(self.reg["A"]/(2 ** combo_oper))
        else:
            raise RuntimeError(f"Invalid instr: {instr}")

        return True


def main(filename, p2):
    with open(filename) as infile:
        lines = [line.strip() for line in infile]

    comp = Computer(lines)
    while comp.step():
        pass

    print(f"Part 1: {comp.outstr}")

    if p2 is False:
        return

    # Part 2 search...
    options = list()
    for ind in range(1024):
        comp = Computer(lines)
        comp.reg["A"] = ind
        while comp.step():
            pass
        options.append(comp.output)

    desired_out = comp.prog
    first_val_opts = [ind
        for ind, lst in enumerate(options)
        if lst[0] == desired_out[0]
    ]

    bits_locked = 10
    prev_opts = first_val_opts
    for val_ind, val in enumerate(comp.prog[1:]):
        next_opts = list()
        bits_toskip = 3 * (val_ind + 1)
        bits_locked = 10 + (3 * val_ind)

        for i in range(8):
            for prev in prev_opts:
                poss = prev + (i << bits_locked)
                out = options[poss >> bits_toskip]
                if out[0] == val:
                    next_opts.append(poss)
        prev_opts = next_opts

    corr_len = [val for val in prev_opts if 8 ** 15 <= val <= 8 ** 16]

    comp = Computer(lines)
    comp.reg["A"] = corr_len[0]
    while comp.step():
        pass

    valid = "was" if comp.output == desired_out else "wasn't"

    print(f"Part 2 {valid} valid: {corr_len}")


if __name__ == "__main__":
    main(sys.argv[1], len(sys.argv) < 3 or sys.argv[2] != "1")
