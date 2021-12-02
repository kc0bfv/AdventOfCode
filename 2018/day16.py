#!/usr/bin/env python3

from collections import defaultdict

class Instruction:
    def exec(self, inregs, a, b, c):
        # Take a tuple of registers in, output a tuple of registers
        return tuple(self.getout(inregs, a, b) if ind == c else regval
                for ind, regval in enumerate(inregs))

class RegImm(Instruction):
    def getout(self, inregs, a, b): return self.opr(inregs[a], b)
class RegReg(Instruction):
    def getout(self, inregs, a, b): return self.opr(inregs[a], inregs[b])
class ImmReg(Instruction):
    def getout(self, inregs, a, b): return self.opr(a, inregs[b])

class Add:
    def opr(self, a, b): return a+b
class Mu:
    def opr(self, a, b): return a*b
class Ba:
    def opr(self, a, b): return a&b
class Bo:
    def opr(self, a, b): return a|b
class Seto:
    def opr(self, a, b): return a
class Gt:
    def opr(self, a, b): return 1 if a > b else 0
class Eq:
    def opr(self, a, b): return 1 if a == b else 0

class Addi(RegImm, Add): pass
class Addr(RegReg, Add): pass

class Muli(RegImm, Mu): pass
class Mulr(RegReg, Mu): pass

class Bani(RegImm, Ba): pass
class Banr(RegReg, Ba): pass

class Bori(RegImm, Bo): pass
class Borr(RegReg, Bo): pass

class Seti(ImmReg, Seto): pass
class Setr(RegReg, Seto): pass

class Gtir(ImmReg, Gt): pass
class Gtri(RegImm, Gt): pass
class Gtrr(RegReg, Gt): pass

class Eqir(ImmReg, Eq): pass
class Eqri(RegImm, Eq): pass
class Eqrr(RegReg, Eq): pass

INSTRUCTIONS = [Addi, Addr, Muli, Mulr, Bani, Banr, Bori, Borr, Seti, Setr,
        Gtir, Gtri, Gtrr, Eqir, Eqri, Eqrr]

if __name__ == "__main__":
    with open("input day 16.txt") as f:
        lines = [line for line in f]

    # Get part1 input parsed
    part1_input = list()
    line_no = 0
    while lines[line_no].startswith("Before:"):
        beg = tuple(int(i)
                for i in lines[line_no+0].split(":")[1].strip()[1:-1].split(","))
        act = tuple(int(i) for i in lines[line_no+1].split(" "))
        end = tuple(int(i)
                for i in lines[line_no+2].split(":")[1].strip()[1:-1].split(","))
        part1_input.append((beg, act, end))
        line_no += 4

    # Parse part2 input
    part2_lines = (line.strip() for line in lines[line_no:] if line.strip())
    part2_input = [tuple(int(i) for i in line.split()) for line in part2_lines]

    like_3_or_more = 0
    inst_poss = defaultdict(lambda : set(INSTRUCTIONS))
    for beg, act, end in part1_input:
        results = (inst().exec(beg, act[1], act[2], act[3]) for inst in INSTRUCTIONS)
        correct = [res == end for res in results]
        like_3_or_more += 1 if len([was_corr for was_corr in correct if was_corr]) >= 3 else 0
        inst_poss[act[0]] = inst_poss[act[0]].intersection(inst
                for (inst, corr) in zip(INSTRUCTIONS, correct) if corr)

    print("Part 1: {}".format(like_3_or_more))

    assigned = [None] * 16

    # If there's only one option for any instruction numbers, assign that number to
    # the instruction, eliminate that option from the other instruction numbers,
    # and repeat until all instructions are assigned / no possibilities remain
    changed = True
    while None in assigned and changed == True:
        changed = False
        for inst_no in inst_poss:
            if assigned[inst_no] is not None:
                continue
            elif len(inst_poss[inst_no]) == 1:
                changed = True
                assigned[inst_no] = inst_poss[inst_no].pop()
                for other_inst in inst_poss:
                    inst_poss[other_inst].discard(assigned[inst_no])
            elif len(inst_poss[inst_no]) == 0:
                print("Bug {}".format(inst_no))

    if None in assigned:
        print("Did not assign all! {}".format(assigned))
        exit()

    # Check for double-assigned instructions...
    checking = set()
    for i in assigned:
        if i in checking:
            print("Double assign: {}".format(i))
        else:
            checking.add(i)

    # Part 2
    regs = (0, 0, 0, 0)
    for act in part2_input:
        regs = assigned[act[0]]().exec(regs, act[1], act[2], act[3])

    print("Part 2: {}".format(regs))
