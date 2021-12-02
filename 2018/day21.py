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

class SRegReg(Instruction):
    def getout(self, inregs, a, b): return self.opr(inregs[a], 0)
class SImmReg(Instruction):
    def getout(self, inregs, a, b): return self.opr(a, 0)

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

class Seti(SImmReg, Seto): pass
class Setr(SRegReg, Seto): pass

class Gtir(ImmReg, Gt): pass
class Gtri(RegImm, Gt): pass
class Gtrr(RegReg, Gt): pass

class Eqir(ImmReg, Eq): pass
class Eqri(RegImm, Eq): pass
class Eqrr(RegReg, Eq): pass

INSTRUCTIONS = [Addi, Addr, Muli, Mulr, Bani, Banr, Bori, Borr, Seti, Setr,
        Gtir, Gtri, Gtrr, Eqir, Eqri, Eqrr]

INSTDICT = {
        "addi": Addi, "addr": Addr, "muli": Muli, "mulr": Mulr,
        "bani": Bani, "banr": Banr, "bori": Bori, "borr": Borr,
        "seti": Seti, "setr": Setr, "gtir": Gtir, "gtri": Gtri,
        "gtrr": Gtrr, "eqir": Eqir, "eqri": Eqri, "eqrr": Eqrr
        }

if __name__ == "__main__":
    with open("input day 21.txt") as f:
        lines = [line.strip() for line in f]

    ip_reg = int(lines[0].split()[1])

    splitlines = (line.split() for line in lines[1:])
    instructions = [(INSTDICT[inst], int(a), int(b), int(c))
            for inst,a,b,c in splitlines]

    R0INIT = 0
    regs = (R0INIT, 0, 0, 0, 0, 0)

    total_insts = 0
    while 0 <= regs[ip_reg] < len(instructions):
        if regs[ip_reg] == 28 :
            print(total_insts, regs, "line {}".format(regs[ip_reg]), instructions[regs[ip_reg]])
        total_insts += 1
        inst, a, b, c = instructions[regs[ip_reg]]
        regs = inst().exec(regs, a, b, c)
        regs = tuple(regs[i] + 1 if i == ip_reg else regs[i]
                for i in range(len(regs)))

    print(regs)
