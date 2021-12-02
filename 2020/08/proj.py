#!/usr/bin/env python3

class CPUHalt(Exception):
    pass

class CPU:
    def __init__(self, prog_file = None, insts = None):
        self.accumulator = 0
        self.inst_cnt = 0
        if insts is None:
            self.insts = self._parse_insts(prog_file)
        else:
            self.insts = [inst.clone() for inst in insts]

    def jmp(self, dist):
        self.inst_cnt += dist

    def get_inst(self):
        if self.inst_cnt >= len(self.insts):
            raise CPUHalt("CPU Halted: {} of {}".format(self.inst_cnt, len(self.insts)))
        return self.insts[self.inst_cnt]

    def run_to_inf_loop(self):
        while self.get_inst().runs < 1:
            self.get_inst().run(self)

    def clone(self):
        return CPU(insts = self.insts)

    def _build_inst(self, line):
        insts = {"nop": NOP, "acc": ACC, "jmp": JMP}

        inst, arg = line.strip().split(" ")

        return insts[inst](arg)

    def _parse_insts(self, filename):
        return [self._build_inst(line) for line in open(filename, "r").readlines()]

class INST:
    name = "INST"
    def __init__(self, arg):
        self.arg = int(arg)
        self.runs = 0
    def __str__(self):
        return "{} {}".format(self.name, self.arg)
    def clone(self):
        return self.__class__(self.arg)
    def run(self, cpu):
        self.runs += 1
        self._run(cpu)
        cpu.jmp(1)
    def _run(self):
        raise RuntimeError("Ran run on INST")

class NOP(INST):
    name = "NOP"
    def _run(self, cpu):
        pass

class ACC(INST):
    name = "ACC"
    def _run(self, cpu):
        cpu.accumulator += self.arg

class JMP(INST):
    name = "JMP"
    def _run(self, cpu):
        cpu.jmp(self.arg-1)

def part2(orig_cpu):
    for to_change in range(len(orig_cpu.insts)):
        inst = orig_cpu.insts[to_change]
        if inst.name in ["NOP", "JMP"]:
            new_cpu = orig_cpu.clone()
            if inst.name == "NOP":
                new_cpu.insts[to_change] = JMP(inst.arg)
            else:
                new_cpu.insts[to_change] = NOP(inst.arg)

            try:
                new_cpu.run_to_inf_loop()
            except CPUHalt as c:
                print("Part 2: {} - {}".format(new_cpu.accumulator, to_change))
                return


if __name__ == "__main__":
    orig_cpu = CPU("input.txt")

    cpu = orig_cpu.clone()

    cpu.run_to_inf_loop()

    print("Part 1: {}".format(cpu.accumulator))

    part2(orig_cpu)
