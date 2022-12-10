#!/usr/bin/env python3

class Inst:
    def __init__(self, machine, line):
        self.machine = machine
        self.parts = line.split(" ")
        assert(self.parts[0] == self.inst)
    def simulate_step():
        raise RuntimeError("Not implemented - parent class")

class Addx(Inst):
    inst = "addx"
    def __init__(self, machine, line):
        super().__init__(machine, line)
        self.cnt = int(self.parts[1])
        self.simulated_steps = 0

    def simulate_step(self):
        self.simulated_steps += 1
        while self.simulated_steps < 2:
            yield
            self.simulated_steps += 1
        self.machine.register["x"] += self.cnt
        yield

class Noop(Inst):
    inst = "noop"
    def __init__(self, machine, line):
        super().__init__(machine, line)

    def simulate_step(self):
        yield

class Machine:
    def __init__(self, lines):
        def inst_class(line):
            insts = {c.inst: c for c in Inst.__subclasses__()}
            return insts[line.split(" ")[0]]
        self.insts = [inst_class(line)(self, line) for line in lines]

        self.register = {"x": 1}
        self.cur_step = 1
        self.screen_pixels = 6 * 40
        self.screen = list(" " * self.screen_pixels)

    def __update_screen(self):
        step_conv = self.cur_step - 1
        if ( step_conv % 40 ) >= self.register["x"]-1 \
                and ( step_conv % 40 ) <= self.register["x"]+1:
            self.screen[step_conv % self.screen_pixels] = "#"
        else:
            self.screen[step_conv % self.screen_pixels] = " "

    def run_machine_steps(self):
        self.__update_screen()
        for inst in self.insts:
            for sim_step in inst.simulate_step():
                self.cur_step += 1
                self.__update_screen()
                yield

    def print_screen(self):
        for i in range(6):
            print("".join(self.screen[((i)*40):((i+1)*40)]))
    

def main(filename):
    with open(filename, "r") as f:
        lines = [l.strip() for l in f]

    machine = Machine(lines)
    strengths = list()

    for _ in machine.run_machine_steps():
        if (machine.cur_step - 20) % 40 == 0:
            strengths.append((machine.register["x"], machine.cur_step))

    p1 = sum(r * c for (r, c) in strengths)

    print(f"Part 1: {p1}")
    machine.print_screen()

if __name__ == "__main__":
    main("samp")
    main("input.txt")
