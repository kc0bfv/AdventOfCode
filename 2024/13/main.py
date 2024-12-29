#!/usr/bin/env python3

from collections import defaultdict
import math
import re
import sys

class Machine:
    def __init__(self, lines):
        print(lines)
        ma = re.match(r"Button A: X\+([0-9]+), Y\+([0-9]+)", lines[0])
        mb = re.match(r"Button B: X\+([0-9]+), Y\+([0-9]+)", lines[1])
        mp = re.match(r"Prize: X=([0-9]+), Y=([0-9]+)", lines[2])
        self.a = int(ma.group(1)), int(ma.group(2))
        self.b = int(mb.group(1)), int(mb.group(2))
        self.p = int(mp.group(1)), int(mp.group(2))
        self.p2 = 10000000000000 + int(mp.group(1)), 10000000000000 + int(mp.group(2))

    def __str__(self):
        return f"A: {self.a}, B: {self.b}, Prize: {self.p}"

    def find_presses(self, a_cst = 3, b_cst = 1, part2 = False):
        solutions = list()
        best_soln = None
        best_soln_cost = None

        prize = self.p if not part2 else self.p2

        max_b_presses = math.ceil(prize[0] / self.b[0])
        print(max_b_presses)
        for b_presses in range(max_b_presses, -1, -1):
            b_dist = b_presses * self.b[0], b_presses * self.b[1]
            remainder = prize[0] - b_dist[0], prize[1] - b_dist[1]
            
            # If the remainder is not evenly divisible by the a presses,
            # There's no solution here.  Move on.
            if remainder[0] % self.a[0] != 0 or remainder[1] % self.a[1] != 0:
                continue

            # If pressing a an amount of times doesn't get us there in both
            # dimensions, there's no solution here
            a_presses_x = remainder[0] / self.a[0]
            a_presses_y = remainder[1] / self.a[1]
            if a_presses_x != a_presses_y:
                continue

            solution = (a_presses_x, b_presses)
            cost = b_presses * b_cst + a_presses_x * a_cst

            solutions.append(solution)
            if best_soln is None or cost < best_soln_cost:
                best_soln = solution
                best_soln_cost = cost
        
        return best_soln_cost


def main(filename):
    with open(filename) as infile:
        lines = [line.strip() for line in infile]

    machines = [Machine(lines[i:i+3]) for i in range(0, len(lines), 4)]

    best_costs_all = [mach.find_presses() for mach in machines]
    print(best_costs_all)
    best_costs = [bc for bc in best_costs_all if bc is not None]
    print(f"Part 1: {sum(best_costs)}")

    best_costs_all = [mach.find_presses(part2=True) for mach in machines]
    print(best_costs_all)
    best_costs = [bc for bc in best_costs_all if bc is not None]
    print(f"Part 2: {sum(best_costs)}")

if __name__ == "__main__":
    main(sys.argv[1])
