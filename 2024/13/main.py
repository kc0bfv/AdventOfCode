#!/usr/bin/env python3

from collections import defaultdict
import math
import re
import sys

class Machine:
    def __init__(self, lines):
        ma = re.match(r"Button A: X\+([0-9]+), Y\+([0-9]+)", lines[0])
        mb = re.match(r"Button B: X\+([0-9]+), Y\+([0-9]+)", lines[1])
        mp = re.match(r"Prize: X=([0-9]+), Y=([0-9]+)", lines[2])
        self.a = int(ma.group(1)), int(ma.group(2))
        self.b = int(mb.group(1)), int(mb.group(2))
        self.p = int(mp.group(1)), int(mp.group(2))
        self.p2 = 10000000000000 + int(mp.group(1)), 10000000000000 + int(mp.group(2))

    def __str__(self):
        return f"A: {self.a}, B: {self.b}, Prize: {self.p}"

    def find_presses_2(self, a_cst = 3, b_cst = 1, part2 = False):
        #print(f"Finding {self}")
        prize = self.p if not part2 else self.p2

        # Find first two valid presses
        xvap2 = [a
            for a in range(self.b[0]*self.a[0])
            if 0 == (prize[0] - (self.a[0] * a)) % self.b[0]]
        yvap2 = [a
            for a in range(self.b[1]*self.a[1])
            if 0 == (prize[1] - (self.a[1] * a)) % self.b[1]]

        xvbp2 = [b
            for b in range(self.b[0]*self.a[0])
            if 0 == (prize[0] - (self.b[0] * b)) % self.a[0]]
        yvbp2 = [b
            for b in range(self.b[1]*self.a[1])
            if 0 == (prize[1] - (self.b[1] * b)) % self.a[1]]

        if len(xvap2) == 0 or len(yvap2) == 0 or len(xvbp2) == 0 or len(yvbp2) == 0:
            return None

        xvap_off, xvap_inter = xvap2[0], xvap2[1]-xvap2[0]
        yvap_off, yvap_inter = yvap2[0], yvap2[1]-yvap2[0]
        xvbp_off, xvbp_inter = xvbp2[0], xvbp2[1]-xvbp2[0]
        yvbp_off, yvbp_inter = yvbp2[0], yvbp2[1]-yvbp2[0]

        tries = 100000

        x_valid_a_presses = {a
            for a in range(xvap_off, tries, xvap_inter)
            if 0 == (prize[0] - (self.a[0] * a)) % self.b[0]}
        y_valid_a_presses = {a
            for a in range(yvap_off, tries, yvap_inter)
            if 0 == (prize[1] - (self.a[1] * a)) % self.b[1]}

        x_valid_b_presses = {b
            for b in range(xvbp_off, tries, xvbp_inter)
            if 0 == (prize[0] - (self.b[0] * b)) % self.a[0]}
        y_valid_b_presses = {b
            for b in range(xvbp_off, tries, xvbp_inter)
            if 0 == (prize[1] - (self.b[1] * b)) % self.a[1]}
        
        # These aren't quite actually valid...  Most (maybe all but one?) of these
        # will not have a number of b presses that will result in landing on the
        # target.  There will be a number of b presses that lands on the x target,
        # and (almost certainly) a different number of b presses that lands on the y
        # target, but now we need to find the values that have the same number of
        # b presses to get on both x and y
        # We can use this to get the constant interval over which a presses can even
        # potentially produce a valid b result though... (and vice/versa)
        intersection_a = sorted(x_valid_a_presses.intersection(y_valid_a_presses))
        intersection_b = sorted(x_valid_b_presses.intersection(y_valid_b_presses))
        #print(self, intersection_a, intersection_b)
        #print(sorted(x_valid_a_presses), sorted(y_valid_a_presses))

        # See if the options are valid
        if len(intersection_a) == 0 or len(intersection_b) == 0:
            return None
        if intersection_a[0] * self.a[0] > prize[0] or \
            intersection_a[0] * self.a[1] > prize[1]:
            return None

        a_press_offset = intersection_a[0]
        a_press_interval = intersection_a[1] - intersection_a[0]
        b_press_offset = intersection_b[0]
        b_press_interval = intersection_b[1] - intersection_b[0]

        minimize_a = True

        # if minimize_a - press_offset represents the current a presses,
        # otherwise it represents the current b presses
        press_offset = a_press_offset if minimize_a else b_press_offset
        press_inter = a_press_interval if minimize_a else b_press_interval

        #print("Off inter ", press_offset, press_inter)

        # if minimize_a - cur_butt is the a button, otherwise it's b
        cur_butt = self.a if minimize_a else self.b
        other_butt = self.b if minimize_a else self.a

        # if minimize_a - cur_cost is a button cost and other_cost is b button
        cur_cost = a_cst if minimize_a else b_cst
        other_cost = b_cst if minimize_a else a_cst

        other_presses = list()
        for ind in range(2):
            cur_press = press_offset + (press_inter * ind)
            mod_x = (prize[0] - (cur_press * cur_butt[0])) % other_butt[0]
            mod_y = (prize[1] - (cur_press * cur_butt[1])) % other_butt[1]
            if 0 != mod_x or 0 != mod_y:
                raise RuntimeError("Bad math!!!")

            other_press_x = (prize[0] - (cur_press * cur_butt[0])) / other_butt[0]
            other_press_y = (prize[1] - (cur_press * cur_butt[1])) / other_butt[1]

            other_presses.append((other_press_x, other_press_y))
        
        diff = other_presses[0][0] - other_presses[1][0], other_presses[0][1] - other_presses[1][1]
        intervals_mod = (other_presses[0][0] - other_presses[0][1]) % (diff[0] - diff[1])
        intervals = int((other_presses[0][0] - other_presses[0][1]) / (diff[0] - diff[1]))
        
        if 0 != intervals_mod:
            #print("Intervals_mod is not integer")
            return None

        #print(intervals, prize)
        cur_press = int(press_offset + (press_inter * intervals))
        other_press = int(other_presses[0][0] - (diff[0] * intervals))
        #print("FIND", cur_press, other_press)

        if cur_press < 0 or other_press < 0:
            raise RuntimeError("under zero!")
        if prize[0] != cur_press * cur_butt[0] + other_press * other_butt[0] or \
            prize[1] != cur_press * cur_butt[1] + other_press * other_butt[1]:
            raise RuntimeError("Bad result!")
        
        return cur_press * cur_cost + other_press * other_cost


        ind = 0
        if minimize_a_press:
            a_press = a_press_offset + (a_press_interval * ind)
            if 0 != (prize[0] - (self.a[0] * a_press)) % self.b[0]:
                raise RuntimeError("Bad math 2?")
            b_press = int((prize[0] - (self.a[0] * a_press)) / self.b[0])
        else:
            b_press = b_press_offset + (b_press_interval * ind)
            if 0 != (prize[0] - (self.b[0] * b_press)) % self.a[0]:
                raise RuntimeError("Bad math 2?")
            a_press = int((prize[0] - (self.b[0] * b_press)) / self.a[0])
        cost = (a_press * a_cst) + (b_press * b_cst)
        retval = (a_press, b_press, cost,
            (a_press * self.a[0] + b_press * self.b[0]) - prize[0],
            (a_press * self.a[1] + b_press * self.b[1]) - prize[1],
            )

        costs = list()
        for ind in range(10):
            a_press = a_press_offset + (a_press_interval * ind)
            if 0 != (prize[0] - (self.a[0] * a_press)) % self.b[0]:
                raise RuntimeError("Bad math 2?")
            b_press = int((prize[0] - (self.a[0] * a_press)) / self.b[0])
            if a_press > 0 and b_press > 0:
                costs.append((a_press * a_cst) + (b_press * b_cst))
            else:
                pass
                #print(f"bad {a_press} {b_press}")

        diffs = [b-a for a, b in zip(costs, costs[1:])]
        incs = [val > 0 for val in diffs]
        all_one_direction = all(incs) if incs[0] else (not any(incs))
        all_same = all(val == diffs[0] for val in diffs)
        if not all_same:
            print("NOT ALL SAME!")
        if not all_one_direction:
            print("NOT ALL ONE DIR!")
        tester_v = (((self.b[0] / b_cst) * a_cst), ((self.b[1] / b_cst) * a_cst))
        tester = tester_v[0] > self.a[0], tester_v[1] > self.a[1]
        tester2 = (tester_v[0] / self.a[0]) + (tester_v[1] / self.a[1])
        print(f"Inc {retval} {incs[0]} {tester} {tester2} {tester_v} {self}")

        if incs[0]:
            if not minimize_a_press:
                raise RuntimeError("Bad assumption incs/min")
        return costs[0]

    def find_presses(self, a_cst = 3, b_cst = 1, part2 = False):
        solutions = list()
        best_soln = None
        best_soln_cost = None

        prize = self.p if not part2 else self.p2

        max_b_presses = math.ceil(prize[0] / self.b[0])
        print(f"Machine {self}")
        print(f"Max B presses: {max_b_presses}")
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

            print(f"Valid: {a_presses_x} {b_presses}")

            solutions.append(solution)
            if best_soln is None or cost < best_soln_cost:
                best_soln = solution
                best_soln_cost = cost
        
        return best_soln_cost


def main(filename):
    with open(filename) as infile:
        lines = [line.strip() for line in infile]

    machines = [Machine(lines[i:i+3]) for i in range(0, len(lines), 4)]

    best_costs_all = [mach.find_presses_2() for mach in machines]
    #print(best_costs_all)
    best_costs = [bc for bc in best_costs_all if bc is not None]
    print(f"Part 1: {sum(best_costs)}")

    best_costs_all = [mach.find_presses_2(part2=True) for mach in machines]
    #print(best_costs_all)
    best_costs = [bc for bc in best_costs_all if bc is not None]
    print(f"Part 2: {sum(best_costs)}")

if __name__ == "__main__":
    main(sys.argv[1])
