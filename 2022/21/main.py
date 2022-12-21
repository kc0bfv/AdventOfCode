#!/usr/bin/env python3

FUNC, NUM = "func", "num"
OPERS = {
    "+": lambda a, b: a + b,
    "-": lambda a, b: a - b,
    "*": lambda a, b: a * b,
    "/": lambda a, b: a / b,
}

class Node:
    def __init__(self, line):
        words = line.split(" ")
        self.name = words[0].strip(":")
        try:
            self.num = int(words[1])
            self.num2 = int(words[1])
            self.job = NUM
        except ValueError as e:
            self.num = None
            self.num2 = None
            self.job = FUNC
            self.operand1 = words[1]
            self.operator = words[2]
            self.operand2 = words[3]

    def __str__(self):
        job_pt = f"{self.operand1}, {self.operator}, {self.operand2}" if self.job is FUNC else f"{self.num}"
        return f"{self.name} - {job_pt}"

    def solve_subtree(self, nodes):
        if self.num is None:
            assert(self.job is FUNC)
            oper1 = nodes[self.operand1].solve_subtree(nodes)
            oper2 = nodes[self.operand2].solve_subtree(nodes)
            self.num = OPERS[self.operator](oper1, oper2)
        return self.num

    def part2_subtrees(self, nodes):
        if self.name == "humn":
            return "H"
        if self.num2 is not None:
            return self.num2

        assert(self.job is FUNC)
        oper1 = nodes[self.operand1].part2_subtrees(nodes)
        oper2 = nodes[self.operand2].part2_subtrees(nodes)
        if self.name == "root":
            return f"{oper1} = {oper2}"
        elif isinstance(oper1, str) or isinstance(oper2, str):
            self.num2 = None
            return f"({oper1}{self.operator}{oper2})"
        else:
            self.num2 = OPERS[self.operator](oper1, oper2)
            return self.num2

    def part2_solver(self, nodes, cur_num):
        if self.name == "humn":
            return cur_num
        if nodes[self.operand1].num2 is not None:
            num_part = nodes[self.operand1]
            eq_part = nodes[self.operand2]
        else:
            num_part = nodes[self.operand2]
            eq_part = nodes[self.operand1]

        next_num = None
        if self.operator == "+":
            next_num = cur_num - num_part.num2
        elif self.operator == "*":
            next_num = cur_num / num_part.num2
        elif self.operator == "-":
            # check left or right side for variable...
            if nodes[self.operand2].num2:
                # left side var
                next_num = cur_num + num_part.num2
            else:
                # right side var
                next_num = -(cur_num - num_part.num2)
        elif self.operator == "/":
            # check left or right side for var, but I think it's only ever left (thank you)
            if nodes[self.operand2].num2:
                # left side var
                next_num = cur_num * num_part.num2
            else:
                # right side var
                print("div on right side notice - no biggie")
                next_num = num_part.num2 / cur_num

        if next_num is None:
            raise RuntimeError("wtf mate?")

        return eq_part.part2_solver(nodes, next_num)

def main(filename):
    with open(filename, "r") as f:
        node_list = [Node(line.strip()) for line in f]

    nodes = {node.name: node for node in node_list}

    root_num = nodes["root"].solve_subtree(nodes)
    print(f"Part 1: {root_num}")

    nodes["humn"].num2 = None
    nodes["root"].operator = "="

    root_equation = nodes["root"].part2_subtrees(nodes)
    if nodes[nodes["root"].operand1].num2 is not None:
        num_part = nodes[nodes["root"].operand1].num2
        eq_part = nodes[nodes["root"].operand2]
    else:
        num_part = nodes[nodes["root"].operand2].num2
        eq_part = nodes[nodes["root"].operand1]
    result = eq_part.part2_solver(nodes, num_part)
    print(f"Part 2: {result}")


if __name__ == "__main__":
    main("samp")
    main("input.txt")
