#!/usr/bin/env python3

class DblLLNode:
    def __init__(self, value):
        self.value = value
        self.prev = None
        self.next = None

class CircDblLL:
    def __init__(self, head):
        self.head = head
        self.head.next = head
        self.head.prev = head

    def append(self, newtail):
        newtail.prev = self.head.prev
        newtail.next = self.head
        self.head.prev.next = newtail
        self.head.prev = newtail

    def shift_right(self, node):
        prev_node_new_next = node.next
        next_node_new_prev = node.prev

        next_node_new_next = node

        node_new_prev = node.next
        node_new_next = node.next.next
        new_next_node_prev = node
        
        node.prev.next = prev_node_new_next
        node.next.prev = next_node_new_prev
        node.next.next = next_node_new_next

        node.prev = node_new_prev
        node.next = node_new_next
        node.next.prev = new_next_node_prev

        if self.head == node:
            self.head = node.prev
        elif self.head.prev == node:
            self.head = node

    def shift_left(self, node):
        prev_node_new_next = node.next
        next_node_new_prev = node.prev

        prev_node_new_prev = node

        node_new_prev = node.prev.prev
        node_new_next = node.prev
        new_prev_node_next = node
        
        node.prev.next = prev_node_new_next
        node.next.prev = next_node_new_prev
        node.prev.prev = prev_node_new_prev

        node.prev = node_new_prev
        node.next = node_new_next
        node.prev.next = new_prev_node_next

        if self.head == node:
            self.head = node.next.next

    def __str__(self):
        ret = f"{self.head.value} "
        cur = self.head.next
        while cur != self.head:
            ret += f"{cur.value} "
            cur = cur.next
        return ret

def run_mixer(orignodes, circlist):
    for node in orignodes:
        for _ in range(abs(node.value) % (len(orignodes)-1)):
            if node.value > 0:
                circlist.shift_right(node)
            elif node.value < 0:
                circlist.shift_left(node)
            else:
                raise RuntimeError("Invalid value")
        #print(node.value)
        #print(circlist)

def main(filename):
    with open(filename, "r") as f:
        values = [int(val.strip()) for val in f]

    mult = 1
    orignodes = [DblLLNode(values[0] * mult)]
    circlist = CircDblLL(orignodes[0])
    #print(circlist)
    for val in values[1:]:
        orignodes.append(DblLLNode(val * mult))
        circlist.append(orignodes[-1])
        #print(circlist)

    run_mixer(orignodes, circlist)

    # Find 0
    cur = circlist.head
    while cur.value != 0:
        cur = cur.next

    vals = []
    for _ in range(3):
        # Go 1000
        for _ in range(1000):
            cur = cur.next
        vals.append(cur.value)

    print(vals)
    print("Part 1: ", sum(vals))

    mult = 811589153
    orignodes = [DblLLNode(values[0] * mult)]
    circlist = CircDblLL(orignodes[0])
    #print(circlist)
    for val in values[1:]:
        orignodes.append(DblLLNode(val * mult))
        circlist.append(orignodes[-1])
        #print(circlist)

    for ind in range(10):
        print("round", ind)
        run_mixer(orignodes, circlist)

    # Find 0
    cur = circlist.head
    while cur.value != 0:
        cur = cur.next

    vals = []
    for _ in range(3):
        # Go 1000
        for _ in range(1000):
            cur = cur.next
        vals.append(cur.value)

    print(vals)
    print("Part 2: ", sum(vals))

if __name__ == "__main__":
    main("samp")
    main("input.txt")
