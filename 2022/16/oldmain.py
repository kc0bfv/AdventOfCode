#!/usr/bin/env python3

class BestDist:
    # Stores the fastest distance and direction to a node from a position
    def __init__(self, node_name, dist, next_node):
        self.node = node_name
        self.dist = dist
        self.next_node = next_node

    def __str__(self):
        return f"{self.node}: {self.dist} {self.next_node}"

class Node:
    def __init__(self, line):
        words = line.split(" ")
        self.name = words[1]
        self.rate = int(words[4][5:-1])
        self.links = [link.strip(",") for link in words[9:]]
        self.best_dists = dict()

    def __str__(self):
        return f"{self.name}: {self.rate} {self.links}\n  " + "\n  ".join(str(bd) for bd in self.best_dists.values())

class Graph:
    def __init__(self, nodes):
        self.nodes = {node.name: node for node in nodes}

    def find_node_min_paths(self, for_node_nm):
        visited = set()
        frontier = set([for_node_nm])
        cur_dist = 0

        self.nodes[for_node_nm].best_dists[for_node_nm] = BestDist(for_node_nm, cur_dist, None)
        
        while not frontier.issubset(visited):
            next_up = set()
            cur_dist += 1
            for exp_node_nm in frontier:
                for node_nm in self.nodes[exp_node_nm].links:
                    if for_node_nm in self.nodes[node_nm].best_dists:
                        # There shouldn't be a better dist after one is already set
                        continue
                    self.nodes[node_nm].best_dists[for_node_nm] = \
                        BestDist(for_node_nm, cur_dist, exp_node_nm)
                next_up.update(self.nodes[exp_node_nm].links)

            visited.update(frontier)
            frontier = next_up

    def find_all_min_paths(self):
        for node_nm in self.nodes:
            self.find_node_min_paths(node_nm)

    def get_valve_sum(self, valve_names):
        return sum(self.nodes[nm].rate for nm in valve_names)


def find_next_best_move(steps_remaining, graph, cur_node_nm, already_on_nms):
    best_move_dist = None
    best_move_score = 0
    for best_dist in graph.nodes[cur_node_nm].best_dists.values():
        if best_dist.node in already_on_nms:
            continue
        num_moves_reqd = best_dist.dist + 1
        moves_left = steps_remaining - num_moves_reqd
        poss_score = moves_left * graph.nodes[best_dist.node].rate

        if poss_score > best_move_score:
            best_move_dist = best_dist
            best_move_score = poss_score
    return best_move_dist, best_move_score 

def find_way_part_1_fail(graph):
    cur_node_nm = "AA"
    already_on_nms = set()
    max_steps = 30
    cur_score = 0
        
    for cur_step in range(max_steps):
        best_move_dist, best_move_score = find_next_best_move(
            max_steps - cur_step, graph, cur_node_nm, already_on_nms
            )
        if best_move_dist is None:
            print(f"Min {cur_step + 1} at {cur_node_nm} standing still")
        elif best_move_dist.next_node is None:
            print(f"Min {cur_step + 1} turning on {cur_node_nm} {best_move_score}")
            # We're on the one we want to turn on
            assert(best_move_dist.node == cur_node_nm)
            # Add the total score we'll gain
            cur_score += best_move_score
            # Turn it on
            already_on_nms.add(best_move_dist.node)
            # Don't change cur_node_nm, turning it on was our turn
        else:
            print(f"Min {cur_step + 1} at {cur_node_nm} moving towards {best_move_dist.node} via {best_move_dist.next_node}")
            # We're not on the one we want to turn on, move closer
            cur_node_nm = best_move_dist.next_node

    return cur_score

def find_way_part_1_dynprog(graph):
    max_steps = 30
    best_moves = dict()

    for cur_node_nm in graph.nodes:
        best_moves[(cur_node_nm, graph.nodes[cur_node_nm].best_dists["AA"].dist)] = None, set(), 0
    #best_moves[("AA", 0)] = None, set(), 0

    for rem_steps in range(1, max_steps+1):
        for cur_node_nm in graph.nodes:
            best_score = -1
            best_move = None

            lookup = (cur_node_nm, rem_steps-1)
            if lookup in best_moves:
                # If we take the rem_steps-1 without any changes...
                pos_move = best_moves[lookup]
                pos_move_nm, pos_valves_on, pos_score = pos_move
                new_score = pos_score + graph.get_valve_sum(pos_valves_on)

                best_score = new_score
                best_move = cur_node_nm, pos_valves_on, new_score

                # If we take the rem_steps-1 and turn on the faucet
                pos_valves_on_now = pos_valves_on.union(set([cur_node_nm]))
                new_score = pos_score + graph.get_valve_sum(pos_valves_on_now)
                if new_score > best_score:
                    best_score = new_score
                    best_move = cur_node_nm, pos_valves_on_now, new_score

            for pos_dest_nm in graph.nodes[cur_node_nm].links:
                lookup = (pos_dest_nm, rem_steps-1)
                if lookup in best_moves:
                    pos_move = best_moves[lookup]
                    pos_move_nm, pos_valves_on, pos_score = pos_move
                    new_score = pos_score + graph.get_valve_sum(pos_valves_on)

                    if new_score > best_score:
                        best_score = new_score
                        best_move = pos_dest_nm, pos_valves_on, new_score
            
            if best_move is not None:
                best_moves[(cur_node_nm, rem_steps)] = best_move

    #print(best_moves)
    #for key in best_moves:
    #    if key[1] == max_steps-1:
    #        print(key, best_moves[key])

    best_end, best_score = None, 0
    for cur_node_nm in graph.nodes:
        bm = best_moves[(cur_node_nm, max_steps)]
        if bm[2] > best_score:
            best_end = cur_node_nm
            best_score = bm[2]

    print()
    cur_pt = best_end
    for ind in range(max_steps, 0-1, -1):
        move = best_moves[(cur_pt, ind)]
        print(cur_pt, move)
        cur_pt = move[0]


def main(filename, end):
    with open(filename, "r") as f:
        lines = [l.strip() for l in f]

    graph = Graph(Node(line) for line in lines)

    graph.find_all_min_paths()
    
    #for node in graph.nodes.values():
    #    print(node)

    score = find_way_part_1_dynprog(graph)
    print(f"Part 1: {score}")
    
    bd = graph.nodes[end].best_dists["AA"]
    print(f"{bd.dist}")


if __name__ == "__main__":
    main("samp", "DD")
    #main("input.txt", "MD")
