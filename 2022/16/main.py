#!/usr/bin/env pypy3

import time

class Link:
    def __init__(self, ends, weight):
        self.ends = ends
        self.weight = weight

    def get_other_end(self, end):
        if end not in self.ends:
            raise RuntimeError(f"Invalid end {end} in {self.ends})")
        return self.ends[1] if end == self.ends[0] else self.ends[0]

    def __str__(self):
        return f"Link {self.ends[0]}-{self.ends[1]} weight {self.weight}"

class NodeDist:
    def __init__(self, node_name, dist, next_node):
        self.node_name = node_name
        self.dist = dist
        self.next_node = next_node

    def __str__(self):
        return f"Dist to {self.node_name} {self.dist} nexthop {self.next_node}"

class Node:
    def __init__(self, name, rate, links):
       self.name = name
       self.rate = rate
       self.links = links
       self.dists = dict()
       self.simplinks = list()

    def __str__(self):
        return f"{self.name}: {self.rate}\n  " + \
            "\n  ".join(str(link) for link in self.links) + "\n\n  " + \
            "\n  ".join(str(dist) for dist in self.dists.values()) + "\n\n  " + \
            "\n  ".join(str(link) for link in self.simplinks)

class Graph:
    def __init__(self, nodes):
        self.nodes = {node.name: node for node in nodes}
        self.find_all_best_dists()
        self.simplify_links()

    def simplify_links(self):
        for node_nm, node in self.nodes.items():
            node.simplinks = list()
            for dest_node_nm, dest_nd in node.dists.items():
                if self.nodes[dest_node_nm].rate <= 0:
                    continue
                node.simplinks.append(Link((node_nm, dest_node_nm), dest_nd.dist))

    def find_node_best_dists(self, src_node_nm):
        visited = set()
        frontier = set([src_node_nm])
        cur_dist = 0

        self.nodes[src_node_nm].dists[src_node_nm] = NodeDist(src_node_nm, cur_dist, None)

        while not frontier.issubset(visited):
            next_up = set()
            cur_dist += 1
            for front_node_nm in frontier:
                for front_node_link in self.nodes[front_node_nm].links:
                    link_end = front_node_link.get_other_end(front_node_nm)
                    if src_node_nm in self.nodes[link_end].dists.keys():
                        # There shouldn't be a better dist after one is already set
                        continue
                    self.nodes[link_end].dists[src_node_nm] = \
                        NodeDist(src_node_nm, cur_dist, front_node_nm)
                next_up.update(link.get_other_end(front_node_nm)
                    for link in self.nodes[front_node_nm].links)
            
            visited.update(frontier)
            frontier = next_up

    def find_all_best_dists(self):
        for node_nm in self.nodes.keys():
            self.find_node_best_dists(node_nm)

    def __str__(self):
        return "\n\n".join(str(node) for node in self.nodes.values())

def dfs_simp(graph, visited, cur_node_nm, cur_weight, max_weight):
    # returns best (score, path)
    best_score = 0
    best_path = list()
    for link in graph.nodes[cur_node_nm].simplinks:
        dest_nm = link.get_other_end(cur_node_nm)
        if dest_nm in visited:
            continue
        weight_used = cur_weight + link.weight + 1
        weight_left = max_weight - weight_used
        if weight_left < 0:
            continue
        immed_score = weight_left * graph.nodes[dest_nm].rate
        (recur_score, recur_path) = dfs_simp(graph, visited.union([dest_nm]), dest_nm, weight_used, max_weight)
        score = immed_score + recur_score
        if score > best_score:
            best_score = score
            best_path = [dest_nm] + recur_path
    return (best_score, best_path)

def dfs_simp_2(graph, visited, cur_node_nm_1, cur_node_nm_2, cur_weight, max_weight, burn_weight_1, burn_weight_2):
    # returns best (score, path_1, path_2)
    best_score = 0
    best_path_1 = list()
    best_path_2 = list()

    if cur_weight > max_weight:
        return (best_score, best_path_1, best_path_2)

    if burn_weight_1 > 0:   
        simplinks_1 = ["BURN"]
    else:
        simplinks_1 = graph.nodes[cur_node_nm_1].simplinks + ["DEF_RUN"]

    if burn_weight_2 > 0:
        simplinks_2 = ["BURN"]
    else:
        simplinks_2 = graph.nodes[cur_node_nm_2].simplinks + ["DEF_RUN"]

    def_ran = False
    for link_1 in simplinks_1:
        temp_visit = set(visited)

        if link_1 == "BURN":
            immed_score_1 = 0
            dest_nm_1 = cur_node_nm_1
            # temp_visit leave alone
            new_burn_weight_1 = max(burn_weight_1 - 1, 0)
        elif link_1 == "DEF_RUN":
            if def_ran:
                continue
            immed_score_1 = 0
            dest_nm_1 = cur_node_nm_1
            # temp_visit leave alone
            new_burn_weight_1 = max(burn_weight_1 - 1, 0)
        else:
            dest_nm_1 = link_1.get_other_end(cur_node_nm_1)
            if dest_nm_1 in temp_visit:
                continue
            weight_used_1 = cur_weight + link_1.weight + 1
            weight_left_1 = max_weight - weight_used_1
            if weight_left_1 < 0:
                continue

            immed_score_1 = weight_left_1 * graph.nodes[dest_nm_1].rate
            temp_visit.add(dest_nm_1)
            new_burn_weight_1 = link_1.weight

        for link_2 in simplinks_2:
            temp_visit_2 = set(temp_visit)
            if link_2 == "BURN":
                immed_score_2 = 0
                dest_nm_2 = cur_node_nm_2
                # temp_visit_2 leave alone
                new_burn_weight_2 = burn_weight_2 - 1
            elif link_2 == "DEF_RUN":
                if def_ran or link_1 == "DEF_RUN":
                    continue
                immed_score_2 = 0
                dest_nm_2 = cur_node_nm_2
                # temp_visit_2 leave alone
                new_burn_weight_2 = max(burn_weight_2 - 1, 0)
            else:
                dest_nm_2 = link_2.get_other_end(cur_node_nm_2)
                if dest_nm_2 in temp_visit_2:
                    continue

                weight_used_2 = cur_weight + link_2.weight + 1
                weight_left_2 = max_weight - weight_used_2
                if weight_left_2 < 0:
                    continue

                immed_score_2 = weight_left_2 * graph.nodes[dest_nm_2].rate
                temp_visit_2.add(dest_nm_2)
                new_burn_weight_2 = link_2.weight

            immed_score = immed_score_1 + immed_score_2

            (recur_score, recur_path_1, recur_path_2) = dfs_simp_2(graph, temp_visit_2, dest_nm_1, dest_nm_2, cur_weight + 1, max_weight, new_burn_weight_1, new_burn_weight_2)
            def_ran = True

            score = immed_score + recur_score
            if score > best_score:
                best_score = score
                best_path_1 = [dest_nm_1] + recur_path_1
                best_path_2 = [dest_nm_2] + recur_path_2
       
    return (best_score, best_path_1, best_path_2)

def line_to_node(line):
    words = line.split(" ")
    name = words[1]
    rate = int(words[4][5:-1])
    links = [Link((name, link.strip(",")), 1) for link in words[9:]]
    return Node(name, rate, links)

def main(filename, p2_rnds):
    with open(filename, "r") as f:
        nodes = [line_to_node(l.strip()) for l in f]

    initial_graph = Graph(nodes)

    print(initial_graph)

    best_score, best_path = dfs_simp(initial_graph, set(["AA"]), "AA", 0, 30)
    print("Part 1: ", best_score, best_path)

    st_time = time.time()
    best_score, best_path_1, best_path_2 = dfs_simp_2(initial_graph, set(["AA"]), "AA", "AA", 0, p2_rnds, 0, 0)
    print("Part 2: ", best_score, best_path_1, best_path_2)
    print(f"P2 time: {time.time() - st_time}")

    return best_score

if __name__ == "__main__":
    assert(main("samp", 26) == 1707)
    main("input.txt", 26)
