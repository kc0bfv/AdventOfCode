#!/usr/bin/env python3

class Tree:
    def __init__(self, height):
        self.height = int(height)
        self.visibility = False

        self.vis_dist = {
            (doRows, doBw): 0
            for doRows in (True, False)
            for doBw in (True, False)
        }

    def get_scenic(self):
        accum = 1
        for val in self.vis_dist.values():
            accum *= val
        return accum

# Vis dists is a list of how far away you can see from any height
# so that's equivalent to - how far away is the closest tree that is >= than index
def reset_vis_dists():
    return [0 for i in range(10)]
def increment_vis_dists(vis_dists):
    return [v+1 for v in vis_dists]
def update_vis_dists(vis_dists, cur_height):
    return [1 if ind <= cur_height else vis_dists[ind] for ind in range(10)]
def get_vis_dist(vis_dists, height):
    return vis_dists[height]

def update_vis(forest, doRows, doBackwards):
    row_cnt = len(forest)
    col_cnt = len(forest[0])
    ind1_len = row_cnt if doRows else col_cnt
    ind2_len = col_cnt if doRows else row_cnt
    
    for ind1 in range(ind1_len):
        max_height = -1
        vis_dists = reset_vis_dists()
        for ind2 in reversed(range(ind2_len)) if doBackwards else range(ind2_len):
            tree = forest[ind1][ind2] if doRows else forest[ind2][ind1]
            if tree.height > max_height:
                tree.visibility = True
                max_height = tree.height

            tree.vis_dist[(doRows, doBackwards)] = get_vis_dist(vis_dists, tree.height)
            vis_dists = increment_vis_dists(vis_dists)
            vis_dists = update_vis_dists(vis_dists, tree.height)

    return forest

def main(filename):
    with open(filename, "r") as f:
        lines = [l.strip() for l in f]

    forest = [[Tree(h) for h in line] for line in lines]

    for doRows in (True, False):
        for doBackwards in (False, True):
            forest = update_vis(forest, doRows, doBackwards)
            #for row in forest:
            #    print("".join(str(tree.vis_dist[(doRows, doBackwards)]) for tree in row))
            #print()

    p1 = sum(sum(1 for t in row if t.visibility) for row in forest)
    p2 = max(max(t.get_scenic() for t in row) for row in forest)

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")


if __name__ == "__main__":
    main("samp")
    main("input.txt")
