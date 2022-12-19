#!/usr/bin/env pypy3

import time

class Situation:
    def __init__(self, sit=None, timeleft=None, blueprint=None):
        assert(sit is not None or (timeleft is not None and blueprint is not None))
        self.ore_bots = sit.ore_bots if sit else 1
        self.clay_bots = sit.clay_bots if sit else 0
        self.obs_bots = sit.obs_bots if sit else 0
        self.geo_bots = sit.geo_bots if sit else 0
        self.time_left = sit.time_left if sit else timeleft
        self.blueprint = sit.blueprint if sit else blueprint
        self.ore = sit.ore if sit else 0
        self.clay = sit.clay if sit else 0
        self.obs = sit.obs if sit else 0
        self.geo = sit.geo if sit else 0

    def dec_time(self):
        new_sit = Situation(self)
        new_sit.time_left -= 1
        return new_sit

    def make_ore_bot(self):
        if self.ore < self.blueprint.ore_bot_ore:
            return None
        new_sit = self.dec_time()
        new_sit.collect()
        new_sit.ore_bots += 1
        new_sit.ore -= self.blueprint.ore_bot_ore
        return new_sit

    def make_clay_bot(self):
        if self.ore < self.blueprint.clay_bot_ore:
            return None
        new_sit = self.dec_time()
        new_sit.collect()
        new_sit.clay_bots += 1
        new_sit.ore -= self.blueprint.clay_bot_ore
        return new_sit

    def make_obs_bot(self):
        if self.ore < self.blueprint.obs_bot_ore or self.clay < self.blueprint.obs_bot_clay:
            return None
        new_sit = self.dec_time()
        new_sit.collect()
        new_sit.obs_bots += 1
        new_sit.ore -= self.blueprint.obs_bot_ore
        new_sit.clay -= self.blueprint.obs_bot_clay
        return new_sit

    def make_geo_bot(self):
        if self.ore < self.blueprint.geo_bot_ore or self.obs < self.blueprint.geo_bot_obs:
            return None
        new_sit = self.dec_time()
        new_sit.collect()
        new_sit.geo_bots += 1
        new_sit.ore -= self.blueprint.geo_bot_ore
        new_sit.obs -= self.blueprint.geo_bot_obs
        new_sit.geo += new_sit.time_left - 1
        return new_sit

    def make_nothing(self):
        new_sit = self.dec_time()
        new_sit.collect()
        return new_sit

    def collect(self):
        self.ore += self.ore_bots
        self.clay += self.clay_bots
        self.obs += self.obs_bots

    def __hash__(self):
        return hash((self.ore_bots, self.clay_bots, self.obs_bots, self.geo_bots, self.time_left, self.ore, self.clay, self.obs, self.geo))
    def __eq__(self, oth):
        return hash(self) == hash(oth)

class Blueprint:
    def __init__(self, line):
        words = line.split(" ")
        self.num = int(words[1].strip(":"))
        self.ore_bot_ore = int(words[6])
        self.clay_bot_ore = int(words[12])
        self.obs_bot_ore = int(words[18])
        self.obs_bot_clay = int(words[21])
        self.geo_bot_ore = int(words[27])
        self.geo_bot_obs = int(words[30])

        self.max_ore_bots_needed = max(self.ore_bot_ore, self.clay_bot_ore, self.obs_bot_ore, self.geo_bot_ore)
        self.max_clay_bots_needed = self.obs_bot_clay
        self.max_obs_bots_needed = self.geo_bot_obs

def sum_1_to_n(n):
    return int(((n + 1) * n)/2)

def possible_future_geo(bp, sit, initial_time):
    one_bound = ((sit.geo + 3) / (initial_time + 1 - sit.time_left) * initial_time)
    # Return the max possible number of end-state geodes as a bounds
    
    # How many ore would you have if you made one ore bot every round?
    future_ore_max = sit.ore + sum_1_to_n(sit.time_left - 1)

    # How many clay would you have if you made one clay bot every round?
    future_clay_max = sit.clay + sum_1_to_n(sit.time_left - 1)

    # How many obsidian would you have if you made one obsidian bot every round?
    future_obs_max = sit.obs + sum_1_to_n(sit.time_left - 1)

    # How many geodes would you have if you made one geode bot every round?
    max_future_geode_bots_by_resource = int(max(future_ore_max / bp.geo_bot_ore, future_obs_max / bp.geo_bot_obs))
    # How long could we generate bots for, given that max-by-resources?
    gen_time = min(max_future_geode_bots_by_resource, sit.time_left - 1)

    # How many geodes would we have if we made all those ASAP?
    # How many geodes would those robots generate during the build-up period?
    build_up = sum_1_to_n(gen_time - 1)
    # How many geodes after the build up?  There are 'gen_time' max robots and (time_left - gen_time) min remaining
    after_build = gen_time * (sit.time_left - gen_time) 
    return min(one_bound, sit.geo + build_up + after_build)
    #return sit.geo + sum_1_to_n(sit.time_left - 1)

def gen_all_next_steps(bp, sit, timeleft, best_so_far, initial_time):
    assert(sit.time_left == timeleft + 1)
    ret_sits = set()
    ret_sits.add(sit.make_geo_bot())
    if None not in ret_sits:
        # If we can make a geo bot, just do that - thanks schveiguy
        return ret_sits
    if sit.ore_bots < bp.max_ore_bots_needed: # thanks justsvamp
        ret_sits.add(sit.make_ore_bot())
    if sit.clay_bots < bp.max_clay_bots_needed: # thanks justsvamp
        ret_sits.add(sit.make_clay_bot())
    if sit.obs_bots < bp.max_obs_bots_needed: # thanks justsvamp
        ret_sits.add(sit.make_obs_bot())
    if None in ret_sits:
        # If we can make all the bots, don't make nothing (don't hoard) - thanks schveiguy
        ret_sits.add(sit.make_nothing())

    if None in ret_sits:
        ret_sits.remove(None)

    future_best_likely = (best_so_far.geo / (initial_time + 1 - timeleft) * initial_time)
    #return set([ret for ret in ret_sits if possible_future_geo(bp, ret, initial_time) > future_best_likely])
    #return set([ret for ret in ret_sits if ret.geo_bots >= best_so_far.geo_bots])

    return set([ret for ret in ret_sits if possible_future_geo(bp, ret, initial_time) > future_best_likely])

def bfs(bp, endtime):
    print(f"Blueprint: {bp.num}")

    initial = Situation(timeleft=endtime+1, blueprint=bp)
    frontier = set([initial])

    best = initial

    for curtime in range(endtime - 1):
        #print(curtime, len(frontier))
        new_frontier = set()
        for cursit in frontier:
            new_frontier.update(gen_all_next_steps(bp, cursit, endtime - curtime, best, endtime))
            frontier = new_frontier

        for cursit in frontier:
            if cursit.geo > best.geo:
                best = cursit

    return best  

def main(filename):
    with open(filename, "r") as f:
        bps = [Blueprint(line) for line in f]

    bests = [bfs(bp, 24) for bp in bps]

    print([best.geo for best in bests])
    print("Part 1: ", sum((i+1)*j for i, j in enumerate(best.geo for best in bests)))

    bests = [bfs(bp, 32) for bp in bps[:3]]

    print([best.geo for best in bests])
    print("Part 2: ", bests[0].geo * bests[1].geo * bests[2].geo)

if __name__ == "__main__":
    #main("samp")
    pre = time.time()
    main("input.txt")
    print("Time: ", time.time()-pre)
