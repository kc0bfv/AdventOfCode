#!/usr/bin/env python3

DIRS_unflat = [
    [(a, 0, 0) for a in (1,-1)],
    [(0, a, 0) for a in (1,-1)],
    [(0, 0, a) for a in (1,-1)],
]
DIRS = DIRS_unflat[0] + DIRS_unflat[1] + DIRS_unflat[2]

def sides_exposed(all_coords, coord):
    sides = 0
    for dirn in DIRS:
        new_coord = (coord[0] + dirn[0], coord[1] + dirn[1], coord[2] + dirn[2])
        if new_coord not in all_coords:
            sides += 1
    return sides

def get_nearby_coords(a):
    return [(a[0]+b[0], a[1]+b[1], a[2]+b[2]) for b in DIRS]

def find_ext_surf_area(all_coords):
    min_coord = list(list(all_coords)[0])
    max_coord = list(min_coord)

    for coord in all_coords:
        for ind in range(3):
            if coord[ind] > max_coord[ind]:
                max_coord[ind] = coord[ind]
            if coord[ind] < min_coord[ind]:
                min_coord[ind] = coord[ind]

    min_coord = (min_coord[0] - 1, min_coord[1] - 1, min_coord[2] - 1)
    max_coord = (max_coord[0] + 1, max_coord[1] + 1, max_coord[2] + 1)

    frontier = set([min_coord])
    visited = set()
    surface = 0
    while len(frontier) > 0:
        new_frontier = set()
        for fr_coord in frontier:
            for near_coord in get_nearby_coords(fr_coord):
                if near_coord in all_coords:
                    surface += 1
                elif near_coord not in visited and near_coord not in frontier \
                        and near_coord[0] >= min_coord[0] and near_coord[0] <= max_coord[0] \
                        and near_coord[1] >= min_coord[1] and near_coord[1] <= max_coord[1] \
                        and near_coord[2] >= min_coord[2] and near_coord[2] <= max_coord[2]:
                    new_frontier.add(near_coord)
        visited.update(frontier)
        frontier = new_frontier

    return surface

def main(filename):
    with open(filename, "r") as f:
        lines = [line.strip() for line in f]

    coords = {tuple(int(i) for i in line.split(",")) for line in lines}

    sides = [sides_exposed(coords, coord) for coord in coords]

    print(f"Part 1: {sum(sides)}")

    print(f"Part 2: {find_ext_surf_area(coords)}")

if __name__ == "__main__":
    main("samp")
    main("samp2")
    main("input.txt")
