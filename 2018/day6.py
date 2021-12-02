#!/usr/bin/env python3

from collections import defaultdict

def find_closest_pt(x, y, points):
    #print(x, y)
    dist = [abs(point[0]-x) + abs(point[1]-y) for point in points]
    #print(dist)
    minval = min(dist)
    minind = dist.index(minval)
    #print(minval, minind)
    try:
        dist.index(minval, minind+1)
        # If we don't get ValueError, then we found at least 2 its closest to
        return None
    except ValueError as e:
        return minind
    raise RuntimeError("Shouldn't get here")

def sum_distances(x, y, points):
    #print(x, y)
    dist = [abs(point[0]-x) + abs(point[1]-y) for point in points]
    #print(dist)
    return sum(dist)
    minval = min(dist)
    minind = dist.index(minval)
    #print(minval, minind)
    try:
        dist.index(minval, minind+1)
        # If we don't get ValueError, then we found at least 2 its closest to
        return None
    except ValueError as e:
        return minind
    raise RuntimeError("Shouldn't get here")

if __name__ == "__main__":
    with open("input day 6.txt") as f:
        lines = [line for line in f]

    points = [(int(line.split(",")[0].strip()), 
                int(line.split(",")[1].strip()))
        for line in lines]
    print(points)

    xs, ys = list(zip(*points))
    x_extents = (min(xs), max(xs))
    y_extents = (min(ys), max(ys))

    if x_extents[0] < 1 or y_extents[0] < 1:
        raise RuntimeError("Unexpected mins!")


    # Why was this failing and setting every row the same?
    """
    board = [[None]*(x_extents[1] + 1)]*(y_extents[1] + 1)
    for cury in range(len(board)):
        for curx in range(len(board[0])):
            board[cury][curx] = find_closest_pt(curx, cury, points)
    """

    board = [[find_closest_pt(curx, cury, points) for curx in range(x_extents[1]+1)] for cury in range(y_extents[1] + 1)]

    if board[0] == board[200]:
        raise RuntimeError("eq")

    # Find infinite areas...
    inf_areas = set()
    for cury, _ in enumerate(board):
        if board[cury][0] is not None:
            inf_areas.add(board[cury][0])
        if board[cury][-1] is not None:
            inf_areas.add(board[cury][-1])

    for curx, _ in enumerate(board[0]):
        if board[0][curx] is not None:
            inf_areas.add(board[0][curx])
        if board[-1][curx] is not None:
            inf_areas.add(board[-1][curx])

    #print(board)
    print(inf_areas)

    area_sizes = defaultdict(int)
    for row in board:
        for entry in row:
            if entry not in inf_areas:
                area_sizes[entry] += 1

    print("Part 1: {}".format(max(area_sizes.values())))
    print(area_sizes)

    board2 = [[sum_distances(curx, cury, points) for curx in range(x_extents[1]+1)] for cury in range(y_extents[1] + 1)]

    area = 0
    for row in board2:
        for entry in row:
            if entry < 10000:
                area += 1
    print("Part 2: {}".format(area))
