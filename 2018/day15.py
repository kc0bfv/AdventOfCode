#!/usr/bin/env python3

# In "reading" order
SURR_OFFSETS = [(-1, 0), (0, -1), (0, 1), (1, 0)]

WALL="#"
GOBLIN="G"
ELF="E"
SPACE="."

PART2 = True

class Position:
    CHAR = "**********"
    def __init__(self, ignore=None):
        pass
    def __str__(self):
        return self.CHAR

class Wall(Position):
    CHAR = WALL

class Space(Position):
    CHAR = SPACE

class Player(Position):
    CHAR = "&&&&&&&&&"
    def __init__(self, ignore=None):
        self.hp = 200
        self.ap = 3

class Elf(Player):
    CHAR = ELF
    def __init__(self, ap=None):
        super().__init__(ap)
        self.ap = ap if ap else 3
        self.enemy = Goblin

class Goblin(Player):
    CHAR = GOBLIN

def pos_factory(pos, elf_ap=None):
    pos_dict = {WALL: Wall, GOBLIN: Goblin, ELF: Elf, SPACE: Space}
    return pos_dict[pos](elf_ap)

def setup_board(inlines, elf_ap=None):
    return [[pos_factory(pos, elf_ap) for pos in line] for line in inlines]

def nearby_sqs(i, j):
    return ((i+offi, j+offj) for (offi, offj) in SURR_OFFSETS)

def enemy_type(player):
    if not isinstance(player, Player):
        raise RuntimeError("Non Player player!")
    return Goblin if isinstance(player, Elf) else Elf

def find_targets(board, player):
    enemy = enemy_type(player)
    return [(i,j) for i in range(len(board)) for j in range(len(board[0]))
            if isinstance(board[i][j], enemy)]

def id_inrange_sqs(board, targets):
    return {(i, j) for (tgti, tgtj) in targets
            for (i,j) in nearby_sqs(tgti, tgtj)
            if isinstance(board[i][j], Space)
            }

def already_in_range(targets, i, j):
    return any(((i, j) in targets) for (i, j) in nearby_sqs(i, j))

def hit_nearby(board, i, j):
    enemy = enemy_type(board[i][j])

    nearby_enemies = [(eni, enj) for (eni, enj) in nearby_sqs(i, j)
            if isinstance(board[eni][enj], enemy)]

    if nearby_enemies == []:
        raise RuntimeError("empty enemies in hit_nearby")

    enemy_powers = [board[eni][enj].hp for (eni, enj) in nearby_enemies]
    tgti, tgtj = nearby_enemies[enemy_powers.index(min(enemy_powers))]

    board[tgti][tgtj].hp -= board[i][j].ap
    
    if board[tgti][tgtj].hp <= 0:
        #print("Enemy died at {} {}".format(tgti, tgtj))
        board[tgti][tgtj] = Space()

    return board

def unlab_space_surroundings(board, labeled, targets):
    return {(i, j) for (tgti, tgtj) in targets
            for (i, j) in nearby_sqs(tgti, tgtj)
            if (isinstance(board[i][j], Space) and labeled[i][j] is None)
            }

def label_board(board, targets, i, j):
    labeled = [[None for _ in range(len(board[0]))] for _ in range(len(board))]
    
    tgt_surroundings = unlab_space_surroundings(board, labeled, targets)
    dist = 0
    while tgt_surroundings != set():
        dist += 1
        for tgti, tgtj in tgt_surroundings:
            labeled[tgti][tgtj] = dist
        tgt_surroundings = unlab_space_surroundings(board, labeled, tgt_surroundings)
    return labeled

def make_move(board, targets, i, j):
    labeled_board = label_board(board, targets, i, j)
    surround_labels = [labeled_board[offi][offj]
            for (offi, offj) in nearby_sqs(i, j)]
    filted_labels = [i for i in surround_labels if i is not None]

    # If no moves - perhaps because nothing is reachable...
    if len(filted_labels) == 0:
        return i, j, board
    diri, dirj = SURR_OFFSETS[surround_labels.index(min(filted_labels))]
    
    newi, newj = i+diri, j+dirj
    temp = board[newi][newj]
    if not isinstance(temp, Space):
        raise RuntimeError("Tried to move non-space")
    board[newi][newj] = board[i][j]
    board[i][j] = temp
    return newi, newj, board

def take_turn(board, i, j):
    targets = find_targets(board, board[i][j])
    if targets == []:
        return board, False

    open_range_sqs = id_inrange_sqs(board, targets)
    if open_range_sqs == set() and not already_in_range(targets, i, j):
        return board, True

    if not already_in_range(targets, i, j):
        i, j, board = make_move(board, targets, i, j)

    if not already_in_range(targets, i, j):
        return board, True

    board = hit_nearby(board, i, j)
    return board, True

def next_round(board):
    cur_board = [[i for i in j] for j in board]
    for i in range(len(board)):
        for j in range(len(board[0])):
            if isinstance(board[i][j], Player) and not \
                    isinstance(cur_board[i][j], Space):
                cur_board, targets_found = take_turn(cur_board, i, j)
                if not targets_found:
                    return cur_board, False
    return cur_board, True

def print_board(board):
    for i in range(len(board)):
        print("".join(str(j) for j in board[i]))

def all_hit_points(board):
    return (j.hp for i in board for j in i if isinstance(j, Player))

def count_elves(board):
    return len([j for i in board for j in i if isinstance(j, Elf)])

def run_to_completion(board, animate=False):
    targets_found = True
    rounds = 0
    while targets_found:
        rounds += 1
        board, targets_found = next_round(board)
        if animate:
            print_board(board)
    sum_hp = sum(all_hit_points(board))
    print("Hitpoint sum: {} Rounds: {} Result: {}".format(
        sum_hp, rounds-1, (rounds-1) * sum_hp)
        )
    return board

if __name__ == "__main__":
    with open("input day 15.txt") as f:
        lines = [line.strip() for line in f]

    animate = False

    board = setup_board(lines)
    run_to_completion(board, animate)

    elf_hp = 2
    init_elves, fin_elves = 0, 1
    while init_elves != fin_elves:
        elf_hp += 1
        board = setup_board(lines, elf_hp)
        init_elves = count_elves(board)
        board = run_to_completion(board, animate)
        fin_elves = count_elves(board)
        print("HP {}: {} {}".format(elf_hp, init_elves, fin_elves))

