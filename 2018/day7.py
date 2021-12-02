#!/usr/bin/env python3

import re
from collections import defaultdict

if __name__ == "__main__":
    with open("input day 7.txt") as f:
        lines = [line for line in f]

    ptrn = re.compile("Step ([A-Z]) must be finished before "
            "step ([A-Z]) can begin.")
    line_grps = [ptrn.search(line).groups() for line in lines]

    possible_starts = set()

    tree_dict = defaultdict(list)
    precond_dict = defaultdict(list)
    for st, ed in line_grps:
        tree_dict[st].append(ed)
        precond_dict[ed].append(st)
        possible_starts.add(st)

    # find starting point of tree... the entries that nobody points to
    for child_list in tree_dict.values():
        for child in child_list:
            possible_starts.discard(child)

    print("Possible starts: {}".format(possible_starts))

    # run process...
    output = ""
    possible_nexts = set(possible_starts)

    num_workers = 5
    worker_free = [True] * num_workers
    worker_cur_task = [None] * num_workers
    worker_task_fin_time = [None] * num_workers

    cur_time = 0

    while possible_nexts or not all(worker_free):
        # Free up any workers that have finished
        free_workers = [i for (i, fintime) in enumerate(worker_task_fin_time)
                if fintime == cur_time]
        if len(free_workers) > 1:
            print("Multiple free workers...")
        for i in free_workers:
            output += worker_cur_task[i]
            possible_nexts.update(tree_dict[worker_cur_task[i]])
            worker_free[i] = True
            worker_cur_task[i] = None
            worker_task_fin_time[i] = None

        # Assign workers...
        assigned_some_worker = False
        if any(worker_free):
            next_worker = worker_free.index(True)
            # Find the next task
            real_next = None
            for next_go in sorted(list(possible_nexts)):
                if next_go in output:
                    continue
                not_yet = False
                for precon in precond_dict[next_go]:
                    if precon not in output:
                        not_yet = True
                if not not_yet:
                    real_next = next_go
                    break
            if real_next is not None:
                #output += real_next
                possible_nexts.discard(real_next)
                task_len = 60 + (ord(real_next) - ord("A") + 1)
                worker_free[next_worker] = False
                worker_cur_task[next_worker] = real_next
                worker_task_fin_time[next_worker] = cur_time + task_len
                assigned_some_worker = True

        # Don't increment time while some worker could be assigned...
        if not assigned_some_worker:
            cur_time += 1

    print("Part 2 time: {}".format(cur_time-1))
    print("Part 2: {}".format(output))
