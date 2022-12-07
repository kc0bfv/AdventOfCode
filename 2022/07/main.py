#!/usr/bin/env python3

from enum import Enum
import operator

class DirStructType(Enum):
    Dir = 0
    File = 1

class DirStruct:
    def __init__(self, line, parent):
        part0, part1 = line.split(" ")
        if part0 == "dir":
            self.type = DirStructType.Dir
            self.name = part1
            self.__file_size = None
            self.sub_dirstructs = dict()
            self.parent = parent
        else:
            self.type = DirStructType.File
            self.name = part1
            # fail w/valueerror if not an int :-)
            self.__file_size = int(part0)
            self.sub_dirstructs = None
            self.parent = parent

    @property
    def size(self):
        # NOTE - do not run this until full dir struct is done.  Ok, that's a bug.
        if self.type == DirStructType.Dir:
            if self.__file_size == None:
                self.__file_size = sum(sub.size for sub in self.sub_dirstructs.values())
            return self.__file_size
        elif self.type == DirStructType.File:
            return self.__file_size
        else:
            raise RuntimeError("invalid type")

    def add_sub(self, line):
        sub = DirStruct(line, self)
        self.sub_dirstructs[sub.name] = sub

    def cd(self, loc):
        if loc == "..":
            return self.parent
        return self.sub_dirstructs[loc]

    def __str__(self):
        if self.type == DirStructType.Dir:
            retval = f"{self.name} - subdirsize {self.size}\n"
            sub_dirs = [str(val) for val in self.sub_dirstructs.values()]
            sub_dirs_join = "\n".join(sub_dirs)
            retval += "\n".join(f"  {line}" for line in sub_dirs_join.split("\n"))
        elif self.type == DirStructType.File:
            retval = f"{self.name} - {self.size}"
        return retval

    def dir_search_cmp_size(self, under_size, cmp_oper):
        dirs_found = list()
        if self.type == DirStructType.File:
            return dirs_found
        elif self.type == DirStructType.Dir:
            if cmp_oper(self.size, under_size):
                dirs_found.append(self)
            for sub_elem in self.sub_dirstructs.values():
                dirs_found += sub_elem.dir_search_cmp_size(under_size, cmp_oper)
            return dirs_found
        else:
            raise RuntimeError("invalid dst")

    def dir_search_over_size(self, under_size):
        return self.dir_search_cmp_size(under_size, operator.ge)

    def dir_search_under_size(self, under_size):
        return self.dir_search_cmp_size(under_size, operator.le)

def drain_ls(line_iter):
    line = next(line_iter)
    subs = list()
    try:
        while line[0] != "$":
            subs.append(line)
            line = next(line_iter)
    except StopIteration:
        # Dumb hack here to get the last dir
        line = None
    return subs, line

def build_dir_struct(lines):
    base_dir = DirStruct("dir /", None)
    cur_dir = base_dir

    line_iter = iter(lines)

    try:
        line = next(line_iter)

        while True:
            if line[0] != "$":
                raise RuntimeError("didn't start with command...")
            cmd = line.split(" ")
            if cmd[1] == "cd":
                if cmd[2] == "/":
                    cur_dir = base_dir
                else:
                    cur_dir = cur_dir.cd(cmd[2])
                line = next(line_iter)
            elif cmd[1] == "ls":
                subs, line = drain_ls(line_iter)
                [cur_dir.add_sub(sub) for sub in subs]

                # Dumb hack to get last dir
                if line == None:
                    raise StopIteration
            else:
                raise RuntimeError("unknown cmd")
    except StopIteration:
        pass

    return base_dir

        

def main(filename):
    with open(filename, "r") as f:
        lines = [line.strip() for line in f]

    root_dir = build_dir_struct(lines)

    p1_dirs = root_dir.dir_search_under_size(100000)
    p1_size = sum(sub.size for sub in p1_dirs)
    print(f"Part 1: {p1_size}")

    min_space_to_free = root_dir.size - 40000000 
    p2_dirs = root_dir.dir_search_over_size(min_space_to_free)
    p2_min = min(p2_dirs, key=(lambda entry: entry.size))

    print(f"Part 2: {p2_min.size}")

if __name__ == "__main__":
    main("samp")
    main("input.txt")
