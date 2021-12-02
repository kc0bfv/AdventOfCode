#!/usr/bin/env python3

def readnode(intdat, stloc):
    child_count = intdat[stloc]
    meta_count = intdat[stloc+1]

    curloc = stloc + 2

    children = list()
    for i in range(child_count):
        child, curloc = readnode(intdat, curloc)
        children.append(child)

    metadata = list()
    for i in range(meta_count):
        metadata.append(intdat[curloc])
        curloc += 1

    value = 0
    if not children:
        value = sum(metadata)
    else:
        for i in metadata:
            ind = i - 1
            if 0 <= ind < len(children):
                value += children[ind]["value"]

    node = {"children": children, "metadata": metadata, "value": value}

    return node, curloc

def list_all_metadata(tree):
    node_metadata = tree["metadata"]
    for child in tree["children"]:
        node_metadata += list_all_metadata(child)

    return node_metadata

if __name__ == "__main__":
    with open("input day 8.txt") as f:
        filedat = f.read()

    indat = filedat.strip().split(" ")
    intdat = [int(i) for i in indat]

    tree, finloc = readnode(intdat, 0)

    metadata_list = list_all_metadata(tree)

    print("Metadata sum: {}".format(sum(metadata_list)))

    print("Root value: {}".format(tree["value"]))
