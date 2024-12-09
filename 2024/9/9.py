#!/usr/bin/python3

def read_first_line_of_file(filename):
    with open(filename, "r") as f:
        return f.read().splitlines()[0]

def unpack_blocks(disk_map):
    fs = []
    id_ = 0
    is_file = True
    for num in disk_map:
        fs += [ id_ if is_file else None for _ in range(num) ]
        id_ += 1 if is_file else 0
        is_file = not is_file
    return fs

def next_empty_slot(i, fs):
    while fs[i] != None:
        i += 1
        if i >= len(fs):
            return None
    return i

def move_blocks_left(fs):
    i = 0
    while True:
        i = next_empty_slot(i, fs)
        if not i:
            return fs
        fs[i] = fs[-1]
        del fs[-1]

def compress_fs(filename):
    return move_blocks_left(unpack_blocks([int(x) for x in read_first_line_of_file(filename)]))

def fs_checksum(filename):
    return sum([i*v for i,v in enumerate(compress_fs(filename))])

assert(fs_checksum("test.txt") == 1928)
print(fs_checksum("input.txt"))
