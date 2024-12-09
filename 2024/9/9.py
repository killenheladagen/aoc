#!/usr/bin/python3

def read_first_line_of_file(filename):
    with open(filename, "r") as f:
        return f.read().splitlines()[0]

def read_fs(filename):
    return [int(x) for x in read_first_line_of_file(filename)]

def unpack_blocks(disk_map):
    fs = []
    id_ = 0
    is_file = True
    for num in disk_map:
        if is_file:
            assert(num > 0)
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

def compress_blocks(filename):
    return move_blocks_left(unpack_blocks(read_fs(filename)))

def checksum(fs):
    return sum([i*v if v else 0 for i,v in enumerate(fs)])

def fs_checksum(filename):
    return checksum(compress_blocks(filename))

assert(fs_checksum("test.txt") == 1928)
res_a = fs_checksum("input.txt")
print(res_a)

def contiguous_length_right(i, fs):
    res = 1
    x = fs[i]
    while True:
        i += 1
        if i >= len(fs) or x != fs[i]:
            return res
        res += 1

def contiguous_length_left(i, fs):
    res = 1
    x = fs[i]
    while True:
        i -= 1
        if x != fs[i]:
            return res
        res += 1

def find_empty_contiguous_slot(space_needed, fs, max_i):
    i = 0
    while True:
        i = next_empty_slot(i, fs)
        if not i or i > max_i:
            return None
        sz = contiguous_length_right(i, fs)
        if sz >= space_needed:
            return i
        i += sz

def move_files_left(fs):
    for file_id in range(fs[-1], 0, -1):
        j = fs.index(file_id)
        space_needed = contiguous_length_right(j, fs)
        leftmost_slot = find_empty_contiguous_slot(space_needed, fs, j - 1)
        # print(f"{fs[j]} x {space_needed}: {leftmost_slot}")
        if leftmost_slot:
            fs[leftmost_slot:leftmost_slot+space_needed] = fs[j:j+space_needed]
            fs[j:j+space_needed] = [0 for _ in range(space_needed)]
    return fs

def compress_files(filename):
    return move_files_left(unpack_blocks(read_fs(filename)))

def fs_defrag_checksum(filename):
    return checksum(compress_files(filename))

assert(fs_defrag_checksum("test.txt") == 2858)
res_b = fs_defrag_checksum("input.txt")
print(res_b)
