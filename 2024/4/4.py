def read_character_matrix_file(filename):
    with open(filename) as f:
        return [list(x) for x in f.read().split()]

all_dirs=[]
for x in [-1,0,1]:
    for y in [-1,0,1]:
        if x or y:
            all_dirs.append([x,y])

def has_word(word, board, x, y, d, offs=0):
    for i in range(len(word)):
        (cx, cy) = (x+d[0]*(i+offs), y+d[1]*(i+offs))
        if cx < 0 or cx >= len(board[0]) or\
           cy < 0 or cy >= len(board) or\
           word[i] != board[cy][cx]:
            return False
    return True

def count_word(word, board):
    count = 0
    for y in range(len(board)):
        for x in range(len(board[0])):
            for d in all_dirs:
                if has_word(word, board, x, y, d):
                    count += 1
    return count

def count_xmas(f):
    return count_word("XMAS", read_character_matrix_file(f))

assert(count_xmas("test.txt") == 18)
print(count_xmas("input.txt"))

def count_cross_word(word, board):
    offs = -int((len(word)-1)/2)
    count = 0
    for y in range(len(board)):
        for x in range(len(board[0])):
            for ds in [[[1,1],[1,-1]],
                       [[1,1],[-1,1]],
                       [[-1,-1],[1,-1]],
                       [[-1,-1],[-1,1]]]:
                if has_word(word, board, x, y, ds[0], offs) and\
                   has_word(word, board, x, y, ds[1], offs):
                    count += 1
    return count

def count_x_max(f):
    return count_cross_word("MAS", read_character_matrix_file(f))

assert(count_x_max("test.txt") == 9)
print(count_x_max("input.txt"))
