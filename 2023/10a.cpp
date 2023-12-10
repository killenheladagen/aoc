#include <algorithm>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <tuple>
#include <vector>
using namespace std;

/*

| is a vertical pipe connecting north and south.
- is a horizontal pipe connecting east and west.
L is a 90-degree bend connecting north and east.
J is a 90-degree bend connecting north and west.
7 is a 90-degree bend connecting south and west.
F is a 90-degree bend connecting south and east.
. is ground; there is no pipe in this tile.
S is the starting position of the animal; there is a pipe on this tile, but your
sketch doesn't show what shape the pipe has.

S: J
 --7
---
 */

auto world = vector<string>{
#include "10.txt"
};

/*
 */
auto exits(char c) {
    switch (c) {
    case '|':
        return "NS";
    case '-':
        return "WE";
    case 'L':
        return "NE";
    case 'S': //
    case 'J':
        return "NW";
    case '7':
        return "SW";
    case 'F':
        return "SE";
    }
    cout << "Bad pipe: " << c << "\n";
    abort();
}

auto opposite(char c) {
    switch (c) {
    case 'N':
        return 'S';
    case 'S':
        return 'N';
    case 'E':
        return 'W';
    case 'W':
        return 'E';
    }
    cout << "Bad dir: " << c << "\n";
    abort();
}

struct point {
    int x, y;

    point(int x, int y) : x(x), y(y) {}
};

auto w(point p) { return world[p.y][p.x]; }

auto operator!=(point a, point b) { return a.x != b.x || a.y != b.y; }

auto &operator<<(ostream &os, point p) {
    return os << "(" << p.x << "," << p.y << "):" << w(p) << " " << exits(w(p));
}

point take_step(point p, char dir) {
    switch (dir) {
    case 'N':
        return {p.x, p.y - 1};
    case 'S':
        return {p.x, p.y + 1};
    case 'E':
        return {p.x + 1, p.y};
    case 'W':
        return {p.x - 1, p.y};
    }
    cout << "Bad dir: " << dir << "\n";
    abort();
}

int main() {
    auto s = 'J';
    point start(-1, -1);
    for (int y = 0; y < world.size(); y++) {
        for (int x = 0; x < world[y].size(); x++) {
            if (world[y][x] == 'S')
                start = point(x, y);
        }
    }
    cout << start << "\n";
    int len = 1;
    char last_dir = exits(s)[0];
    point here = take_step(start, last_dir);
    cout << here << "\n";
    while (here != start) {
        last_dir = opposite(last_dir);
        auto dirs = exits(w(here));
        if (last_dir == dirs[0])
            last_dir = dirs[1];
        else if (last_dir == dirs[1])
            last_dir = dirs[0];
        else {
            cout << "Dirs " << dirs << " not in last_dir " << last_dir << "\n";
            abort();
        }
        here = take_step(here, last_dir);
        cout << last_dir << " -> " << here << "\n";
        len++;
    }
    cout << len << "\n";
    cout << len / 2 << "\n";
    return 0;
}
