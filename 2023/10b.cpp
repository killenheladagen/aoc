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
    case ' ':
        return "cleared";
    case '@':
        return "marked";
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

char dummy = '@';
auto &w(point p) {
    if (p.x < 0 || p.x >= world[0].size() || p.y < 0 || p.y >= world.size())
        return dummy;
    return world[p.y][p.x];
}

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

void print(vector<string> v) {
    cout << "\n\n";
    for (auto e : v) {
        for (auto c : e)
            cout << [&]() {
                switch (c) {
                case '-':
                    return "─"s;
                case '|':
                    return "│"s;
                case 'L':
                    return "└"s;
                case 'S': //
                case 'J':
                    return "┘"s;
                case '7':
                    return "┐"s;
                case 'F':
                    return "┌"s;
                default:
                    return string(1, c);
                }
            }();
        cout << "\n";
    }
}

int main() {
    auto s = 'J';
    print(world);
    point start(-1, -1);
    for (int y = 0; y < world.size(); y++) {
        for (int x = 0; x < world[y].size(); x++) {
            if (world[y][x] == 'S')
                start = point(x, y);
        }
    }
    auto world2 = world;
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
        auto last_here = here;
        here = take_step(here, last_dir);
        // cout << last_dir << " -> " << here << "\n";
        w(last_here) = '@';
        len++;
    }
    cout << len << "\n";
    cout << len / 2 << "\n";

    vector<point> todo = {point(0, 0)};
    int iter = 0;
    do {
        bool verbose = iter >= 1489000;
        iter++;
        if (verbose) {
            cerr << iter << ": ";
            if (0) {
                cerr << "todo: ";
                for (auto e : todo)
                    cerr << e << ",";
                cerr << "\n";
            } else {
                cerr << "todo: " << todo.size() << "\n";
            }
        }
        if (verbose) {
            cerr << "_\n";
        }
        point toclear = todo.back();
        todo.pop_back();
        for (auto dir : "NSEW"s) {
            auto p = take_step(toclear, dir);
            char &c = w(p);
            if (verbose)
                cerr << "check " << c << "\n";
            if (c != ' ' && c != '@') {
                c = ' ';
                if (verbose)
                    cerr << "push " << p << "\n";
                todo.push_back(p);
            }
        }
        w(toclear) = ' ';
        world2[toclear.y][toclear.x] = ' ';
    } while (todo.size());

    auto candidates = world;
    print(candidates);
    for (int x = 0; x < world[0].size(); x++)
        for (int y = 0; y < world.size(); y++) {
            char c = candidates[y][x];
            if (c != ' ' && c != '@')
                world2[y][x] = '@';
        }

    world = world2;
    print(world);

    for (int x = 0; x < world[0].size(); x++)
        for (int y = 0; y < world.size(); y++) {
            char c = world[y][x];
            if (c == '@') {
                if (c != ' ' && c != '@')
                    world2[y][x] = '@';
            }
        }
    return 0;
}
