#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <tuple>
#include <vector>

using namespace std;

auto weight(vector<string> const &world) {
    auto h = world.size();
    auto sum = 0;
    for (int i = 0; i < h; i++) {
        auto const &s = world[i];
        sum += (h - i) *
               count_if(s.begin(), s.end(), [](auto c) { return c == 'O'; });
    }
    return sum;
}

void roll_north(vector<string> &world) {
    auto h = world.size();
    for (int i = 1; i < h; i++) {
        auto const &s = world[i];
        for (int x = 0; x < s.size(); x++) {
            if (s[x] == 'O') {
                int y = i;
                world[y][x] = '.';
                while (y > 0 && world[y - 1][x] == '.')
                    y--;
                world[y][x] = 'O';
            }
        }
    }
}

void roll_south(vector<string> &world) {
    auto h = world.size();
    for (int i = h - 1; i >= 0; i--) {
        auto const &s = world[i];
        for (int x = 0; x < s.size(); x++) {
            if (s[x] == 'O') {
                int y = i;
                world[y][x] = '.';
                while (y < (h - 1) && world[y + 1][x] == '.')
                    y++;
                world[y][x] = 'O';
            }
        }
    }
}

void roll_west(vector<string> &world) {
    auto h = world[0].size();
    for (auto &s : world) {
        for (int i = 1; i < h; i++) {
            if (s[i] == 'O') {
                int x = i;
                s[x] = '.';
                while (x > 0 && s[x - 1] == '.')
                    x--;
                s[x] = 'O';
            }
        }
    }
}

void roll_east(vector<string> &world) {
    auto h = world[0].size();
    for (auto &s : world) {
        for (int i = h - 2; i >= 0; i--) {
            if (s[i] == 'O') {
                int x = i;
                s[x] = '.';
                while (x < (h - 1) && s[x + 1] == '.')
                    x++;
                s[x] = 'O';
            }
        }
    }
}

void roll_cycle(vector<string> &world) {
    roll_north(world);
    roll_west(world);
    roll_south(world);
    roll_east(world);
}

void roll_billion(vector<string> &world) {
    vector<int> w;
    for (int i = 0; i < 1000; i++) {
        auto last_world = world;
        roll_cycle(world);
        auto x = weight(world);
        cout << "\n" << i << ": " << x;
        w.push_back(x);
        if (last_world == world)
            return;
    }

    int pos = 0;
    for (int i = 0; i < 999; i++) {
        if (w[i] == w[999])
            pos = i;
    }
    int fq = 999 - pos;
    cout << "\n freq=" << fq;
    int j = 1000000000 - 1;
    while (j > 999)
        j -= fq;
    cout << "\n w[" << j << "] = " << w[j];
}

void print(vector<string> const &world) {
    cout << "\nWORLD:";
    for (auto const &e : world)
        cout << "\n" << e;
}

int main() {
    for (int i = 0; i < 2; i++) {
        auto fname = "14-"s + to_string(i) + ".txt";
        vector<string> world;
        ifstream f(fname.c_str());
        for (string s; getline(f, s);) {
            world.push_back(s);
        }
        print(world);
        roll_north(world);
        roll_billion(world);
        print(world);
        cout << "\n" << weight(world);
        // for (auto const &e : world)
        //     cout << "\n" << e;
    }
    return 0;
}
