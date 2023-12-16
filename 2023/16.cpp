#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <optional>
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
        sum += count_if(s.begin(), s.end(), [](auto c) { return c == '#'; });
    }
    return sum;
}

void print(vector<string> const &world) {
    cout << "\nWORLD:";
    for (auto const &e : world)
        cout << "\n" << e;
}

using direction = char;

struct position {
    int x;
    int y;
};

auto take_step(position p, direction d) {
    switch (d) {
    case 'R':
        p.x++;
        break;
    case 'L':
        p.x--;
        break;
    case 'U':
        p.y--;
        break;
    case 'D':
        p.y++;
        break;
    default:
        cerr << "Illegal direction " << d << "\n";
        abort();
    }
    return p;
}

struct ray {
    position p;
    direction d;

    ray(position p, direction d) : p(p), d(d) {}
};

bool operator==(position a, position b) { return a.x == b.x && a.y == b.y; }

bool operator==(ray a, ray b) { return a.p == b.p && a.d == b.d; }

auto take_step(ray r) { return take_step(r.p, r.d); }

auto &posref(auto &vov, position p) { return vov[p.y][p.x]; }

bool outside(position p, vector<string> const &world) {
    return p.x < 0 || p.x >= world[0].size() || p.y < 0 || p.y >= world.size();
}

direction turn(direction d, char m) {
    switch (d) {
    case 'R':
        return m == '/' ? 'U' : 'D';
    case 'L':
        return m == '/' ? 'D' : 'U';
    case 'U':
        return m == '/' ? 'R' : 'L';
    case 'D':
        return m == '/' ? 'L' : 'R';
    default:
        cerr << "Illegal direction " << d << "\n";
        abort();
    }
}

bool trace(ray r, vector<vector<vector<ray>>> &rays, vector<string> &world,
           vector<string> &energy) {
    posref(energy, r.p) = '#';
    auto &wr = posref(world, r.p);
    if (wr == '.')
        wr = '@';
    auto &q = posref(rays, r.p);
    if (find(begin(q), end(q), r) != end(q))
        return true;
    q.push_back(r);

    bool split((wr == '|' && (r.d == 'R' || r.d == 'L')) ||
               (wr == '-' && (r.d == 'U' || r.d == 'D')));
    if (!split) {
        if (wr == '/' || wr == '\\')
            r.d = turn(r.d, wr);
        r.p = take_step(r);
        return outside(r.p, world) ? true : trace(r, rays, world, energy);
    }

    auto rs = vector<ray>{ray{r.p, wr == '|' ? 'U' : 'L'},
                          ray{r.p, wr == '|' ? 'D' : 'R'}};
    for (auto &re : rs) {
        re.p = take_step(re);
        if (!outside(re.p, world))
            trace(re, rays, world, energy);
    }
    return true;
}

int trace_and_count(ray r, vector<vector<vector<ray>>> rays,
                    vector<string> world, vector<string> energy, bool verbose) {
    if (verbose)
        print(world);
    trace(r, rays, world, energy);
    if (verbose)
        print(energy);
    if (verbose)
        print(world);
    return weight(energy);
}

void foo(bool verbose, bool maximize) {
    for (int i = 0; i < 2; i++) {
        auto fname = "16-"s + to_string(i) + ".txt";
        vector<string> world;
        vector<string> energy;
        vector<vector<vector<ray>>> rays;
        ifstream f(fname.c_str());
        for (string s; getline(f, s);) {
            world.push_back(s);
            energy.push_back(string(s.size(), '.'));
            rays.push_back(vector<vector<ray>>(s.size()));
        }

        if (maximize) {
            int wmax = 0;
            vector<ray> starts;
            int e = world.size() - 1;
            for (int i = 0; i <= e; i++) {
                starts.push_back(ray({i, 0}, 'D'));
                starts.push_back(ray({i, e}, 'U'));
                starts.push_back(ray({0, i}, 'R'));
                starts.push_back(ray({e, i}, 'L'));
            }
            for (auto s : starts) {
                auto w = trace_and_count(s, rays, world, energy, verbose);
                cout << "\n" << w;
                if (w > wmax)
                    wmax = w;
            }
            cout << "\n\nmax=" << wmax << "\n";
        } else {
            auto w =
                trace_and_count(ray({0, 0}, 'R'), rays, world, energy, verbose);
            cout << "\n" << w;
        }
    }
}

int main() {
    foo(false, false);
    foo(false, true);
    return 0;
}
