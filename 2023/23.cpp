#include "helpers.h"
#include "point.h"
#include <deque>
using namespace std;

void show_path_on_map(vector<string> world, vector<point> const &path) {
    for (auto e : path)
        char_at(world, e) = '*';
    draw_map(world);
}

auto next_steps(vector<string> const &world, vector<point> const &path,
                bool slippery) {
    vector<point> steps;
    auto dim = map_dimensions(world);
    for (auto dir : "^>v<"s) {
        auto p = path.back() + dir;
        if (p.x >= 0 && p.y >= 0 && p.x < dim.x && p.y < dim.y &&
            ((slippery &&
              (char_at(world, p) == '.' || char_at(world, p) == dir)) ||
             (!slippery && char_at(world, p) != '#')))
            if (find(path.begin(), path.end(), p) == path.end())
                steps.push_back(p);
    }
    return steps;
}

struct node {
    point pos;
    bool visited = false;
    map<int, node *> nbors;
    int max_dist;
};

bool adjacent_to(point a, point b) {
    return (a.x == b.x && abs(a.y - b.y) == 1) ||
           (a.y == b.y && abs(a.x - b.x) == 1);
}

int longest_path(vector<string> const &world, point const &start,
                 point const &finish) {
    map<int64_t, node> all;
    for (int y = 0; y < world.size(); y++)
        for (int x = 0; x < world[y].size(); x++)
            if (world[y][x] != '#') {
                node n;
                n.pos = point(x, y);
                all[n.pos.hash()] = n;
            }

    node *cur = &all[finish.hash()];
    cur->max_dist = 0;
    vector<node *> unvisited = {cur};

    while (!unvisited.empty()) {
        return 56;
        for (auto ns : next_steps(world, {cur->pos}, false)) {
            cerr << "\n" << ns << ": " << all[ns.hash()].pos;
        }
        cerr << "\n--";
    }
    return 42;
#if 0
    node *cur;
    for (auto &n : all)
        if (n.pos == finish)
            cur = &n;

    Expects(cur);
    cur->distance = 0;

    while (true) {
        cur->visited = true;
        cerr << "\ncur = " << cur->pos << " " << cur->distance;
        node *next = cur;
        for (auto &n : all) {
            if (adjacent_to(n.pos, cur->pos) && n.distance <= cur->distance) {
                n.distance = cur->distance + 1;
                cerr << "\n" << n.pos << " " << n.distance;
            }
            if (!n.visited && n.distance > next->distance)
                next = &n;
            if (n.pos == start && n.distance >= 0)
                return n.distance;
        }
        Expects(next != cur);
        cur = next;
    }
#endif
}

vector<vector<point>> longest_paths(vector<string> const &world,
                                    point const &start, point const &finish,
                                    bool slippery) {
    vector<vector<point>> finished;
    deque<vector<point>> wip;
    wip.push_back(vector<point>{start});
    int attempts = 0;
    while (!wip.empty()) {
        if (wip.front().back() == finish) {
            attempts++;
            cerr << "\nFinished for #" << attempts << ": " << wip.front();
            if (finished.empty() ||
                (finished.front().size() < wip.front().size())) {
                finished.clear();
                finished.push_back(wip.front());
                cerr << "\n Best so far: " << wip.front().size() - 1;
            }
            wip.pop_front();
        } else {
            auto n = next_steps(world, wip.front(), slippery);
            switch (n.size()) {
            case 0: // Dead end
                attempts++;
                cerr << "\nDead end for #" << attempts << ": " << wip.front();
                wip.pop_front();
                break;
            default: // Fork path
                for (int i = 1; i < n.size(); i++) {
                    wip.push_back(wip.front());
                    wip.back().push_back(n[i]);
                }
                // Fall through...
            case 1: // Keep adding to this one
                wip.front().push_back(n[0]);
                break;
            }
        }
    }
    return finished;
}

void foo(bool verbose, bool slippery, vector<int> const &expected) {
    auto exp_it = expected.begin();

    for (int i = 0; i < 2; i++) {
        auto world = read_file_as_string_vector("23-"s + to_string(i) + ".txt");
        auto const dim = map_dimensions(world);
        auto const start = point(1, 0);
        auto const finish = dim - point(2, 1);
        Expects(char_at(world, start) == '.');
        Expects(char_at(world, finish) == '.');

        if (verbose)
            draw_map(world);

        auto const path_len = slippery ?
	  [&]() {
        vector<point> path;
        for (auto e : longest_paths(world, start, finish, slippery))
            if (e.size() > path.size())
                path = e;
	return path.size()-1;}() : longest_path(world, start, finish);

        if (exp_it != expected.end()) {
            if (*exp_it != path_len) {
                cerr << "\n\nExpected " << *exp_it << " but counted "
                     << path_len << "\n";
                exit(1);
            }
            exp_it++;
        }
        cerr << "\n\nNum steps: " << path_len << "\n";
    }
}

int main() {
    // foo(true, true, {94, 2182});
    foo(true, false, {154});
    return 0;
}
