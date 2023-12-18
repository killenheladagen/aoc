#include "helpers.h"
using namespace std;

struct node;

struct point {
    int x;
    int y;
    point(int x, int y) : x(x), y(y) {}
    point() : point(-1, -1) {}
};

auto &operator<<(ostream &os, point p) {
    return os << "{" << p.x << "," << p.y << "}";
}

char opposite_dir(char dir) {
    switch (dir) {
    case 'v':
        return '^';
    case '^':
        return 'v';
    case '>':
        return '<';
    case '<':
        return '>';
    default:
        cerr << "\nBad dir " << dir << "\n";
        Expects(0);
    }
}

point step(point source, char dir) {
    source.x += dir == '>' ? 1 : dir == '<' ? -1 : 0;
    source.y += dir == 'v' ? 1 : dir == '^' ? -1 : 0;
    return source;
}

optional<point> step(point source, char dir, point dim) {
    auto dest = step(source, dir);
    if (dest.x < 0 || dest.y < 0 || dest.x >= dim.x || dest.y >= dim.x)
        return nullopt;
    return dest;
}

struct edge {
    char dir;
    int weight;
    node *source;
    node *dest;
};

struct node {
    point pos;
    vector<edge> edges;
    bool visited = false;
    int distance = 999999999;
    string path;
};

auto follow_dir(node &n, char dir) {
    auto it = find_if(n.edges.begin(), n.edges.end(),
                      [&](auto &x) { return x.dir == dir; });
    return it == n.edges.end() ? nullptr : &(*it);
}

auto &operator<<(ostream &os, edge const &e) {
    return os << e.dir << e.weight << ":" << e.dest->pos;
}

auto &operator<<(ostream &os, vector<edge> v) {
    os << "[";
    for (int i = 0; i < v.size(); i++) {
        if (i > 0)
            os << ",";
        os << v[i];
    }
    os << "]";
    return os;
}

auto &operator<<(ostream &os, node const &n) {
    return os << n.pos << ": dist=" << n.distance << " edges=" << n.edges
              << " path=" << n.path;
}

void draw_map(vector<string> const &w) {
    cerr << "\n";
    for (auto &row : w)
        cerr << row << "\n";
}

void draw_on_map(vector<string> &w, node *n, char sym) {
    w[n->pos.y][n->pos.x] = sym;
}

void draw_path(vector<string> w, node *n, string const &path) {
    for (auto dir : path) {
        draw_on_map(w, n, dir);
        n = follow_dir(*n, dir)->dest;
    }
    draw_on_map(w, n, '*');
    draw_map(w);
}

void print_path(node *n, string const &path) {
    cerr << *n;
    for (auto dir : path) {
        n = follow_dir(*n, dir)->dest;
        cerr << "\n  [" << dir << "]" << n->pos << " " << n->distance;
    }
}

void foo(bool verbose, bool maximize) {
    for (int i = 0; i < 2; i++) {
        auto fname = "17-"s + to_string(i) + ".txt";
        vector<string> world;
        vector<vector<node>> node_matrix;
        ifstream f(fname.c_str());
        for (string s; getline(f, s);) {
            node_matrix.push_back(vector<node>(s.size()));
            world.push_back(s);
        }

        int w = world[0].size();
        int h = world.size();
        auto dim = point{w, h};
        for (int x = 0; x < w; x++) {
            for (int y = 0; y < h; y++) {
                auto &n = node_matrix[y][x];
                n.pos = point{x, y};
                int w = world[y][x] - '0';
                edge e;
                e.weight = w;
                e.source = &n;
                for (auto dir : "v^<>"s) {
                    if (auto dest_pos = step(n.pos, dir, dim)) {
                        e.dest = &node_matrix[dest_pos->y][dest_pos->x];
                        e.dir = dir;
                        n.edges.push_back(e);
                    }
                }
            }
        }
        auto start = &node_matrix[h - 1][w - 1];
        start->distance = 0;
        cerr << *start;

        set<node *> unvisited;
        for (auto &row : node_matrix)
            for (auto &n : row)
                unvisited.insert(&n);

        auto cur = start;

        while (cur->pos.x != 0 || cur->pos.y != 0) {
            for (auto &e : cur->edges) {
                Expects(e.source == cur);
                if (!e.dest->visited) {
                    auto new_dist = cur->distance + e.weight;
                    e.dest->distance = min(new_dist, e.dest->distance);
                }
            }
            cur->visited = true;
            // unvisited.remove(cur);
            edge *best_edge = nullptr;
            for (auto &e : cur->edges)
                if (!e.dest->visited &&
                    //(string(3, e.dir) != cur->path.substr(0, 3)) &&
                    (!best_edge ||
                     e.dest->distance < best_edge->dest->distance))
                    best_edge = &e;
            if (best_edge) {
                best_edge->dest->path = string(1, best_edge->dir) + cur->path;
                cur = best_edge->dest;
            } else {
                cerr << "\nNo candidate found\n" << cur->path;
                cur->distance = 8888888;
                auto odir = opposite_dir(cur->path[0]);
                auto last_edge = follow_dir(*cur, odir);
                Expects(last_edge);
                cur = last_edge->dest;
            }
            cerr << "\n" << *cur;
        }

        auto h_final = cur->distance;

        cout << "\n";
        draw_map(world);
        cout << "\n\n";
        print_path(start, reverse(cur->path));
        draw_path(world, start, reverse(cur->path));

        cout << "\n\n" << DUMP(cur->path);
        cout << "\n\n" << DUMP(h_final) << "\n";
    }
}

int main() {
    Expects(reverse("abcde") == "edcba");
    foo(true, false);
    // foo(false, true);
    return 0;
}
