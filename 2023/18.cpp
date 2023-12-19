#include "helpers.h"
#include "point.h"
using namespace std;

auto empty_world(point dim) {
    vector<string> world(dim.y);
    for (auto &row : world)
        row = string(dim.x, '.');
    return world;
}

auto count_non_empty(vector<string> const &w) {
    auto result = 0;
    for (auto row : w)
        for (auto c : row)
            result += c == '.' ? 0 : 1;
    return result;
}

auto count_non_empty_through_xyt(
    tuple<vector<int64_t>, vector<int64_t>> const &xyt, vector<string> &world) {
    auto &xt = get<0>(xyt);
    auto &yt = get<1>(xyt);
    cerr << "\n" << DUMP(xt);
    cerr << "\n" << DUMP(yt);
    auto w = world[0].size();
    auto h = world.size();
    int64_t result = 0;
    for (int64_t x = 0; x < w; x++) {
        for (int64_t y = 0; y < h; y++) {
            if (world[y][x] == '.')
                continue;
            if (!(x > 0 && y > 0 && x < (w - 1) && y < (h - 1))) {
                ostringstream os;
                os << "Expected to be well inside " << point(w, h)
                   << " but found non-empty point at " << point(x, y);
                throw runtime_error(os.str());
            }
            auto xlen = (xt[x + 1] - xt[x - 1]);
            auto ylen = (yt[y + 1] - yt[y - 1]);
            auto area = xlen * ylen;
            if (0)
                cerr << "\n"
                     << point(x, y) << ":" << xt[x - 1] << ".." << xt[x + 1]
                     << " => " << xlen << "  " << yt[y - 1] << ".." << yt[y + 1]
                     << " => " << ylen;
            world[y][x] = area < 10 ? '0' + area : '*';
            result += area;
        }
    }
    return result / 4;
}
void flood_fill(vector<string> &w, point cur) {
    vector<point> fill_stack{cur};
    while (!fill_stack.empty()) {
        cur = fill_stack.back();
        fill_stack.pop_back();
        for (auto dir : "UDLR"s) {
            auto const p = cur + dir;
            if (p.x < 0 || p.y < 0 || p.y >= w.size() || p.x >= w[0].size() ||
                char_at(w, p) != '.')
                continue;
            fill_stack.push_back(p);
            char_at(w, p) = '$';
        }
    }
}

auto start_and_dim(vector<tuple<char, int>> const &plan, bool verbose = false) {
    auto start = point(0, 0);
    auto cur = start;
    auto minp = cur;
    auto maxp = cur;

    for (auto [dir, steps] : plan) {
        while (steps--)
            cur += dir;
        minp.x = min(minp.x, cur.x);
        minp.y = min(minp.y, cur.y);
        maxp.x = max(maxp.x, cur.x);
        maxp.y = max(maxp.y, cur.y);
    }
    if (cur != start) {
        ostringstream os;
        os << "Plan expected to be a loop, but " << cur << " != " << start;
        throw runtime_error(os.str());
    }
    if (verbose) {
        cerr << "\n" << DUMP(minp);
        cerr << "\n" << DUMP(maxp);
    }
    return make_tuple(cur - minp, point(1, 1) + maxp - minp);
}

auto follow_plan(point cur, vector<tuple<char, int>> const &plan,
                 bool verbose = false) {
    vector<point> result;
    cerr << "\n" << cur;
    for (auto [dir, steps] : plan) {
        if (verbose)
            cerr << "\n" << dir << "x" << steps;
        while (steps--)
            cur += dir;
        if (verbose)
            cerr << " " << cur;
        result.push_back(cur);
    }
    return result;
}

void draw_lines(vector<string> &world, point cur, vector<point> const &v,
                bool verbose = false) {
    world[cur.y][cur.x] = 'S';
    cerr << "\n" << cur;
    for (auto p : v) {
        auto [dir, steps] = straight_trajectory(cur, p);
        if (verbose)
            cerr << "\n" << dir << "x" << steps;
        while (steps--) {
            cur += dir;
            auto &cref = char_at(world, cur);
            if (cref == '.')
                cref = '#';
        }
        if (verbose)
            cerr << " " << cur;
        auto &cref = char_at(world, cur);
        if (cref != 'S')
            cref = '@';
        assert(cur == p);
    }
}

auto useful_coords(vector<int64_t> const &v) {
    auto res = v;
    for (auto e : unique_sorted(v)) {
        res.push_back(e - 1);
        res.push_back(e + 1);
    }
    return unique_sorted(res);
}

auto operator+(point p, vector<point> v) {
    vector<point> res{p};
    res.reserve(v.size() + 1);
    for (auto &e : v)
        res.push_back(e);
    return res;
}

auto operator+(vector<point> const &v, point p) {
    vector res = v;
    res.push_back(p);
    return res;
}

auto build_xy_transform(vector<point> const &original_edges) {
    auto xs =
        useful_coords(mapcar(original_edges, [](auto &p) { return p.x; }));
    auto ys =
        useful_coords(mapcar(original_edges, [](auto &p) { return p.y; }));
    // cerr << "\n#" << xs.size() << ": " << DUMP(xs);
    // cerr << "\n#" << ys.size() << ": " << DUMP(ys);
    //  1 2 . . 5 6:  (2-0)/2 + (5-1)/2 + (6-2)/2 + (7-5)/2 = 1+2+2+1=6
    return make_tuple(xs, ys);
}

auto map_xy(tuple<vector<int64_t>, vector<int64_t>> const &xyt, point &p) {
    p.x =
        find(get<0>(xyt).begin(), get<0>(xyt).end(), p.x) - get<0>(xyt).begin();
    p.y =
        find(get<1>(xyt).begin(), get<1>(xyt).end(), p.y) - get<1>(xyt).begin();
}

auto map_xy(tuple<vector<int64_t>, vector<int64_t>> const &xyt,
            vector<point> &v) {
    for (auto &p : v)
        map_xy(xyt, p);
}

void foo(bool verbose, bool use_hex, bool sparse,
         vector<int64_t> const &expected) {
    auto exp_it = expected.begin();
    auto const dir_map = "RDLU"s;
    for (int i = -1; i < 2; i++) {
        cerr << "\n\n" << DUMP(i) << DUMP(use_hex);
        vector<tuple<char, int>> plan;
        for (auto line :
             read_file_as_string_vector("18-"s + to_string(i) + ".txt")) {
            auto v = split(line, ' ');
            assert(v.size() == 3);
            auto dir = use_hex ? dir_map[(v[2][7] - '0')] : v[0][0];
            auto steps =
                use_hex ? stoi(v[2].substr(2, 5), nullptr, 16) : stoi(v[1]);
            if (verbose && i == 0)
                cerr << "\n" << dir << " " << steps;
            plan.push_back(make_tuple(dir, steps));
        }
        auto [start, dim] = start_and_dim(plan);
        cerr << "\n" << DUMP(start) << DUMP(dim);
        auto plan_length = 0LL;
        for (auto [dir, steps] : plan)
            plan_length += steps;
        cerr << "\n" << DUMP(plan_length);
        auto edges = follow_plan(start, plan);
        auto xyt = build_xy_transform(start + edges + dim);

        if (sparse) {
            if (verbose)
                cerr << "\nbefore: " << edges;
            map_xy(xyt, edges);
            if (verbose)
                cerr << "\nafter:  " << edges;
            map_xy(xyt, start);
            cerr << "\nbefore: " << DUMP(dim);
            dim = point(get<0>(xyt).size(), get<1>(xyt).size());
            cerr << "\nafter:  " << DUMP(dim);
        }

        auto world = empty_world(dim);
        cerr << "\nfollow_plan...";
        draw_lines(world, start, edges);
        cerr << "\nflood_fill...";
        flood_fill(world, start + (i == 0 ? point(1, 1) : point(1, 1)));
        if (verbose)
            draw_map(world);
        cerr << "\ncount...\n";
        auto num_non_empty = sparse ? count_non_empty_through_xyt(xyt, world)
                                    : count_non_empty(world);
        cerr << "\n" << DUMP(num_non_empty);
        if (verbose && sparse)
            draw_map(world);

        if (exp_it != expected.end()) {
            if (num_non_empty != *exp_it) {
                ostringstream os;
                os << "Expected " << *exp_it;
                throw runtime_error(os.str());
            }
            exp_it++;
        }

        verbose = false;
    }
}

int main() {
    foo(!false, false, false, {42LL, 62LL, 56923LL});
    foo(!false, false, true, {42LL, 62LL, 56923LL});
    foo(false, true, true, {42LL, 952408144115LL});
    return 0;
}
