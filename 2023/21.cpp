#include "helpers.h"
#include "point.h"
using namespace std;

auto const base64_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                          "abcdefghijklmnopqrstuvwxyz"
                          "0123456789+/"s;

inline bool is_base64(unsigned char c) {
    return (isalnum(c) || (c == '+') || (c == '/'));
}

auto find_in_map(vector<string> const &w, char c) {
    vector<point> res;
    for (int y = 0; y < w.size(); y++)
        for (int x = 0; x < w[y].size(); x++)
            if (w[y][x] == c)
                res.emplace_back(x, y);
    return res;
}

char char_at_infinite(vector<std::string> const &w, point const p,
                      point const dim) {
    return char_at(w, point(modulo(p.x, dim.x), modulo(p.y, dim.y)));
}

struct infinite_map {
    vector<string> og_world;
    point origin{0, 0};
    vector<string> world;

    infinite_map(vector<string> const &og_world)
        : og_world(og_world), world(og_world) {}

    auto og_full_row(int y) {
        auto s = og_world[y];
        auto len = max(world[0].size(), world.back().size());
        while (s.size() < len)
            s += og_world[y];
        // cerr << "\n" << s << "  <- [" << y << "]";
        return s;
    }

    void putc(point p, char c) {
        auto const dim = map_dimensions(world);
        auto np = p - origin;
        if (origin.x || origin.y) {
            // cerr << "\n" << DUMP(origin) << ": " << p << " => " << np;
        }
        if (np.y < 0) {
            // cerr << "\n" << DUMP(np.y) << " < 0";
            origin.y -= og_world.size();
            world.resize(dim.y + og_world.size());
            for (auto y = world.size() - 1; y >= og_world.size(); y--)
                world[y] = move(world[y - og_world.size()]);
            for (int y = 0; y < og_world.size(); y++)
                world[y] = og_full_row(y);
            // cerr << "\nAFTER y<0";
            // draw_map(world);
            putc(p, c);
        } else if (np.y >= dim.y) {
            // cerr << "\n" << DUMP(np.y) << " >= " << DUMP(dim.y);
            for (auto y = 0; y < og_world.size(); y++)
                world.push_back(og_full_row(y));
            // cerr << "\nAFTER y>=max";
            // draw_map(world);
            putc(p, c);
        } else if (np.x < 0) {
            // cerr << "\n" << DUMP(np.x) << " < 0";
            origin.x -= og_world[0].size();
            // draw_map(world);
            for (int y = 0; y < dim.y; y++)
                world[y] = og_world[y % og_world.size()] + world[y];
            // cerr << "\nAFTER x<0";
            // draw_map(world);
            putc(p, c);
        } else if (np.x >= dim.x) {
            // cerr << "\n" << DUMP(np.x) << " >= " << DUMP(dim.x);
            // draw_map(world);
            for (int y = 0; y < dim.y; y++)
                world[y] += og_world[y % og_world.size()];
            // cerr << "\nAFTER x>=max";
            // draw_map(world);
            putc(p, c);
        } else {
            char_at(world, np) = c;
        }
    }
};

void foo(bool verbose, int i, vector<int64_t> const &expected) {
    auto world = read_file_as_string_vector("21-"s + to_string(i) + ".txt");
    auto const dim = map_dimensions(world);

    auto cur = find_in_map(world, 'S');
    Expects(cur.size() == 1);
    char_at(world, cur[0]) = '.';
    if (verbose && i == 0)
        draw_map(world);

    vector<point> next_pos;
    auto num_steps = 0LL;

    auto exp_it = expected.begin();
    while (exp_it != expected.end()) {
        num_steps++;
        next_pos.clear();
        for (auto const &pos : cur)
            for (auto dir : "v^<>"s) {
                auto dest_pos = pos + dir;
                if (char_at_infinite(world, dest_pos, dim) == '.' &&
                    find(next_pos.begin(), next_pos.end(), dest_pos) ==
                        next_pos.end())
                    next_pos.push_back(dest_pos);
            }
        cur = move(next_pos);
        if (verbose) {
            cerr << "\n"
                 << DUMP(num_steps) << DUMP(cur.size()); // << DUMP(cur);
#if 0
            auto scratch_map = infinite_map(world);
            for (auto p : cur)
                scratch_map.putc(p, 'O');
            draw_map(scratch_map.world);
#endif
        }
        if (*exp_it == num_steps) {
            exp_it++;
            if (*exp_it) {
                if (*exp_it != cur.size()) {
                    cerr << "\n\nExpected " << *exp_it << " but counted "
                         << cur.size() << "\n";
                    abort();
                }
            } else {
                cerr << "\n\nReachable tiles: " << cur.size() << "\n";
            }
            exp_it++;
        }
    }
}

int main() {
    Expects(modulo(34, 10) == 4);
    Expects(modulo(-82, 10) == 8);

    foo(true, 0,
        {6LL, 16LL, 10LL, 50LL, 50LL, 1594LL, 100LL, 6536LL, 500LL, 167004LL,
         1000LL, 668697LL, 5000LL, 16733044LL});
    foo(false, 1, {64LL, 3591LL, 26501365LL, 0LL});
    return 0;
}
