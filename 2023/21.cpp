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

void foo(bool verbose, vector<int64_t> const &expected) {
    auto exp_it = expected.begin();
    for (int i = 0; i < 2; i++) {
        auto num_steps = (i == 0) ? 6 : 64;
        auto world = read_file_as_string_vector("21-"s + to_string(i) + ".txt");
        auto const dim = map_dimensions(world);

        auto cur = find_in_map(world, 'S');
        for (auto bchar : base64_chars.substr(0, num_steps)) {
            for (auto const &pos : cur)
                for (auto dir : "v^<>"s)
                    if (auto dest_pos = step(pos, dir, dim)) {
                        auto &cr = char_at(world, *dest_pos);
                        if (cr != '#')
                            cr = bchar;
                    }
            if (verbose)
                draw_map(world);
            cur = find_in_map(world, bchar);
        }
        cerr << "\n\nReachable tiles: " << cur.size() << "\n";
    }
}

int main() {
    foo(!false, {16LL});
    return 0;
}
