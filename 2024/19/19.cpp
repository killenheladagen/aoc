#include <cassert>
#include <cstring>
#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>

using namespace std;

#if 1
static bool build_design(string const &design,
                         map<char, vector<string>> const &towels) {
    if (design.size() == 0)
        return true;
    auto it = towels.find(design[0]);
    if (it != towels.end())
        for (auto &towel : it->second)
            if (design.starts_with(towel) &&
                build_design(design.substr(towel.size()), towels))
                return true;
    return false;
}
#else
static bool build_design(string design,
                         map<char, vector<string>> const &towels) {
    stack<string> back_track;

    while (true) {

        auto it = towels.find(design[0]);
        if (it == towels.end()) {
            if (back_track.empty())
                return false;
            design = back_track.top();
            back_track.pop();
        } else {
            auto x =
                find_if(it->second.begin(), it->second.end(),
                        [&](auto &towel) { return design.starts_with(towel); });

                build_design(design.substr(towel.size()), towels))
                return true;
                return false;
        }
    }
#endif

auto build_towel_map(vector<string> const &row) {
    map<char, vector<string>> towels;
    for (auto towel : row)
        towels[towel[0]].push_back(towel);
    return towels;
}

auto prune(vector<string> towels) {
    cerr << "Before prune: " << towels.size() << "\n";
    vector<string> keep;
    for (size_t i = 0; i < towels.size(); i++) {
        auto all_but_candidate = towels;
        all_but_candidate[i] = "QQQ";
        auto towel_map = build_towel_map(all_but_candidate);
        if (!build_design(towels[i], towel_map))
            keep.push_back(towels[i]);
    }
    cerr << "After prune: " << keep.size() << "\n";
    return keep;
}

auto split(const string &str, const string &regex_str) {
    regex regexz(regex_str);
    vector<string> list(
        sregex_token_iterator(str.begin(), str.end(), regexz, -1),
        sregex_token_iterator());
    return list;
}

static auto read_csv_and_rows(const char *filename) {
    string row;
    auto f = ifstream(filename);

    getline(f, row);
    auto towels = build_towel_map(prune(split(row, ", ")));
    getline(f, row);
    assert(row == "");

    vector<string> designs;
    while (getline(f, row))
        designs.push_back(row);
    return make_tuple(towels, designs);
}

static auto num_possible_designs(const char *filename) {
    auto [towels, designs] = read_csv_and_rows(filename);
    return count_if(designs.begin(), designs.end(), [&towels](string const &x) {
        cerr << "test " << x << "\n";
        return build_design(x.c_str(), towels);
    });
}

int main() {
    assert(num_possible_designs("test.txt") == 6);
    // cerr << num_possible_designs("fake.txt") << "\n";
    cerr << num_possible_designs("input.txt") << "\n";
    return 0;
}
