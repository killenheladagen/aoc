#include <cassert>
#include <cstring>
#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>

using namespace std;

static int64_t
number_of_possible_designs(string const &design,
                           map<char, vector<string>> const &towels,
                           bool stop_after_first, map<string, int64_t> &cache) {
    if (design.size() == 0)
        return 1;

    auto cache_it = cache.find(design);
    if (cache_it != cache.end())
        return cache_it->second;

    auto it = towels.find(design[0]);
    int64_t num = 0;
    if (it != towels.end())
        for (auto &towel : it->second)
            if (design.starts_with(towel)) {
                num +=
                    number_of_possible_designs(design.substr(towel.size()),
                                               towels, stop_after_first, cache);
                if (num && stop_after_first)
                    return 1;
            }

    cache[design] = num;

    return num;
}

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
        map<string, int64_t> cache;
        if (number_of_possible_designs(towels[i], towel_map, true, cache) == 0)
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

static auto read_csv_and_rows(const char *filename, bool do_prune = true) {
    string row;
    auto f = ifstream(filename);

    getline(f, row);
    auto towels_raw = split(row, ", ");
    auto towels = build_towel_map(do_prune ? prune(towels_raw) : towels_raw);
    getline(f, row);
    assert(row == "");

    vector<string> designs;
    while (getline(f, row))
        designs.push_back(row);
    return make_tuple(towels, designs);
}

static auto possible_designs(const char *filename) {
    map<string, int64_t> cache;
    vector<string> possible;
    auto const [towels, designs] = read_csv_and_rows(filename);
    copy_if(designs.begin(), designs.end(), back_inserter(possible),
            [&towels, &cache](string const &x) {
                return number_of_possible_designs(x.c_str(), towels, true,
                                                  cache) != 0;
            });
    return possible;
}

static auto number_of_different_ways(vector<string> const &designs,
                                     const char *filename) {
    map<string, int64_t> cache;
    auto const towels = get<0>(read_csv_and_rows(filename, false));
    int64_t num = 0;
    for (auto &design : designs)
        num += number_of_possible_designs(design, towels, false, cache);
    return num;
}

int main() {
    auto possible = possible_designs("test.txt");
    assert(possible.size() == 6);
    auto num_diff = number_of_different_ways(possible, "test.txt");
    assert(num_diff == 16);

    possible = possible_designs("input.txt");
    cerr << possible.size() << "\n";
    assert(possible.size() == 306);
    num_diff = number_of_different_ways(possible, "input.txt");
    cerr << num_diff << "\n";

    return 0;
}
