#include "helpers.h"

using namespace std;
using namespace std::chrono;

auto input = vector<vector<tuple<string, vector<int>>>>{{
#include "12-ex.cpp"
                                                        },
                                                        {
#include "12-input.cpp"
                                                        }};

auto split(string s, char sep) {
    vector<string> res;
    while (true) {
        auto pos = s.find(sep);
        if (pos == s.npos) {
            res.push_back(s);
            return res;
        }
        res.push_back(s.substr(0, pos));
        s = s.substr(pos + 1);
    }
}

auto join(vector<int> const &v, string sep) {
    Expects(!v.empty());
    string res = to_string(v[0]);
    for (int i = 1; i < v.size(); i++)
        res += sep + to_string(v[i]);
    return res;
}

auto read_string_int64_map(string const &fn) {
    map<string, int64_t> m;
    ifstream f(fn.c_str());
    for (string s; getline(f, s);) {
        auto v = split(s, ' ');
        // cerr << "\n" << DUMP(s) << DUMP(v);
        Expects(v.size() == 2);
        m[v[0]] = stoi(v[1]);
    }
    return m;
}

void append_line_to_file(string const &fn, string const &line) {
    ofstream f(fn.c_str(), f.binary | f.app);
    f.write(line.c_str(), line.size());
    f.write("\n", 1);
}

char *skip_char(char *s, char c) {
    while (*s && *s == c)
        s++;
    return s;
}

char *skip_dots(char *s) { return skip_char(s, '.'); }
char *skip_hashes(char *s) { return skip_char(s, '#'); }

char const *skip_char(char const *s, char c) { return skip_char((char *)s, c); }
char const *skip_dots(char const *s) { return skip_dots((char *)s); }
char const *skip_hashes(char const *s) { return skip_hashes((char *)s); }

auto is_valid(char const *pat, vector<int> const &m) {
    auto p = pat;
    for (auto e : m) {
        p = skip_dots(p);
        Expects(*p != '?');
        auto s0 = p;
        p = skip_hashes(p);
        Expects(*p != '?');
        if ((p - s0) != e)
            return false;
    }
    p = skip_dots(p);
    return *p ? false : true;
}

int64_t num_combos(char *pat, vector<int> const &m, atomic<bool> &kill) {
    if (kill)
        return -9999999999999;

    pat = skip_dots(pat);

    auto p = &pat[0];
    while (*p) {
        if (*p == '?') {
            *p = '.';
            auto r0 = num_combos(pat, m, kill);
            *p = '#';
            auto r1 = num_combos(pat, m, kill);
            *p = '?';
            return r0 + r1;
        }
        p++;
    }
    return is_valid(pat, m) ? 1 : 0;
}

map<string, int64_t> cache;

auto cache_key(string const &path, vector<int> const &m) {
    return path + ":" + join(m, ",");
}

const char *cache_fn = "12-cache.txt";

optional<int64_t> num_combos_or_cache(string const &pat, vector<int> const &m) {
    atomic<bool> kill = false;
    auto k = cache_key(pat, m);
    auto it = cache.find(k);
    if (it != cache.end())
        return it->second;
    cerr << "\nCache miss for \"" << k << "\"";
    string s = pat;
    auto fut =
        async(launch::async, [&]() { return num_combos(&s[0], m, kill); });
    switch (auto status = fut.wait_for(20s); status) {
    case future_status::ready: {
        auto res = fut.get();
        cache[k] = res;
        append_line_to_file(cache_fn, k + " " + to_string(res));
        return res;
    }
    case future_status::timeout:
        cerr << "\nTimeout for \"" << k << "\"";
    default:
        cerr << "\nUnexpected task status for \"" << k << "\"";
    }
    kill = true;
    return nullopt;
}

auto expand_pattern(string pat0, int n) {
    auto pat = pat0;
    for (int i = 0; i < n; i++)
        pat += "?" + pat0;
    return pat;
}

auto expand_match(vector<int> const &m0, int n) {
    vector<int> m(m0.size() * (n + 1));
    for (int i = 0; i < m.size(); i++)
        m[i] = m0[i % m0.size()];
    return m;
}

optional<int64_t> sum_of_num_combos(auto const &v, int x) {
    bool fail = false;
    optional<int64_t> sum = 0;
    for (auto const &pm : v) {
        auto pat = get<0>(pm);
        auto &m = get<1>(pm);
        auto res =
            num_combos_or_cache(expand_pattern(pat, x), expand_match(m, x));
        if (!res)
            sum = nullopt;
        if (res && sum)
            *sum += *res;
    }
    return sum;
}

void main2() {
    cache = read_string_int64_map(cache_fn);
    auto cache_size = cache.size();
    cerr << DUMP(cache_size) << "\n";
    int i = 0;
    for (auto const &input_e : input) {
        for (auto const x : {0, 4}) {
            auto maybe_sum = sum_of_num_combos(input_e, x);
            auto sum = maybe_sum.value_or(0);
            cerr << "\n" << DUMP(i) << DUMP(x) << DUMP(sum) << "\n";
        }
        i++;
    }
}

int main() {
    auto const max_run_time = 24h;

    atomic_bool done{false};
    thread t([&]() {
        auto step = 100ms;
        int wdt = max_run_time / step;
        while (!done) {
            this_thread::sleep_for(step);
            if (!--wdt)
                abort();
        }
    });

    main2();

    done = true;
    t.join();
    return 0;
};
