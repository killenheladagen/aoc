#include <algorithm>
#include <cmath>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <tuple>
#include <vector>
using namespace std;

auto input = vector<tuple<string, vector<int>>> {
#if 1
#include "12-input.cpp"
#else
#include "12-ex.cpp"
#endif
};

enum foo { INVALID = 0, VALID = 10, MAYBE = 5 };

foo test_validity(string const &pat, vector<int> const &m) {
    auto p = pat.c_str();
    for (auto e : m) {
        while (*p == '.' || *p == '0')
            p++;
        if (*p == '?')
            return MAYBE;
        auto s0 = p;
        while (*p == '#' || *p == '1')
            p++;
        if (*p == '?')
            return MAYBE;
        if ((p - s0) != e)
            return INVALID;
    }
    bool has_q = false;
    while (*p == '.' || *p == '0' || *p == '?') {
        if (*p == '?')
            has_q = true;
        p++;
    }
    if (*p)
        return INVALID;
    return has_q ? MAYBE : VALID;
}

bool next_cand(string &pat) {
    bool carry = true;
    for (auto &c : pat) {
        if (c == '?') {
            c = '0';
            carry = false;
        } else if (c == '0') { // 0+c
            c = carry ? '1' : '0';
            carry = false;
        } else if (c == '1') { // 1+c
            c = carry ? '0' : '1';
            carry = carry;
        }
    }
    return !carry;
}
#if 0
bool prev_cand(string &pat) {
    for (int i = pat.length() - 1; i >= 0; i--) {
        auto &c = pat[i];
        if (c == '1' || c == '0') {
            c = (c == '1') ? '0' : '?';
            return true;
        }
    }
    return false;
}
#endif

int64_t num_combos(string pat, vector<int> const &m, int x) {
    // cerr << "\n"
    //        << pat0 << " " << pat0 << " " << pat0 << " " << pat0 << " " <<
    //        pat0;

    if (x == 1) {
        auto patx = pat + "?" + pat;
        vector<int> mx(m.size() * 2);
        for (int i = 0; i < mx.size(); i++)
            mx[i] = m[i % m.size()];
        return num_combos(patx, mx, 0);
    } else if (x == 4) {
        auto a = num_combos(pat, m, 0);
        auto b = num_combos(pat, m, 1);
        auto c = a * (int(round(pow(b / a, 4))));
        cerr << "\n f(" << a << ", " << b << ") => " << c;
        return c;
    }

    cerr << "\n" << pat << " [";
    for (auto e : m)
        cerr << " " << e;
    cerr << "]";

    int64_t res = 0;
    string first_valid;
    while (next_cand(pat)) {
        auto s = test_validity(pat, m);
        // cerr << "\n   " << pat << " => " << s;
        if (s == VALID) {
            if (first_valid.empty())
                first_valid = pat;
            res++;
        }
    }
    cerr << " => " << res;
    cerr << " (" << first_valid << ")";
    return res;
}

int main() {
    int64_t sum = 0;
    for (auto const &e : input)
        sum += num_combos(get<0>(e), get<1>(e), 4);
    cerr << "\n\n" << sum << "\n";
    return 0;
};
