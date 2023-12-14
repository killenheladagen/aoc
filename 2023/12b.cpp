#include <algorithm>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <tuple>
#include <vector>
using namespace std;

auto input = vector<tuple<string, vector<int>>>{
//#include "12-input.cpp"
#include "12-ex.cpp"
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
int64_t num_combos(string const &pat0, vector<int> const &m0) {
    // cerr << "\n"
    //        << pat0 << " " << pat0 << " " << pat0 << " " << pat0 << " " <<
    //        pat0;
    auto pat = pat0 + "?" + pat0 + "?" + pat0 + "?" + pat0 + "?" + pat0;
    vector<int> m(m0.size() * 5);
    for (int i = 0; i < m.size(); i++)
        m[i] = m0[i % m0.size()];

        // pat = pat0;
        // m = m0;

#if 0
    cerr << "\n" << pat << "   ";
    for (auto e : m)
        cerr << " " << e;
    cerr << "   ";
#endif
    int64_t res = 0;
    while (next_cand(pat)) {
        auto s = test_validity(pat, m);
        // cerr << "\n   " << pat << " => " << s;
        if (s == VALID)
            res++;
    }
    cerr << "\n" << pat0 << "  " << res;
    return res;
}

int main() {
    int64_t sum = 0;
    for (auto const &e : input)
        sum += num_combos(get<0>(e), get<1>(e));
    cerr << "\n\n" << sum << "\n";
    return 0;
};
