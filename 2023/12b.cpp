#include <algorithm>
#include <atomic>
#include <cmath>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <thread>
#include <tuple>
#include <vector>

using namespace std;

auto input = vector<tuple<string, vector<int>>> {
#if 0
#include "12-input.cpp"
#else
#include "12-ex.cpp"
#endif
};

auto &operator<<(ostream &os, vector<string> const &v) {
    os << "[";
    for (int i = 0; i < v.size(); i++) {
        if (i > 0)
            os << ",";
        os << '"' << v[i] << '"';
    }
    os << "]";
    return os;
}

enum foo { INVALID = 0, VALID = 1, MAYBE = 42 };

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

#if 0
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
#endif
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

auto expand(tuple<string, vector<int>> const &tm, int n) {
    auto pat0 = get<0>(tm);
    auto m0 = get<1>(tm);
    auto pat = pat0;
    for (int i = 0; i < n; i++)
        pat += "?" + pat0;
    vector<int> m(m0.size() * (n + 1));
    for (int i = 0; i < m.size(); i++)
        m[i] = m0[i % m0.size()];
    return make_tuple(pat, m);
}

int num_q(string const &s) { return count(s.begin(), s.end(), '?'); }

bool is_wild(string const &s) { return num_q(s) == s.size(); }

bool has_q(string const &s) { return num_q(s) > 0; }

bool is_fixed(string const &s) { return !has_q(s); }

bool is_fixed(vector<string> const &v) {
    for (auto &e : v)
        if (has_q(e))
            return false;
    return true;
}

int max_num_seg(string const &s) {
    if (s.size() <= 2)
        return 1;
    auto qpos = s.find_first_of("q");
    if (qpos == string::npos || qpos == (s.size() - 1))
        return 1;
    else if (qpos == 0)
        return 1 + max_num_seg(s.substr(2));
    else
        return 1 + max_num_seg(s.substr(qpos));
}

string join(vector<string> const &v, char sep) {
    string res = v[0];
    for (int i = 1; i < v.size(); i++)
        res += sep + v[i];
    return res;
}

int64_t num_combos(vector<string> sp, vector<int> const &m) {
    if (sp.size() > m.size()) {
        auto num_wild = count_if(sp.begin(), sp.end(), is_wild);
        // wild segments could be empty
        if ((sp.size() - num_wild) > m.size())
            return 0;
        if ((sp.size() - num_wild) == m.size()) {
            vector<string> new_sp;
            for (auto &e : sp)
                if (!is_wild(e))
                    new_sp.push_back(e);
            return num_combos(new_sp, m);
        }
    }

    if (is_fixed(sp))
        return test_validity(join(sp, '.'), m);

    for (auto &pat : sp) {
        auto p = &pat[0];
        while (*p) {
            if (*p == '?') {
                *p = '.';
                auto r0 = num_combos(sp, m);
                *p = '#';
                auto r1 = num_combos(sp, m);
                return r0 + r1;
            }
            p++;
        }
    }
    cerr << "Expected '?' in " << sp;
    abort();
}

int64_t num_combos(tuple<string, vector<int>> const &pm, int x = 0,
                   bool verbose = false, bool force_calc = false) {
    if (!force_calc && x == 4) {
        verbose = false;
        auto a = num_combos(pm, 0, verbose);
        auto b = num_combos(pm, 1, verbose);
        auto b_div_a = (double)b / a;
        if (fabs(b_div_a - round(b_div_a)) < 0.001) {
            auto c = a * (int64_t(round(pow(b_div_a, 4))));
            cerr << "\n f(" << a << ", " << b << ") => " << c;
            return c;
        }
        cerr << "  " << b << " / " << a << " = " << b_div_a << " ...";
        return num_combos(pm, x, true, true);
    } else if (x > 0) {
        return num_combos(expand(pm, x), 0, verbose);
    }

    auto pat = get<0>(pm);
    auto const &m = get<1>(pm);
    if (verbose) {
        cerr << "\n" << pat << " [";
        for (auto e : m)
            cerr << " " << e;
        cerr << "]";
    }

    auto v = test_validity(pat, m);
    if (v != MAYBE) {
        return v;
    }

    vector<string> sp;
    // cerr << "  " << pat;
    auto p = &pat[0];
    while (*p) {
        while (*p == '.')
            p++;
        auto s0 = p;
        while (*p == '#' || *p == '?')
            p++;
        if (s0 != p)
            sp.push_back(string(s0, p));
    }
#if 1
    return num_combos(sp, m);
#else
    if (verbose)
        cerr << "  " << sp;
    p = &pat[0];
    while (*p != '?')
        p++;
    *p = '.';
    auto r0 = num_combos({pat, m});
    *p = '#';
    auto r1 = num_combos({pat, m});
    return r0 + r1;
#endif
}

int main() {
    atomic_bool done{false};
    thread t([&done]() {
        int wdt = 100;
        while (!done) {
            this_thread::sleep_for(100ms);
            if (!--wdt)
                abort();
        }
    });

    int64_t sum = 0;
    for (auto const &e : input) {
        auto res = num_combos(e, 4, true, true);
        cerr << ": " << res;
        sum += res;
        cerr << "  (sum=" << sum << ")";
    }
    cerr << "\n\n" << sum << "\n";
    if (sum <= 91464976098328)
        cerr << "TOO LOW\n";
    done = true;
    t.join();
    return 0;
};
