#include <algorithm>
#include <atomic>
#include <cmath>
#include <iostream>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <thread>
#include <tuple>
#include <vector>

using namespace std;
using namespace std::chrono;

auto input = vector<vector<tuple<string, vector<int>>>>{{
#include "12-ex.cpp"
                                                        },
                                                        {
#include "12-input.cpp"
                                                        }};

template <typename T> auto &operator<<(ostream &os, vector<T> const &v) {
    os << "[";
    for (int i = 0; i < v.size(); i++) {
        if (i > 0)
            os << ",";
        os << v[i];
    }
    os << "]";
    return os;
}

template <> auto &operator<<(ostream &os, vector<string> const &v) {
    os << "[";
    for (int i = 0; i < v.size(); i++) {
        if (i > 0)
            os << ",";
        os << '"' << v[i] << '"';
    }
    os << "]";
    return os;
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

int max_vector_length(char const *s) {
    s = skip_dots(s);
    int len = 0;
    while (*s) {
        if (*s == '#' || *s == '?') {
            len++;
            s++;
        }
        s = skip_char(s, '#');
        if (*s == '?')
            s++;
        s = skip_dots(s);
    }
    // cerr << "  max:" << len;
    return len;
}

enum foo { INVALID = 0, VALID = 1, MAYBE = 42 };

static int64_t test_count = 0;

foo test_validity(string const &pat, vector<int> const &m) {
    test_count++;
    // cerr << "\n  test_validity(" << pat << "," << m << ")";
    auto p = pat.c_str();
    for (auto e : m) {
        p = skip_dots(p);
        if (*p == '?')
            return MAYBE;
        auto s0 = p;
        p = skip_hashes(p);
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

bool has_char(char const *s, char const c) {
    while (*s) {
        if (*s == c)
            return true;
        s++;
    }
    return false;
}

bool has_q(char const *s) { return has_char(s, '?'); }
bool has_q(string const &s) { return has_q(s.c_str()); }
bool has_hash(char const *s) { return has_char(s, '#'); }

int leading_hashes(char const *s) {
    int num = 0;
    while (*(s++) == '#')
        num++;
    return num;
}

bool is_fixed(auto s) { return !has_q(s); }

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
#if 0
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
#endif
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

auto cdr(vector<int> const &v) {
    vector<int> res(v.size() - 1);
    for (int i = 0; i < res.size(); i++)
        res[i] = v[i + 1];
    return res;
}

int64_t brute_force_combos2(char *pat, vector<int> const &m) {
    // cerr << "\n trim(" << pat << ") → ";
    pat = skip_dots(pat);
    // cerr << pat;

    if (max_vector_length(pat) < m.size())
        return 0;

    // cerr << "\n  " << pat << " (?)";
    auto v = test_validity(pat, m);
    if (v == VALID)
        return 1;
    if (v == INVALID)
        return 0;

    auto p = &pat[0];
    while (*p != '?')
        p++;

    *p = '.';
    auto r0 = brute_force_combos2(pat, m);
    *p = '#';
    auto r1 = brute_force_combos2(pat, m);
    *p = '?';
    return r0 + r1;
}

int64_t brute_force_combos(char *pat, vector<int> m) {
    if (m.empty())
        return has_hash(pat) ? 0 : 1;

    pat = skip_dots(pat);
    if (max_vector_length(pat) < m.size())
        return 0;
    if (is_fixed(pat)) {
        auto v = test_validity(pat, m);
        if (v == VALID)
            return 1;
        if (v == INVALID)
            return 0;
        abort();
    }

    auto lh = leading_hashes(pat);
    if (lh > m[0])
        return 0;
    if (m[0] > lh && pat[lh] == '.')
        return 0;
    if (m[0] == lh && pat[lh] == '.') {
        pat += lh;
        return brute_force_combos(pat, cdr(m));
    }

    auto p = &pat[0];
    while (*p != '?')
        p++;

    *p = '.';
    auto r0 = brute_force_combos(pat, m);
    *p = '#';
    auto r1 = brute_force_combos(pat, m);
    *p = '?';
    return r0 + r1;
}

int64_t num_combos(tuple<string, vector<int>> pm, int x = 0,
                   bool verbose = false, bool force_calc = false) {
    pm = expand(pm, x);
    auto pat = get<0>(pm);
    auto &m = get<1>(pm);
    return brute_force_combos(&pat[0], m);
#if 0
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
#endif
}

auto f(int64_t a, int64_t b, int64_t x) {
    return a * int64_t(round(pow(b / a, x)));
}

optional<int64_t> predict(vector<int64_t> const &history, int x) {
    bool verify = !false;
    if (history.size() < 2)
        return nullopt;
    auto const a = history[0];
    auto const b = history[1];
    if ((b % a) != 0) {
        cerr << "\n No prediction for " << history;
        return nullopt;
    }
    if (verify) {
        if (history.size() < 3)
            return nullopt;
        auto const c_exp = history[2];
        auto const c_act = f(a, b, 2);
        if (c_act != c_exp) {
            cerr << "\n Verification failed for " << history;
            abort();
        }
    }
    return f(a, b, x);
}

void print_series_report(vector<vector<int64_t>> const &series) {
    set<vector<int64_t>> s;
    for (auto &e : series) {
        if (e.size() <= 1)
            return;
        s.insert(e);
    }
    cerr << "Series:";
    for (auto &e : s) {
        cerr << "\n" << e;
    }
    cerr << "\n";
}

bool starts_with(vector<int64_t> const &v, int64_t a, int64_t b) {
    return v.size() >= 2 && v[0] == a && v[1] == b;
}

int main() {
    atomic_bool done{false};
    thread t([&done]() {
        auto step = 100ms;
        int wdt = 140s / step;
        while (!done) {
            this_thread::sleep_for(step);
            if (!--wdt)
                abort();
        }
    });

    int64_t sum = 0;
    vector<int64_t> expected_sums = {21,     206,  2612,  36308,
                                     525152, 7670, 682696};
    expected_sums.clear();
    auto expected = expected_sums.cbegin();
    for (auto const &input_e : input) {
        vector<vector<int64_t>> series(input_e.size());
        for (auto const x : {0, 1, 2, 3, 4}) {
            sum = 0;
            auto ser = series.begin();
            for (auto const &e : input_e) {
                if (ser->size() < 2 || starts_with(*ser, 3, 12)) {
                    cerr << "\n"
                         << get<0>(e)
                         << " <=" << max_vector_length(get<0>(e).c_str()) << " "
                         << get<1>(e) << " =" << get<1>(e).size();

                    auto const t0 = steady_clock::now();
                    auto const res = [&]() {
                        if (auto const pred = predict(*ser, x))
                            return *pred;
                        else
                            return num_combos(e, x, true, true);
                    }();
                    auto const t_ms =
                        duration_cast<milliseconds>(steady_clock::now() - t0)
                            .count();
                    ser->push_back(res);
                    cerr << ": " << *ser;
                    sum += res;
                    cerr << " (sum=" << sum;
                    if (t_ms && ser->size() > 2)
                        cerr << ", t=" << t_ms << " ms";
                    cerr << ")";
                }
                ser++;
            }
            cerr << "\n\n" << sum << "  (test_count=" << test_count << ")\n";
            if (expected != expected_sums.end()) {
                if (sum != *expected) {
                    cerr << "  (Expected " << *expected << ")";
                    abort();
                }
                expected++;
            }

            print_series_report(series);
        }
    }
    if (sum <= 91464976098328)
        cerr << "TOO LOW\n";
    done = true;
    t.join();
    return 0;
};
