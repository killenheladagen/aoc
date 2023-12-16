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

auto &operator<<(ostream &os, pair<int, int> const &p) {
    return os << "{" << p.first << "," << p.second << "}";
}

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

string scratch(1024, '\0');
auto &operator<<(ostream &os, vector<pair<int, int>> const &v) {
    int last = v.back().first + v.back().second;
    for (int i = 0; i < last; i++)
        scratch[i] = '.';
    for (auto e : v) {
        for (int i = 0; i < e.second; i++)
            scratch[e.first + i] = '#';
    }
    scratch[last] = '\0';
    os << scratch.c_str() << "  ";
    for (auto e : v)
        os << e;
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
bool has_hash(string const &s) { return has_hash(s.c_str()); }
bool has_dot(char const *s) { return has_char(s, '.'); }
bool has_dot(string const &s) { return has_dot(s.c_str()); }

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

int64_t brute_force_combos3(char *pat, vector<int> m) {
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
        return brute_force_combos3(pat, cdr(m));
    }

    auto p = &pat[0];
    while (*p != '?')
        p++;

    *p = '.';
    auto r0 = brute_force_combos3(pat, m);
    *p = '#';
    auto r1 = brute_force_combos3(pat, m);
    *p = '?';
    return r0 + r1;
}

bool can_fill(string const &pat, pair<int, int> ir) {
    auto si = ir.first;
    auto len = ir.second;
    auto ei = si + len;
    if (si < 0 || ei > pat.size())
        return false;
    if (si != 0 && pat[si - 1] == '#')
        return false;
    if (ei != pat.size() && pat[ei] == '#')
        return false;
    auto sub = pat.substr(si, len);
    if (has_dot(sub))
        return false;
    else
        return true;
}

bool can_be_empty(string const &pat, pair<int, int> ir) {
    auto si = ir.first;
    auto len = ir.second;
    auto ei = si + len;
    if (si < 0 || ei > pat.size())
        return false;
    if (si != 0 && pat[si - 1] == '.')
        return false;
    if (ei != pat.size() && pat[ei] == '.')
        return false;
    auto sub = pat.substr(si, len);
    if (has_hash(sub))
        return false;
    else
        return true;
}

bool is_valid(string const &pat, vector<pair<int, int>> const &ms) {
    auto before = make_pair(-2, 1);
    for (auto e : ms) {
        if ((before.first + before.second + 1) > e.first) {
            cerr << " strage spacing";
            return false;
        }
        before = e;
    }

    if ((ms.back().first + ms.back().second) > pat.size()) {
        cerr << "\n" << ms << " too long";
        return false;
    }

    test_count++;
    return all_of(ms.begin(), ms.end(),
                  [&](auto &e) { return can_fill(pat, e); }) &&
           [&]() {
               int last_end = 0;
               for (auto &e : ms) {
                   if (!can_be_empty(pat, {last_end, e.first - last_end}))
                       return false;
                   last_end = e.first + e.second;
               }
               return true;
           }();
}

/*
[1,3,1]

  (0,1) (2,3) (6,1)  .. 10

5+3-1=7 fasta
10-7=3 att fördela på 4 ställen

+----------+
 # ### #xxx
 # ### x#xx
 # ### xx#x
 # ### xxx#
 # x### #xx
 # x### x#x
 # xx### #x
 x# ### #xx
 x# ### x#x
 x# ### xx#
 x# x### #x
 x# x### x#
 x# xx### #
...
 xxx# ### #
+----------+
 */

int sum_of(vector<int> const &v) {
    int sum = 0;
    for (auto e : v)
        sum += e;
    return sum;
}

bool next_space_dist(vector<int> &extra, int const max_space) {
    do {
        int col = 0;
        extra[col]++;
        while (extra[col] > max_space) {
            extra[col] = 0;
            col++;
            if (col >= extra.size())
                return false;
            extra[col]++;
        }
    } while (sum_of(extra) > max_space);
    return true;
}

#define DUMP(VAR) " " << #VAR << "=" << VAR

int64_t num_combos(tuple<string, vector<int>> pm, int x = 0,
                   bool verbose = false, bool force_calc = false) {
    pm = expand(pm, x);
    auto pat = get<0>(pm);
    auto &m = get<1>(pm);

    int fixed_space = sum_of(m) + m.size() - 1;
    int ne = pat.size() - fixed_space;
    int nc = 0;
    auto const nq = num_q(pat);
    cerr << DUMP(nq) << DUMP(ne);
    if (ne < 10 || nq < 50) {
        cerr << " B";
        nc = brute_force_combos2(&pat[0], m);
    } else {
        cerr << " E";
	vector<int> extra(m.size(), 0);
	vector<pair<int, int>> ms(m.size());
        do {
            auto last = make_pair(-2, 1);
            for (int i = 0; i < ms.size(); i++) {
                ms[i] =
                    make_pair(last.first + last.second + 1 + extra[i], m[i]);
                last = ms[i];
            }
            bool v = is_valid(&pat[0], ms);
            // cerr << "\n" << extra << ": " << (v ? "VALID" : "invalid");
            if (v)
                nc++;
        } while (next_space_dist(extra, ne));
    }
    return nc;
}

auto f(int64_t a, int64_t b, int64_t x) {
    return a * int64_t(round(pow(b / a, x)));
}

optional<int64_t> predict(vector<int64_t> const &history, int x) {
    return nullopt;
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
        int wdt = 100s / step;
        while (!done) {
            this_thread::sleep_for(step);
            if (!--wdt)
                abort();
        }
    });

    int64_t sum = 0;
    vector<int64_t> expected_sums =        //
        {21,   206,   2612, 36308, 525152, //
         7670, 682696};
    // expected_sums.clear();
    auto expected = expected_sums.cbegin();
    int i = 0;
    for (auto const &input_e : input) {
        vector<vector<int64_t>> series(input_e.size());
        for (auto const x : {0, 1, 2, 3, 4}) {
            sum = 0;
            auto ser = series.begin();
            int j = 0;
            for (auto const &e : input_e) {
                cerr << "\n"
                     << "i" << i << "x" << x << "j" << j << " " << get<0>(e)
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

                ser++;
                j++;
            }
            cerr << "\n\n" << sum << "  (test_count=" << test_count << ")\n";
            if (expected != expected_sums.end()) {
                if (sum != *expected) {
                    cerr << "  (Expected " << *expected << ")";
                    abort();
                }
                expected++;
            }

            // print_series_report(series);
        }
        i++;
    }
    if (sum <= 91464976098328)
        cerr << "TOO LOW\n";
    done = true;
    t.join();
    return 0;
};
