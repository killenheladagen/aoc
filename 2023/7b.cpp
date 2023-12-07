#include <algorithm>
#include <iostream>
#include <string>
#include <tuple>
#include <vector>
using namespace std;

auto v = vector<tuple<string, int>>
#if 0
    {{"32T3K"s, 765},
     {"T55J5"s, 684},
     {"KK677"s, 28},
     {"KTJJT"s, 220},
     {"QQQJA"s, 483}}
#else
#include "7-input.h"
#endif
    ;

struct Hand {
    Hand(string z) {
        for (auto c : z) {
            string xx = "J23456789TQKA";
            auto v = xx.find(c);
            h.push_back(v);
        }
        q = h;
        sort(q.begin(), q.end());
        reverse(q.begin(), q.end());
        int max_type = 0;
        for (int ji = -1; ji < 5; ji++) {
            for (int r = 1; r <= 13; r++) {

                auto i = (ji >= 0 && q[ji] == 0) ? ji : 5;
                if (i != 5 && false) {
                    cout << z << ": ";
                    cout << "r=" << r;
                    cout << " i=" << i << "\n";
                }
                auto a = (i <= 0) ? r : q[0];
                auto b = (i <= 1) ? r : q[1];
                auto c = (i <= 2) ? r : q[2];
                auto d = (i <= 3) ? r : q[3];
                auto e = (i <= 4) ? r : q[4];
                vector<int> w = {a, b, c, d, e};
                sort(w.begin(), w.end());
                reverse(w.begin(), w.end());
                a = w[0];
                b = w[1];
                c = w[2];
                d = w[3];
                e = w[4];

                if (i != 5)
                    cout << hex << a << b << c << d << e << dec << "\n";
                type = 1;
                if (a == b || b == c || c == d || d == e)
                    type = 2;
                if ((a == b && c == d) || (a == b && d == e) ||
                    (b == c && d == e))
                    type = 3;
                if ((a == b && b == c) || (c == d && b == c) ||
                    (c == d && d == e))
                    type = 4;
                if ((a == b && b == c && d == e) ||
                    (a == b && c == d && d == e))
                    type = 5;
                if ((a == b && b == c && c == d) ||
                    (b == c && c == d && d == e))
                    type = 6;
                if ((a == e))
                    type = 7;

                if (type > max_type)
                    max_type = type;
            }
        }
        type = max_type;
    }
    int type;
    vector<int> h;
    vector<int> q;
};

vector<string> name = {"?",     "high card", "pair", "two pairs",
                       "three", "house",     "four", "five"};
auto &operator<<(ostream &os, Hand const &h) {
    os << "[";
    for (auto e : h.h) {
        os << hex << e;
    }
    os << "]";
    os << "[";
    for (auto e : h.q) {
        if (e)
            os << hex << e << dec;
        else
            os << "_";
    }
    os << "]"
       << " " << name[h.type];
    return os;
}

bool operator<(Hand a, Hand b) {
    if (a.type == b.type)
        return a.h < b.h;
    else
        return a.type < b.type;
}

//  win=bid*rank
//  rank=1..num_hands

//  5905
int main() {
    vector<tuple<Hand, int>> input;
    for (auto const &e : v) {
        input.push_back(make_tuple(Hand(get<0>(e)), get<1>(e)));
    }
    auto x = input;
    sort(x.begin(), x.end());
    int total = 0;
    for (int rank = 1; rank <= x.size(); rank++) {
        auto e = x[rank - 1];
        cout << get<0>(e) << "\n";
        total += get<1>(e) * rank;
    }
    cout << "total:\n" << total << "\n";
    return 0;
}
