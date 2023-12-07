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
            string xx = "-23456789TJQKA";
            auto v = xx.find(c);
            h.push_back(v);
        }
        q = h;
        sort(q.begin(), q.end());
        reverse(q.begin(), q.end());
        type = 1;
        auto a = q[0];
        auto b = q[1];
        auto c = q[2];
        auto d = q[3];
        auto e = q[4];
        if (a == b || b == c || c == d || d == e)
            type = 2;
        if ((a == b && c == d) || (a == b && d == e) || (b == c && d == e))
            type = 3;
        if ((a == b && b == c) || (c == d && b == c) || (c == d && d == e))
            type = 4;
        if ((a == b && b == c && d == e) || (a == b && c == d && d == e))
            type = 5;
        if ((a == b && b == c && c == d) || (b == c && c == d && d == e))
            type = 6;
        if ((a == e))
            type = 7;
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
        os << hex << e << dec;
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

//  6440
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
