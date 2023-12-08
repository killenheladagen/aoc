#include <algorithm>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <tuple>
#include <vector>
using namespace std;

#define USE 0

map<string, vector<string>> q;

int64_t rec = -1;
int64_t step = 0;

auto &operator<<(ostream &os, vector<string> const &v) {
    for (auto &e : v) {
        os << e << " ";
    }
    return os;
}

auto &operator<<(ostream &os, vector<int64_t> const &v) {
    for (auto &e : v) {
        os << e << " ";
    }
    return os;
}

bool all_end_with_z(vector<string> const &at) {
    int64_t num = 0;
    for (auto &x : at) {
        if (x[2] == 'Z')
            num++;
    }
    if (num > rec) {
        rec = num;
        cout << at << step << ": " << num << "\n";
    }
    return num == at.size();
    // for (auto x : at) {
    //     if (x[2] != 'Z')
    //         return false;
    // }
    // return true;
}

int main() {
#include "8b.txt"
    cout << "seq len=" << seq.size() << "\n";
    vector<string> start;
    for (auto [k, v] : q) {
        if (k[2] == 'A')
            start.push_back(k);
    }
    vector<int64_t> w;
    for (auto at : start) {
        step = 0;
        while (at[2] != 'Z') {
            // cout << at << "\n";
            auto x = seq[step % seq.size()];
            at = q[at][x];
            step++;
        }
        cout << " step =\n" << step << "\n";
        w.push_back(step);
    }
    /*
    cout << "gcd(" << w[0] << "," << w[1] << "," << w[2] << "," << w[3] << ","
         << w[4] << ")\n";
    */
    // auto prod = w[0] / g * w[1] / g * w[2] / g * w[3] / g * w[4] / g;
    int g = 271;
    for (auto &e : w)
        e /= g;
    auto wstart = w;

    while (*min_element(begin(w), end(w)) != *max_element(begin(w), end(w))) {
        auto it = min_element(begin(w), end(w));
        (*it) += wstart[distance(begin(w), it)];
        // cout << w << "\n";
    }
    cout << w[0] * g << "\n";
    // cout << prod * g << "\n";
    return 0;
}

/*
make
g++ 8b.cpp
clang-format -i 8b.cpp
./a.out
AAA BBA BLA DRA NFA PSA 0: 0
SLF NKC SLZ DDP XSH PPL 11653: 1
FDS HPD SLZ CMQ XRZ NPK 547691: 2
DBC LLT SLZ FDZ XRZ QBR 29027623: 3
JVX PKH SLZ FDZ XRZ HCZ 1712629757: 4
too low:                600796817
too low:                162815937407

 */
