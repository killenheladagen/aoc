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

    vector<string> at;
    for (auto [k, v] : q) {
        if (k[2] == 'A')
            at.push_back(k);
    }
    for(
    while (!all_end_with_z(at)) {
        // cout << at << "\n";
        auto x = seq[step % seq.size()];
        for (auto &i : at) {
            i = q[i][x];
        }
        step++;
    }
    cout << " step =\n" << step << "\n";
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

 */
