#include <iostream>
#include <tuple>
#include <vector>

using namespace std;

auto v1 = vector<tuple<int64_t, int64_t>>{{71530LL, 940200LL}};

vector<int64_t> count_ways(vector<tuple<int64_t, int64_t>> v) {
    vector<int64_t> v_ways;
    for (auto e : v) {
        auto [time, dist] = e;
        int64_t ways = 0;
        for (int64_t charge = 1; charge < time; charge++) {
            int64_t travel = charge * (time - charge);
            // cout << charge << ": " << travel << "\n";
            if (travel > dist)
                ways++;
        }
        v_ways.push_back(ways);
    }
    return v_ways;
}

// 71503
// prod=12227056
// prod=35961505

auto v2 = vector<tuple<int64_t, int64_t>>{{60808676LL, 601116315591300LL}};

int main() {
    for (auto v : {v1, v2}) {
        auto prod = 1;
        auto ways = count_ways(v);
        for (auto w : ways)
            prod *= w;
        cout << "prod=" << prod << "\n";
    }
    return 0;
}
