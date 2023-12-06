#include <iostream>
#include <tuple>
#include <vector>

using namespace std;

auto v1 = vector<tuple<int, int>>{{7, 9}, {15, 40}, {30, 200}};

vector<int> count_ways(vector<tuple<int, int>> v) {
    vector<int> v_ways;
    for (auto e : v) {
        auto [time, dist] = e;
        int ways = 0;
        for (int charge = 1; charge < time; charge++) {
            int travel = charge * (time - charge);
            cout << charge << ": " << travel << "\n";
            if (travel > dist)
                ways++;
        }
        v_ways.push_back(ways);
    }
    return v_ways;
}
/*
 */
// travel > time, how many?
// multiply how many

// 288

auto v2 =
    vector<tuple<int, int>>{{60, 601}, {80, 1163}, {86, 1559}, {76, 1300}};

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
