#include <cassert>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>

using namespace std;

static int64_t extract_num(const char *&p) {
    try {
        size_t pos;
        int64_t num = stoi(string(p), &pos);
        if (pos < 1 || pos > 3)
            return -1;
        p += pos;
        // cerr << num << "\n";
        return num;
    } catch (invalid_argument const &) {
        return -1;
    }
}

static auto sum_muls(const char *filename) {
    int64_t sum = 0;
    auto f = ifstream{filename};
    stringstream buffer;
    buffer << f.rdbuf();
    auto const s = buffer.str();
    const char *p = &s[0];
    while (p = strstr(p, "mul(")) {
        p += 4;
        auto a = extract_num(p);
        if (a >= 0 && *p == ',') {
            p++;
            auto b = extract_num(p);
            if (b >= 0 && *p == ')') {
                p++;
                sum += a * b;
                // cerr << "  sum=" << sum << "\n";
            }
        }
    }
    return sum;
}

static auto sum_muls2(const char *filename) {
    bool enabled = true;
    int64_t sum = 0;
    auto f = ifstream{filename};
    stringstream buffer;
    buffer << f.rdbuf();
    auto const s = buffer.str();
    const char *p = &s[0];
    while (true) {
        // cerr << p << "\n";
        auto pm = strstr(p, "mul(");
        auto pe = strstr(p, "do()");
        auto pd = strstr(p, "don't()");
        if (!pm && !pe && !pd)
            return sum;
        if (pe && (!pm || (pe < pm)) && (!pd || (pe < pd))) {
            // cerr << "en\n";
            enabled = true;
            p = pe + 4;
        } else if (pd && (!pm || (pd < pm)) && (!pe || (pd < pe))) {
            // cerr << "di\n";
            enabled = false;
            p = pd + 6;
        } else {
            // cerr << "mul: " << pm << "\n";
            assert(pm && (!pe || (pm < pe)) && (!pd || (pm < pd)));
            p = pm + 4;
            auto a = extract_num(p);
            if (a >= 0 && *p == ',') {
                p++;
                auto b = extract_num(p);
                if (b >= 0 && *p == ')') {
                    p++;
                    if (enabled)
                        sum += a * b;
                    // cerr << "  sum=" << sum << "\n";
                }
            }
        }
    }
}

int main() {
    auto x = sum_muls("test.txt");
    assert(x == 161);
    cerr << sum_muls("input.txt") << "\n";

    x = sum_muls2("test2.txt");
    assert(x == 48);
    cerr << sum_muls2("input.txt") << "\n";
    return 0;
}
