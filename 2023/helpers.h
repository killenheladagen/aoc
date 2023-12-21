#pragma once
#include <algorithm>
#include <atomic>
#include <cassert>
#include <cmath>
#include <fstream>
#include <future>
#include <gsl/gsl>
#include <iostream>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <thread>
#include <tuple>
#include <vector>

#define DUMP(VAR) " " << #VAR << "=" << VAR

namespace pretty {
#if 0
#else
std::string list_head("{");
std::string list_tail("}");
std::string item_sep(",");
#endif
} // namespace pretty

inline auto list_car(auto v) {
    v.resize(1);
    return v;
}

inline auto &operator<<(std::ostream &os, std::vector<std::string> const &v) {
    os << "[";
    for (int i = 0; i < v.size(); i++) {
        if (i > 0)
            os << ",";
        os << '"' << v[i] << '"';
    }
    os << "]";
    return os;
}

inline auto &operator<<(std::ostream &os, std::vector<int> const &v) {
    os << "[";
    for (int64_t i = 0; i < v.size(); i++) {
        if (i > 0)
            os << ",";
        os << v[i];
    }
    os << "]";
    return os;
}

inline auto &operator<<(std::ostream &os, std::vector<int64_t> const &v) {
    os << pretty::list_head;
    for (int64_t i = 0; i < v.size(); i++) {
        if (i > 0)
            os << pretty::item_sep;
        os << v[i];
    }
    os << pretty::list_tail;
    return os;
}

inline auto reverse(std::string s) {
    for (int i = 0; i <= (s.size() / 2); i++)
        std::swap(s[i], s[s.size() - i - 1]);
    return s;
}

inline auto split(std::string s, char sep) {
    using namespace std;
    vector<string> res;
    while (true) {
        auto pos = s.find(sep);
        if (pos == s.npos) {
            res.push_back(s);
            return res;
        }
        res.push_back(s.substr(0, pos));
        s = s.substr(pos + 1);
    }
}

inline auto read_file_as_string_vector(std::string const &filename) {
    using namespace std;
    vector<string> result;
    ifstream f(filename.c_str());
    for (string s; getline(f, s);)
        result.push_back(s);
    return result;
}

template <typename ElemT, typename MapFuncT>
inline auto mapcar(std::vector<ElemT> const &input, MapFuncT map_func) {
    using namespace std;
    vector<int64_t> result;
    result.reserve(input.size());
    transform(input.begin(), input.end(), back_inserter(result), map_func);
    return result;
}

inline auto unique_sorted(std::vector<int64_t> v) {
    using namespace std;
    sort(v.begin(), v.end());
    auto last = unique(v.begin(), v.end());
    v.erase(last, v.end());
    return v;
}

inline int modulo(int64_t k, int64_t n) { return (n + (k % n)) % n; }
