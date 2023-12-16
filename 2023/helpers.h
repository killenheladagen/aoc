#pragma once
#include <algorithm>
#include <atomic>
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
    for (int i = 0; i < v.size(); i++) {
        if (i > 0)
            os << ",";
        os << v[i];
    }
    os << "]";
    return os;
}
