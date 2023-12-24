#pragma once
#include <sstream>
#include <stdexcept>

struct point {
    int64_t x;
    int64_t y;
    point(int64_t x, int64_t y) : x(x), y(y) {}
    point() : point(-1, -1) {}

    auto &operator+=(point const b) {
        x += b.x;
        y += b.y;
        return *this;
    }
    auto &operator+=(char dir) {
        using namespace std;
        switch (dir) {
        case 'v':
        case 'D':
            y++;
            break;
        case '^':
        case 'U':
            y--;
            break;
        case '>':
        case 'R':
            x++;
            break;
        case '<':
        case 'L':
            x--;
            break;
        default:
            throw runtime_error("Illegal direction character '"s + dir + "'"s);
        }
        return *this;
    }

    int64_t hash() const { return y * (1LL << 30LL) + x; }
};

bool operator<(point a, point b) { return a.hash() < b.hash(); }

std::optional<point> step(point pos, char dir, point dim) {
    pos += dir;
    if (pos.x < 0 || pos.y < 0 || pos.x >= dim.x || pos.y >= dim.x)
        return std::nullopt;
    return pos;
}

auto straight_trajectory(point a, point b) {
    using namespace std;
    if (a.x == b.x)
        return (a.y > b.y) ? make_tuple('U', a.y - b.y)
                           : make_tuple('D', b.y - a.y);
    else if (a.y == b.y)
        return (a.x > b.x) ? make_tuple('L', a.x - b.x)
                           : make_tuple('R', b.x - a.x);
    else
        throw runtime_error("No straight trajectory between points");
}

auto &operator<<(std::ostream &os, point p) {
    return os << "{" << p.x << "," << p.y << "}";
}

auto &operator<<(std::ostream &os, std::vector<point> const &v) {
    os << "[";
    for (int64_t i = 0; i < v.size(); i++) {
        if (i > 0)
            os << ",";
        os << v[i];
    }
    os << "]";
    return os;
}

auto &operator<<(std::ostream &os, std::set<point> const &v) {
    os << "[";
    bool add_sep = false;
    for (auto const e : v) {
        if (add_sep)
            os << ",";
        add_sep = true;
        os << e;
    }
    os << "]";
    return os;
}

bool operator==(point a, point b) { return a.x == b.x && a.y == b.y; }

bool operator!=(point a, point b) { return !(a == b); }

auto operator+(point p, char dir) {
    p += dir;
    return p;
}

auto operator+(point a, point b) { return point{a.x + b.x, a.y + b.y}; }

auto operator-(point a, point b) { return point{a.x - b.x, a.y - b.y}; }

auto char_at(std::vector<std::string> const &w, point p) {
    using namespace std;
    if (p.x < 0 || p.y < 0 || p.y >= w.size() || p.x >= w[p.y].size()) {
        ostringstream os;
        os << "Illegal coordinates " << p;
        throw runtime_error(os.str());
    }
    return w[p.y][p.x];
}

auto &char_at(std::vector<std::string> &w, point p) {
    using namespace std;
    if (p.x < 0 || p.y < 0 || p.y >= w.size() || p.x >= w[p.y].size()) {
        ostringstream os;
        os << "Illegal coordinates " << p;
        throw runtime_error(os.str());
    }
    return w[p.y][p.x];
}

void draw_map(std::vector<std::string> const &w) {
    using namespace std;
    cerr << "\n";
    for (auto &row : w)
        cerr << row << "\n";
}

auto map_dimensions(std::vector<std::string> const &w) {
    return point(w[0].size(), w.size());
}
