
// https://godbolt.org/z/js3WKT

#include <fmt/core.h>
#include <algorithm>
#include <vector>

using namespace std::ranges;
using interval = std::pair<float,float>;

auto find_next(auto i, auto const& c, auto upper) {
    auto f = i;
    while (++i != c.end() && i->first <= upper)
        if (i->second > f->second) f = i;
    return f;
}

auto find_first_interval(auto const& c, float a) {
    auto f = find_if(c, [&](auto e) { return e.first <= a && e.second > a; });
    return find_next(f, c, a);
}

auto find_minimal_subcover(std::vector<interval> c, interval x) {
    auto [a, b] = x;
    sort(c);
    auto f   = find_first_interval(c, a);
    auto res = std::vector<interval>{*f};
    while (f->second < b) {
        f = find_next(f, c, f->second);
        res.push_back(*f);
    }
    return res;
}

int main() {
    auto x = interval{0, 10};
    auto c = std::vector<interval>{{1,9},{0,8},{5,10}};

    for (auto i : find_minimal_subcover(c, x))
        fmt::print("{},{}\n", i.first, i.second);
}
