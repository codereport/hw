
// https://godbolt.org/z/W7z6oo

#include <fmt/core.h>
#include <string>
#include <numeric>
#include <algorithm>

struct helper { int left, right, max; };

auto one_direction(auto s) {
    return std::accumulate(std::begin(s), std::end(s), helper{},
        [](auto h, auto c) {
            c == '{' ? ++h.left : ++h.right;
            if (h.left == h.right) h.max = std::max(h.left * 2, h.max);
            if (h.right > h.left) h.left = 0, h.right = 0;
            return h;
        }).max;
}

auto longest_valid_paren(auto s) {
    using namespace std::views;
    auto t = s | transform([](auto c) { return c == '}' ? '{' : '}'; })
               | reverse;
    return std::max(one_direction(s), one_direction(t));
}

int main() {
    std::string s = "{{{}}"; // "}{}{{}}}}{}" "{}{{{}}}"
    fmt::print("{}", longest_valid_paren(s));
    return 0;
}
