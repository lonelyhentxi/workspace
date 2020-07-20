#include <cstdint>
#include <array>
#include <algorithm>
#include "io_utils.hpp"

using namespace std;
using namespace std::filesystem;

static auto arr = array<int32_t, 10000000>{};

int main() {
    auto is = path_to_ifstream(path("../data/ch1/p1_i.csv"), ios_base::in);
    if (is == nullopt) {
        return -1;
    }
    uint32_t i = 0;
    while (!is->eof()) {
        int32_t v;
        *is >> v;
        arr[i] = v;
        i++;
    }
    sort(arr.begin(), arr.begin() + i);
    auto os = path_to_ofstream(path("../data/ch1/p1_o.csv"), ios_base::out);
    if (os == nullopt) {
        return -1;
    }
    for (auto j = 0; j < i; j++) {
        *os << arr[j] << endl;
    }
    return 0;
}