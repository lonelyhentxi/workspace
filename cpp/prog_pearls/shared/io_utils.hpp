#ifndef PROG_PEARLS_IO_UTILS_HPP
#define PROG_PEARLS_IO_UTILS_HPP

#include <algorithm>
#include <functional>
#include <iostream>
#include <cctype>
#include <locale>
#include <filesystem>
#include <optional>
#include <string>
#include <fstream>
#include <type_traits>

using namespace std;
using namespace std::filesystem;

string trim(string &&s) {
    s.erase(s.cbegin(), find_if(s.cbegin(), s.cend(), [](auto ch) { return !isspace(ch); }));
    s.erase(find_if(s.crbegin(), s.crend(), [](auto ch) { return !isspace(ch); }).base(), s.cend());
    return s;
}

optional<ifstream> path_to_ifstream(const path &p, ios_base::openmode mode) {
    if (!exists(p)) {
        return nullopt;
    }
    const auto input_entry = directory_entry{p};
    if (input_entry.status().type() != file_type::regular) {
        return nullopt;
    }
    return ifstream{p, mode};
}

optional<ofstream> path_to_ofstream(const path &p, ios_base::openmode mode) {
    if (!exists(p)) {
        return nullopt;
    }
    const auto input_entry = directory_entry{p};
    if (input_entry.status().type() != file_type::regular) {
        return nullopt;
    }
    return ofstream{p, mode};
}

#endif //PROG_PEARLS_IO_UTILS_HPP
