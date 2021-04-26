#ifndef CODEWARS_THE_OBSERVED_PIN_HPP
#define CODEWARS_THE_OBSERVED_PIN_HPP

#include <vector>
#include <unordered_map>
#include <cassert>
#include <algorithm>
#include <memory>

namespace codewars {
    namespace fourkyu {
        using namespace std;


        const vector<vector<char>> keypad = {{'1', '2', '3'},
                                             {'4', '5', '6'},
                                             {'7', '8', '9'},
                                             {'X', '0', 'X'}};
        const vector<tuple<int, int>> directions = {{1,  0},
                                                    {-1, 0},
                                                    {0,  1},
                                                    {0,  -1},
                                                    {0,  0}};

        unordered_map<char, shared_ptr<vector<char>>> gen_adjacent_map(vector<vector<char>> keypad) {
            const auto row_len = keypad.size();
            const auto col_len = keypad[0].size();
            auto res = unordered_map<char, shared_ptr<vector<char>>>{};
            for (auto i = 0; i < row_len; i++) {
                assert(keypad[i].size() == col_len);
                for (auto j = 0; j < col_len; j++) {
                    const auto current_center = keypad[i][j];
                    res[current_center] = make_shared<vector<char>>();
                    const auto p_adj = res[current_center];
                    for (const auto &direction: directions) {
                        const auto next_x = i + get<0>(direction);
                        const auto next_y = j + get<1>(direction);
                        if (next_x >= 0 && next_y >= 0 && next_x < row_len && next_y < col_len) {
                            auto key = keypad[next_x][next_y];
                            if (key != 'X') {
                                (*p_adj).push_back(key);
                            }
                        }
                    }
                }
            }
            return res;
        }

        vector<string> get_pins(const string &observed) {
            const unordered_map<char, shared_ptr<vector<char>>> adjacent_map = gen_adjacent_map(keypad);
            auto collection = vector<string>{""};
            auto probs = vector<shared_ptr<vector<char>>>(observed.size());
            transform(observed.cbegin(), observed.cend(), probs.begin(),
                      [&adjacent_map](char c) -> shared_ptr<vector<char>> {
                          return adjacent_map.at(c);
                      });
            for (const auto &digits:probs) {
                auto newCollection = decltype(collection){};
                for (const auto &prepared:collection) {
                    for (const auto &digit: (*digits)) {
                        newCollection.push_back(prepared + digit);
                    }
                }
                collection = std::move(newCollection);
            }
            return collection;
        }
    }
}

#endif //CODEWARS_THE_OBSERVED_PIN_HPP
