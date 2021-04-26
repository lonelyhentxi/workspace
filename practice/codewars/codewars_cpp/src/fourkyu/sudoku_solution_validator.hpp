#ifndef CODEWARS_SUDOKU_SOLUTION_VALIDATOR_HPP
#define CODEWARS_SUDOKU_SOLUTION_VALIDATOR_HPP

#include <unordered_set>
#include <array>

namespace codewars {
    namespace fourkyu {
        using namespace std;

        bool validate_solution(const unsigned int board[9][9]) {
            for(auto i=0;i<9;i++) {
                auto row_unique_elements = unordered_set<int>();
                for(auto j=0;j<9;j++) {
                    row_unique_elements.insert(board[i][j]);
                }
                if(row_unique_elements.size()!=9||row_unique_elements.find(0)!=row_unique_elements.end()) {
                    return false;
                }
            }
            for(auto j=0;j<9;j++) {
                auto col_unique_elements = unordered_set<int>();
                for(auto i=0;i<9;i++) {
                    col_unique_elements.insert(board[i][j]);
                }
                if(col_unique_elements.size()!=9||col_unique_elements.find(0)!=col_unique_elements.end()) {
                    return false;
                }
            }
            auto unique_element_blocks = array<unordered_set<int>,9>{};
            for(auto i=0;i<9;i++) {
                for(auto j=0;j<9;j++) {
                    unique_element_blocks[i/3*3+j/3].insert(board[i][j]);
                }
            }
            for(auto i=0;i<9;i++){
                const auto & unique_block = unique_element_blocks[i];
                if(unique_block.size()!=9||unique_block.find(0)!=unique_block.end()) {
                    return false;
                }
            }
            return true;
        }
    }
}

#endif //CODEWARS_SUDOKU_SOLUTION_VALIDATOR_HPP
