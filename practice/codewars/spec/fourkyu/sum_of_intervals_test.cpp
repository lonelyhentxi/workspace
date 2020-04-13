#include <catch2/catch.hpp>
#include "sum_of_intervals.hpp"

TEST_CASE("sum_intervals", "[sum_of_intervals]") {
    using namespace codewars::fourkyu;

    SECTION("single item") {
        std::vector<std::pair<int, int>> intervals = {{1, 5}};
        REQUIRE(sum_intervals(intervals) == 4);
    }

    SECTION("several items") {
        std::vector<std::pair<int, int>> intervals = {{1, 5},
                                                      {6, 10}};
        REQUIRE(sum_intervals(intervals) == 8);
    }

    SECTION("overlapped items") {
        std::vector<std::pair<int, int>> intervals = {{1,  5},
                                                      {10, 20},
                                                      {1,  6},
                                                      {16, 19},
                                                      {5,  11}};
        REQUIRE(sum_intervals(intervals) == 19);
    }
}