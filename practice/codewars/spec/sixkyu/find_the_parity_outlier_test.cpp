#include "find_the_parity_outlier.hpp"
#include <catch2/catch.hpp>
#include <iostream>

TEST_CASE("FindOutlier", "[find_the_parity_outlier]") {
    using codewars::find_the_parity_outlier::FindOutlier;
    REQUIRE(FindOutlier({2, 3, 4}) == 3);
    REQUIRE(FindOutlier({1, 2, 3}) == 2);
    REQUIRE(FindOutlier({4, 1, 3, 5, 9}) == 4);
}