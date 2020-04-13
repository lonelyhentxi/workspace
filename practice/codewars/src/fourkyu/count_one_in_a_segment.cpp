#include <catch2/catch.hpp>
#include "count_one_in_a_segment.hpp"

TEST_CASE("count_ones","[count_one_in_a_segment]") {
    REQUIRE(codewars::fourkyu::count_ones(4,7)==8);
}