#include <catch2/catch.hpp>
#include "number_of_trailing_zeros_of_n.hpp"

TEST_CASE("zeros","[number_of_trailing_zeros_of_n]") {
    using codewars::number_of_trailing_zeros_of_n::zeros;
    REQUIRE(zeros(0)==0);
    REQUIRE(zeros(6)==1);
    REQUIRE(zeros(30)==7);
    REQUIRE(zeros(100)==24);
    REQUIRE(zeros(1000)==249);
    REQUIRE(zeros(100000)==24999);
    REQUIRE(zeros(1000000000)==249999998);
}