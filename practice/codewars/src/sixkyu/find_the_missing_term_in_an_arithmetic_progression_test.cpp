#include "./find_the_missing_term_in_an_arithmetic_progression.hpp"
#include <catch2/catch.hpp>

TEST_CASE("findMissing","[find_the_missing_term_in_an_arithmetic_progression]") {
    using codewars::find_the_missing_term_in_an_arithmetic_progression::findMissing;
    REQUIRE(findMissing({1,3,5,9,11})==7);
}