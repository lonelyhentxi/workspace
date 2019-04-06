#include "does_my_number_look_big_in_this.hpp"
#include <catch2/catch.hpp>


TEST_CASE("narcissistic","[does_my_number_look_big_in_this]") {
    using codewars::does_my_number_look_big_in_this::narcissistic;
    REQUIRE(narcissistic(7));
    REQUIRE(narcissistic(371));
    REQUIRE(!narcissistic(122));
    REQUIRE(!narcissistic(4887));
}
