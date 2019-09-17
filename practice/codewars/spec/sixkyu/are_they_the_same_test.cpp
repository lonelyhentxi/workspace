#include <catch2/catch.hpp>
#include "are_they_the_same.hpp"

TEST_CASE("comp","[are_they_the_same]"){
    using codewars::are_they_the_same::Same;
    REQUIRE(Same::comp(
            {121, 144, 19, 161, 19, 144, 19, 11},
            {14641, 20736, 361, 25921, 361, 20736, 361, 121}));
    REQUIRE(!Same::comp(
            {121, 144, 19, 161, 19, 144, 19, 11},
            {14641, 20736, 361, 25921, 361, 20736, 362, 121}
            ));
}

