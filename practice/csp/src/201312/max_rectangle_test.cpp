#include "max_rectangle.hpp"
#ifdef WITH_CMAKE
#include <catch2/catch.hpp>
#endif

TEST_CASE("max_rectangle","[max_rectangle]") {
    REQUIRE(max_rectangle(6,{3,1,6,5,2,3})==10);
}