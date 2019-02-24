#include "fun_number.hpp"
#ifdef WITH_CMAKE
#include <catch2/catch.hpp>
#endif

TEST_CASE("fun_number","[fun_number]") {
   REQUIRE(fun_number(4)==3);
}