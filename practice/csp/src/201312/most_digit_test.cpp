#include "most_digit.hpp"
#ifdef WITH_CMAKE
#include <catch2/catch.hpp>
#endif


TEST_CASE("most_digit","[most_digit]")
{
	int32_t size = 6;
    auto numbers = vector<int32_t>{10,1,10,20,30,20};
	int32_t expected = 10;
	REQUIRE(most_digit(size,numbers)==expected);
}

