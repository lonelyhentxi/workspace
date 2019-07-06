#include <catch2/catch.hpp>
#include "parseint_preload.hpp"

TEST_CASE("parse_int","[parseint_preload]")
{
	using namespace codewars::twokyu;
	REQUIRE(parse_int("one") == 1);
	REQUIRE(parse_int("twenty") == 20);
	REQUIRE(parse_int("two hundred and forty-six")==246);
}