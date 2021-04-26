#include <catch2/catch.hpp>
#include "last_digit_of_a_huge_number.hpp"

TEST_CASE("last_digit","[last_digit_of_a_huge_number]")
{
	using namespace codewars::threekyu;
	SECTION("fixed test")
	{
		REQUIRE(last_digit({})==1);
		REQUIRE(last_digit({ 0,0 })==1);
		REQUIRE(last_digit({ 0,0,0 })==0);
		REQUIRE(last_digit({ 1,2 })==1);
		REQUIRE(last_digit({ 3,4,5 })==1);
		REQUIRE(last_digit({ 4,3,6 })==4);
		REQUIRE(last_digit({ 7,6,21 })==1);
		REQUIRE(last_digit({ 12,30,21 })==6);
		REQUIRE(last_digit({ 2,2,2,0 })==4);
		REQUIRE(last_digit({ 937640,767456,981242 })==0);
		REQUIRE(last_digit({ 123232,694022,140249 })==6);
		REQUIRE(last_digit({ 499942,898102,846073 })==6);
		REQUIRE(last_digit({ 2,2,2,2,2,1,1,2,1,2,1,2,2,1,1,2,2,0 })==6);
	}

	SECTION("random test")
	{
		int rand1 = rand() % 100;
		int rand2 = rand() % 10;
		REQUIRE(last_digit({ rand1 })==(rand1 % 10));
		REQUIRE(last_digit({ rand1,rand2 })==((int)pow(rand1 % 10, rand2) % 10));
	}
}