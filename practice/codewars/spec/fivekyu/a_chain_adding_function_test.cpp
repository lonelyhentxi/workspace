#include  <catch2/catch.hpp>
#include "a_chain_adding_function.hpp"

TEST_CASE("add","[a_chain_adding_function]")
{
	using namespace codewars::fivekyu;
	REQUIRE(add(1)==1);
	REQUIRE(add(1)(2)==3);
	REQUIRE(add(1)(2)(3)==6);
	auto a = add(1)(2);
	a(3);
	REQUIRE(a == 3);
	auto b = add(3)(4);
	REQUIRE(b == 7);
}