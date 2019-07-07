#include <catch2/catch.hpp>
#include "./infix_to_postfix_converter.hpp"

TEST_CASE("to_postfix", "[infix_to_postfix_converter]")
{
	using codewars::fourkyu::to_postfix;
	REQUIRE(to_postfix("2+7*5")=="275*+");
	REQUIRE(to_postfix("3*3/(7+1)")=="33*71+/");
	REQUIRE(to_postfix("5+(6-2)*9+3^(7-1)")=="562-9*+371-^+");
	REQUIRE(to_postfix("(5-4-1)+9/5/2-7/1/7")=="54-1-95/2/+71/7/-");
}
