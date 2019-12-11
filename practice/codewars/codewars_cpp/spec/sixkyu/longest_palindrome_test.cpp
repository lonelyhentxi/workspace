#include <catch2/catch.hpp>
#include "longest_palindrome.hpp"

TEST_CASE("longest_palindrome","[longest_palindrome]")
{
	using namespace codewars::sixkyu;
	REQUIRE(longest_palindrome("a")==1);
	REQUIRE(longest_palindrome("aa")==2);
	REQUIRE(longest_palindrome("baa")==2);
	REQUIRE(longest_palindrome("aab")==2);
	REQUIRE(longest_palindrome("zyabyz")!=6);
	REQUIRE(longest_palindrome("baabcd")==4);
	REQUIRE(longest_palindrome("baablkj12345432133d")==9);
}