#include <catch2/catch.hpp>
#include "path_finder3_the_alpinist.hpp"

TEST_CASE( "path_finder3","[path_finder3_the_alpinist]" )
{
	using namespace codewars::threekyu::path_finder3;
	const string s1 =

		"000\n"
		"000\n"
		"000";

	const string s2 =

		"010\n"
		"010\n"
		"010";

	const string s3 =

		"010\n"
		"101\n"
		"010";

	const string s4 =

		"0707\n"
		"7070\n"
		"0707\n"
		"7070";

	const string s5 =

		"700000\n"
		"077770\n"
		"077770\n"
		"077770\n"
		"077770\n"
		"000007";

	const string s6 =

		"777000\n"
		"007000\n"
		"007000\n"
		"007000\n"
		"007000\n"
		"007777";

	const string s7 =

		"000000\n"
		"000000\n"
		"000000\n"
		"000010\n"
		"000109\n"
		"001010";

	REQUIRE(path_finder(s1)==0);
	REQUIRE(path_finder(s2)==2);
	REQUIRE(path_finder(s3)==4);
	REQUIRE(path_finder(s4)==42);
	REQUIRE(path_finder(s5)==14);
	REQUIRE(path_finder(s6)==0);
	REQUIRE(path_finder(s7)==4);
}
