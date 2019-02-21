#include <catch2/catch.hpp>
#include "path_finder4_where_are_you.hpp"

TEST_CASE("path_finder4","[path_finder4_where_are_you]")
{
	using namespace codewars::fourkyu::path_finder4;
	REQUIRE(i_am_here("")==std::vector<int>{0, 0});
	REQUIRE(i_am_here("RLrl")==std::vector<int>{0, 0});
	REQUIRE(i_am_here("r5L2l4")==std::vector<int>{4, 3});
}