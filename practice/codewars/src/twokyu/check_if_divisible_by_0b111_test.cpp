#include <catch2/catch.hpp>
#include "check_if_divisible_by_0b111.hpp"

#include <iostream>
#include <regex>
#include <bitset>

std::string int_to_bin(int i) {
	std::string str = std::bitset<64>(i).to_string();
	return str.erase(0, std::min(str.find_first_not_of('0'), str.size() - 1));
}

TEST_CASE("dfa_to_regex","[check_if_divisible_by_0b111]") {
    using namespace codewars::twokyu;
    auto transitions = vector<vector<size_t>>{{0,1},{2,3},{4,5},{6,0},{1,2},{3,4},{5,6},{0,1}};
    size_t start = 7;
    auto accepts = unordered_set<size_t>{0};
    auto symbols = vector<char>{'0','1'};
	const std::regex solution = std::regex{ dfa_to_regex(transitions,symbols,start,accepts) };

	SECTION("edge cases")
	{
		std::cout << "Testing for: empty string" << std::endl;
		REQUIRE(!std::regex_match("", solution));
		std::cout << "Testing for: 0" << std::endl;
		REQUIRE(std::regex_match("0", solution));
	}
	SECTION("fixed tests 100")
	{
		for (int i = 1; i <= 100; i++) {
			std::cout << "Testing for: " << i << std::endl;
			REQUIRE(std::regex_match(int_to_bin(i), solution)==(i % 7 == 0));
		}
	}
}
