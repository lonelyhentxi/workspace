#include <catch2/catch.hpp>
#include "path_finder1_can_you_reach_the_exit.hpp"


TEST_CASE("path_finder1","[path_finder1_can_you_reach_the_exit]") {
	using namespace codewars::fourkyu;
	/*
	  Maze:
		.W.
		.W.
		...
	*/
	REQUIRE(path_finder1(".W.\n.W.\n...")==true);
	/*
	  Maze:
		.W.
		.W.
		W..
	*/
	REQUIRE(path_finder1(".W.\n.W.\nW..")==false);
	/*
	  Maze:
		......
		......
		......
		......
		......
		......
	*/
	REQUIRE(path_finder1("......\n......\n......\n......\n......\n......")==true);
	/*
	  Maze:
		......
		......
		......
		......
		.....W
		....W.
	*/
	REQUIRE(path_finder1("......\n......\n......\n......\n.....W\n....W.")==false);
}
