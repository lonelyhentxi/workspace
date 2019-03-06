#include <catch2/catch.hpp>
#include "path_finder2_shortest_path.hpp"


TEST_CASE("path_finder2","[path_finder2_shortest_path.hpp]") {
	using namespace codewars::fourkyu;
	/*
	  Maze:
		.W.
		.W.
		...
	*/
	REQUIRE(path_finder2(".W.\n.W.\n...")==4);
	/*
	  Maze:
		.W.
		.W.
		W..
	*/
	REQUIRE(path_finder2(".W.\n.W.\nW..")==-1);
	/*
	  Maze:
		......
		......
		......
		......
		......
		......
	*/
	REQUIRE(path_finder2("......\n......\n......\n......\n......\n......")==10);
	/*
	  Maze:
		......
		......
		......
		......
		.....W
		....W.
	*/
	REQUIRE(path_finder2("......\n......\n......\n......\n.....W\n....W.")==-1);
}
