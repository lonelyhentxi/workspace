#include "isbn_number.hpp"
#ifdef WITH_CMAKE
#include <catch2/catch.hpp>
#endif


TEST_CASE("isbn_number","[isbn_number]")
{
	REQUIRE(isbn_number("0-670-82162-4").first);
	REQUIRE(isbn_number("0-670-82162-0").second=="0-670-82162-4");
}

