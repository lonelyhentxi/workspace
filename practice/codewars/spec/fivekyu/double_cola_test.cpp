#include "double_cola.hpp"
#include <catch2/catch.hpp>

TEST_CASE("who_is_next", "[double_cola]") {
    std::vector<std::string> names = {"Sheldon", "Leonard", "Penny", "Rajesh", "Howard"};
    using codewars::double_cola::who_is_next;
    REQUIRE(who_is_next(names, 1)=="Sheldon");
    REQUIRE(who_is_next(names, 52)=="Penny");
    REQUIRE(who_is_next(names, 10010)=="Howard");
}