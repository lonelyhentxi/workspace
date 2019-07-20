#include <catch2/catch.hpp>
#include "./playing_with_passphrases.hpp"

TEST_CASE("PlayPass::playPass", "[playing_with_passphrases]") {
    using codewars::playing_with_passphrases::PlayPass;
    REQUIRE(PlayPass::playPass("I LOVE YOU!!!", 1) == "!!!vPz fWpM J");
    REQUIRE(PlayPass::playPass("I LOVE YOU!!!", 0) == "!!!uOy eVoL I");
    REQUIRE(PlayPass::playPass("AAABBCCY", 1) == "zDdCcBbB");
    REQUIRE(PlayPass::playPass("MY GRANMA CAME FROM NY ON THE 23RD OF APRIL 2015", 2) ==
            "4897 NkTrC Hq fT67 GjV Pq aP OqTh gOcE CoPcTi aO");
}