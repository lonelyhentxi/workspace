#include <catch2/catch.hpp>
#include <boost/filesystem.hpp>
#include <cstddef>
#include <valarray>
#include "extmem_dev.hpp"

using namespace std;

namespace fs = boost::filesystem;
using namespace tinydb::extmem;

TEST_CASE("create_extmem_dev","[extmem_dev]")
{
	auto extmem = extmem_device_manager::make("test0", 1024, 64, 520);
	const auto target_path = fs::path("./data/test0");
	REQUIRE(fs::exists(target_path));
}

TEST_CASE("block_write_and_read","[extmem_dev]") {
    auto extmem = extmem_device_manager::make("test0", 1024, 64, 520);
    const auto target_path = fs::path("./data/test0");
    auto test_buf = valarray<byte>{64};
    for(int i=0;i<64;i++) {
        test_buf[i] = static_cast<byte>(i);
    }
    const auto fail_buf_slice = slice(1,63,0);
    const auto test_buf_slice = slice(1,64,0);
    SECTION("basic_write") {
        REQUIRE(extmem->write_at_b(1,test_buf,test_buf_slice));
        REQUIRE(!extmem->write_at_b(0,test_buf,test_buf_slice));
        REQUIRE(!extmem->write_at_b(1,test_buf,fail_buf_slice));
    }
}