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
	auto test_buf1 = valarray<byte>{ 64 };
    for(int i=0;i<64;i++) {
        test_buf[i] = static_cast<byte>(i);
		test_buf1[i] = static_cast<byte>(0);
    }
    const auto fail_buf_slice = slice(0,63,1);
    const auto test_buf_slice = slice(0,64,1);
    SECTION("basic_write") {
        REQUIRE(extmem->write_at_b(1,test_buf,test_buf_slice));
        REQUIRE(!extmem->write_at_b(0,test_buf,test_buf_slice));
        REQUIRE(!extmem->write_at_b(1,test_buf,fail_buf_slice));
		REQUIRE(fs::exists("./data/test0/1.blk"));
    }
	SECTION("basic_read")
    {
		REQUIRE(extmem->write_at_b(1, test_buf, test_buf_slice));
		REQUIRE(extmem->read_at_b(1, test_buf1, test_buf_slice));
		REQUIRE(!extmem->read_at_b(0, test_buf1, test_buf_slice));
		REQUIRE(!extmem->read_at_b(1, test_buf1, fail_buf_slice));
		bool is_equal = true;
    	for(int i=0;i<64;i++)
		{
			is_equal = test_buf1[i] == test_buf[i]?is_equal:false;
		}
		REQUIRE(is_equal);
    }
}