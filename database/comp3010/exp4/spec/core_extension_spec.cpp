#include <catch2/catch.hpp>
#include <sstream>
#include "extension.hpp"
#include "query.hpp"
#include "core.hpp"

using namespace tinydb::core;
using namespace tinydb::extmem;
using std::stringstream;

TEST_CASE("table_basic_usage","[extension]")
{
	SECTION("table_read_all")
	{
		auto rt_res = vector<shared_ptr<record>>{};
		auto st_res = vector<shared_ptr<record>>{};
		const auto eng = engine();
		auto rt = make_shared<r_table>();
		auto st = make_shared<s_table>();
		eng.inject(rt, 1, 112);
		eng.inject(st, 20, 224);
		auto rt_iter = rt -> get_iterator(rt);
		while (true)
		{
			rt_res.push_back(rt_iter->retrieve());
			auto next = rt_iter->next();
			if (!next.has_value())
			{
				break;
			}
			rt_iter = *next;
		}
		const auto first_r_record = dynamic_pointer_cast<r_record>(rt_res[0]);
		REQUIRE(first_r_record->get<0>() -> value == 2);
		REQUIRE(first_r_record->get<1>() -> value == 0);
	}
}

TEST_CASE("where","[query]")
{
	SECTION("linear where")
	{
		const auto eng = engine();
		auto rt = make_shared<r_table>();
		const int32_t target_value = 2;
		auto linear_wear_rt = make_shared<r_table>();
		eng.inject(rt, 1, 112);
		eng.inject(linear_wear_rt, 20, 224);
		auto stored_tb = dynamic_pointer_cast<r_table>(linear_where(linear_wear_rt, rt,[=](const auto &r)-> bool
		{
				auto r1 = std::dynamic_pointer_cast<r_record>(r);
				return r1->get<0>() -> value == target_value;
		}));
		const auto first_r_record = dynamic_pointer_cast<r_record>(stored_tb->get_iterator(stored_tb)->retrieve());
		REQUIRE(first_r_record->get<1>()->value == int32_t(0));
	}
}
