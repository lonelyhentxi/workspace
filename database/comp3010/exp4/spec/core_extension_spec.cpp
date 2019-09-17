#include <catch2/catch.hpp>
#include <sstream>
#include "extension.hpp"
#include "query.hpp"
#include "core.hpp"
#include "cppbtree/btree.h"

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
		auto rt_iter = rt->get_iterator(rt);
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
	const auto eng = engine();
	SECTION("linear where")
	{
		auto rt = make_shared<r_table>();
		const int32_t target_value = 2;
		auto linear_wear_rt = make_shared<r_table>();
		eng.inject(rt, 1, 112);
		eng.inject(linear_wear_rt, 20, 224);
		auto stored_tb = dynamic_pointer_cast<r_table>(
			linear_where(linear_wear_rt, rt, [target_value](const auto& r)-> bool
			{
				auto r1 = std::dynamic_pointer_cast<r_record>(r);
				return r1->get<0>()->value == target_value;
			}));
		const auto first_r_record = dynamic_pointer_cast<r_record>(stored_tb->get_iterator(stored_tb)->retrieve());
		REQUIRE(first_r_record->get<1>()->value == int32_t(0));
	}

	SECTION("binary search")
	{
		auto r = make_shared<r_table>();
		eng.inject(r, 1, 112);
		auto order_target = make_shared<r_table>();
		eng.inject(order_target, 60, 171);
		order_target = dynamic_pointer_cast<r_table>(
			order_by(order_target, r, [](const auto& lhs, const auto& rhs)-> bool
			{
				return dynamic_pointer_cast<r_record>(lhs)->get<0>()->value < dynamic_pointer_cast<r_record>(rhs)->get<0>()->value;
			}));
		auto bs_result = binary_search<int32_t>(order_target, [](const auto& lhs,int32_t rhs)-> int
		{
			return dynamic_pointer_cast<r_record>(lhs)->get<0>()->value - rhs;
			}, int32_t{ 4 });
		REQUIRE(bs_result->offset + bs_result->start == 0);
	}

	SECTION("btree index")
	{
		auto r = make_shared<r_table>();
		eng.inject(r, 1, 112);
		auto res = bptree_search<int32_t>(r, [](const shared_ptr<record> & rc)->int32_t
			{
				return dynamic_pointer_cast<r_record>(rc)->get<0>()->value;
			}, int32_t{ 4 });
		REQUIRE(res);
	}
}

TEST_CASE("projection_and_sets","[query]")
{
	const auto eng = engine();
	auto r = make_shared<r_table>();
	eng.inject(r, 1, 112);
	auto s = make_shared<s_table>();
	eng.inject(s, 20, 224);
	auto t = make_shared<s_table>
}
