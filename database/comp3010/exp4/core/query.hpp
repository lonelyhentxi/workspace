#ifndef TINY_DB_ENGINE_QUERY_HPP
#define TINY_DB_ENGINE_QUERY_HPP

#include <cppbtree/btree_set.h>
#include <optional>
#include <functional>
#include "core.hpp"
#include "extension.hpp"

namespace tinydb::core
{

	using std::function;

	shared_ptr<table> linear_where(shared_ptr<table> target, const shared_ptr<table>& source, function<bool(const shared_ptr<record>&)> pred);

	shared_ptr<table> order_by(shared_ptr<table> target, const shared_ptr<table>& source, function<bool(const shared_ptr<record>&, const shared_ptr<record>&)> comp);

	template<typename T>
	optional<table_iterator> binary_search(const shared_ptr<table>& source, function<int(const shared_ptr<record>&, T value)> comp, T value)
	{
		size_t lo = 0;
		size_t hi = source->num();
		auto iter = source->get_iterator(source);
		while (hi - lo <=1)
		{
			size_t mi = (lo + hi) / 2;
			iter = iter->to(mi);
			if (comp(iter->retrieve(), value) > 0)
			{
				hi = mi;
			}
			else if (comp(iter->retrieve(), value) == 0)
			{
				return { iter };
			}
			else
			{
				lo = mi;
			}
		}
		return {};
	}

	template<typename Key>
	auto bptree_search(const shared_ptr<table>& source,function<Key(const shared_ptr<record>&)> key_fn, const Key& value)
	{
		auto container = btree::btree_set<int32_t>();
		auto rt_iter = source->get_iterator(source);
		while (true)
		{
			auto key = key_fn(rt_iter->retrieve());
			container.insert(key);
			auto next = rt_iter->next();
			if (!next.has_value())
			{
				break;
			}
			rt_iter = *next;
		}
		return container.find(value)!=container.end();
	}

	shared_ptr<table> project(shared_ptr<table> target, const shared_ptr<table>& source, function<shared_ptr<record>(const shared_ptr<record>& reco)>& map);
}

#endif