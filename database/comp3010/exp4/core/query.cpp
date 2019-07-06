#include "query.hpp"
#include "core.hpp"
#include <algorithm>
#include <optional>
#include <unordered_map>

namespace tinydb::core
{
	using std::unordered_map;
	using std::pair;
	using tinydb::core::table_iterator;
	using tinydb::core::table_insertor;

	shared_ptr<table> linear_where(shared_ptr<table> target, const shared_ptr<table>& source,
	                               function<bool(const shared_ptr<record>&)> pred)
	{
		auto tb_iter = source->get_iterator(source);
		auto target_iter = target->get_insertor(target);
		while (true)
		{
			if (!tb_iter.has_value())
			{
				break;
			}
			const auto value = tb_iter->retrieve();
			if (pred(value))
			{
				target_iter->retrieve(value);
				target_iter = target_iter->next();
			}
			tb_iter = tb_iter->next();
		}
		target_iter->save();
		return target;
	}

	shared_ptr<table> order_by(shared_ptr<table> target, const shared_ptr<table>& source,
	                           function<bool(const shared_ptr<record>&, const shared_ptr<record>&)> comp)
	{
		auto collections = vector<shared_ptr<record>>{};
		auto rt_iter = source->get_iterator(source);
		while (true)
		{
			if (!rt_iter.has_value())
			{
				break;
			}
			collections.push_back(rt_iter->retrieve());
			rt_iter = rt_iter->next();
		}
		std::sort(collections.begin(), collections.end(), comp);
		auto target_iter = target->get_insertor(target);
		for (const auto& item : collections)
		{
			target_iter->retrieve(item);
			target_iter = target_iter->next();
		}
		target_iter->save();
		return target;
	}

	shared_ptr<table> project(shared_ptr<table> target, const shared_ptr<table>& source,
	                          function<shared_ptr<record>(const shared_ptr<record>& reco)>& map)
	{
		auto iter = source->get_iterator(source);
		auto ins = target->get_insertor(target);
		while (true)
		{
			if (!iter.has_value())
			{
				break;
			}
			ins->retrieve(map(iter->retrieve()));
			ins = ins->next();
			iter = iter->next();
		}
		ins->save();
		return target;
	}

	template <typename Key>
	std::unordered_map<Key, shared_ptr<record>> hash(shared_ptr<table> source,
	                                                 function<Key(const shared_ptr<record>&)>& hash_fn)
	{
		auto iter = source->get_iterator(source);
		auto set = std::unordered_map<Key, shared_ptr<record>>();
		while (true)
		{
			if (!iter.has_value())
			{
				break;
			}
			set.insert(hash_fn(iter->retrieve()), iter->retrieve());
			iter = iter->next();
		}
		return set;
	}

	template <typename Key>
	shared_ptr<table> union_(shared_ptr<table> target, const shared_ptr<table>& src1, const shared_ptr<table>& src2,
	                         function<Key(const shared_ptr<record>&)>& hash_fn)
	{
		auto ins = target->get_insertor(target);
		auto set1 = hash(src1, hash_fn);
		auto set2 = hash(src2, hash_fn);
		for (const auto& i : set1)
		{
			set2.insert(i->first, i->second);
		}
		for (const auto& i : set2)
		{
			ins->retrieve(i->second);
			ins = ins->next();
		}
		ins->save();
		return target;
	}

	template <typename Key>
	shared_ptr<table> intersection(shared_ptr<table> target, const shared_ptr<table>& src1,
	                               const shared_ptr<table>& src2, function<Key(const shared_ptr<record>&)>& hash_fn)
	{
		auto ins = target->get_insertor(target);
		auto set1 = hash(src1, hash_fn);
		auto set2 = hash(src2, hash_fn);
		for (const auto& i : set1)
		{
			if (set2.find(i->first) != set2.cend())
			{
				ins->retrieve(i->second);
				ins = ins->next();
			}
		}
		ins->save();
		return target;
	}

	template <typename Key>
	shared_ptr<table> difference(shared_ptr<table> target, const shared_ptr<table>& src1, const shared_ptr<table>& src2,
	                             function<Key(const shared_ptr<record>&)>& hash_fn)
	{
		auto ins = target->get_insertor(target);
		auto set1 = hash(src1, hash_fn);
		auto set2 = hash(src2, hash_fn);
		for (const auto& i : set1)
		{
			if (set2.find(i->first) == set2.cend())
			{
				ins->retrieve(i->second);
				ins = ins->next();
			}
		}
		ins->save();
		return target;
	}

	shared_ptr<table> nest_loop_join(shared_ptr<table> target, const shared_ptr<table>& src1,
	                                 const shared_ptr<table>& src2,
	                                 function<bool(const shared_ptr<record>&, const shared_ptr<record>&)>& join_fn,
	                                 function<shared_ptr<record>(const shared_ptr<record>&, const shared_ptr<record>&)>&
	                                 link_fn)
	{
		auto ins = target->get_insertor(target);
		auto iter1 = src1->get_iterator(src1);
		while (true)
		{
			if (!iter1.has_value())
			{
				break;
			}
			const auto item1 = iter1->retrieve();
			auto iter2 = src2->get_iterator(src2);
			while (true)
			{
				if (!iter2.has_value())
				{
					break;
				}
				const auto item2 = iter2->retrieve();
				if (join_fn(item1, item2))
				{
					ins->retrieve(link_fn(item1, item2));
					ins = ins->next();
				}
			}
			iter1 = iter1->next();
		}
		ins->save();
		return target;
	}

	template <typename Key>
	shared_ptr<table> hash_join(shared_ptr<table> target, const shared_ptr<table>& src1,
		const shared_ptr<table>& src2,
		function<bool(const shared_ptr<record>&, const shared_ptr<record>&)>& join_fn,
		function<shared_ptr<record>(const shared_ptr<record>&, const shared_ptr<record>&)>&
		link_fn, function<Key(const shared_ptr<record>&)>& hash_fn)
	{
		auto ins = target->get_insertor(target);
		auto iter1 = src1->get_iterator(src1);
		auto iter2 = src2->get_iterator(src2);
		auto bucket = unordered_map<Key, pair<vector<shared_ptr<record>>,vector<shared_ptr<record>>>>();
		while(true)
		{
			if(!iter1.has_value())
			{
				break;
			}
			auto item1 = iter1->retrieve();
			auto hash1 = hash_fn(item1);
			auto found = bucket.find(hash1);
			if(found!=bucket.end())
			{
				bucket.insert(hash1, { {item1},{} });
			}
			else
			{
				bucket[hash1].first.push_back(item1);
			}
			iter1 = iter1->next();
		}
		while (true)
		{
			if (!iter2.has_value())
			{
				break;
			}
			auto item2 = iter2->retrieve();
			auto hash2 = hash_fn(item2);
			auto found = bucket.find(hash2);
			if (found != bucket.end())
			{
				bucket.insert(hash2, { {},{item2} });
			}
			else
			{
				bucket[hash2].second.push_back(item2);
			}
			iter2 = iter2->next();
		}
		for(const auto &entry:bucket)
		{
			for(const auto &x:entry.second.first)
			{
				for(const auto &y: entry.second.second)
				{
					if(join_fn(x,y))
					{
						ins->retrieve(link_fn(x, y));
						ins = ins->next();
					}
				}
			}
		}
		ins->save();
		return target;
	}
}
