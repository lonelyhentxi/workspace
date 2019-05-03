#ifndef TINY_DB_ENGINE_QUERY_HPP
#define TINY_DB_ENGINE_QUERY_HPP

#include <functional>
#include "core.hpp"

namespace tinydb::core
{

	using std::function;

	shared_ptr<table> linear_where(shared_ptr<table> target,const shared_ptr<table>& source,function<bool(const shared_ptr<extmem::record>&)> pred)
	{
		auto tb_iter = source->get_iterator(source);
		auto target_iter = target->get_insertor(target);
		while (true)
		{
			const auto value = tb_iter->retrieve();
			if(pred(value))
			{
				target_iter->retrieve(value);
				target_iter = target_iter->next();
			}
			auto next = tb_iter->next();
			if (!next.has_value())
			{
				break;
			}
			tb_iter = *next;
		}
		target_iter->save();
		return target;
	}

	void binary_where()
	{
		
	}

	void select()
	{
		
	}
}

#endif