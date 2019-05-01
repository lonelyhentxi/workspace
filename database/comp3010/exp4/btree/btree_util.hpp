#pragma once

#include <array>
#include <memory>

namespace tinydb::btree
{
	using std::array;
	using std::shared_ptr;

	constexpr size_t M = 2;

	struct btree_node
	{
		int k[2 * M - 1];
		array<shared_ptr<btree_node>, 2 * M> p;
		int num;
		bool is_leaf;
		shared_ptr<btree_node> prev;
		shared_ptr<btree_node> next;
	};


	struct storage_node
	{
		btree_node bnode;
		int index[M];
	};

	struct storage_struct
	{
		storage_node* snode;
		int len;
	};
}
