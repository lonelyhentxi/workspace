#pragma once
#include "btree_util.hpp"
#include <cstdio>
#include <iostream>
#include <memory>
#include <vector>

namespace tinydb::btree
{
	using std::shared_ptr;
	using std::make_shared;
	using std::vector;

	class tree
	{
	protected:
		int btree_node_num_;
		shared_ptr<btree_node> roots_;
		virtual shared_ptr<btree_node> btree_create() = 0;
		virtual shared_ptr<btree_node> btree_node_new() = 0;
		virtual void btree_split_child(shared_ptr<btree_node> parent, int pos, shared_ptr<btree_node> child) = 0;
		virtual void btree_insert_nonfull(shared_ptr<btree_node> node, int target) = 0;
		virtual void btree_merge_child(shared_ptr<btree_node> root, int pos, shared_ptr<btree_node> y, shared_ptr<btree_node> z) = 0;
		virtual void btree_delete_nonone(shared_ptr<btree_node> root, int target) = 0;
		virtual int btree_search_successor(shared_ptr<btree_node> root) = 0;
		virtual int btree_search_predecessor(shared_ptr<btree_node> root) = 0;
		virtual void btree_shift_to_left_child(shared_ptr<btree_node> root, int pos, shared_ptr<btree_node> y, shared_ptr<btree_node> z) = 0;
		virtual void btree_shift_to_right_child(shared_ptr<btree_node> root, int pos, shared_ptr<btree_node> y, shared_ptr<btree_node> z) = 0;
		virtual shared_ptr<btree_node> btree_insert(shared_ptr<btree_node> root, int target) = 0;
		virtual shared_ptr<btree_node> btree_delete(shared_ptr<btree_node> root, int target) = 0;
		virtual void btree_inorder_print(shared_ptr<btree_node> root) = 0;
		virtual void btree_level_display(shared_ptr<btree_node> root) = 0;
		virtual void save(shared_ptr<btree_node> root) = 0;
	public:
		tree() :btree_node_num_{ 0 }, roots_{nullptr} {};
		tree(const tree&) = default;
		tree(tree&&) noexcept = default;
		tree& operator=(const tree&) = default;
		tree& operator=(tree&&) noexcept = default;
		virtual ~tree()
		{
			btree_node_num_ = 0;
			roots_.reset();
		}
		void insert(const int target)
		{
			roots_ = btree_insert(roots_, target);
			save(roots_);
		}
		void level_display()
		{
			btree_level_display(roots_);
		};
		void del(const int target)
		{
			roots_ = btree_delete(roots_, target);
			save(roots_);
		};
		void inorder_print()
		{
			btree_inorder_print(roots_);
		};
		void node_num_print() const
		{
			printf("%d\n", btree_node_num_);
		};
	};
}