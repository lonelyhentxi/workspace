#pragma once
#include "btree_util.hpp"
#include "tree.hpp"

namespace tinydb::btree
{
	class bptree final: public tree
	{
	protected:
		shared_ptr<btree_node> btree_create() override;
		shared_ptr<btree_node> btree_node_new() override;
		void btree_split_child(shared_ptr<btree_node> parent, int pos, shared_ptr<btree_node> child) override;
		void btree_insert_nonfull(shared_ptr<btree_node> node, int target) override;
		void btree_merge_child(shared_ptr<btree_node> root, int pos, shared_ptr<btree_node> y, shared_ptr<btree_node> z) override;
		void btree_delete_nonone(shared_ptr<btree_node> root, int target) override;
		int btree_search_successor(shared_ptr<btree_node> root) override;
		int btree_search_predecessor(shared_ptr<btree_node> root) override;
		void btree_shift_to_left_child(shared_ptr<btree_node> root, int pos, shared_ptr<btree_node> y, shared_ptr<btree_node> z) override;
		void btree_shift_to_right_child(shared_ptr<btree_node> root, int pos, shared_ptr<btree_node> y, shared_ptr<btree_node> z) override;
		shared_ptr<btree_node> btree_insert(shared_ptr<btree_node> root, int target) override;
		shared_ptr<btree_node> btree_delete(shared_ptr<btree_node> root, int target) override;
		void btree_inorder_print(shared_ptr<btree_node> root) override;
		void btree_level_display(shared_ptr<btree_node> root) override;
		void save(shared_ptr<btree_node> root) override;
		void btree_linear_print(shared_ptr<btree_node> root);

	public:
		bptree() = default;
		bptree(const bptree&) = default;
		bptree(bptree&&) noexcept = default;
		bptree& operator=(const bptree&) = default;
		bptree& operator=(bptree&&) noexcept = default;
		~bptree() noexcept = default;
		void linear_print();
	};

}