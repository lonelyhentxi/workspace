#pragma once
#include "btree_util.hpp"
#include "tree.hpp"
#include <memory>

namespace tinydb::btree
{
	using std::shared_ptr;

	class btree final: public tree
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

	public:
		btree() = default;
		btree(const btree&) = default;
		btree(btree&&) noexcept = default;
		btree& operator=(const btree&) = default;
		btree& operator=(btree&&) noexcept = default;
		~btree() override = default;
	};
}
