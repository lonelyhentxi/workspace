#include "bptree.hpp"
#include "tree.hpp"
#include <cstdio>
#include <memory>

namespace tinydb::btree
{

	shared_ptr<btree_node> bptree::btree_node_new()
	{
		auto node = make_shared<btree_node>();
		for (int i = 0; i < 2 * M - 1; i++) {
			node->k[i] = 0;
		}

		for (int i = 0; i < 2 * M; i++) {
			node->p[i] = nullptr;
		}

		node->num = 0;
		node->is_leaf = true;
		node->prev = nullptr;
		node->next = nullptr;
		return node;
	}

	shared_ptr<btree_node> bptree::btree_create()
	{
		shared_ptr<btree_node> node = btree_node_new();

		node->next = node;
		node->prev = node;

		return node;
	}

	void bptree::btree_split_child(shared_ptr<btree_node> parent, int pos, shared_ptr<btree_node> child)
	{
		shared_ptr<btree_node> new_child = btree_node_new();
		new_child->is_leaf = child->is_leaf;
		new_child->num = M - 1;

		for (int i = 0; i < M - 1; i++) {
			new_child->k[i] = child->k[i + M];
		}

		if (!new_child->is_leaf) {
			for (int i = 0; i < M; i++) {
				new_child->p[i] = child->p[i + M];
			}
		}

		child->num = M - 1;
		if (child->is_leaf) {
			child->num++;
		}

		for (int i = parent->num; i > pos; i--) {
			parent->p[i + 1] = parent->p[i];
		}
		parent->p[pos + 1] = new_child;

		for (int i = parent->num - 1; i >= pos; i--) {
			parent->k[i + 1] = parent->k[i];
		}
		parent->k[pos] = child->k[M - 1];

		parent->num += 1;

		if (child->is_leaf) {
			new_child->next = child->next;
			child->next->prev = new_child;
			new_child->prev = child;
			child->next = new_child;
		}
	}

	void bptree::btree_insert_nonfull(shared_ptr<btree_node> node, int target)
	{
		if (node->is_leaf) {
			int pos = node->num;
			while (pos >= 1 && target < node->k[pos - 1]) {
				node->k[pos] = node->k[pos - 1];
				pos--;
			}

			node->k[pos] = target;
			node->num += 1;
			btree_node_num_ += 1;

		}
		else {
			int pos = node->num;
			while (pos > 0 && target < node->k[pos - 1]) {
				pos--;
			}

			if (2 * M - 1 == node->p[pos]->num) {
				btree_split_child(node, pos, node->p[pos]);
				if (target > node->k[pos]) {
					pos++;
				}
			}

			btree_insert_nonfull(node->p[pos], target);
		}
	}

	shared_ptr<btree_node> bptree::btree_insert(shared_ptr<btree_node> root, int target)
	{
		if (nullptr == root) {
			return nullptr;
		}

		if (2 * M - 1 == root->num) {
			shared_ptr<btree_node> node = btree_node_new();
			if (nullptr == node) {
				return root;
			}

			node->is_leaf = false;
			node->p[0] = root;
			btree_split_child(node, 0, root);
			btree_insert_nonfull(node, target);
			return node;
		}
		else {
			btree_insert_nonfull(root, target);
			return root;
		}
	}

	void bptree::btree_merge_child(shared_ptr<btree_node> root, int pos, shared_ptr<btree_node> y, shared_ptr<btree_node> z)
	{
		if (y->is_leaf) {
			y->num = 2 * M - 2;
			for (int i = M; i < 2 * M - 1; i++) {
				y->k[i - 1] = z->k[i - M];
			}
		}
		else {
			y->num = 2 * M - 1;
			for (int i = M; i < 2 * M - 1; i++) {
				y->k[i] = z->k[i - M];
			}
			y->k[M - 1] = root->k[pos];
			for (int i = M; i < 2 * M; i++) {
				y->p[i] = z->p[i - M];
			}
		}

		for (int j = pos + 1; j < root->num; j++) {
			root->k[j - 1] = root->k[j];
			root->p[j] = root->p[j + 1];
		}

		root->num -= 1;

		if (y->is_leaf) {
			y->next = z->next;
			z->next->prev = y;
		}
	}

	shared_ptr<btree_node> bptree::btree_delete(shared_ptr<btree_node> root, int target)
	{
		if (1 == root->num) {
			shared_ptr<btree_node> y = root->p[0];
			shared_ptr<btree_node> z = root->p[1];
			if (nullptr != y && nullptr != z &&
				M - 1 == y->num && M - 1 == z->num) {
				btree_merge_child(root, 0, y, z);
				btree_delete_nonone(y, target);
				return y;
			}
			else {
				btree_delete_nonone(root, target);
				return root;
			}
		}
		else {
			btree_delete_nonone(root, target);
			return root;
		}
	}

	void bptree::btree_delete_nonone(shared_ptr<btree_node> root, int target)
	{
		if (root->is_leaf) {
			int i = 0;
			while (i < root->num && target > root->k[i]) i++;
			if (target == root->k[i]) {
				for (int j = i + 1; j < 2 * M - 1; j++) {
					root->k[j - 1] = root->k[j];
				}
				root->num -= 1;
				btree_node_num_ -= 1;

			}
			else {
				printf("target not found\n");
			}
		}
		else {
			int i = 0;
			shared_ptr<btree_node> y = nullptr, z = nullptr;
			while (i < root->num && target > root->k[i]) i++;

			y = root->p[i];
			if (i < root->num) {
				z = root->p[i + 1];
			}
			shared_ptr<btree_node> p = nullptr;
			if (i > 0) {
				p = root->p[i - 1];
			}

			if (y->num == M - 1) {
				if (i > 0 && p->num > M - 1) {
					btree_shift_to_right_child(root, i - 1, p, y);
				}
				else if (i < root->num && z->num > M - 1) {
					btree_shift_to_left_child(root, i, y, z);
				}
				else if (i > 0) {
					btree_merge_child(root, i - 1, p, y);
					y = p;
				}
				else {
					btree_merge_child(root, i, y, z);
				}
				btree_delete_nonone(y, target);
			}
			else {
				btree_delete_nonone(y, target);
			}
		}
	}

	int bptree::btree_search_predecessor(shared_ptr<btree_node> root)
	{
		shared_ptr<btree_node> y = root;
		while (!y->is_leaf) {
			y = y->p[y->num];
		}
		return y->k[y->num - 1];
	}

	int bptree::btree_search_successor(shared_ptr<btree_node> root)
	{
		shared_ptr<btree_node> z = root;
		while (!z->is_leaf) {
			z = z->p[0];
		}
		return z->k[0];
	}

	void bptree::btree_shift_to_right_child(shared_ptr<btree_node> root, int pos,
		shared_ptr<btree_node> y, shared_ptr<btree_node> z)
	{
		z->num += 1;

		if (! z->is_leaf) {
			z->k[0] = root->k[pos];
			root->k[pos] = y->k[y->num - 1];
		}
		else {
			z->k[0] = y->k[y->num - 1];
			root->k[pos] = y->k[y->num - 2];
		}

		for (int i = z->num - 1; i > 0; i--) {
			z->k[i] = z->k[i - 1];
		}

		if (! z->is_leaf) {
			for (int i = z->num; i > 0; i--) {
				z->p[i] = z->p[i - 1];
			}
			z->p[0] = y->p[y->num];
		}

		y->num -= 1;
	}

	void bptree::btree_shift_to_left_child(shared_ptr<btree_node> root, int pos,
		shared_ptr<btree_node> y, shared_ptr<btree_node> z)
	{
		y->num += 1;

		if (!z->is_leaf) {
			y->k[y->num - 1] = root->k[pos];
			root->k[pos] = z->k[0];
		}
		else {
			y->k[y->num - 1] = z->k[0];
			root->k[pos] = z->k[0];
		}

		for (int j = 1; j < z->num; j++) {
			z->k[j - 1] = z->k[j];
		}

		if (! z->is_leaf) {
			y->p[y->num] = z->p[0];
			for (int j = 1; j <= z->num; j++) {
				z->p[j - 1] = z->p[j];
			}
		}

		z->num -= 1;
	}

	void bptree::btree_inorder_print(shared_ptr<btree_node> root)
	{
		if (nullptr != root) {
			btree_inorder_print(root->p[0]);
			for (int i = 0; i < root->num; i++) {
				printf("%d ", root->k[i]);
				btree_inorder_print(root->p[i + 1]);
			}
		}
	}

	void bptree::btree_linear_print(shared_ptr<btree_node> root)
	{
		if (nullptr != root) {
			shared_ptr<btree_node> leftmost = root;
			while (false == leftmost->is_leaf) {
				leftmost = leftmost->p[0];
			}

			shared_ptr<btree_node> iter = leftmost;
			do {
				for (int i = 0; i < iter->num; i++) {
					printf("%d ", iter->k[i]);
				}
				iter = iter->next;
			} while (iter != leftmost);
			printf("\n");
		}
	}

	void bptree::save(shared_ptr<btree_node> root)
	{
	}

	void bptree::btree_level_display(shared_ptr<btree_node> root)
	{
		auto queue =  vector<shared_ptr<btree_node> >(200, nullptr);
		int front = 0;
		int rear = 0;

		queue[rear++] = root;

		while (front < rear) {
			shared_ptr<btree_node> node = queue[front++];

			printf("[");
			for (int i = 0; i < node->num; i++) {
				printf("%d ", node->k[i]);
			}
			printf("]");

			for (int i = 0; i <= node->num; i++) {
				if (nullptr != node->p[i]) {
					queue[rear++] = node->p[i];
				}
			}
		}
		printf("\n");
	}

	void bptree::linear_print()
	{
		btree_linear_print(roots_);
	}
}