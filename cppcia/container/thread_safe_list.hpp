#pragma once

#include <mutex>

#include <utility>

namespace eevent
{
	using std::mutex;
	using std::unique_ptr;
	using std::shared_ptr;
	using std::make_shared;
	using std::lock_guard;

	using std::unique_lock;

	using std::move;

	template<typename T>
	class thread_safe_list
	{

		struct node
		{
			mutex m;
			shared_ptr<T> data;
			unique_ptr<node> next;

			node() :
				next()
			{}

			explicit node(T const& value) :
				data(make_shared<T>(value))
			{}
		};

		node head;

	public:
		thread_safe_list()
		{}

		~thread_safe_list()
		{
			remove_if([](T const&) {return true; });
		}

		thread_safe_list(thread_safe_list const& other) = delete;
		thread_safe_list& operator=(thread_safe_list const& other) = delete;

		void push_front(T const& value)
		{
			unique_ptr<node> new_node(new node(value));
			lock_guard<mutex> lk(head.m);
			new_node->next = move(head.next);
			head.next = move(new_node);
		}

		template<typename Function>
		void for_each(Function f)
		{
			node* current = &head;
			unique_lock<mutex> lk(head.m);
			while (node* const next = current->next.get())
			{
				unique_lock<mutex> next_lk(next->m);
				lk.unlock();
				f(*next->data);
				current = next;
				lk = move(next_lk);
			}
		}

		template<typename Predicate>
		shared_ptr<T> find_first_if(Predicate p)
		{
			node* current = &head;
			unique_lock<mutex> lk(head.m);
			while (node* const next = current->next.get())
			{
				unique_lock<mutex> next_lk(next->m);
				lk.unlock();
				if (p(*next->data))
				{
					return next->data;
				}
				current = next;
				lk = move(next_lk);
			}
			return shared_ptr<T>();
		}

		template<typename Predicate>
		void remove_if(Predicate p)
		{
			node* current = &head;
			unique_lock<mutex> lk(head.m);
			while (node* const next = current->next.get())
			{
				unique_lock<mutex> next_lk(next->m);
				if (p(*next->data))
				{
					unique_ptr<node> old_next = move(current->next);
					current->next = move(next->next);
					next_lk.unlock();
				}
				else
				{
					lk.unlock();
					current = next;
					lk = move(next_lk);
				}
			}
		}
	};
}