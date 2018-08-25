#pragma once
#include <type_traits>
#include <future>

#include <utility>

namespace eevent
{

	using std::thread;
	using std::packaged_task;
	using std::future;
	using std::result_of;

	using std::move;

	template<typename F, typename A>
	future<result_of<F(A&&)>::type>
		spawn_task(F&& f, A&& a)
	{
		using result_type = result_of<F(A&&)>::type;
		packaged_task<result_type(A&&)>
			task(move(f));
			future<result_type> res(task.get_future());
			thread t(move(task), move(a));
			t.detach();
			return res;
	}
}
