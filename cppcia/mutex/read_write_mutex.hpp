#pragma once

#include <mutex>  
#include <condition_variable>  

namespace eevent
{
	using std::mutex;
	using std::condition_variable;
	using std::unique_lock;

	class read_write_mutex
	{
	public:
		read_write_mutex() = default;
		~read_write_mutex() = default;
		void lock_read()
		{
			unique_lock<mutex> ulk(counter_mutex);
			cond_r.wait(ulk, [=]()->bool {return write_cnt == 0; });
			++read_cnt;
		}
		void lock_write()
		{
			unique_lock<mutex> ulk(counter_mutex);
			++write_cnt;
			cond_w.wait(ulk, [=]()->bool {return read_cnt == 0 && !inwriteflag; });
			inwriteflag = true;
		}
		void release_read()
		{
			unique_lock<mutex> ulk(counter_mutex);
			if (--read_cnt == 0 && write_cnt > 0)
			{
				cond_w.notify_one();
			}
		}
		void release_write()
		{
			unique_lock<mutex> ulk(counter_mutex);
			if (--write_cnt == 0)
			{
				cond_r.notify_all();
			}
			else
			{
				cond_w.notify_one();
			}
			inwriteflag = false;
		}

	private:
		volatile size_t read_cnt{ 0 };
		volatile size_t write_cnt{ 0 };
		volatile bool inwriteflag{ false };
		mutex counter_mutex;
		condition_variable cond_w;
		condition_variable cond_r;
	};

	template <typename ReadWriteLockable>
	class unique_write_guard
	{
	public:
		explicit unique_write_guard(ReadWriteLockable &rw_lockable)
			: rw_lockable_(rw_lockable)
		{
			rw_lockable_.lock_write();
		}
		~unique_write_guard()
		{
			rw_lockable_.release_write();
		}
	private:
		unique_write_guard() = delete;
		unique_write_guard(const unique_write_guard&) = delete;
		unique_write_guard& operator=(const unique_write_guard&) = delete;
	private:
		ReadWriteLockable & rw_lockable_;
	};

	template <typename ReadWriteLockable>
	class unique_read_guard
	{
	public:
		explicit unique_read_guard(ReadWriteLockable &rw_lockable)
			: rw_lockable_(rw_lockable)
		{
			rw_lockable_.lock_read();
		}
		~unique_read_guard()
		{
			rw_lockable_.release_read();
		}
	private:
		unique_read_guard() = delete;
		unique_read_guard(const unique_read_guard&) = delete;
		unique_read_guard& operator=(const unique_read_guard&) = delete;
	private:
		ReadWriteLockable & rw_lockable_;
	};
}