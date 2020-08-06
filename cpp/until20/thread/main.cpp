#include <iostream>
#include <thread>
#include <mutex>
#include <future>
#include <chrono>
#include <queue>
#include <condition_variable>
#include <atomic>

#define let const auto

int main()
{
	{
		std::thread t([]()
			{
				std::cout << "hello world." << std::endl;
			});
		t.join();
	}
	{
		int32_t v = 1;
		let critical_section = [&v](const int32_t change_v)
		{
			static std::mutex mtx;
			std::lock_guard<std::mutex> lock(mtx);
			v = change_v;
		};
		std::thread t1(critical_section, 2);
		std::thread t2(critical_section, 3);
		t1.join();
		t2.join();
		std::cout << v << std::endl;
	}
	{
		int32_t v = 1;
		let critical_section = [&v](const int32_t change_v)
		{
			static std::mutex mtx;
			std::unique_lock<std::mutex> lock(mtx);
			v = change_v;
			std::cout << v << std::endl;
			lock.unlock();

			lock.lock();
			v += 1;
			std::cout << v << std::endl;
		};

		std::thread t1(critical_section, 2), t2(critical_section, 3);
		t1.join();
		t2.join();

		std::cout << v << std::endl;
	}
	{
		std::packaged_task<int()> task([]() { return 7; });
		std::future<int> result = task.get_future();
		std::thread(std::move(task)).detach();
		std::cout << "waiting...";
		result.wait();
		std::cout << "done!" << std::endl << "future result is" << result.get() << std::endl;
	}
	{
		auto produced_nums = std::queue<int32_t>();
		std::mutex mtx;
		std::condition_variable cv;
		bool notified = false;
		bool stop = false;

		let producer = [&]()
		{
			for (int32_t i = 0; ; i++)
			{
				std::this_thread::sleep_for(std::chrono::milliseconds(90));
				std::unique_lock<std::mutex> lock(mtx);
				std::cout << "producing " << i << std::endl;
				produced_nums.push(i);
				notified = true;
				if (i >= 10)
				{
					stop = true;
					cv.notify_all();
					break;
				}
				cv.notify_all();
			}
			std::cout << "producer stop" << std::endl;
		};
		let consumer = [&]()
		{
			while(true)
			{
				std::unique_lock<std::mutex> lock(mtx);
				while(!notified)
				{
					cv.wait(lock);
				}
				lock.unlock();
				std::this_thread::sleep_for(std::chrono::milliseconds(100));
				lock.lock();
				while(!produced_nums.empty())
				{
					std::cout << "consuming " << produced_nums.front() << std::endl;
					produced_nums.pop();
				}
				notified = false;
				if (stop)
				{
					std::cout << "consumer stop" << std::endl;
					return;
				}
			}
		};
		std::thread p(producer);
		std::thread cs[2];
		for(int i=0;i<2;i++)
		{
			cs[i] = std::thread(consumer);
		}
		p.join();
		for(int i=0;i<2;i++)
		{
			cs[i].join();
		}
	}
	{
		struct A
		{
			float x;
			int y;
			long long z;
		};
		std::atomic<A> a;
		std::cout << std::boolalpha << a.is_lock_free() << std::endl;
	}
	{
		auto counter = std::atomic<int>{ 1 };
		auto vt = std::vector<std::thread>{};
		for(int32_t i = 0; i < 100; i++)
		{
			vt.emplace_back([&]()
				{
					counter.fetch_add(1, std::memory_order_relaxed);
				});
		}
		for(auto &t: vt)
		{
			t.join();
		}
		std::cout << "current counter" << counter << std::endl;
	}
	{
		auto ptr = std::atomic<int*>(nullptr);
		int v;
		std::thread producer([&]()
			{
				int* p = new int(42);
				v = 1024;
				ptr.store(p, std::memory_order_release);
			});

		std::thread consumer([&]()
			{
				int* p;
				while (!(p = ptr.load(std::memory_order_consume)));

				std::cout << "p: " << *p << std::endl;
				std::cout << "v: " << v << std::endl;
			});
		producer.join();
		consumer.join();
	}
	{
		auto v = std::vector<int>{};
		auto flag = std::atomic<int>{ 0 };
		std::thread release([&]
			{
				v.push_back(42);
				flag.store(1, std::memory_order_release);
			});
		std::thread acqrel([&]()
			{
				int expected = 1;
			while(!flag.compare_exchange_strong(expected, 2, std::memory_order_acq_rel))
			{
				expected = 1;
			}
			});
		std::thread acquire([&]()
			{
				while (flag.load(std::memory_order_acquire) < 2);
				std::cout << v.at(0) << std::endl;
			});
		release.join();
		acqrel.join();
		acquire.join();
	}
	{
		std::atomic<int> counter = { 0 };
		std::vector<std::thread> vt;
		for(int i = 0; i < 100; ++ i)
		{
			vt.emplace_back([&]
				{
					counter.fetch_add(1, std::memory_order_seq_cst);
				});
		}
		for(auto & t: vt)
		{
			t.join();
		}
		std::cout << "current counter:" << counter << std::endl;
	}
	return 0;
}