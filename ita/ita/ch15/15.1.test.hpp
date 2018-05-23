#pragma once
#include <boost/test/unit_test.hpp>

#include "cutting_pod.hpp"
#include "fibonacci.hpp"

namespace enfw
{
	namespace ita
	{
		namespace dp
		{
			namespace test
			{
				BOOST_AUTO_TEST_CASE(test_15_1_fibonacci)
				{
					const auto fib = fibonacci{};
					BOOST_CHECK(fib(10) == 89);
				}

				BOOST_AUTO_TEST_CASE(test_15_1_cutting_pod) {
					const std::vector<long> test = { 0,1,5,8,9,10,17,17,20,24,30 };
					const auto pod_pair = cutting_pod::with_method_cost_cutting_pod<decltype(test.cbegin()), long>(test.cbegin(), 10, 0);
					BOOST_CHECK(pod_pair == std::make_pair(std::vector<long>{ {0, 1, 5, 8, 10, 13, 17, 18, 22, 25, 30}}, std::vector<unsigned>{ {0, 1, 2, 3, 2, 2, 6, 1, 2, 3, 10}}));
					BOOST_CHECK(cutting_pod::with_cost_cutting_pod(test.cbegin(), 10, 0) == 30);
				}
			}
		}
	}
}

