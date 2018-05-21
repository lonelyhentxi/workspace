#pragma once
#include <boost/test/unit_test.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/config.hpp>
#include <sstream>
#include <boost/random/random_device.hpp>
#include <boost/random/uniform_int_distribution.hpp>
#include <boost/lexical_cast.hpp>
#include <chrono>

#include "./matrix_multiply.hpp"
#include "./common_dp_sln.hpp"

namespace enfw
{
	namespace ita
	{
		namespace dp
		{
			namespace test
			{
				BOOST_AUTO_TEST_CASE(test_15_3_3_matrix_largest_chain_multiply) {
					boost::random_device device(random_provider);
					const boost::random::uniform_int_distribution<long> random(-1, 1);
					const boost::random::uniform_int_distribution<long> size_random(1, 100);

					using namespace std;
					using ds::tiny_matrix;
					using size_type = unsigned int;
					auto matrix_scale_arr = vector<size_type>{};
					for (auto i = 0; i < 100; i++)
					{
						matrix_scale_arr.push_back(size_random(device));
					}
					matrix_scale_arr.push_back(10);
					vector<tiny_matrix<long>> vec_mat(1);
					vec_mat[0] = tiny_matrix<long>{};
					for (size_type i = 0; i < matrix_scale_arr.size() - 1; i++)
					{
						tiny_matrix<long> mat{ matrix_scale_arr[i] ,matrix_scale_arr[i + 1],0 };
						for (size_type j = 0; j < matrix_scale_arr[i]; ++j)
						{
							for (size_type k = 0; k < matrix_scale_arr[i + 1]; ++k)
							{
								mat.insert_element(j, k, random(device));
							}
						}
						vec_mat.push_back(mat);
					}
					{
						using namespace chrono;
						auto start_time = high_resolution_clock::now();
						auto tmp_mult_res = vec_mat[1];
						for (decltype(matrix_scale_arr.size()) i = 2; i < matrix_scale_arr.size(); ++i)
						{
							tmp_mult_res = matrix_multiply::simple_matrix_multiply(tmp_mult_res, vec_mat[i]);
						}
						const auto simple_mult_duration = duration_cast<microseconds>(high_resolution_clock::now() - start_time);
						start_time = high_resolution_clock::now();
						start_time = high_resolution_clock::now();
						auto optimal_greatest_mult_res = matrix_multiply::wrapped_matrix_chain_multiply(vec_mat.begin(), matrix_scale_arr.size() - 1, greater<unsigned>());
						auto optimal_greatest_mult_duration = duration_cast<microseconds>(high_resolution_clock::now() - start_time);
						stringstream ss;
						ss << tmp_mult_res;
						BOOST_TEST_MESSAGE(ss.str());
						ss = stringstream{};
						ss << optimal_greatest_mult_res;
						BOOST_TEST_MESSAGE(ss.str());
						BOOST_CHECK(ds::matrix_equal(tmp_mult_res, optimal_greatest_mult_res));
						ss = stringstream{};
						ss << "\n" << (matrix_scale_arr.size() - 1)
							<< "matrix\n""[simple multiply]:\nrun: " << simple_mult_duration.count() << "us \n"
							<< "[optimal greatest multiply]:\nrun: " << optimal_greatest_mult_duration.count() << "us \n";
						BOOST_TEST_MESSAGE(ss.str());
					}
				}

				BOOST_AUTO_TEST_CASE(test_15_3_common_dp_sln) {
					const std::vector<long> test = { 0,1,5,8,9,10,17,17,20,24,30 };
					const auto pod_pair = common_dp_sln::common_linear_dp(test.cbegin(), 10, 0);
					BOOST_CHECK(pod_pair == std::make_pair(std::vector<long>{ {0, 1, 5, 8, 10, 13, 17, 18, 22, 25, 30}}, std::vector<unsigned>{ {0, 1, 2, 3, 2, 2, 6, 1, 2, 3, 10}}));
				}
			}
		}
	}
}