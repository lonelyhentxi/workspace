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

static const std::string random_provider =
#ifdef BOOST_WINDOWS
"Microsoft Strong Cryptographic Provider"
#else
"/dev/urandom"
#endif
;

namespace enfw
{
	namespace ita
	{ 
		namespace dp
		{
			namespace test
			{
				BOOST_AUTO_TEST_CASE(test_15_2_simple_matrix_multiply) {
					using boost::numeric::ublas::matrix;
					auto ma = matrix<long>{};
					auto mb = matrix<long>{};
					using array_type = matrix<long>::array_type;
					{
						const unsigned x = 3;
						const unsigned y = 3;
						const unsigned z = 4;
						array_type arr1(x*z);
						for (auto &a : arr1)
						{
							a = 1;
						}
						array_type arr2(y*z);
						for (auto &a : arr2)
						{
							a = 0;
						}
						ma = matrix<long>(x, z, arr1);
						mb = matrix<long>(z, y, arr2);
					}
					auto mc = matrix<long>(3, 3, 0);
					auto md = matrix_multiply::simple_matrix_multiply(ma, mb);
					BOOST_CHECK(enfw::ds::matrix_equal(md, mc));
				}

				auto matrix_scale_arr = std::vector<unsigned>{30,35,15,5,10,20,25};
				auto result = matrix_multiply::matrix_chain_order(matrix_scale_arr.cbegin(), 6);

				BOOST_AUTO_TEST_CASE(test_15_2_matrix_chain_order) {
					using namespace std;
					stringstream expense, method;
					expense << result.first;
					method << result.second;
					BOOST_TEST_MESSAGE("[matrix chain expense]:\n" + expense.str()); 
					BOOST_TEST_MESSAGE("[matrix chain method]:\n" + method.str());
					BOOST_CHECK(result.first.at_element(1, 6) == 15125);
				}

				BOOST_AUTO_TEST_CASE(test_15_2_print_optimal_parens) {
					using namespace std;
					stringstream parens;
					matrix_multiply::print_optimal_parens(result.second, 1, 6, parens);
					BOOST_TEST_MESSAGE("[optimal parens]:\n"+parens.str());
					BOOST_CHECK(parens.str() == std::string{"((A1(A2A3))((A4A5)A6))"});
				}

				BOOST_AUTO_TEST_CASE(test_15_2_1) {
					using namespace std;
					auto matrix_scale_arr = vector<unsigned int>{5,10,3,12,5,50,6};
					auto result = matrix_multiply::matrix_chain_order(matrix_scale_arr.cbegin(), 6);
					stringstream parens;
					matrix_multiply::print_optimal_parens(result.second, 1, 6, parens);
					BOOST_TEST_MESSAGE("[optimal parens]:\n" + parens.str());
				}

				BOOST_AUTO_TEST_CASE(test_15_2_2_matrix_chain_multiply) {
					boost::random_device device(random_provider);
					const boost::random::uniform_int_distribution<long> random(-1, 1);
					const boost::random::uniform_int_distribution<long> size_random(1, 100);

					using namespace std;
					using ds::tiny_matrix;
					using size_type = unsigned int;
					auto matrix_scale_arr = vector<size_type>{};
					for(auto i=0;i<100;i++)
					{
						matrix_scale_arr.push_back(size_random(device));
					}
					matrix_scale_arr.push_back(10);
					vector<tiny_matrix<long>> vec_mat(1);
					vec_mat[0] = tiny_matrix<long>{};
					for(size_type i=0;i<matrix_scale_arr.size()-1;i++)
					{
						tiny_matrix<long> mat{ matrix_scale_arr[i] ,matrix_scale_arr[i + 1],0 };
						for(size_type j = 0; j < matrix_scale_arr[i];++j )
						{
							for(size_type k=0;k < matrix_scale_arr[i+1];++k)
							{
							     mat.insert_element(j,k,random(device));
							}
						}
						vec_mat.push_back(mat);
					}
					{
						using namespace chrono;
						auto start_time = high_resolution_clock::now();
						auto tmp_mult_res = vec_mat[1];
						for (decltype(matrix_scale_arr.size()) i = 2; i<matrix_scale_arr.size(); ++i)
						{
							tmp_mult_res = matrix_multiply::simple_matrix_multiply(tmp_mult_res, vec_mat[i]);
						}
						const auto simple_mult_duration = duration_cast<microseconds>(high_resolution_clock::now() - start_time);
						start_time = high_resolution_clock::now();
						auto optimal_mult_res = matrix_multiply::wrapped_matrix_chain_multiply(vec_mat.begin(), matrix_scale_arr.size() - 1);
						const auto optimal_mult_duration = duration_cast<microseconds>(high_resolution_clock::now() - start_time);
						start_time = high_resolution_clock::now();
						stringstream ss;
						ss << tmp_mult_res;
						BOOST_TEST_MESSAGE(ss.str());
						ss = stringstream{};
						ss << optimal_mult_res;
						BOOST_TEST_MESSAGE(ss.str());
						BOOST_CHECK(ds::matrix_equal(tmp_mult_res, optimal_mult_res));
						ss = stringstream{};
						ss << "\n" << (matrix_scale_arr.size() - 1) 
						<< "matrix\n""[simple multiply]:\nrun: " << simple_mult_duration.count() <<"us \n"
						<< "[optimal multiply]:\nrun: " << optimal_mult_duration.count() << "us \n";
						BOOST_TEST_MESSAGE(ss.str());
					}
				}
			}
		}
	}

	namespace ds
	{
		namespace test
		{
			BOOST_AUTO_TEST_CASE(test_15_2_tiny_matrix) {

				boost::random_device device(random_provider);
				const boost::random::uniform_int_distribution<long> random(0, 100);

				namespace ublas = boost::numeric::ublas;
				namespace ds = ds;

				auto tiny_m = ds::tiny_matrix<long>{ 3,4,0 };
				auto boost_m = ublas::matrix<long>{ 3,4,0 };
				std::stringstream temp;
				temp << tiny_m;
				BOOST_TEST_MESSAGE("[empty matrix]:\n" + temp.str());
				BOOST_CHECK(ds::matrix_equal(tiny_m, boost_m));
				for (auto i = 0; i < 3; i++)
				{
					for (auto j = 0; j < 4; j++)
					{
						const int rand = random(device);
						tiny_m.insert_element(i, j, rand);
						boost_m.insert_element(i, j, rand);
					}
				}
				temp = std::stringstream{};
				temp << tiny_m;
				BOOST_TEST_MESSAGE("[processed matrix]:\n"+temp.str());
				BOOST_CHECK(ds::matrix_equal(tiny_m, boost_m));
				tiny_matrix<long> tiny_m_1{3,4,0};
				temp >> tiny_m_1;
				BOOST_CHECK(ds::matrix_equal(tiny_m, tiny_m_1));
			}
		}
	}
}