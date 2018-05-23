#pragma once

#include <stdexcept>
#include "./tiny_matrix.hpp"

namespace enfw
{
	namespace ita
	{
		namespace dp
		{
			class matrix_multiply
			{
			public:
				/**
				* TODO: @FIXME ?? boost bugs, cannot use const matrix here, because thus we cannot use at_element
				*/
				template<typename Matrix>
				static Matrix simple_matrix_multiply(Matrix & ma, Matrix &mb)
				{
					using value_type = typename Matrix::value_type;
					using size_type = typename Matrix::size_type;
					if (ma.size2() != mb.size1())
					{
						throw std::logic_error::logic_error("incompatible dimensions");
					}
					else
					{
						Matrix mc{ ma.size1(),mb.size2() };
						for (size_type i = 0; i< ma.size1(); ++i)
						{
							for (size_type j = 0; j< mb.size2(); ++j)
							{
								long tempc = 0;
								for (size_type k = 0; k< ma.size2(); ++k)
								{
									tempc = tempc + ma.at_element(i, k)*mb.at_element(k, j);
								}
								mc.insert_element(i, j, tempc);
							}
						}
						return mc;
					}
				}

				template<typename Iterator, typename Length = unsigned, typename Matrix = ds::tiny_matrix<Length>>
				static std::pair<Matrix, Matrix> matrix_chain_order(Iterator p, Length size, const std::function<bool(Length,Length)> &compare = std::less<Length>())
				{
					auto expense = Matrix(size + 1,size + 1,0 );
					auto method = Matrix(size + 1,size + 1,0 );
					for (Length l = 2; l <= size; ++l)
					{
						for (Length i = 1; i <= size - l + 1; ++i)
						{
							Length j = i + l - 1, k = i;
							Length current_q = expense.at_element(i, k) + expense.at_element(k + 1, j) + ((*(p + i - 1)) * (*(p + k)) * (*(p + j)));
							Length q = current_q, s = k;
							for (k = i + 1; k<=j-1; ++k)
							{
								current_q = expense.at_element(i, k) + expense.at_element(k + 1, j) + *(p + i - 1) * *(p + k) * *(p + j);
								if (compare(current_q,q))
								{
									q = current_q;
									s = k;
								}
							}
							expense.insert_element(i, j, std::move(q));
							method.insert_element(i, j, std::move(s));
						}
					}
					return std::make_pair(expense, method);
				}

				template<typename Matrix,typename CharT>
				static std::basic_ostream<CharT> &print_optimal_parens(Matrix &matrix_optimal_method,
					const typename Matrix::value_type &i,const typename Matrix::value_type &j,std::basic_ostream<CharT> &os)
				{
					if (i == j) os << "A" << i;
					else
					{
						os << "(";
						print_optimal_parens(matrix_optimal_method, i, matrix_optimal_method.at_element(i, j), os);
						print_optimal_parens(matrix_optimal_method, matrix_optimal_method.at_element(i, j) + 1, j, os);
						os << ")";
					}
					return os;
				}

				template<typename Iterator,typename MethodMatrix>
				static typename Iterator::value_type matrix_chain_multiply(Iterator aitr,
					MethodMatrix &matrix_optimal_method,
					const typename Iterator::value_type::size_type &start,
					const typename Iterator::value_type::size_type &end,
					const std::function<bool(typename Iterator::value_type::size_type,
						typename Iterator::value_type::size_type)> &compare = std::less<typename Iterator::value_type::size_type>()
					)
				{
					using matrix_type = typename Iterator::value_type;
					using size_type = typename matrix_type::size_type;
					auto &s = matrix_optimal_method;
					if (start == end) return *(aitr + start);
					else
					{
						matrix_type left = matrix_chain_multiply(aitr, s, start, s.at_element(start, end),compare);
						matrix_type right = matrix_chain_multiply(aitr, s, s.at_element(start, end) + 1, end,compare);
						return std::move(simple_matrix_multiply(left, right));
					}
				}

				template<typename Iterator>
				static typename Iterator::value_type wrapped_matrix_chain_multiply(Iterator aitr,
					const typename Iterator::value_type::size_type &size,
					const std::function< bool(typename Iterator::value_type::size_type,
						typename Iterator::value_type::size_type)> &compare = std::less<typename Iterator::value_type::size_type>()
					)
				{
					using matrix_type = typename Iterator::value_type;
					using size_type = typename Iterator::value_type::size_type;
					std::vector<size_type> matrix_scale_arr( size + 1 );
					for(size_type i=1;i<=size;++i)
					{
						matrix_scale_arr[i] = (*(aitr + i)).size2();
					}
					matrix_scale_arr[0] = (*(aitr+1)).size1();
					auto matrix_optimal_method = matrix_multiply::matrix_chain_order(matrix_scale_arr.begin(), size, compare).second;
					return matrix_multiply::matrix_chain_multiply(aitr, matrix_optimal_method, static_cast<typename Iterator::value_type::size_type>(1), size,compare);
				}
			};
		}
	}
}