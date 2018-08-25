#pragma once
#include <vector>
#include <utility>
#include <sstream>

namespace enfw
{
	namespace ds
	{
		template <typename T, typename L = unsigned, typename A = std::vector<T>>
		class tiny_matrix
		{
		public:
			using value_type = T;
			using size_type = L;
			using array_type = A;
			using reference_type = T & ;

			tiny_matrix() = default;
			tiny_matrix(L size1, L size2, T default_value = T{}) :data_(size1*size2, default_value), size1_(size1), size2_(size2) {};

			size_type size1() const
			{
				return size1_;
			}

			size_type size2() const
			{
				return size2_;
			}

			size_type get_position(const size_type &x, const size_type &y) const
			{
				if (x >= size1() || y >= size2() || x<0 || y<0)
				{
					throw std::out_of_range{ "matrix position out of range" };
				}
				return x * size2() + y;
			}

			reference_type at_element(const size_type &x, const size_type &y)
			{
				return data_[get_position(x,y)];
			}

			reference_type insert_element(const size_type &x,const  size_type &y, value_type v)
			{
				return (at_element(x, y) = std::move(v));
			}
		private:
			array_type data_;
			size_type size1_;
			size_type size2_;
		};

		template <typename CharT,typename T, typename L, typename A>
		std::basic_ostream<CharT> & operator<<(std::basic_ostream<CharT> &os, tiny_matrix<T,L,A> &m)
		{
			using size_type = typename tiny_matrix<T,L,A>::size_type;
			for (size_type i = 0; i<m.size1(); ++i)
			{
				for (size_type j = 0; j<m.size2(); ++j)
				{
					os << m.at_element(i, j) << '\t';
				}
				os << std::endl;
			}
			return os;
		}

		template <typename CharT, typename T, typename L, typename A>
		std::basic_istream<CharT> & operator>>(std::basic_istream<CharT> &is, tiny_matrix<T, L, A> &m)
		{
			using value_type = typename tiny_matrix<T, L, A>::value_type;
			using size_type = typename tiny_matrix<T, L, A>::size_type;
			std::basic_string<CharT> black_hole;
			value_type v{};
			for (size_type i = 0; i<m.size1(); ++i)
			{
				for (size_type j = 0; j<m.size2(); ++j)
				{
					is >> v;
					m.insert_element(i, j, v);
				}
				std::getline(is, black_hole);
			}
			return is;
		}

		/**
		* TODO: @FIXME ?? boost bugs, cannot use const matrix here, because thus we cannot use at_element
		*/
		template<typename M1, typename M2>
		bool matrix_equal(M1 &m1, M2 &m2)
		{
			if (m1.size1() != m2.size1() || m1.size2() != m2.size2()) return false;
			using size_type = typename M1::size_type;
			for(size_type i=0;i<m1.size1();++i)
			{
				for(size_type j=0;j<m1.size2();++j)
				{
					if(m1.at_element(i, j) != m2.at_element(i, j))
					{
						return false;
					}
				}
			}
			return true;
		}
	}
}