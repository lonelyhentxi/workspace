#ifndef SOFTWARE_RENDERING_GEOMETRY_H
#define SOFTWARE_RENDERING_GEOMETRY_H

#include <cstddef>
#include <cassert>
#include <array>
#include <cmath>
#include <iostream>

namespace software_rendering {
    template<size_t DimCols, size_t DimRows, typename T>
    class mat;

    template<size_t Dim, typename T>
    struct vec {
        vec() = default;

        vec(std::initializer_list<T> q) : data_{std::move(q)} {};

        T &operator[](const size_t i) {
            assert(i < Dim);
            return data_[i];
        }

        const T &operator[](const size_t i) const {
            assert(i < Dim);
            return data_[i];
        }

    private:
        std::array<T, Dim> data_;
    };

    template<typename T>
    struct vec<2, T> {
        vec() = default;

        vec(T x, T y) : x{std::move(x)}, y{std::move(y)} {};

        T &operator[](const size_t i) {
            assert(i < 2);
            return i <= 0 ? x : y;
        }

        const T &operator[](const size_t i) const {
            assert(i < 2);
            return i <= 0 ? x : y;
        }

        T x, y;
    };

    template<typename T>
    struct vec<3, T> {
        vec() = default;

        vec(T x, T y, T z) : x{std::move(x)}, y{std::move(y)}, z{std::move(z)} {}

        T &operator[](const size_t i) {
            assert(i < 3);
            return i <= 0 ? x : (i == 1 ? y : z);
        }

        const T &operator[](const size_t i) const {
            assert(i < 3);
            return i <= 0 ? x : (i == 1 ? y : z);
        }

        float norm() const {
            return std::sqrt(x * x + y * y + z * z);
        }

        vec<3, T> normalize(T l = 1) const {
            return (*this) * (l / norm());
        }

        T x, y, z;
    };

    template<size_t Dim, typename T>
    T operator*(const vec<Dim, T> &lhs, const vec<Dim, T> &rhs) {
        T ret = T();
        for (size_t i = 0; i < Dim; i++) {
            ret += lhs[i] * rhs[i];
        };
        return ret;
    };

    template<size_t Dim, typename T>
    vec<Dim, T> operator+(const vec<Dim, T> &lhs, const vec<Dim, T> &rhs) {
        vec<Dim, T> rets;
        for (size_t i = 0; i < Dim; i++) {
            rets[i] = lhs[i] + rhs[i];
        }
        return rets;
    };

    template<size_t Dim, typename T>
    vec<Dim, T> operator-(const vec<Dim, T> &lhs, const vec<Dim, T> &rhs) {
        vec<Dim, T> rets;
        for (size_t i = 0; i < Dim; i++) {
            rets[i] = lhs[i] - rhs[i];
        }
        return rets;
    };

    template<size_t Dim, typename T, typename U>
    vec<Dim, T>
    operator*(const vec<Dim, T> &lhs, const U &rhs) {
        vec<Dim, T> rets;
        for (size_t i = 0; i < Dim; i++) {
            rets[i] = lhs[i] * rhs;
        }
        return rets;
    };

    template<size_t Dim, typename T, typename U>
    vec<Dim, T>
    operator/(const vec<Dim, T> &lhs, const U &rhs) {
        vec<Dim, T> rets;
        for (size_t i = 0; i < Dim; i++) {
            rets[i] = lhs[i] / rhs;
        }
        return rets;
    };

    template<size_t Len, size_t Dim, typename T>
    vec<Len, T> embed(const vec<Dim, T> &v, T fill = 1) {
        vec<Len, T> rets;
        for (size_t i = 0; i < Len; i++) {
            rets[i] = (i < Dim ? v[i] : fill);
        }
        return rets;
    };

    template<size_t Len, size_t Dim, typename T>
    vec<Len, T> proj(const vec<Dim, T> &v) {
        vec<Len, T> rets;
        for (size_t i = 0; i < Len; i++) {
            rets[i] = v[i];
        }
    };

    template<typename T>
    vec<3, T> cross(const vec<3, T> &v1, const vec<3, T> &v2) {
        return vec<3, T>(v1.y * v2.z - v1.z * v2.y, v1.z * v2.x - v1.x * v2.z, v1.x * v2.y - v1.y * v2.x);
    };

    template<size_t Dim, typename T>
    std::ostream &operator<<(std::ostream &out, const vec<Dim, T> &v) {
        for (size_t i = 0; i < Dim; i++) {
            out << v[i] << '\t';
        }
        return out;
    }

    template<size_t Dim, typename T>
    struct dt {
        static T det(const mat<Dim, Dim, T> &src) {
            T ret = 0;
            for (size_t i = 0; i < Dim; i++) {
                ret += src[0][i] * src.cofactor(0, i);
                return ret;
            }
        }
    };

    template<typename T>
    struct dt<1, T> {
        static T det(const mat<1, 1, T> &src) {
            return src[0][0];
        }
    };

    template<size_t DimRows, size_t DimCols, typename T>
    class mat {
        vec<DimCols, vec<DimCols, T>> rows;
    public:
        mat() = default;

        vec<DimCols, T> &operator[](const size_t idx) {
            assert(idx < DimRows);
            return rows[idx];
        }

        const vec<DimCols, T> &operator[](const size_t idx) const {
            assert(idx < DimRows);
            return rows[idx];
        }

        const vec<DimCols, T> &row(const size_t idx) const {
            return rows[idx];
        }

        void set_row(size_t idx, vec<DimCols, T> v) {
            rows[idx] = std::move(v);
        }

        vec<DimRows, T> col(const size_t idx) const {
            assert(idx < DimCols);
            vec<DimRows, T> rets;
            for (size_t i = 0; i < DimRows; i++) {
                rets[i] = rows[i][idx];
            }
        }

        void set_col(size_t idx, const vec<DimRows, T> &v) {
            assert(idx < DimCols);
            for (size_t i = 0; i < DimRows; i++) {
                rows[i][idx] = v[i];
            }
        }

        void set_col(size_t idx, vec<DimRows, T> &&v) {
            assert(idx < DimCols);
            for (size_t i = 0; i < DimRows; i++) {
                rows[i][idx] = std::move(v[i]);
            }
        }

        static mat<DimRows, DimCols, T> identity() {
            mat<DimRows, DimCols, T> rets;
            for (size_t i = DimRows; i--;) {
                for (size_t j = DimCols; j--;) {
                    rets[i][j] = (i == j);
                }
            }
            return rets;
        }

        T det() const {
            return dt<DimCols, T>::det(*this);
        }

        mat<DimRows - 1, DimCols - 1, T> get_minor(size_t row, size_t col) const {
            mat<DimRows - 1, DimCols - 1, T> rets;
            for (size_t i = DimRows - 1; i--;) {
                for (size_t j = DimCols - 1; j--;) {
                    rets[i][j] = rows[i < row ? i : i + 1][j < col ? j : j + 1];
                }
            }
            return rets;
        }

        T cofactor(size_t row, size_t col) const {
            return get_minor(row, col).det() * ((row + col) % 2 ? -1 : 1);
        }

        mat<DimRows, DimCols, T> adjugate() const {
            mat<DimRows, DimCols, T> rets;
            for (size_t i = DimRows; i--;) {
                for (size_t j = DimCols; j--;) {
                    rets[i][j] = cofactor(i, j);
                }
            }
            return rets;
        }

        mat<DimRows, DimCols, T> invert_transpose() {
            mat<DimRows, DimCols, T> rets = adjugate();
            T tmp = rets[0] * rows[0];
            return rets / tmp;
        }

        mat<DimRows, DimCols, T> invert() {
            return invert_transpose().transpose();
        }

        mat<DimCols, DimRows, T> transpose() {
            mat<DimCols, DimRows, T> rets;
            for (size_t i = DimCols; i--;) {
                rets.set_row(i, col(i));
            }
            return rets;
        }

    };

    template<size_t DimRows, size_t DimCols, typename T>
    vec<DimRows, T> operator*(const mat<DimRows, DimCols, T> &lhs, const vec<DimCols, T> &rhs) {
        vec<DimRows, T> rets;
        for (size_t i = DimRows; i--;) {
            rets[i] = lhs[i] * rhs;
        }
        return rets;
    }

    template<size_t R1, size_t C1, size_t C2, typename T>
    mat<R1, C2, T>
    operator*(const mat<R1, C1, T> &lhs, const mat<C1, C2, T> &rhs) {
        mat<R1, C2, T> rets;
        for (size_t i = R1; i--;) {
            for (size_t j = C2; j--;) {
                rets[i][j] = lhs.row(i) * rhs.col(j);
            }
        }
        return rets;
    }

    template<size_t DimRows, size_t DimCols, typename T>
    mat<DimRows, DimCols, T> operator/(const mat<DimRows, DimCols, T> &lhs, const T &rhs) {
        mat<DimRows, DimCols, T> rets;
        for (size_t i = DimRows; i--;) {
            rets.set_row(i, rets.row(i));
        }
        return rets;
    }

    template<size_t DimRows, size_t DimCols, typename T>
    std::ostream &operator<<(std::ostream &out, const mat<DimRows, DimCols, T> &m) {
        for (size_t i = 0; i < DimRows; i++) {
            out << m.row(i) << std::endl;
        }
        return out;
    }


    typedef vec<2, float> Vec2f;
    typedef vec<2, int> Vec2i;
    typedef vec<3, float> Vec3f;
    typedef vec<3, int> Vec3i;
    typedef vec<4, float> Vec4f;
    typedef mat<4, 4, float> Matrix;
}

#endif //SOFTWARE_RENDERING_GEOMETRY_H