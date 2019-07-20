#ifndef TINY_DB_ENGINE_FILESYSTEM_PATH_HPP
#define TINY_DB_ENGINE_FILESYSTEM_PATH_HPP

#include <boost/config.hpp>
#include "path_traits.hpp"
#include <boost/system/error_code.hpp>
#include <boost/system/system_error.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/io/detail/quoted_manip.hpp>
#include <boost/static_assert.hpp>
#include <boost/functional/hash_fwd.hpp>
#include <string>
#include <iterator>
#include <cstring>
#include <iosfwd>
#include <stdexcept>
#include <cassert>
#include <locale>
#include <algorithm>

#include <boost/config/abi_prefix.hpp>

namespace tinydb::filesystem {

    class path {
    public:

        using value_type = char;
        constexpr static value_type separator = '/';
        constexpr static value_type preferred_separator = '/';
        constexpr static value_type dot = '.';
        typedef std::basic_string<value_type> string_type;
        typedef std::codecvt<wchar_t, char,
                std::mbstate_t> codecvt_type;


        path() noexcept = default;

        path(const path &p) = default;

        template<class Source>
        path(Source const &source,
             typename boost::enable_if<path_traits::is_pathable<
                     typename boost::decay<Source>::type>>::type * = nullptr) {
            path_traits::dispatch(source, m_pathname);
        }

        explicit path(const value_type *s) : m_pathname(s) {}

        explicit path(value_type *s) : m_pathname(s) {}

        explicit path(const string_type &s) : m_pathname(s) {}


        path(path &&p) noexcept { m_pathname = std::move(p.m_pathname); }

        path &operator=(path &&p) noexcept {
            m_pathname = std::move(p.m_pathname);
            return *this;
        }

        template<class Source>
        path(Source const &source, const codecvt_type &cvt) {
            path_traits::dispatch(source, m_pathname, cvt);
        }

        template<class InputIterator>
        path(InputIterator begin, InputIterator end) {
            if (begin != end) {

                std::basic_string<typename std::iterator_traits<InputIterator>::value_type>
                        seq(begin, end);
                path_traits::convert(seq.c_str(), seq.c_str() + seq.size(), m_pathname);
            }
        }

        template<class InputIterator>
        path(InputIterator begin, InputIterator end, const codecvt_type &cvt) {
            if (begin != end) {

                std::basic_string<typename std::iterator_traits<InputIterator>::value_type>
                        seq(begin, end);
                path_traits::convert(seq.c_str(), seq.c_str() + seq.size(), m_pathname, cvt);
            }
        }


        path &operator=(const path &p) {
            m_pathname = p.m_pathname;
            return *this;
        }

        template<class Source>
        typename boost::enable_if<path_traits::is_pathable<
                typename boost::decay<Source>::type>, path &>::type
        operator=(Source const &source) {
            m_pathname.clear();
            path_traits::dispatch(source, m_pathname);
            return *this;
        }


        path &operator=(const value_type *ptr) {
            m_pathname = ptr;
            return *this;
        }

        path &operator=(value_type *ptr) {
            m_pathname = ptr;
            return *this;
        }

        path &operator=(const string_type &s) {
            m_pathname = s;
            return *this;
        }

        path &operator=(string_type &s) {
            m_pathname = s;
            return *this;
        }

        path &assign(const value_type *ptr, const codecvt_type &) {
            m_pathname = ptr;
            return *this;
        }

        template<class Source>
        path &assign(Source const &source, const codecvt_type &cvt) {
            m_pathname.clear();
            path_traits::dispatch(source, m_pathname, cvt);
            return *this;
        }

        template<class InputIterator>
        path &assign(InputIterator begin, InputIterator end) {
            m_pathname.clear();
            if (begin != end) {
                std::basic_string<typename std::iterator_traits<InputIterator>::value_type>
                        seq(begin, end);
                path_traits::convert(seq.c_str(), seq.c_str() + seq.size(), m_pathname);
            }
            return *this;
        }

        template<class InputIterator>
        path &assign(InputIterator begin, InputIterator end, const codecvt_type &cvt) {
            m_pathname.clear();
            if (begin != end) {
                std::basic_string<typename std::iterator_traits<InputIterator>::value_type>
                        seq(begin, end);
                path_traits::convert(seq.c_str(), seq.c_str() + seq.size(), m_pathname, cvt);
            }
            return *this;
        }


        template<class Source>
        typename boost::enable_if<path_traits::is_pathable<
                typename boost::decay<Source>::type>, path &>::type
        operator+=(Source const &source) {
            return concat(source);
        }


        path &operator+=(const path &p) {
            m_pathname += p.m_pathname;
            return *this;
        }

        path &operator+=(const value_type *ptr) {
            m_pathname += ptr;
            return *this;
        }

        path &operator+=(value_type *ptr) {
            m_pathname += ptr;
            return *this;
        }

        path &operator+=(const string_type &s) {
            m_pathname += s;
            return *this;
        }

        path &operator+=(string_type &s) {
            m_pathname += s;
            return *this;
        }

        path &operator+=(value_type c) {
            m_pathname += c;
            return *this;
        }

        template<class CharT>
        typename boost::enable_if<std::is_integral<CharT>, path &>::type
        operator+=(CharT c) {
            CharT tmp[2];
            tmp[0] = c;
            tmp[1] = 0;
            return concat(tmp);
        }

        template<class Source>
        path &concat(Source const &source) {
            path_traits::dispatch(source, m_pathname);
            return *this;
        }

        template<class Source>
        path &concat(Source const &source, const codecvt_type &cvt) {
            path_traits::dispatch(source, m_pathname, cvt);
            return *this;
        }

        template<class InputIterator>
        path &concat(InputIterator begin, InputIterator end) {
            if (begin == end)
                return *this;
            std::basic_string<typename std::iterator_traits<InputIterator>::value_type>
                    seq(begin, end);
            path_traits::convert(seq.c_str(), seq.c_str() + seq.size(), m_pathname);
            return *this;
        }

        template<class InputIterator>
        path &concat(InputIterator begin, InputIterator end, const codecvt_type &cvt) {
            if (begin == end)
                return *this;
            std::basic_string<typename std::iterator_traits<InputIterator>::value_type>
                    seq(begin, end);
            path_traits::convert(seq.c_str(), seq.c_str() + seq.size(), m_pathname, cvt);
            return *this;
        }


        path &operator/=(const path &p);

        template<class Source>
        typename boost::enable_if<path_traits::is_pathable<
                typename boost::decay<Source>::type>, path &>::type
        operator/=(Source const &source) {
            return append(source);
        }

        path &operator/=(const value_type *ptr);

        path &operator/=(value_type *ptr) {
            return this->operator/=(const_cast<const value_type *>(ptr));
        }

        path &operator/=(const string_type &s) { return this->operator/=(path(s)); }

        path &operator/=(string_type &s) { return this->operator/=(path(s)); }

        path &append(const value_type *ptr) {
            this->operator/=(ptr);
            return *this;
        }

        path &append(const value_type *ptr, const codecvt_type &) {
            this->operator/=(ptr);
            return *this;
        }

        template<class Source>
        path &append(Source const &source);

        template<class Source>
        path &append(Source const &source, const codecvt_type &cvt);

        template<class InputIterator>
        path &append(InputIterator begin, InputIterator end);

        template<class InputIterator>
        path &append(InputIterator begin, InputIterator end, const codecvt_type &cvt);


        void clear() noexcept { m_pathname.clear(); }

        path &make_preferred() { return *this; }

        path &remove_filename();

        path &remove_trailing_separator();

        path &replace_extension(const path &new_extension = path());

        void swap(path &rhs) noexcept { m_pathname.swap(rhs.m_pathname); }

        const string_type &native() const noexcept { return m_pathname; }

        const value_type *c_str() const noexcept { return m_pathname.c_str(); }

        string_type::size_type size() const noexcept { return m_pathname.size(); }

        template<class String>
        String string() const;

        template<class String>
        String string(const codecvt_type &cvt) const;

        const std::string &string() const { return m_pathname; }

        const std::string &string(const codecvt_type &) const { return m_pathname; }

        const std::wstring wstring() const {
            std::wstring tmp;
            if (!m_pathname.empty())
                path_traits::convert(&*m_pathname.begin(), &*m_pathname.begin() + m_pathname.size(),
                                     tmp);
            return tmp;
        }

        const std::wstring wstring(const codecvt_type &cvt) const {
            std::wstring tmp;
            if (!m_pathname.empty())
                path_traits::convert(&*m_pathname.begin(), &*m_pathname.begin() + m_pathname.size(),
                                     tmp, cvt);
            return tmp;
        }

        path generic_path() const {
            return path(*this);
        }

        template<class String>
        String generic_string() const;

        template<class String>
        String generic_string(const codecvt_type &cvt) const;

        const std::string &generic_string() const { return m_pathname; }

        const std::string &generic_string(const codecvt_type &) const { return m_pathname; }

        const std::wstring generic_wstring() const { return wstring(); }

        const std::wstring generic_wstring(const codecvt_type &cvt) const { return wstring(cvt); }


        int compare(const path &p) const noexcept;

        int compare(const string_type &s) const { return compare(path(s)); }

        int compare(const value_type *s) const { return compare(path(s)); }


        path root_path() const;

        path root_name() const;

        path root_directory() const;

        path relative_path() const;

        path parent_path() const;

        path filename() const;

        path stem() const;

        path extension() const;


        bool empty() const noexcept { return m_pathname.empty(); }

        bool filename_is_dot() const;

        bool filename_is_dot_dot() const;

        bool has_root_path() const { return has_root_directory() || has_root_name(); }

        bool has_root_name() const { return !root_name().empty(); }

        bool has_root_directory() const { return !root_directory().empty(); }

        bool has_relative_path() const { return !relative_path().empty(); }

        bool has_parent_path() const { return !parent_path().empty(); }

        bool has_filename() const { return !m_pathname.empty(); }

        bool has_stem() const { return !stem().empty(); }

        bool has_extension() const { return !extension().empty(); }

        bool is_relative() const { return !is_absolute(); }

        bool is_absolute() const {
            return has_root_directory();
        }


        path lexically_normal() const;

        path lexically_relative(const path &base) const;

        path lexically_proximate(const path &base) const {
            path tmp(lexically_relative(base));
            return tmp.empty() ? *this : tmp;
        }


        class iterator;

        typedef iterator const_iterator;

        class reverse_iterator;

        typedef reverse_iterator const_reverse_iterator;

        iterator begin() const;

        iterator end() const;

        reverse_iterator rbegin() const;

        reverse_iterator rend() const;


        static std::locale imbue(const std::locale &loc);

        static const codecvt_type &codecvt();


# if defined(TINY_DB_ENGINE_FILESYSTEM_DEPRECATED) && defined(TINY_DB_ENGINE_FILESYSTEM_NO_DEPRECATED)
#   error both TINY_DB_ENGINE_FILESYSTEM_DEPRECATED and TINY_DB_ENGINE_FILESYSTEM_NO_DEPRECATED are defined
# endif

# if !defined(TINY_DB_ENGINE_FILESYSTEM_NO_DEPRECATED)

        path &normalize() {
            path tmp(lexically_normal());
            m_pathname.swap(tmp.m_pathname);
            return *this;
        }

        path &remove_leaf() { return remove_filename(); }

        path leaf() const { return filename(); }

        path branch_path() const { return parent_path(); }

        path generic() const { return generic_path(); }

        bool has_leaf() const { return !m_pathname.empty(); }

        bool has_branch_path() const { return !parent_path().empty(); }

        bool is_complete() const { return is_absolute(); }

# endif

    private:

#   if defined(_MSC_VER)
#     pragma warning(push)
#     pragma warning(disable : 4251) 
#   endif
/*
      m_pathname has the type, encoding, and format required by the native
      operating system. Thus for POSIX and Windows there is no conversion for
      passing m_pathname.c_str() to the O/S API or when obtaining a path from the
      O/S API. POSIX encoding is unspecified other than for dot and slash
      characters; POSIX just treats paths as a sequence of bytes. Windows
      encoding is UCS-2 or UTF-16 depending on the version.
*/
        string_type m_pathname;

#   if defined(_MSC_VER)
#     pragma warning(pop) 
#   endif

        string_type::size_type m_append_separator_if_needed();


        void m_erase_redundant_separator(string_type::size_type sep_pos);

        string_type::size_type m_parent_path_end() const;

        path &m_normalize();


        friend class iterator;

        friend bool operator<(const path &lhs, const path &rhs);


        static void m_path_iterator_increment(path::iterator &it);

        static void m_path_iterator_decrement(path::iterator &it);

    };

    namespace detail {

        int lex_compare(path::iterator first1, path::iterator last1,
                        path::iterator first2, path::iterator last2);

        const path &dot_path();

        const path &dot_dot_path();
    }

    typedef path wpath;


    class path::iterator
            : public boost::iterator_facade<
                    path::iterator,
                    path const,
                    boost::bidirectional_traversal_tag> {
    private:
        friend class boost::iterator_core_access;

        friend class tinydb::filesystem::path;

        friend class tinydb::filesystem::path::reverse_iterator;

        friend void m_path_iterator_increment(path::iterator &it);

        friend void m_path_iterator_decrement(path::iterator &it);

        const path &dereference() const { return m_element; }

        bool equal(const iterator &rhs) const {
            return m_path_ptr == rhs.m_path_ptr && m_pos == rhs.m_pos;
        }


        void increment() { m_path_iterator_increment(*this); }

        void decrement() { m_path_iterator_decrement(*this); }

        path m_element;
        const path *m_path_ptr;
        string_type::size_type m_pos;


    };


    class path::reverse_iterator
            : public boost::iterator_facade<
                    path::reverse_iterator,
                    path const,
                    boost::bidirectional_traversal_tag> {
    public:

        explicit reverse_iterator(iterator itr) : m_itr(itr) {
            if (itr != itr.m_path_ptr->begin())
                m_element = *--itr;
        }

    private:
        friend class boost::iterator_core_access;

        friend class tinydb::filesystem::path;

        const path &dereference() const { return m_element; }

        bool equal(const reverse_iterator &rhs) const { return m_itr == rhs.m_itr; }

        void increment() {
            --m_itr;
            if (m_itr != m_itr.m_path_ptr->begin()) {
                iterator tmp = m_itr;
                m_element = *--tmp;
            }
        }

        void decrement() {
            m_element = *m_itr;
            ++m_itr;
        }

        iterator m_itr;
        path m_element;

    };


    inline bool lexicographical_compare(path::iterator first1, path::iterator last1,
                                        path::iterator first2, path::iterator last2) {
        return detail::lex_compare(first1, last1, first2, last2) < 0;
    }

    inline bool operator==(const path &lhs, const path &rhs) { return lhs.compare(rhs) == 0; }

    inline bool operator==(const path &lhs, const path::string_type &rhs) { return lhs.compare(rhs) == 0; }

    inline bool operator==(const path::string_type &lhs, const path &rhs) { return rhs.compare(lhs) == 0; }

    inline bool operator==(const path &lhs, const path::value_type *rhs) { return lhs.compare(rhs) == 0; }

    inline bool operator==(const path::value_type *lhs, const path &rhs) { return rhs.compare(lhs) == 0; }

    inline bool operator!=(const path &lhs, const path &rhs) { return lhs.compare(rhs) != 0; }

    inline bool operator!=(const path &lhs, const path::string_type &rhs) { return lhs.compare(rhs) != 0; }

    inline bool operator!=(const path::string_type &lhs, const path &rhs) { return rhs.compare(lhs) != 0; }

    inline bool operator!=(const path &lhs, const path::value_type *rhs) { return lhs.compare(rhs) != 0; }

    inline bool operator!=(const path::value_type *lhs, const path &rhs) { return rhs.compare(lhs) != 0; }

    inline bool operator<(const path &lhs, const path &rhs) { return lhs.compare(rhs) < 0; }

    inline bool operator<=(const path &lhs, const path &rhs) { return !(rhs < lhs); }

    inline bool operator>(const path &lhs, const path &rhs) { return rhs < lhs; }

    inline bool operator>=(const path &lhs, const path &rhs) { return !(lhs < rhs); }

    inline std::size_t hash_value(const path &x) {
        return boost::hash_range(x.native().begin(), x.native().end());
    }

    inline void swap(path &lhs, path &rhs) { lhs.swap(rhs); }

    inline path operator/(const path &lhs, const path &rhs) { return path(lhs) /= rhs; }


    template<class Char, class Traits>
    inline std::basic_ostream<Char, Traits> &
    operator<<(std::basic_ostream<Char, Traits> &os, const path &p) {
        return os
                << boost::io::quoted(p.template string<std::basic_string<Char> >(), static_cast<Char>('&'));
    }

    template<class Char, class Traits>
    inline std::basic_istream<Char, Traits> &
    operator>>(std::basic_istream<Char, Traits> &is, path &p) {
        std::basic_string<Char> str;
        is >> boost::io::quoted(str, static_cast<Char>('&'));
        p = str;
        return is;
    }

    bool portable_posix_name(const std::string &name);

    bool windows_name(const std::string &name);

    bool portable_name(const std::string &name);

    bool portable_directory_name(const std::string &name);

    bool portable_file_name(const std::string &name);

    bool native(const std::string &name);

    namespace detail {
        inline bool is_directory_separator(path::value_type c) noexcept {
            return c == path::separator;
        }

        inline bool is_element_separator(path::value_type c) noexcept {
            return c == path::separator;
        }
    }


    inline path::reverse_iterator path::rbegin() const { return reverse_iterator(end()); }

    inline path::reverse_iterator path::rend() const { return reverse_iterator(begin()); }

    inline bool path::filename_is_dot() const {
        path p(filename());
        return p.size() == 1 && *p.c_str() == dot;
    }

    inline bool path::filename_is_dot_dot() const {
        return size() >= 2 && m_pathname[size() - 1] == dot && m_pathname[size() - 2] == dot
               && (m_pathname.size() == 2 || detail::is_element_separator(m_pathname[size() - 3]));
    }


    template<class InputIterator>
    path &path::append(InputIterator begin, InputIterator end) {
        if (begin == end)
            return *this;
        string_type::size_type sep_pos(m_append_separator_if_needed());
        std::basic_string<typename std::iterator_traits<InputIterator>::value_type>
                seq(begin, end);
        path_traits::convert(seq.c_str(), seq.c_str() + seq.size(), m_pathname);
        if (sep_pos)
            m_erase_redundant_separator(sep_pos);
        return *this;
    }

    template<class InputIterator>
    path &path::append(InputIterator begin, InputIterator end, const codecvt_type &cvt) {
        if (begin == end)
            return *this;
        string_type::size_type sep_pos(m_append_separator_if_needed());
        std::basic_string<typename std::iterator_traits<InputIterator>::value_type>
                seq(begin, end);
        path_traits::convert(seq.c_str(), seq.c_str() + seq.size(), m_pathname, cvt);
        if (sep_pos)
            m_erase_redundant_separator(sep_pos);
        return *this;
    }

    template<class Source>
    path &path::append(Source const &source) {
        if (path_traits::empty(source))
            return *this;
        string_type::size_type sep_pos(m_append_separator_if_needed());
        path_traits::dispatch(source, m_pathname);
        if (sep_pos)
            m_erase_redundant_separator(sep_pos);
        return *this;
    }

    template<class Source>
    path &path::append(Source const &source, const codecvt_type &cvt) {
        if (path_traits::empty(source))
            return *this;
        string_type::size_type sep_pos(m_append_separator_if_needed());
        path_traits::dispatch(source, m_pathname, cvt);
        if (sep_pos)
            m_erase_redundant_separator(sep_pos);
        return *this;
    }


    template<>
    inline
    std::string path::string<std::string>() const { return string(); }

    template<>
    inline
    std::wstring path::string<std::wstring>() const { return wstring(); }

    template<>
    inline
    std::string path::string<std::string>(const codecvt_type &cvt) const { return string(cvt); }

    template<>
    inline
    std::wstring path::string<std::wstring>(const codecvt_type &cvt) const { return wstring(cvt); }

    template<>
    inline
    std::string path::generic_string<std::string>() const { return generic_string(); }

    template<>
    inline
    std::wstring path::generic_string<std::wstring>() const { return generic_wstring(); }

    template<>
    inline
    std::string path::generic_string<std::string>(const codecvt_type &cvt) const { return generic_string(cvt); }

    template<>
    inline
    std::wstring path::generic_string<std::wstring>(const codecvt_type &cvt) const { return generic_wstring(cvt); }

    namespace path_traits {

        inline
        void convert(const char *from,
                     const char *from_end,
                     std::wstring &to) {
            convert(from, from_end, to, path::codecvt());
        }

        inline
        void convert(const wchar_t *from,
                     const wchar_t *from_end,
                     std::string &to) {
            convert(from, from_end, to, path::codecvt());
        }

        inline
        void convert(const char *from,
                     std::wstring &to) {
            BOOST_ASSERT(from);
            convert(from, nullptr, to, path::codecvt());
        }

        inline
        void convert(const wchar_t *from,
                     std::string &to) {
            BOOST_ASSERT(from);
            convert(from, nullptr, to, path::codecvt());
        }
    }
}


#include <boost/config/abi_suffix.hpp>

#endif  
