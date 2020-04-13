#include <boost/config.hpp>

#include "../cxxstd/path.hpp"
#include "operations.hpp"
#include <boost/scoped_array.hpp>
#include <boost/system/error_code.hpp>
#include <boost/assert.hpp>
#include <algorithm>
#include <cstddef>
#include <cstring>
#include <cassert>

# include <boost/filesystem/detail/utf8_codecvt_facet.hpp>

#ifdef TINY_DB_ENGINE_FILESYSTEM_DEBUG
# include <iostream>
# include <iomanip>
#endif

namespace fs = tinydb::filesystem;

using tinydb::filesystem::path;

using std::string;
using std::wstring;

using boost::system::error_code;


namespace {


    typedef path::value_type value_type;
    typedef path::string_type string_type;
    typedef string_type::size_type size_type;

    const char *const separators = "/";
    const char *separator_string = "/";
    const char *preferred_separator_string = "/";

    bool is_root_separator(const string_type &str, size_type pos);


    size_type filename_pos(const string_type &str,
                           size_type end_pos);


    size_type root_directory_start(const string_type &path, size_type size);


    void first_element(
            const string_type &src,
            size_type &element_pos,
            size_type &element_size,
            size_type size = string_type::npos
    );

}


namespace tinydb::filesystem {
    path &path::operator/=(const path &p) {
        if (p.empty())
            return *this;
        if (this == &p) {
            path rhs(p);
            if (!detail::is_directory_separator(rhs.m_pathname[0]))
                m_append_separator_if_needed();
            m_pathname += rhs.m_pathname;
        } else {
            if (!detail::is_directory_separator(*p.m_pathname.begin()))
                m_append_separator_if_needed();
            m_pathname += p.m_pathname;
        }
        return *this;
    }

    path &path::operator/=(const value_type *ptr) {
        if (!*ptr)
            return *this;
        if (ptr >= m_pathname.data()
            && ptr < m_pathname.data() + m_pathname.size()) {
            path rhs(ptr);
            if (!detail::is_directory_separator(rhs.m_pathname[0]))
                m_append_separator_if_needed();
            m_pathname += rhs.m_pathname;
        } else {
            if (!detail::is_directory_separator(*ptr))
                m_append_separator_if_needed();
            m_pathname += ptr;
        }
        return *this;
    }

    int path::compare(const path &p) const BOOST_NOEXCEPT {
        return detail::lex_compare(begin(), end(), p.begin(), p.end());
    }


    path::string_type::size_type path::m_append_separator_if_needed() {
        if (!m_pathname.empty() &&
            !detail::is_directory_separator(*(m_pathname.end() - 1))) {
            string_type::size_type tmp(m_pathname.size());
            m_pathname += preferred_separator;
            return tmp;
        }
        return 0;
    }


    void path::m_erase_redundant_separator(string_type::size_type sep_pos) {
        if (sep_pos
            && sep_pos < m_pathname.size()
            && (m_pathname[sep_pos + 1] == separator
            )) { m_pathname.erase(sep_pos, 1); }
    }


    path &path::remove_filename() {
        m_pathname.erase(m_parent_path_end());
        return *this;
    }

    path &path::remove_trailing_separator() {
        if (!m_pathname.empty()
            && detail::is_directory_separator(m_pathname[m_pathname.size() - 1]))
            m_pathname.erase(m_pathname.size() - 1);
        return *this;
    }

    path &path::replace_extension(const path &new_extension) {

        m_pathname.erase(m_pathname.size() - extension().m_pathname.size());

        if (!new_extension.empty()) {

            if (new_extension.m_pathname[0] != dot)
                m_pathname.push_back(dot);
            m_pathname.append(new_extension.m_pathname);
        }

        return *this;
    }


    path path::root_path() const {
        path temp(root_name());
        if (!root_directory().empty()) temp.m_pathname += root_directory().c_str();
        return temp;
    }

    path path::root_name() const {
        iterator itr(begin());

        return (itr.m_pos != m_pathname.size()
                && (
                        (itr.m_element.m_pathname.size() > 1
                         && detail::is_directory_separator(itr.m_element.m_pathname[0])
                         && detail::is_directory_separator(itr.m_element.m_pathname[1])
                        )
                ))
               ? itr.m_element
               : path();
    }

    path path::root_directory() const {
        size_type pos(root_directory_start(m_pathname, m_pathname.size()));

        return pos == string_type::npos
               ? path()
               : path(m_pathname.c_str() + pos, m_pathname.c_str() + pos + 1);
    }

    path path::relative_path() const {
        iterator itr(begin());

        for (; itr.m_pos != m_pathname.size()
               && (detail::is_directory_separator(itr.m_element.m_pathname[0])
               ); ++itr) {}

        return path(m_pathname.c_str() + itr.m_pos);
    }

    string_type::size_type path::m_parent_path_end() const {
        size_type end_pos(filename_pos(m_pathname, m_pathname.size()));

        bool filename_was_separator(m_pathname.size()
                                    && detail::is_directory_separator(m_pathname[end_pos]));


        size_type root_dir_pos(root_directory_start(m_pathname, end_pos));
        for (;
                end_pos > 0
                && (end_pos - 1) != root_dir_pos
                && detail::is_directory_separator(m_pathname[end_pos - 1]);
                --end_pos) {}

        return (end_pos == 1 && root_dir_pos == 0 && filename_was_separator)
               ? string_type::npos
               : end_pos;
    }

    path path::parent_path() const {
        size_type end_pos(m_parent_path_end());
        return end_pos == string_type::npos
               ? path()
               : path(m_pathname.c_str(), m_pathname.c_str() + end_pos);
    }

    path path::filename() const {
        size_type pos(filename_pos(m_pathname, m_pathname.size()));
        return (m_pathname.size()
                && pos
                && detail::is_directory_separator(m_pathname[pos])
                && !is_root_separator(m_pathname, pos))
               ? detail::dot_path()
               : path(m_pathname.c_str() + pos);
    }

    path path::stem() const {
        path name(filename());
        if (name == detail::dot_path() || name == detail::dot_dot_path()) return name;
        size_type pos(name.m_pathname.rfind(dot));
        return pos == string_type::npos
               ? name
               : path(name.m_pathname.c_str(), name.m_pathname.c_str() + pos);
    }

    path path::extension() const {
        path name(filename());
        if (name == detail::dot_path() || name == detail::dot_dot_path()) return path();
        size_type pos(name.m_pathname.rfind(dot));
        return pos == string_type::npos
               ? path()
               : path(name.m_pathname.c_str() + pos);
    }


    namespace detail {


        inline
        std::pair<path::iterator, path::iterator> mismatch(path::iterator it1,
                                                           path::iterator it1end, path::iterator it2,
                                                           path::iterator it2end) {
            for (; it1 != it1end && it2 != it2end && *it1 == *it2;) {
                ++it1;
                ++it2;
            }
            return std::make_pair(it1, it2);
        }
    }

    path path::lexically_relative(const path &base) const {
        std::pair<path::iterator, path::iterator> mm
                = detail::mismatch(begin(), end(), base.begin(), base.end());
        if (mm.first == begin() && mm.second == base.begin())
            return path();
        if (mm.first == end() && mm.second == base.end())
            return detail::dot_path();
        path tmp;
        for (; mm.second != base.end(); ++mm.second)
            tmp /= detail::dot_dot_path();
        for (; mm.first != end(); ++mm.first)
            tmp /= *mm.first;
        return tmp;
    }


    path path::lexically_normal() const {
        if (m_pathname.empty())
            return *this;

        path temp;
        iterator start(begin());
        iterator last(end());
        iterator stop(last--);
        for (iterator itr(start); itr != stop; ++itr) {

            if (itr->native().size() == 1
                && (itr->native())[0] == dot
                && itr != start
                && itr != last)
                continue;


            if (!temp.empty()
                && itr->native().size() == 2
                && (itr->native())[0] == dot
                && (itr->native())[1] == dot) {
                string_type lf(temp.filename().native());
                if (lf.size() > 0
                    && (lf.size() != 1
                        || (lf[0] != dot
                            && lf[0] != separator))
                    && (lf.size() != 2
                        || (lf[0] != dot
                            && lf[1] != dot
                        )
                    )
                        ) {
                    temp.remove_filename();


                    iterator next(itr);
                    if (temp.empty() && ++next != stop
                        && next == last && *last == detail::dot_path()) {
                        temp /= detail::dot_path();
                    }
                    continue;
                }
            }

            temp /= *itr;
        };

        if (temp.empty())
            temp /= detail::dot_path();
        return temp;
    }

}

namespace {


    bool is_root_separator(const string_type &str, size_type pos) {
        BOOST_ASSERT_MSG(!str.empty() && fs::detail::is_directory_separator(str[pos]),
                         "precondition violation");


        while (pos > 0 && fs::detail::is_directory_separator(str[pos - 1]))
            --pos;


        if (pos == 0)
            return true;


        if (pos < 3 || !fs::detail::is_directory_separator(str[0])
            || !fs::detail::is_directory_separator(str[1]))
            return false;

        return str.find_first_of(separators, 2) == pos;
    }


    size_type filename_pos(const string_type &str,
                           size_type end_pos) {

        if (end_pos == 2
            && fs::detail::is_directory_separator(str[0])
            && fs::detail::is_directory_separator(str[1]))
            return 0;


        if (end_pos && fs::detail::is_directory_separator(str[end_pos - 1]))
            return end_pos - 1;


        size_type pos(str.find_last_of(separators, end_pos - 1));
        return (pos == string_type::npos
                || (pos == 1 && fs::detail::is_directory_separator(str[0])))
               ? 0
               : pos + 1;
    }


    size_type root_directory_start(const string_type &path, size_type size) {


        if (size == 2
            && fs::detail::is_directory_separator(path[0])
            && fs::detail::is_directory_separator(path[1]))
            return string_type::npos;


        if (size > 3
            && fs::detail::is_directory_separator(path[0])
            && fs::detail::is_directory_separator(path[1])
            && !fs::detail::is_directory_separator(path[2])) {
            string_type::size_type pos(path.find_first_of(separators, 2));
            return pos < size ? pos : string_type::npos;
        }


        if (size > 0 && fs::detail::is_directory_separator(path[0])) return 0;

        return string_type::npos;
    }


    void first_element(
            const string_type &src,
            size_type &element_pos,
            size_type &element_size,
            size_type size
    ) {
        if (size == string_type::npos) size = src.size();
        element_pos = 0;
        element_size = 0;
        if (src.empty()) return;

        string_type::size_type cur(0);


        if (size >= 2 && fs::detail::is_directory_separator(src[0])
            && fs::detail::is_directory_separator(src[1])
            && (size == 2
                || !fs::detail::is_directory_separator(src[2]))) {
            cur += 2;
            element_size += 2;
        } else if (fs::detail::is_directory_separator(src[0])) {
            ++element_size;

            while (cur + 1 < size
                   && fs::detail::is_directory_separator(src[cur + 1])) {
                ++cur;
                ++element_pos;
            }
            return;
        }


        while (cur < size
               && !fs::detail::is_directory_separator(src[cur])) {
            ++cur;
            ++element_size;
        }
        return;
    }

}


namespace tinydb::filesystem::detail {

    int lex_compare(path::iterator first1, path::iterator last1,
                    path::iterator first2, path::iterator last2) {
        for (; first1 != last1 && first2 != last2;) {
            if (first1->native() < first2->native()) return -1;
            if (first2->native() < first1->native()) return 1;
            BOOST_ASSERT(first2->native() == first1->native());
            ++first1;
            ++first2;
        }
        if (first1 == last1 && first2 == last2)
            return 0;
        return first1 == last1 ? -1 : 1;
    }


    const path &dot_path() {
        static const fs::path dot_pth(".");
        return dot_pth;
    }


    const path &dot_dot_path() {
        static const fs::path dot_dot("..");
        return dot_dot;
    }
}


path::iterator path::begin() const {
    iterator itr;
    itr.m_path_ptr = this;
    size_type element_size;
    first_element(m_pathname, itr.m_pos, element_size);
    itr.m_element = m_pathname.substr(itr.m_pos, element_size);
    if (itr.m_element.m_pathname == preferred_separator_string)
        itr.m_element.m_pathname = separator_string;
    return itr;
}

path::iterator path::end() const {
    iterator itr;
    itr.m_path_ptr = this;
    itr.m_pos = m_pathname.size();
    return itr;
}

void path::m_path_iterator_increment(path::iterator &it) {
    BOOST_ASSERT_MSG(it.m_pos < it.m_path_ptr->m_pathname.size(),
                     "path::basic_iterator increment past end()");


    it.m_pos += it.m_element.m_pathname.size();


    if (it.m_pos == it.m_path_ptr->m_pathname.size()) {
        it.m_element.clear();
        return;
    }


    bool was_net(it.m_element.m_pathname.size() > 2
                 && detail::is_directory_separator(it.m_element.m_pathname[0])
                 && detail::is_directory_separator(it.m_element.m_pathname[1])
                 && !detail::is_directory_separator(it.m_element.m_pathname[2]));


    if (detail::is_directory_separator(it.m_path_ptr->m_pathname[it.m_pos])) {

        if (was_net) {
            it.m_element.m_pathname = separator;
            return;
        }


        while (it.m_pos != it.m_path_ptr->m_pathname.size()
               && detail::is_directory_separator(it.m_path_ptr->m_pathname[it.m_pos])) { ++it.m_pos; }


        if (it.m_pos == it.m_path_ptr->m_pathname.size()
            && !is_root_separator(it.m_path_ptr->m_pathname, it.m_pos - 1)) {
            --it.m_pos;
            it.m_element = detail::dot_path();
            return;
        }
    }


    size_type end_pos(it.m_path_ptr->m_pathname.find_first_of(separators, it.m_pos));
    if (end_pos == string_type::npos)
        end_pos = it.m_path_ptr->m_pathname.size();
    it.m_element = it.m_path_ptr->m_pathname.substr(it.m_pos, end_pos - it.m_pos);
}

void path::m_path_iterator_decrement(path::iterator &it) {
    BOOST_ASSERT_MSG(it.m_pos, "path::iterator decrement past begin()");
    size_type end_pos(it.m_pos);
    if (it.m_pos == it.m_path_ptr->m_pathname.size()
        && it.m_path_ptr->m_pathname.size() > 1
        && detail::is_directory_separator(it.m_path_ptr->m_pathname[it.m_pos - 1])
        && !is_root_separator(it.m_path_ptr->m_pathname, it.m_pos - 1)
            ) {
        --it.m_pos;
        it.m_element = detail::dot_path();
        return;
    }

    size_type root_dir_pos(root_directory_start(it.m_path_ptr->m_pathname, end_pos));
    for (;
            end_pos > 0
            && (end_pos - 1) != root_dir_pos
            && detail::is_directory_separator(it.m_path_ptr->m_pathname[end_pos - 1]);
            --end_pos) {}

    it.m_pos = filename_pos(it.m_path_ptr->m_pathname, end_pos);
    it.m_element = it.m_path_ptr->m_pathname.substr(it.m_pos, end_pos - it.m_pos);
    if (it.m_element.m_pathname == preferred_separator_string)
        it.m_element.m_pathname = separator_string;

}

namespace {

    std::locale default_locale() {
        std::locale global_loc = std::locale();
        return std::locale(global_loc, new boost::filesystem::detail::utf8_codecvt_facet);
    }

    std::locale &path_locale() {

        static std::locale loc(default_locale());
#ifdef TINY_DB_ENGINE_FILESYSTEM_DEBUG
        std::cout << "***** path_locale() called" << std::endl;
#endif
        return
                loc;
    }
}

namespace tinydb::filesystem {


    const path::codecvt_type &path::codecvt() {
#ifdef TINY_DB_ENGINE_FILESYSTEM_DEBUG
        std::cout << "***** path::codecvt() called" << std::endl;
#endif
        BOOST_ASSERT_MSG(&path_locale(), "boost::filesystem::path locale initialization error");

        return std::use_facet<std::codecvt<wchar_t, char, std::mbstate_t> >(path_locale());
    }

    std::locale path::imbue(const std::locale &loc) {
#ifdef TINY_DB_ENGINE_FILESYSTEM_DEBUG
        std::cout << "***** path::imbue() called" << std::endl;
#endif
        std::locale temp(path_locale());
        path_locale() = loc;
        return temp;
    }

}  
