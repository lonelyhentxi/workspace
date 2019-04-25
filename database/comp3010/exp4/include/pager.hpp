#ifndef TINY_DB_ENGINE_PAGER_HPP
#define TINY_DB_ENGINE_PAGER_HPP

#include <cstddef>
#include <iterator>

namespace tinydb::pager {
    class page_storage_interface {
    public:
        virtual size_t page_size() const = 0;
        virtual size_t page_content_size() const = 0;
        virtual size_t page_header_size() const = 0;
        virtual size_t buffer_pages() const = 0;
        virtual ~page_storage_interface() = default;

        class iterator {
            long num = FROM;
        public:
            iterator(long _num = 0) : num(_num) {}
            iterator& operator++() {num = TO >= FROM ? num + 1: num - 1; return *this;}
            iterator operator++(int) {iterator retval = *this; ++(*this); return retval;}
            bool operator==(iterator other) const {return num == other.num;}
            bool operator!=(iterator other) const {return !(*this == other);}
            long operator*() {return num;}
            // iterator traits
            using difference_type = long;
            using value_type = long;
            using pointer = const long*;
            using reference = const long&;
            using iterator_category = std::forward_iterator_tag;
        };
        iterator begin() {return FROM;}
        iterator end() {return TO >= FROM? TO+1 : TO-1;}
    };
}

#endif //TINY_DB_ENGINE_PAGER_HPP
