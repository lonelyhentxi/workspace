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
    };
}

#endif //TINY_DB_ENGINE_PAGER_HPP
