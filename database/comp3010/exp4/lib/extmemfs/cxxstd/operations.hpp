#ifndef TINY_DB_ENGINE_FILESYSTEM_OPERATIONS_HPP
#define TINY_DB_ENGINE_FILESYSTEM_OPERATIONS_HPP

#include <boost/config.hpp>
#include "path.hpp"

#include <boost/detail/scoped_enum_emulation.hpp>
#include <boost/detail/bitmask.hpp>
#include <boost/system/error_code.hpp>
#include <boost/system/system_error.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/utility/enable_if.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/cstdint.hpp>
#include <boost/assert.hpp>
#include <string>
#include <utility> 
#include <ctime>
#include <vector>
#include <stack>
#include <fstream>
#include <boost/config/abi_prefix.hpp> 



namespace tinydb::filesystem {

    
    
    
    
    

    class filesystem_error : public boost::system::system_error {
        

        

        
        

    public:
        

        filesystem_error(
                const std::string &what_arg, boost::system::error_code ec) noexcept
                : boost::system::system_error(ec, what_arg) {
            try {
                m_imp_ptr.reset(new m_imp);
            }
            catch (...) { m_imp_ptr.reset(); }
        }

        filesystem_error(
                const std::string &what_arg, const path &path1_arg,
                boost::system::error_code ec) noexcept
                : boost::system::system_error(ec, what_arg) {
            try {
                m_imp_ptr.reset(new m_imp);
                m_imp_ptr->m_path1 = path1_arg;
            }
            catch (...) { m_imp_ptr.reset(); }
        }

        filesystem_error(
                const std::string &what_arg, const path &path1_arg,
                const path &path2_arg, boost::system::error_code ec) noexcept
                : boost::system::system_error(ec, what_arg) {
            try {
                m_imp_ptr.reset(new m_imp);
                m_imp_ptr->m_path1 = path1_arg;
                m_imp_ptr->m_path2 = path2_arg;
            }
            catch (...) { m_imp_ptr.reset(); }
        }

        ~filesystem_error() BOOST_NOEXCEPT_OR_NOTHROW {}

        const path &path1() const noexcept {
            static const path empty_path;
            return m_imp_ptr.get() ? m_imp_ptr->m_path1 : empty_path;
        }

        const path &path2() const noexcept {
            static const path empty_path;
            return m_imp_ptr.get() ? m_imp_ptr->m_path2 : empty_path;
        }

        const char *what() const BOOST_NOEXCEPT_OR_NOTHROW {
            if (!m_imp_ptr.get())
                return boost::system::system_error::what();

            try {
                if (m_imp_ptr->m_what.empty()) {
                    m_imp_ptr->m_what = boost::system::system_error::what();
                    if (!m_imp_ptr->m_path1.empty()) {
                        m_imp_ptr->m_what += ": \"";
                        m_imp_ptr->m_what += m_imp_ptr->m_path1.string();
                        m_imp_ptr->m_what += "\"";
                    }
                    if (!m_imp_ptr->m_path2.empty()) {
                        m_imp_ptr->m_what += ", \"";
                        m_imp_ptr->m_what += m_imp_ptr->m_path2.string();
                        m_imp_ptr->m_what += "\"";
                    }
                }
                return m_imp_ptr->m_what.c_str();
            }
            catch (...) {
                return boost::system::system_error::what();
            }
        }

    private:
        struct m_imp {
            path m_path1; 
            path m_path2; 
            std::string m_what;  
        };
        boost::shared_ptr<m_imp> m_imp_ptr;
    };





    enum file_type {
        status_error,
        status_unknown = status_error,
        file_not_found,
        regular_file,
        directory_file,
        
                symlink_file,
        block_file,
        character_file,
        fifo_file,
        socket_file,
        reparse_file,  
        type_unknown,  
        

        _detail_directory_symlink  
    };





    enum perms {
        no_perms = 0,       

        
        

        

        owner_read = 0400,  
        owner_write = 0200, 
        owner_exe = 0100,   
        owner_all = 0700,   

        group_read = 040,   
        group_write = 020,  
        group_exe = 010,    
        group_all = 070,    

        others_read = 04,   
        others_write = 02,  
        others_exe = 01,    
        others_all = 07,    

        all_all = 0777,     

        

        set_uid_on_exe = 04000, 
        set_gid_on_exe = 02000, 
        sticky_bit = 01000, 
        
        
        
        
        

        perms_mask = 07777,     

        perms_not_known = 0xFFFF, 

        

        add_perms = 0x1000,     
        remove_perms = 0x2000,  
        
        
        

        symlink_perms = 0x4000, 

        
                _detail_extend_perms_32_1 = 0x7fffffff,
        _detail_extend_perms_32_2 = -0x7fffffff - 1
    };

    BOOST_BITMASK(perms)





    class  file_status
            {
                    public:
                    file_status() noexcept
                    : m_value(status_error), m_perms(perms_not_known) {}
                    explicit file_status(file_type v) noexcept
                    : m_value(v), m_perms(perms_not_known)  {}
                    file_status(file_type v, perms prms) noexcept
                    : m_value(v), m_perms(prms) {}

                    
                    
                    

                    file_status(const file_status& rhs) noexcept
                    : m_value(rhs.m_value), m_perms(rhs.m_perms) {}
                    file_status& operator=(const file_status& rhs) noexcept
                    {
                        m_value = rhs.m_value;
                        m_perms = rhs.m_perms;
                        return *this;
                    }

# if !defined(BOOST_NO_CXX11_RVALUE_REFERENCES)
                    file_status(file_status&& rhs) noexcept
                    {
                        m_value = std::move(rhs.m_value);
                        m_perms = std::move(rhs.m_perms);
                    }
                    file_status& operator=(file_status&& rhs) noexcept
                    {
                        m_value = std::move(rhs.m_value);
                        m_perms = std::move(rhs.m_perms);
                        return *this;
                    }
# endif


                    
                    file_type  type() const noexcept            { return m_value; }
                    perms      permissions() const noexcept     { return m_perms; }

                    
                    void type(file_type v) noexcept       { m_value = v; }
                    void permissions(perms prms) noexcept { m_perms = prms; }

                    bool operator==(const file_status& rhs) const noexcept
                    {
                        return type() == rhs.type() &&
                               permissions() == rhs.permissions();
                    }
                    bool operator!=(const file_status& rhs) const noexcept
                    { return !(*this == rhs); }

                    private:
                    file_type   m_value;
                    enum perms  m_perms;
            };

    inline bool type_present(file_status
    f) noexcept {
    return f.

    type()

    !=
    status_error;
}

inline bool permissions_present(file_status f) noexcept { return f.permissions() != perms_not_known; }

inline bool status_known(file_status f) noexcept { return type_present(f) && permissions_present(f); }

inline bool exists(file_status f) noexcept {
    return f.type() != status_error
           && f.type() != file_not_found;
}

inline bool is_regular_file(file_status f) noexcept { return f.type() == regular_file; }

inline bool is_directory(file_status f) noexcept { return f.type() == directory_file; }

inline bool is_symlink(file_status f) noexcept { return f.type() == symlink_file; }

inline bool is_other(file_status f) noexcept {
    return exists(f) && !is_regular_file(f)
           && !is_directory(f) && !is_symlink(f);
}

# ifndef TINY_DB_ENGINE_FILESYSTEM_NO_DEPRECATED

inline bool is_regular(file_status f) noexcept { return f.type() == regular_file; }

# endif

struct space_info {
    
    boost::uintmax_t capacity;
    boost::uintmax_t free;      
    boost::uintmax_t available; 
};

BOOST_SCOPED_ENUM_START(copy_option) {
    none = 0, fail_if_exists = none, overwrite_if_exists
};
BOOST_SCOPED_ENUM_END





namespace detail {
    
    
    
    enum copy_option {
        none = 0, fail_if_exists = none, overwrite_if_exists
    };

    
            file_status

    status(const path &p, boost::system::error_code *ec = 0);

    
            file_status

    symlink_status(const path &p, boost::system::error_code *ec = 0);

    
    bool is_empty(const path &p, boost::system::error_code *ec = 0);

    
            path
    initial_path(boost::system::error_code
    *
    ec = 0
    );
    
            path

    canonical(const path &p, const path &base, boost::system::error_code *ec = 0);

    
    void copy(const path &from, const path &to, boost::system::error_code *ec = 0);

    
    void copy_directory(const path &from, const path &to, boost::system::error_code *ec = 0);

    
    void copy_file(const path &from, const path &to,  
                   detail::copy_option option, boost::system::error_code *ec = 0);

    
    void copy_symlink(const path &existing_symlink, const path &new_symlink, boost::system::error_code *ec = 0);

    
    bool create_directories(const path &p, boost::system::error_code *ec = 0);

    
    bool create_directory(const path &p, boost::system::error_code *ec = 0);

    
    void create_directory_symlink(const path &to, const path &from,
                                  boost::system::error_code *ec = 0);

    
    void create_hard_link(const path &to, const path &from, boost::system::error_code *ec = 0);

    
    void create_symlink(const path &to, const path &from, boost::system::error_code *ec = 0);

    
            path
    current_path(boost::system::error_code
    *
    ec = 0
    );

    
    void current_path(const path &p, boost::system::error_code *ec = 0);

    
    bool equivalent(const path &p1, const path &p2, boost::system::error_code *ec = 0);

    
            boost::uintmax_t

    file_size(const path &p, boost::system::error_code *ec = 0);

    
            boost::uintmax_t

    hard_link_count(const path &p, boost::system::error_code *ec = 0);

    
            std::time_t

    last_write_time(const path &p, boost::system::error_code *ec = 0);

    
    void last_write_time(const path &p, const std::time_t new_time,
                         boost::system::error_code *ec = 0);

    
    void permissions(const path &p, perms prms, boost::system::error_code *ec = 0);

    
            path

    read_symlink(const path &p, boost::system::error_code *ec = 0);

    
            path

    relative(const path &p, const path &base, boost::system::error_code *ec = 0);

    
    bool remove(const path &p, boost::system::error_code *ec = 0);

    
            boost::uintmax_t

    remove_all(const path &p, boost::system::error_code *ec = 0);

    
    void rename(const path &old_p, const path &new_p, boost::system::error_code *ec = 0);

    
    void resize_file(const path &p, uintmax_t size, boost::system::error_code *ec = 0);

    
            space_info

    space(const path &p, boost::system::error_code *ec = 0);

    
            path

    system_complete(const path &p, boost::system::error_code *ec = 0);

    
            path
    temp_directory_path(boost::system::error_code
    *
    ec = 0
    );
    
            path

    unique_path(const path &p, boost::system::error_code *ec = 0);

    
            path

    weakly_canonical(const path &p, boost::system::error_code *ec = 0);
}  







inline
file_status status(const path &p) { return detail::status(p); }

inline
file_status status(const path &p, boost::system::error_code &ec) { return detail::status(p, &ec); }

inline
file_status symlink_status(const path &p) { return detail::symlink_status(p); }

inline
file_status symlink_status(const path &p, boost::system::error_code &ec) { return detail::symlink_status(p, &ec); }

inline
bool exists(const path &p) { return exists(detail::status(p)); }

inline
bool exists(const path &p, boost::system::error_code &ec) { return exists(detail::status(p, &ec)); }

inline
bool is_directory(const path &p) { return is_directory(detail::status(p)); }

inline
bool is_directory(const path &p, boost::system::error_code &ec) { return is_directory(detail::status(p, &ec)); }

inline
bool is_regular_file(const path &p) { return is_regular_file(detail::status(p)); }

inline
bool is_regular_file(const path &p, boost::system::error_code &ec) { return is_regular_file(detail::status(p, &ec)); }

inline
bool is_other(const path &p) { return is_other(detail::status(p)); }

inline
bool is_other(const path &p, boost::system::error_code &ec) { return is_other(detail::status(p, &ec)); }

inline
bool is_symlink(const path &p) { return is_symlink(detail::symlink_status(p)); }

inline
bool is_symlink(const path &p, boost::system::error_code &ec) { return is_symlink(detail::symlink_status(p, &ec)); }

# ifndef TINY_DB_ENGINE_FILESYSTEM_NO_DEPRECATED

inline
bool is_regular(const path &p) { return is_regular(detail::status(p)); }

inline
bool is_regular(const path &p, boost::system::error_code &ec) { return is_regular(detail::status(p, &ec)); }

# endif

inline
bool is_empty(const path &p) { return detail::is_empty(p); }

inline
bool is_empty(const path &p, boost::system::error_code &ec) { return detail::is_empty(p, &ec); }









path current_path();  
path initial_path();


        path

absolute(const path &p, const path &base = current_path());


inline
path canonical(const path &p, const path &base = current_path()) { return detail::canonical(p, base); }

inline
path canonical(const path &p, boost::system::error_code &ec) { return detail::canonical(p, current_path(), &ec); }

inline
path canonical(const path &p, const path &base, boost::system::error_code &ec) { return detail::canonical(p, base, &ec); }

# ifndef TINY_DB_ENGINE_FILESYSTEM_NO_DEPRECATED

inline
path complete(const path &p) {
    return absolute(p, initial_path());
}

inline
path complete(const path &p, const path &base) {
    return absolute(p, base);
}

# endif

inline
void copy(const path &from, const path &to) { detail::copy(from, to); }

inline
void copy(const path &from, const path &to, boost::system::error_code &ec) noexcept { detail::copy(from, to, &ec); }

inline
void copy_directory(const path &from, const path &to) { detail::copy_directory(from, to); }

inline
void copy_directory(const path &from, const path &to, boost::system::error_code &ec) noexcept {
    detail::copy_directory(from, to, &ec);
}

inline
void copy_file(const path &from, const path &to,   
               copy_option option) {
    detail::copy_file(from, to, static_cast<detail::copy_option>(option));
}

inline
void copy_file(const path &from, const path &to) {
    detail::copy_file(from, to, detail::fail_if_exists);
}

inline
void copy_file(const path &from, const path &to,   
               copy_option option, boost::system::error_code &ec) noexcept {
    detail::copy_file(from, to, static_cast<detail::copy_option>(option), &ec);
}

inline
void copy_file(const path &from, const path &to, boost::system::error_code &ec) noexcept {
    detail::copy_file(from, to, detail::fail_if_exists, &ec);
}

inline
void copy_symlink(const path &existing_symlink,
                  const path &new_symlink) { detail::copy_symlink(existing_symlink, new_symlink); }

inline
void copy_symlink(const path &existing_symlink, const path &new_symlink,
                  boost::system::error_code &ec) noexcept { detail::copy_symlink(existing_symlink, new_symlink, &ec); }

inline
bool create_directories(const path &p) { return detail::create_directories(p); }

inline
bool create_directories(const path &p, boost::system::error_code &ec) noexcept {
    return detail::create_directories(p, &ec);
}

inline
bool create_directory(const path &p) { return detail::create_directory(p); }

inline
bool create_directory(const path &p, boost::system::error_code &ec) noexcept { return detail::create_directory(p, &ec); }

inline
void create_directory_symlink(const path &to, const path &from) { detail::create_directory_symlink(to, from); }

inline
void create_directory_symlink(const path &to, const path &from, boost::system::error_code &ec) noexcept {
    detail::create_directory_symlink(to, from, &ec);
}

inline
void create_hard_link(const path &to, const path &new_hard_link) { detail::create_hard_link(to, new_hard_link); }

inline
void create_hard_link(const path &to, const path &new_hard_link,
                      boost::system::error_code &ec) noexcept { detail::create_hard_link(to, new_hard_link, &ec); }

inline
void create_symlink(const path &to, const path &new_symlink) { detail::create_symlink(to, new_symlink); }

inline
void create_symlink(const path &to, const path &new_symlink, boost::system::error_code &ec) noexcept {
    detail::create_symlink(to, new_symlink, &ec);
}

inline
path current_path() { return detail::current_path(); }

inline
path current_path(boost::system::error_code &ec) noexcept { return detail::current_path(&ec); }

inline
void current_path(const path &p) { detail::current_path(p); }

inline
void current_path(const path &p, boost::system::error_code &ec) noexcept { detail::current_path(p, &ec); }

inline
bool equivalent(const path &p1, const path &p2) { return detail::equivalent(p1, p2); }

inline
bool equivalent(const path &p1, const path &p2, boost::system::error_code &ec) noexcept {
    return detail::equivalent(p1, p2, &ec);
}

inline
boost::uintmax_t file_size(const path &p) { return detail::file_size(p); }

inline
boost::uintmax_t file_size(const path &p, boost::system::error_code &ec) noexcept { return detail::file_size(p, &ec); }

inline
boost::uintmax_t hard_link_count(const path &p) { return detail::hard_link_count(p); }

inline
boost::uintmax_t hard_link_count(const path &p, boost::system::error_code &ec) noexcept {
    return detail::hard_link_count(p, &ec);
}

inline
path initial_path() { return detail::initial_path(); }

inline
path initial_path(boost::system::error_code &ec) { return detail::initial_path(&ec); }

template<class Path>
path initial_path() { return initial_path(); }

template<class Path>
path initial_path(boost::system::error_code &ec) { return detail::initial_path(&ec); }

inline
std::time_t last_write_time(const path &p) { return detail::last_write_time(p); }

inline
std::time_t last_write_time(const path &p, boost::system::error_code &ec) noexcept {
    return detail::last_write_time(p, &ec);
}

inline
void last_write_time(const path &p, const std::time_t new_time) { detail::last_write_time(p, new_time); }

inline
void last_write_time(const path &p, const std::time_t new_time,
                     boost::system::error_code &ec) noexcept { detail::last_write_time(p, new_time, &ec); }

inline
void permissions(const path &p, perms prms) { detail::permissions(p, prms); }

inline
void permissions(const path &p, perms prms, boost::system::error_code &ec) noexcept {
    detail::permissions(p, prms, &ec);
}

inline
path read_symlink(const path &p) { return detail::read_symlink(p); }

inline
path read_symlink(const path &p, boost::system::error_code &ec) { return detail::read_symlink(p, &ec); }

inline

bool remove(const path &p) { return detail::remove(p); }

inline
bool remove(const path &p, boost::system::error_code &ec) noexcept { return detail::remove(p, &ec); }

inline
boost::uintmax_t remove_all(const path &p) { return detail::remove_all(p); }

inline
boost::uintmax_t remove_all(const path &p, boost::system::error_code &ec) noexcept { return detail::remove_all(p, &ec); }

inline
void rename(const path &old_p, const path &new_p) { detail::rename(old_p, new_p); }

inline
void rename(const path &old_p, const path &new_p, boost::system::error_code &ec) noexcept {
    detail::rename(old_p, new_p, &ec);
}

inline  
void resize_file(const path &p, uintmax_t size) { detail::resize_file(p, size); }

inline
void resize_file(const path &p, uintmax_t size, boost::system::error_code &ec) noexcept {
    detail::resize_file(p, size, &ec);
}

inline
path relative(const path &p, const path &base = current_path()) { return detail::relative(p, base); }

inline
path relative(const path &p, boost::system::error_code &ec) { return detail::relative(p, current_path(), &ec); }

inline
path relative(const path &p, const path &base, boost::system::error_code &ec) { return detail::relative(p, base, &ec); }

inline
space_info space(const path &p) { return detail::space(p); }

inline
space_info space(const path &p, boost::system::error_code &ec) noexcept { return detail::space(p, &ec); }

# ifndef TINY_DB_ENGINE_FILESYSTEM_NO_DEPRECATED

inline bool symbolic_link_exists(const path &p) { return is_symlink(symlink_status(p)); }

# endif

inline
path system_complete(const path &p) { return detail::system_complete(p); }

inline
path system_complete(const path &p, boost::system::error_code &ec) { return detail::system_complete(p, &ec); }

inline
path temp_directory_path() { return detail::temp_directory_path(); }

inline
path temp_directory_path(boost::system::error_code &ec) { return detail::temp_directory_path(&ec); }

inline
path unique_path(const path &p = "%%%%-%%%%-%%%%-%%%%") { return detail::unique_path(p); }

inline
path unique_path(const path &p, boost::system::error_code &ec) { return detail::unique_path(p, &ec); }

inline
path weakly_canonical(const path &p) { return detail::weakly_canonical(p); }

inline
path weakly_canonical(const path &p, boost::system::error_code &ec) { return detail::weakly_canonical(p, &ec); }











class  directory_entry
        {
                public:
                typedef tinydb::filesystem::path::value_type value_type;   

                directory_entry() noexcept = default;
                explicit directory_entry(tinydb::filesystem::path p)
                : m_path(std::move(p)), m_status(file_status()), m_symlink_status(file_status())
                {}
                directory_entry(tinydb::filesystem::path p,
                file_status st, file_status symlink_st = file_status())
                : m_path(std::move(p)), m_status(std::move(st)), m_symlink_status(std::move(symlink_st)) {}

                directory_entry(const directory_entry& rhs) = default;

                directory_entry& operator=(const directory_entry& rhs) = default;

                directory_entry(directory_entry&& rhs) noexcept = default;
                directory_entry& operator=(directory_entry&& rhs) noexcept = default;

                void assign(tinydb::filesystem::path p,
                file_status st = file_status(), file_status symlink_st = file_status())
                {
                    m_path = std::move(p);
                    m_status = std::move(st);
                    m_symlink_status = std::move(symlink_st);
                }

                void replace_filename(const tinydb::filesystem::path& p,
                file_status st = file_status(), file_status symlink_st = file_status())
                {
                    m_path.remove_filename();
                    m_path /= p;
                    m_status = st;
                    m_symlink_status = symlink_st;
                }

                void replace_leaf(const tinydb::filesystem::path& p,
                file_status st, file_status symlink_st)
                { replace_filename(p, st, symlink_st); }

                const tinydb::filesystem::path&  path() const noexcept { return m_path; }
                operator const class path&() const noexcept
                { return m_path; }
                file_status   status() const                                { return m_get_status(); }
                file_status   status(boost::system::error_code& ec) const noexcept
                { return m_get_status(&ec); }
                file_status   symlink_status() const                        { return m_get_symlink_status(); }
                file_status   symlink_status(boost::system::error_code& ec) const noexcept
                { return m_get_symlink_status(&ec); }

                bool operator==(const directory_entry& rhs) const noexcept { return m_path == rhs.m_path; }
                bool operator!=(const directory_entry& rhs) const noexcept { return m_path != rhs.m_path; }
                bool operator< (const directory_entry& rhs) const noexcept { return m_path < rhs.m_path; }
                bool operator<=(const directory_entry& rhs) const noexcept { return m_path <= rhs.m_path; }
                bool operator> (const directory_entry& rhs) const noexcept { return m_path > rhs.m_path; }
                bool operator>=(const directory_entry& rhs) const noexcept { return m_path >= rhs.m_path; }

                private:
                tinydb::filesystem::path   m_path;
                mutable file_status       m_status;           
                mutable file_status       m_symlink_status;   

                file_status m_get_status(boost::system::error_code* ec=0) const;
                file_status m_get_symlink_status(boost::system::error_code* ec=0) const;
        }; 

class directory_iterator;

namespace detail {
    
            boost::system::error_code

    dir_itr_close(
            void *&handle
            , void *& buffer
    );

    struct dir_itr_imp {
        directory_entry dir_entry;
        void *handle;

        void*            buffer;

        dir_itr_imp() : handle(0)
        , buffer(0)
        {}

        ~dir_itr_imp() 
        {
            dir_itr_close(handle
                    , buffer
            );
        }
    };

    
     void directory_iterator_construct(directory_iterator &it,
                                                                     const path &p, boost::system::error_code *ec);

     void directory_iterator_increment(directory_iterator &it,
                                                                     boost::system::error_code *ec);

}  







class directory_iterator
        : public boost::iterator_facade<directory_iterator,
                directory_entry,
                boost::single_pass_traversal_tag> {
public:

    directory_iterator() noexcept = default;

    
    
    explicit directory_iterator(const path &p)
            : m_imp(new detail::dir_itr_imp) { detail::directory_iterator_construct(*this, p, 0); }

    directory_iterator(const path &p, boost::system::error_code &ec) noexcept
            : m_imp(new detail::dir_itr_imp) { detail::directory_iterator_construct(*this, p, &ec); }

    ~directory_iterator() = default;

    directory_iterator &increment(boost::system::error_code &ec) noexcept {
        detail::directory_iterator_increment(*this, &ec);
        return *this;
    }

private:
    friend struct detail::dir_itr_imp;

    friend  void detail::directory_iterator_construct(directory_iterator &it,
                                                                                    const path &p,
                                                                                    boost::system::error_code *ec);

    friend  void detail::directory_iterator_increment(directory_iterator &it,
                                                                                    boost::system::error_code *ec);

    
    
    boost::shared_ptr<detail::dir_itr_imp> m_imp;

    friend class boost::iterator_core_access;

    boost::iterator_facade<
            directory_iterator,
            directory_entry,
            boost::single_pass_traversal_tag>::reference dereference() const {
        BOOST_ASSERT_MSG(m_imp.get(), "attempt to dereference end iterator");
        return m_imp->dir_entry;
    }

    void increment() { detail::directory_iterator_increment(*this, 0); }

    bool equal(const directory_iterator &rhs) const {
        return m_imp == rhs.m_imp
               || (!m_imp && rhs.m_imp && !rhs.m_imp->handle)
               || (!rhs.m_imp && m_imp && !m_imp->handle);
    }

};  






inline
const directory_iterator &begin(const directory_iterator &iter) noexcept { return iter; }

inline
directory_iterator end(const directory_iterator &) noexcept { return directory_iterator(); }



inline
directory_iterator &range_begin(directory_iterator &iter) noexcept { return iter; }

inline
directory_iterator range_begin(const directory_iterator &iter) noexcept { return iter; }

inline
directory_iterator range_end(directory_iterator &) noexcept { return directory_iterator(); }

inline
directory_iterator range_end(const directory_iterator &) noexcept { return directory_iterator(); }

}  


template<typename C, typename Enabler>
struct range_mutable_iterator;

template<>
struct range_mutable_iterator<tinydb::filesystem::directory_iterator, void> {
    typedef tinydb::filesystem::directory_iterator type;
};

template<typename C, typename Enabler>
struct range_const_iterator;

template<>
struct range_const_iterator<tinydb::filesystem::directory_iterator, void> {
    typedef tinydb::filesystem::directory_iterator type;
};

namespace tinydb::filesystem {







    BOOST_SCOPED_ENUM_START(symlink_option) {
        none,
        no_recurse = none,         
        recurse,                   
        _detail_no_push = recurse << 1, 

        
                _detail_extend_symlink_option_32_1 = 0x7fffffff,
        _detail_extend_symlink_option_32_2 = -0x7fffffff - 1
    };
    BOOST_SCOPED_ENUM_END

    BOOST_BITMASK(symlink_option)

    namespace detail {
        struct recur_dir_itr_imp {
            typedef directory_iterator element_type;
            std::stack<element_type, std::vector<element_type> > m_stack;
            int m_level;
            symlink_option m_options;

            recur_dir_itr_imp() : m_level(0), m_options(symlink_option::none) {}

            void increment(boost::system::error_code *ec);  

            bool push_directory(boost::system::error_code &ec) noexcept;

            void pop();

        };

        
        
        

        inline
        bool recur_dir_itr_imp::push_directory(boost::system::error_code &ec) noexcept
        
        {
            ec.clear();

            
            

            if ((m_options & symlink_option::_detail_no_push) == symlink_option::_detail_no_push) {
                m_options &= ~symlink_option::_detail_no_push;
                return false;
            }

            file_status symlink_stat;

            
            
            if ((m_options & symlink_option::recurse) != symlink_option::recurse) {
                symlink_stat = m_stack.top()->symlink_status(ec);
                if (ec)
                    return false;
            }

            
            
            
            
            
            
            

            if ((m_options & symlink_option::recurse) == symlink_option::recurse
                || !is_symlink(symlink_stat)) {
                file_status stat = m_stack.top()->status(ec);
                if (ec || !is_directory(stat))
                    return false;

                directory_iterator next(m_stack.top()->path(), ec);
                if (!ec && next != directory_iterator()) {
                    m_stack.push(next);
                    ++m_level;
                    return true;
                }
            }
            return false;
        }

        inline
        void recur_dir_itr_imp::increment(boost::system::error_code *ec)
        
        
        
        
        
        
        
        {
            boost::system::error_code ec_push_directory;

            
            if (push_directory(ec_push_directory)) {
                if (ec)
                    ec->clear();
                return;
            }

            
            
            
            while (!m_stack.empty() && ++m_stack.top() == directory_iterator()) {
                m_stack.pop();
                --m_level;
            }

            
            if (ec_push_directory) {
                if (ec)
                    *ec = ec_push_directory;
                else {
                    throw filesystem_error(
                            "filesystem::recursive_directory_iterator directory error",
                            ec_push_directory);
                }
            } else if (ec)
                ec->clear();
        }

        inline
        void recur_dir_itr_imp::pop() {
            BOOST_ASSERT_MSG(m_level > 0,
                             "pop() on recursive_directory_iterator with level < 1");

            do {
                m_stack.pop();
                --m_level;
            } while (!m_stack.empty() && ++m_stack.top() == directory_iterator());
        }
    }



    class recursive_directory_iterator
            : public boost::iterator_facade<
                    recursive_directory_iterator,
                    directory_entry,
                    boost::single_pass_traversal_tag> {
    public:

        recursive_directory_iterator() noexcept = default;

        explicit recursive_directory_iterator(const path &dir_path)  
                : m_imp(new detail::recur_dir_itr_imp) {
            m_imp->m_options = symlink_option::none;
            m_imp->m_stack.push(directory_iterator(dir_path));
            if (m_imp->m_stack.top() == directory_iterator()) { m_imp.reset(); }
        }

        recursive_directory_iterator(const path &dir_path,
                                     symlink_option opt)  
                : m_imp(new detail::recur_dir_itr_imp) {
            m_imp->m_options = opt;
            m_imp->m_stack.push(directory_iterator(dir_path));
            if (m_imp->m_stack.top() == directory_iterator()) { m_imp.reset(); }
        }

        recursive_directory_iterator(const path &dir_path,
                                     symlink_option opt,
                                     boost::system::error_code &ec) noexcept
                : m_imp(new detail::recur_dir_itr_imp) {
            m_imp->m_options = opt;
            m_imp->m_stack.push(directory_iterator(dir_path, ec));
            if (m_imp->m_stack.top() == directory_iterator()) { m_imp.reset(); }
        }

        recursive_directory_iterator(const path &dir_path,
                                     boost::system::error_code &ec) noexcept
                : m_imp(new detail::recur_dir_itr_imp) {
            m_imp->m_options = symlink_option::none;
            m_imp->m_stack.push(directory_iterator(dir_path, ec));
            if (m_imp->m_stack.top() == directory_iterator()) { m_imp.reset(); }
        }

        recursive_directory_iterator &increment(boost::system::error_code &ec) noexcept {
            BOOST_ASSERT_MSG(m_imp.get(),
                             "increment() on end recursive_directory_iterator");
            m_imp->increment(&ec);
            if (m_imp->m_stack.empty())
                m_imp.reset(); 
            return *this;
        }

        int depth() const noexcept {
            BOOST_ASSERT_MSG(m_imp.get(),
                             "depth() on end recursive_directory_iterator");
            return m_imp->m_level;
        }

        int level() const noexcept { return depth(); }

        bool recursion_pending() const noexcept {
            BOOST_ASSERT_MSG(m_imp.get(),
                             "is_no_push_requested() on end recursive_directory_iterator");
            return (m_imp->m_options & symlink_option::_detail_no_push)
                   == symlink_option::_detail_no_push;
        }

        bool no_push_pending() const noexcept { return recursion_pending(); }

#   ifndef TINY_DB_ENGINE_FILESYSTEM_NO_DEPRECATED

        bool no_push_request() const noexcept { return no_push_pending(); }

#   endif

        void pop() {
            BOOST_ASSERT_MSG(m_imp.get(),
                             "pop() on end recursive_directory_iterator");
            m_imp->pop();
            if (m_imp->m_stack.empty()) m_imp.reset(); 
        }

        void disable_recursion_pending(bool value = true) noexcept {
            BOOST_ASSERT_MSG(m_imp.get(),
                             "no_push() on end recursive_directory_iterator");
            if (value)
                m_imp->m_options |= symlink_option::_detail_no_push;
            else
                m_imp->m_options &= ~symlink_option::_detail_no_push;
        }

        void no_push(bool value = true) noexcept { disable_recursion_pending(value); }

        file_status status() const {
            BOOST_ASSERT_MSG(m_imp.get(),
                             "status() on end recursive_directory_iterator");
            return m_imp->m_stack.top()->status();
        }

        file_status symlink_status() const {
            BOOST_ASSERT_MSG(m_imp.get(),
                             "symlink_status() on end recursive_directory_iterator");
            return m_imp->m_stack.top()->symlink_status();
        }

    private:

        
        
        
        boost::shared_ptr<detail::recur_dir_itr_imp> m_imp;

        friend class boost::iterator_core_access;

        boost::iterator_facade<
                recursive_directory_iterator,
                directory_entry,
                boost::single_pass_traversal_tag>::reference
        dereference() const {
            BOOST_ASSERT_MSG(m_imp.get(),
                             "dereference of end recursive_directory_iterator");
            return *m_imp->m_stack.top();
        }

        void increment() {
            BOOST_ASSERT_MSG(m_imp.get(),
                             "increment of end recursive_directory_iterator");
            m_imp->increment(0);
            if (m_imp->m_stack.empty())
                m_imp.reset(); 
        }

        bool equal(const recursive_directory_iterator &rhs) const {
            return m_imp == rhs.m_imp
                   || (!m_imp && rhs.m_imp && rhs.m_imp->m_stack.empty())
                   || (!rhs.m_imp && m_imp && m_imp->m_stack.empty());
        }

    };  

    

    
    
    
    inline
    const recursive_directory_iterator &
    begin(const recursive_directory_iterator &iter) noexcept { return iter; }

    inline
    recursive_directory_iterator
    end(const recursive_directory_iterator &) noexcept { return recursive_directory_iterator(); }

    

    inline
    recursive_directory_iterator &
    range_begin(recursive_directory_iterator &iter) noexcept { return iter; }

    inline
    recursive_directory_iterator
    range_begin(const recursive_directory_iterator &iter) noexcept { return iter; }

    inline
    recursive_directory_iterator
    range_end(recursive_directory_iterator &) noexcept { return recursive_directory_iterator(); }

    inline
    recursive_directory_iterator
    range_end(const recursive_directory_iterator &) noexcept { return recursive_directory_iterator(); }
}  


template<>
struct range_mutable_iterator<tinydb::filesystem::recursive_directory_iterator, void> {
    typedef tinydb::filesystem::recursive_directory_iterator type;
};
template<>
struct range_const_iterator<tinydb::filesystem::recursive_directory_iterator, void> {
    typedef tinydb::filesystem::recursive_directory_iterator type;
};

namespace tinydb::filesystem {

# if !defined(TINY_DB_ENGINE_FILESYSTEM_NO_DEPRECATED)
    typedef recursive_directory_iterator wrecursive_directory_iterator;
# endif







    namespace detail {
         bool possible_large_file_size_support();
    }

} 

#include <boost/config/abi_suffix.hpp> 

#endif 
