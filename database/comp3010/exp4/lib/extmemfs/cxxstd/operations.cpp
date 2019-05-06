#ifndef _POSIX_PTHREAD_SEMANTICS
# define _POSIX_PTHREAD_SEMANTICS
#endif

#include "operations.hpp"
#include <boost/scoped_array.hpp>
#include <boost/detail/workaround.hpp>
#include <limits>
#include <vector>
#include <cstdlib>
#include <cstring>
#include <cstdio>

#include <cerrno>
#include <iostream>

namespace fs = tinydb::filesystem;
using tinydb::filesystem::path;
using tinydb::filesystem::filesystem_error;
using tinydb::filesystem::perms;
using boost::system::error_code;
using boost::system::error_category;
using boost::system::system_category;
using std::string;
using std::wstring;

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/statvfs.h>

#define BOOST_STATVFS statvfs
#define BOOST_STATVFS_F_FRSIZE vfs.f_frsize

#include <dirent.h>
#include <unistd.h>
#include <fcntl.h>
#include <utime.h>
#include "limits.h"

typedef int err_t;


#   define BOOST_ERRNO    errno
#   define BOOST_SET_CURRENT_DIRECTORY(P)(::chdir(P)== 0)
#   define BOOST_CREATE_DIRECTORY(P)(::mkdir(P, S_IRWXU|S_IRWXG|S_IRWXO)== 0)
#   define BOOST_CREATE_HARD_LINK(F, T)(::link(T, F)== 0)
#   define BOOST_CREATE_SYMBOLIC_LINK(F, T, Flag)(::symlink(T, F)== 0)
#   define BOOST_REMOVE_DIRECTORY(P)(::rmdir(P)== 0)
#   define BOOST_DELETE_FILE(P)(::unlink(P)== 0)
#   define BOOST_COPY_DIRECTORY(F, T)(!(::stat(from.c_str(), &from_stat)!= 0\
         || ::mkdir(to.c_str(),from_stat.st_mode)!= 0))
#   define BOOST_COPY_FILE(F, T, FailIfExistsBool)copy_file_api(F, T, FailIfExistsBool)
#   define BOOST_MOVE_FILE(OLD, NEW)(::rename(OLD, NEW)== 0)
#   define BOOST_RESIZE_FILE(P, SZ)(::truncate(P, SZ)== 0)

#   define BOOST_ERROR_NOT_SUPPORTED ENOSYS
#   define BOOST_ERROR_ALREADY_EXISTS EEXIST


namespace {

    fs::file_type query_file_type(const path &p, error_code *ec);

    tinydb::filesystem::directory_iterator end_dir_itr;


    bool error(err_t error_num, error_code *ec, const char *message);

    bool error(err_t error_num, const path &p, error_code *ec, const char *message);

    bool error(err_t error_num, const path &p1, const path &p2, error_code *ec,
               const char *message);

    const error_code ok;


    bool error(err_t error_num, error_code *ec, const char *message) {
        if (!error_num) {
            if (ec != nullptr) ec->clear();
        } else {
            if (ec == nullptr)
                throw (filesystem_error(message,
                                        error_code(error_num, system_category())));
            else
                ec->assign(error_num, system_category());
        }
        return error_num != 0;
    }

    bool error(err_t error_num, const path &p, error_code *ec, const char *message) {
        if (!error_num) {
            if (ec != nullptr) ec->clear();
        } else {
            if (ec == nullptr)
                throw (filesystem_error(message,
                                        p, error_code(error_num, system_category())));
            else
                ec->assign(error_num, system_category());
        }
        return error_num != 0;
    }

    bool error(err_t error_num, const path &p1, const path &p2, error_code *ec,
               const char *message) {
        if (!error_num) {
            if (ec != nullptr) ec->clear();
        } else {
            if (ec == nullptr)
                throw (filesystem_error(message,
                                        p1, p2, error_code(error_num, system_category())));
            else
                ec->assign(error_num, system_category());
        }
        return error_num != 0;
    }


    bool is_empty_directory(const path &p, error_code *ec) {
        return (ec != nullptr ? fs::directory_iterator(p, *ec) : fs::directory_iterator(p))
               == end_dir_itr;
    }

    bool not_found_error(int errval);


    bool remove_directory(const path &p) {
        return BOOST_REMOVE_DIRECTORY(p.c_str())
               || not_found_error(BOOST_ERRNO);
    }


    bool remove_file(const path &p) {
        return BOOST_DELETE_FILE(p.c_str())
               || not_found_error(BOOST_ERRNO);
    }


    bool remove_file_or_directory(const path &p, fs::file_type type, error_code *ec) {
        if (type == fs::file_not_found) {
            if (ec != nullptr) ec->clear();
            return false;
        }

        if (type == fs::directory_file
                ) {
            if (error(!remove_directory(p) ? BOOST_ERRNO : 0, p, ec,
                      "tinydb::filesystem::remove"))
                return false;
        } else {
            if (error(!remove_file(p) ? BOOST_ERRNO : 0, p, ec,
                      "tinydb::filesystem::remove"))
                return false;
        }
        return true;
    }

    boost::uintmax_t remove_all_aux(const path &p, fs::file_type type,
                                    error_code *ec) {
        boost::uintmax_t count = 0;

        if (type == fs::directory_file) {
            fs::directory_iterator itr;
            if (ec != nullptr) {
                itr = fs::directory_iterator(p, *ec);
                if (*ec)
                    return count;
            } else
                itr = fs::directory_iterator(p);

            while (itr != end_dir_itr) {
                fs::file_type tmp_type = query_file_type(itr->path(), ec);
                if (ec != nullptr && *ec)
                    return count;

                count += remove_all_aux(itr->path(), tmp_type, ec);
                if (ec != nullptr && *ec)
                    return count;

                fs::detail::directory_iterator_increment(itr, ec);
                if (ec != nullptr && *ec)
                    return count;
            }
        }

        remove_file_or_directory(p, type, ec);
        if (ec != nullptr && *ec)
            return count;

        return ++count;
    }

    const char dot = '.';

    bool not_found_error(int errval) {
        return errno == ENOENT || errno == ENOTDIR;
    }

    bool
    copy_file_api(const std::string &from_p,
                  const std::string &to_p, bool fail_if_exists) {
        const std::size_t buf_sz = 32768;
        boost::scoped_array<char> buf(new char[buf_sz]);
        int infile = -1, outfile = -1;


        if ((infile = ::open(from_p.c_str(), O_RDONLY)) < 0) { return false; }

        struct stat from_stat;
        if (::stat(from_p.c_str(), &from_stat) != 0) {
            ::close(infile);
            return false;
        }

        int oflag = O_CREAT | O_WRONLY | O_TRUNC;
        if (fail_if_exists)
            oflag |= O_EXCL;
        if ((outfile = ::open(to_p.c_str(), oflag, from_stat.st_mode)) < 0) {
            int open_errno = errno;
            BOOST_ASSERT(infile >= 0);
            ::close(infile);
            errno = open_errno;
            return false;
        }

        ssize_t sz, sz_read = 1, sz_write;
        while (sz_read > 0
               && (sz_read = ::read(infile, buf.get(), buf_sz)) > 0) {


            sz_write = 0;
            do {
                BOOST_ASSERT(sz_read - sz_write > 0);


                if ((sz = ::write(outfile, buf.get() + sz_write,
                                  sz_read - sz_write)) < 0) {
                    sz_read = sz;
                    break;
                }
                BOOST_ASSERT(sz > 0);
                sz_write += sz;
            } while (sz_write < sz_read);
        }

        if (::close(infile) < 0)
            sz_read = -1;
        if (::close(outfile) < 0)
            sz_read = -1;

        return sz_read >= 0;
    }

    inline fs::file_type query_file_type(const path &p, error_code *ec) {
        return fs::detail::symlink_status(p, ec).type();
    }


}


namespace tinydb::filesystem {


    path absolute(const path &p, const path &base) {


        path abs_base(base.is_absolute() ? base : absolute(base));


        path p_root_name(p.root_name());
        path base_root_name(abs_base.root_name());
        path p_root_directory(p.root_directory());

        if (p.empty())
            return abs_base;

        if (!p_root_name.empty()) {
            if (p_root_directory.empty())
                return p_root_name / abs_base.root_directory()
                       / abs_base.relative_path() / p.relative_path();

        } else if (!p_root_directory.empty()) {
            if (base_root_name.empty())
                return p;
            return base_root_name / p;
        } else {
            return abs_base / p;
        }

        return p;
    }

    namespace detail {
        bool possible_large_file_size_support() {
            struct stat lcl_stat;
            return sizeof(lcl_stat.st_size) > 4;
        }


        path canonical(const path &p, const path &base, boost::system::error_code *ec) {
            path source(p.is_absolute() ? p : absolute(p, base));
            path root(source.root_path());
            path result;

            boost::system::error_code local_ec{};
            file_status stat(status(source, local_ec));

            if (stat.type() == fs::file_not_found) {
                if (ec == nullptr)
                    throw (filesystem_error(
                            "tinydb::filesystem::canonical", source,
                            error_code(boost::system::errc::no_such_file_or_directory,
                                       boost::system::generic_category())));
                ec->assign(boost::system::errc::no_such_file_or_directory, boost::system::generic_category());
                return result;
            } else if (local_ec) {
                if (ec == nullptr)
                    throw (filesystem_error(
                            "tinydb::filesystem::canonical", source, local_ec));
                *ec = local_ec;
                return result;
            }

            bool scan(true);
            while (scan) {
                scan = false;
                result.clear();
                for (path::iterator itr = source.begin(); itr != source.end(); ++itr) {
                    if (*itr == dot_path())
                        continue;
                    if (*itr == dot_dot_path()) {
                        if (result != root)
                            result.remove_filename();
                        continue;
                    }

                    result /= *itr;

                    bool is_sym(is_symlink(detail::symlink_status(result, ec)));
                    if (ec && *ec)
                        return path();

                    if (is_sym) {
                        path link(detail::read_symlink(result, ec));
                        if (ec && *ec)
                            return path();
                        result.remove_filename();

                        if (link.is_absolute()) {
                            for (++itr; itr != source.end(); ++itr)
                                link /= *itr;
                            source = link;
                        } else {
                            path new_source(result);
                            new_source /= link;
                            for (++itr; itr != source.end(); ++itr)
                                new_source /= *itr;
                            source = new_source;
                        }
                        scan = true;
                        break;
                    }
                }
            }
            if (ec != nullptr)
                ec->clear();
            BOOST_ASSERT_MSG(result.is_absolute(), "canonical() implementation error; please report");
            return result;
        }


        void copy(const path &from, const path &to, boost::system::error_code *ec) {
            file_status s(detail::symlink_status(from, ec));
            if (ec != nullptr && *ec) return;

            if (is_symlink(s)) {
                detail::copy_symlink(from, to, ec);
            } else if (is_directory(s)) {
                detail::copy_directory(from, to, ec);
            } else if (is_regular_file(s)) {
                detail::copy_file(from, to, detail::fail_if_exists, ec);
            } else {
                if (ec == nullptr)
                    throw (filesystem_error("tinydb::filesystem::copy",
                                            from, to, error_code(BOOST_ERROR_NOT_SUPPORTED,
                                                                 system_category())));
                ec->assign(BOOST_ERROR_NOT_SUPPORTED, system_category());
            }
        }


        void copy_directory(const path &from, const path &to, boost::system::error_code *ec) {
            struct stat from_stat;
            error(!BOOST_COPY_DIRECTORY(from.c_str(), to.c_str()) ? BOOST_ERRNO : 0,
                  from, to, ec, "tinydb::filesystem::copy_directory");
        }


        void copy_file(const path &from, const path &to, copy_option option, error_code *ec) {
            error(!BOOST_COPY_FILE(from.c_str(), to.c_str(),
                                   option == fail_if_exists) ? BOOST_ERRNO : 0,
                  from, to, ec, "tinydb::filesystem::copy_file");
        }


        void copy_symlink(const path &existing_symlink, const path &new_symlink,
                          boost::system::error_code *ec) {
            path p(read_symlink(existing_symlink, ec));
            if (ec != nullptr && *ec) return;
            create_symlink(p, new_symlink, ec);
        }


        bool create_directories(const path &p, boost::system::error_code *ec) {
            if (p.empty()) {
                if (ec == nullptr)
                    throw filesystem_error(
                            "tinydb::filesystem::create_directories", p,
                            boost::system::errc::make_error_code(boost::system::errc::invalid_argument));
                else
                    ec->assign(boost::system::errc::invalid_argument, boost::system::generic_category());
                return false;
            }

            if (p.filename_is_dot() || p.filename_is_dot_dot())
                return create_directories(p.parent_path(), ec);

            error_code local_ec{};
            file_status p_status = status(p, local_ec);

            if (p_status.type() == directory_file) {
                if (ec != nullptr)
                    ec->clear();
                return false;
            }

            path parent = p.parent_path();
            BOOST_ASSERT_MSG(parent != p, "internal error: p == p.parent_path()");
            if (!parent.empty()) {

                file_status parent_status = status(parent, local_ec);


                if (parent_status.type() == file_not_found) {
                    create_directories(parent, local_ec);
                    if (local_ec) {
                        if (ec == nullptr)
                            throw filesystem_error(
                                    "tinydb::filesystem::create_directories", parent, local_ec);
                        else
                            *ec = local_ec;
                        return false;
                    }
                }
            }


            return create_directory(p, ec);
        }


        bool create_directory(const path &p, error_code *ec) {
            if (BOOST_CREATE_DIRECTORY(p.c_str())) {
                if (ec != nullptr)
                    ec->clear();
                return true;
            }


            int errval(BOOST_ERRNO);
            error_code dummy{};

            if (is_directory(p, dummy)) {
                if (ec != nullptr)
                    ec->clear();
                return false;
            }


            if (ec == nullptr)
                throw filesystem_error("tinydb::filesystem::create_directory",
                                       p, error_code(errval, system_category()));
            else
                ec->assign(errval, system_category());

            return false;
        }


        void create_directory_symlink(const path &to, const path &from,
                                      boost::system::error_code *ec) {

            error(!BOOST_CREATE_SYMBOLIC_LINK(from.c_str(), to.c_str(),
                                              SYMBOLIC_LINK_FLAG_DIRECTORY) ? BOOST_ERRNO : 0,
                  to, from, ec, "tinydb::filesystem::create_directory_symlink");
        }


        void create_hard_link(const path &to, const path &from, error_code *ec) {


            error(!BOOST_CREATE_HARD_LINK(from.c_str(), to.c_str()) ? BOOST_ERRNO : 0, to, from, ec,
                  "tinydb::filesystem::create_hard_link");
        }


        void create_symlink(const path &to, const path &from, error_code *ec) {
            error(!BOOST_CREATE_SYMBOLIC_LINK(from.c_str(), to.c_str(), 0) ? BOOST_ERRNO : 0,
                  to, from, ec, "tinydb::filesystem::create_symlink");
        }


        path current_path(error_code *ec) {
            path cur;
            for (long path_max = 128;; path_max *= 2) {
                boost::scoped_array<char>
                        buf(new char[static_cast<std::size_t>(path_max)]);
                if (::getcwd(buf.get(), static_cast<std::size_t>(path_max)) == 0) {
                    if (error(errno != ERANGE ? errno : 0, ec, "tinydb::filesystem::current_path")) {
                        break;
                    }
                } else {
                    cur = buf.get();
                    if (ec != nullptr) ec->clear();
                    break;
                }
            }
            return cur;
        }


        void current_path(const path &p, boost::system::error_code *ec) {
            error(!BOOST_SET_CURRENT_DIRECTORY(p.c_str()) ? BOOST_ERRNO : 0,
                  p, ec, "tinydb::filesystem::current_path");
        }


        bool equivalent(const path &p1, const path &p2, boost::system::error_code *ec) {
            struct stat s2;
            int e2(::stat(p2.c_str(), &s2));
            struct stat s1;
            int e1(::stat(p1.c_str(), &s1));

            if (e1 != 0 || e2 != 0) {


                error(e1 != 0 && e2 != 0, p1, p2, ec, "tinydb::filesystem::equivalent");
                return false;
            }


            return s1.st_dev == s2.st_dev && s1.st_ino == s2.st_ino
                   && s1.st_size == s2.st_size && s1.st_mtime == s2.st_mtime;
        }


        boost::uintmax_t file_size(const path &p, error_code *ec) {
            struct stat path_stat;
            if (error(::stat(p.c_str(), &path_stat) != 0 ? BOOST_ERRNO : 0,
                      p, ec, "tinydb::filesystem::file_size"))
                return static_cast<boost::uintmax_t>(-1);
            if (error(!S_ISREG(path_stat.st_mode) ? EPERM : 0,
                      p, ec, "tinydb::filesystem::file_size"))
                return static_cast<boost::uintmax_t>(-1);

            return static_cast<boost::uintmax_t>(path_stat.st_size);
        }


        boost::uintmax_t hard_link_count(const path &p, boost::system::error_code *ec) {
            struct stat path_stat;
            return error(::stat(p.c_str(), &path_stat) != 0 ? BOOST_ERRNO : 0,
                         p, ec, "tinydb::filesystem::hard_link_count")
                   ? 0
                   : static_cast<boost::uintmax_t>(path_stat.st_nlink);
        }


        path initial_path(error_code *ec) {
            static path init_path;
            if (init_path.empty())
                init_path = current_path(ec);
            else if (ec != nullptr) ec->clear();
            return init_path;
        }


        bool is_empty(const path &p, boost::system::error_code *ec) {
            struct stat path_stat;
            if (error(::stat(p.c_str(), &path_stat) != 0,
                      p, ec, "tinydb::filesystem::is_empty"))
                return false;
            return S_ISDIR(path_stat.st_mode)
                   ? is_empty_directory(p, ec)
                   : path_stat.st_size == 0;
        }


        std::time_t last_write_time(const path &p, boost::system::error_code *ec) {
            struct stat path_stat;
            if (error(::stat(p.c_str(), &path_stat) != 0 ? BOOST_ERRNO : 0,
                      p, ec, "tinydb::filesystem::last_write_time"))
                return std::time_t(-1);
            return path_stat.st_mtime;
        }


        void last_write_time(const path &p, const std::time_t new_time,
                             boost::system::error_code *ec) {
            struct stat path_stat;
            if (error(::stat(p.c_str(), &path_stat) != 0,
                      p, ec, "tinydb::filesystem::last_write_time"))
                return;
            ::utimbuf buf;
            buf.actime = path_stat.st_atime;
            buf.modtime = new_time;
            error(::utime(p.c_str(), &buf) != 0 ? BOOST_ERRNO : 0,
                  p, ec, "tinydb::filesystem::last_write_time");
        }

        const perms active_bits(all_all | set_uid_on_exe | set_gid_on_exe | sticky_bit);

        inline mode_t mode_cast(perms prms) { return prms & active_bits; }


        void permissions(const path &p, perms prms, boost::system::error_code *ec) {
            BOOST_ASSERT_MSG(!((prms & add_perms) && (prms & remove_perms)),
                             "add_perms and remove_perms are mutually exclusive");
            prms = perms::all_all;
        }


        path read_symlink(const path &p, boost::system::error_code *ec) {
            path symlink_path;
            for (std::size_t path_max = 64;; path_max *= 2) {
                boost::scoped_array<char> buf(new char[path_max]);
                ssize_t result;
                if ((result = ::readlink(p.c_str(), buf.get(), path_max)) == -1) {
                    if (ec == nullptr)
                        throw (filesystem_error("tinydb::filesystem::read_symlink",
                                                p, error_code(errno, system_category())));
                    else ec->assign(errno, system_category());
                    break;
                } else {
                    if (result != static_cast<ssize_t>(path_max)) {
                        symlink_path.assign(buf.get(), buf.get() + result);
                        if (ec != nullptr) ec->clear();
                        break;
                    }
                }
            }
            return symlink_path;
        }


        path relative(const path &p, const path &base, error_code *ec) {
            error_code tmp_ec;
            path wc_base(weakly_canonical(base, &tmp_ec));
            if (error(tmp_ec.value(), base, ec, "tinydb::filesystem::relative"))
                return path();
            path wc_p(weakly_canonical(p, &tmp_ec));
            if (error(tmp_ec.value(), base, ec, "tinydb::filesystem::relative"))
                return path();
            return wc_p.lexically_relative(wc_base);
        }


        bool remove(const path &p, error_code *ec) {
            error_code tmp_ec;
            file_type type = query_file_type(p, &tmp_ec);
            if (error(type == status_error ? tmp_ec.value() : 0, p, ec,
                      "tinydb::filesystem::remove"))
                return false;


            return remove_file_or_directory(p, type, ec);
        }


        boost::uintmax_t remove_all(const path &p, error_code *ec) {
            error_code tmp_ec;
            file_type type = query_file_type(p, &tmp_ec);
            if (error(type == status_error ? tmp_ec.value() : 0, p, ec,
                      "tinydb::filesystem::remove_all"))
                return 0;

            return (type != status_error && type != file_not_found)
                   ? remove_all_aux(p, type, ec)
                   : 0;
        }


        void rename(const path &old_p, const path &new_p, error_code *ec) {
            error(!BOOST_MOVE_FILE(old_p.c_str(), new_p.c_str()) ? BOOST_ERRNO : 0, old_p, new_p,
                  ec, "tinydb::filesystem::rename");
        }


        void resize_file(const path &p, uintmax_t size, boost::system::error_code *ec) {
            if (BOOST_UNLIKELY(size > static_cast< uintmax_t >((std::numeric_limits<off_t>::max)()))) {
                error(boost::system::errc::file_too_large, p, ec, "tinydb::filesystem::resize_file");
                return;
            }
            error(!BOOST_RESIZE_FILE(p.c_str(), size) ? BOOST_ERRNO : 0, p, ec,
                  "tinydb::filesystem::resize_file");
        }


        space_info space(const path &p, error_code *ec) {
            struct BOOST_STATVFS vfs;
            space_info info;
            if (!error(::BOOST_STATVFS(p.c_str(), &vfs) ? BOOST_ERRNO : 0,
                       p, ec, "tinydb::filesystem::space")) {
                info.capacity
                        = static_cast<boost::uintmax_t>(vfs.f_blocks) * BOOST_STATVFS_F_FRSIZE;
                info.free
                        = static_cast<boost::uintmax_t>(vfs.f_bfree) * BOOST_STATVFS_F_FRSIZE;
                info.available
                        = static_cast<boost::uintmax_t>(vfs.f_bavail) * BOOST_STATVFS_F_FRSIZE;
            } else {
                info.capacity = info.free = info.available = 0;
            }
            return info;
        }


        file_status status(const path &p, error_code *ec) {
            struct stat path_stat;
            if (::stat(p.c_str(), &path_stat) != 0) {
                if (ec != nullptr)
                    ec->assign(errno, system_category());

                if (not_found_error(errno)) {
                    return fs::file_status(fs::file_not_found, fs::no_perms);
                }
                if (ec == nullptr)
                    throw (filesystem_error("tinydb::filesystem::status",
                                            p, error_code(errno, system_category())));
                return fs::file_status(fs::status_error);
            }
            if (ec != nullptr) ec->clear();;
            if (S_ISDIR(path_stat.st_mode))
                return fs::file_status(fs::directory_file,
                                       static_cast<perms>(path_stat.st_mode) & fs::perms_mask);
            if (S_ISREG(path_stat.st_mode))
                return fs::file_status(fs::regular_file,
                                       static_cast<perms>(path_stat.st_mode) & fs::perms_mask);
            if (S_ISBLK(path_stat.st_mode))
                return fs::file_status(fs::block_file,
                                       static_cast<perms>(path_stat.st_mode) & fs::perms_mask);
            if (S_ISCHR(path_stat.st_mode))
                return fs::file_status(fs::character_file,
                                       static_cast<perms>(path_stat.st_mode) & fs::perms_mask);
            if (S_ISFIFO(path_stat.st_mode))
                return fs::file_status(fs::fifo_file,
                                       static_cast<perms>(path_stat.st_mode) & fs::perms_mask);
            if (S_ISSOCK(path_stat.st_mode))
                return fs::file_status(fs::socket_file,
                                       static_cast<perms>(path_stat.st_mode) & fs::perms_mask);
            return fs::file_status(fs::type_unknown);
        }


        file_status symlink_status(const path &p, error_code *ec) {
            struct stat path_stat;
            if (::lstat(p.c_str(), &path_stat) != 0) {
                if (ec != nullptr)
                    ec->assign(errno, system_category());

                if (errno == ENOENT || errno == ENOTDIR) {
                    return fs::file_status(fs::file_not_found, fs::no_perms);
                }
                if (ec == nullptr)
                    throw (filesystem_error("tinydb::filesystem::status",
                                            p, error_code(errno, system_category())));
                return fs::file_status(fs::status_error);
            }
            if (ec != nullptr) ec->clear();
            if (S_ISREG(path_stat.st_mode))
                return fs::file_status(fs::regular_file,
                                       static_cast<perms>(path_stat.st_mode) & fs::perms_mask);
            if (S_ISDIR(path_stat.st_mode))
                return fs::file_status(fs::directory_file,
                                       static_cast<perms>(path_stat.st_mode) & fs::perms_mask);
            if (S_ISLNK(path_stat.st_mode))
                return fs::file_status(fs::symlink_file,
                                       static_cast<perms>(path_stat.st_mode) & fs::perms_mask);
            if (S_ISBLK(path_stat.st_mode))
                return fs::file_status(fs::block_file,
                                       static_cast<perms>(path_stat.st_mode) & fs::perms_mask);
            if (S_ISCHR(path_stat.st_mode))
                return fs::file_status(fs::character_file,
                                       static_cast<perms>(path_stat.st_mode) & fs::perms_mask);
            if (S_ISFIFO(path_stat.st_mode))
                return fs::file_status(fs::fifo_file,
                                       static_cast<perms>(path_stat.st_mode) & fs::perms_mask);
            if (S_ISSOCK(path_stat.st_mode))
                return fs::file_status(fs::socket_file,
                                       static_cast<perms>(path_stat.st_mode) & fs::perms_mask);
            return fs::file_status(fs::type_unknown);
        }


        path temp_directory_path(boost::system::error_code *ec) {
            const char *val = nullptr;

            (val = std::getenv("TMPDIR")) ||
            (val = std::getenv("TMP")) ||
            (val = std::getenv("TEMP")) ||
            (val = std::getenv("TEMPDIR"));
            const char *default_tmp = "/tmp";
            path p((val != nullptr) ? val : default_tmp);
            if (p.empty() || (ec && !is_directory(p, *ec)) || (!ec && !is_directory(p))) {
                error(ENOTDIR, p, ec, "tinydb::filesystem::temp_directory_path");
                return p;
            }
            return p;
        }


        path system_complete(const path &p, boost::system::error_code *ec) {
            return (p.empty() || p.is_absolute())
                   ? p : current_path() / p;
        }


        path weakly_canonical(const path &p, boost::system::error_code *ec) {
            path head(p);
            path tail;
            boost::system::error_code tmp_ec{};
            path::iterator itr = p.end();

            for (; !head.empty(); --itr) {
                file_status head_status = status(head, tmp_ec);
                if (error(head_status.type() == fs::status_error,
                          head, ec, "tinydb::filesystem::weakly_canonical"))
                    return path();
                if (head_status.type() != fs::file_not_found)
                    break;
                head.remove_filename();
            }

            bool tail_has_dots = false;
            for (; itr != p.end(); ++itr) {
                tail /= *itr;

                if (itr->native().size() <= 2
                    && itr->native()[0] == dot
                    && (itr->native().size() == 1 || itr->native()[1] == dot))
                    tail_has_dots = true;
            }

            if (head.empty())
                return p.lexically_normal();
            head = canonical(head, tmp_ec);
            if (error(tmp_ec.value(), head, ec, "tinydb::filesystem::weakly_canonical"))
                return path();
            return tail.empty()
                   ? head
                   : (tail_has_dots
                      ? (head / tail).lexically_normal()
                      : head / tail);
        }
    }


    file_status
    directory_entry::m_get_status(boost::system::error_code *ec) const {
        if (!status_known(m_status)) {


            if (status_known(m_symlink_status)
                && !is_symlink(m_symlink_status)) {
                m_status = m_symlink_status;
                if (ec != nullptr) ec->clear();
            } else m_status = detail::status(m_path, ec);
        } else if (ec != nullptr) ec->clear();
        return m_status;
    }

    file_status
    directory_entry::m_get_symlink_status(boost::system::error_code *ec) const {
        if (!status_known(m_symlink_status))
            m_symlink_status = detail::symlink_status(m_path, ec);
        else if (ec != nullptr) ec->clear();
        return m_symlink_status;
    }


    namespace path_traits {
        void dispatch(const directory_entry &de,
                      std::string &to,
                      const codecvt_type &) {
            to = de.path().native();
        }

        void dispatch(const directory_entry &de,
                      std::string &to
        ) {
            to = de.path().native();
        }
    }
}


namespace {

    error_code path_max(std::size_t &result) {
        static std::size_t max = 0;
        if (max == 0) {
            errno = 0;
            long tmp = ::pathconf("/", _PC_NAME_MAX);
            if (tmp < 0) {
                if (errno == 0)
                    max = 4096;
                else return error_code(errno, system_category());
            } else max = static_cast<std::size_t>(tmp + 1);
        }
        result = max;
        return ok;
    }

    error_code dir_itr_first(void *&handle, void *&buffer,
                             const char *dir, string &target,
                             fs::file_status &, fs::file_status &) {
        if ((handle = ::opendir(dir)) == 0)
            return error_code(errno, system_category());
        target = string(".");


        std::size_t path_size(0);
        error_code ec = path_max(path_size);
        if (ec)return ec;
        dirent de;
        buffer = std::malloc((sizeof(dirent) - sizeof(de.d_name))
                             + path_size + 1);
        return ok;
    }


    inline int readdir_r_simulator(DIR *dirp, struct dirent *entry,
                                   struct dirent **result) {
        errno = 0;

        struct dirent *p;
        *result = nullptr;
        if ((p = ::readdir(dirp)) == nullptr)
            return errno;
        std::strcpy(entry->d_name, p->d_name);
        *result = entry;
        return 0;
    }

    error_code dir_itr_increment(void *&handle, void *&buffer,
                                 string &target, fs::file_status &sf, fs::file_status &symlink_sf) {
        BOOST_ASSERT(buffer != nullptr);
        dirent *entry(static_cast<dirent *>(buffer));
        dirent *result;
        int return_code;
        if ((return_code = readdir_r_simulator(static_cast<DIR *>(handle), entry, &result)) != 0)
            return error_code(errno, system_category());
        if (result == nullptr)
            return fs::detail::dir_itr_close(handle, buffer);
        target = entry->d_name;
        sf = symlink_sf = fs::file_status(fs::status_error);
        return ok;
    }

    const error_code not_found_error_code(
            ENOENT, system_category());

}

namespace tinydb ::filesystem ::detail {


    boost::system::error_code dir_itr_close(
            void *&handle, void *&buffer
    ) {
        std::free(buffer);
        buffer = nullptr;
        if (handle == nullptr)return ok;
        DIR *h(static_cast<DIR *>(handle));
        handle = nullptr;
        return error_code(::closedir(h) == 0 ? 0 : errno, system_category());
    }

    void directory_iterator_construct(directory_iterator &it,
                                      const path &p, boost::system::error_code *ec) {
        if (error(p.empty() ? not_found_error_code.value() : 0, p, ec,
                  "tinydb::filesystem::directory_iterator::construct"))
            return;

        path::string_type filename;
        file_status file_stat, symlink_file_stat;
        error_code result = dir_itr_first(it.m_imp->handle,
                                          it.m_imp->buffer,
                                          p.c_str(), filename, file_stat, symlink_file_stat);

        if (result) {
            it.m_imp.reset();
            error(result.value(), p,
                  ec, "tinydb::filesystem::directory_iterator::construct");
            return;
        }

        if (it.m_imp->handle == nullptr)
            it.m_imp.reset();
        else {
            it.m_imp->dir_entry.assign(p / filename, file_stat, symlink_file_stat);
            if (filename[0] == dot
                && (filename.size() == 1
                    || (filename[1] == dot
                        && filename.size() == 2))) { detail::directory_iterator_increment(it, ec); }
        }
    }

    void directory_iterator_increment(directory_iterator &it,
                                      boost::system::error_code *ec) {
        BOOST_ASSERT_MSG(it.m_imp.get(), "attempt to increment end iterator");
        BOOST_ASSERT_MSG(it.m_imp->handle != nullptr, "internal program error");

        path::string_type filename;
        file_status file_stat, symlink_file_stat;
        boost::system::error_code temp_ec;

        for (;;) {
            temp_ec = dir_itr_increment(it.m_imp->handle, it.m_imp->buffer, filename, file_stat, symlink_file_stat);

            if (temp_ec) {
                path error_path(it.m_imp->dir_entry.path().parent_path());
                it.m_imp.reset();
                if (ec == nullptr)
                    throw filesystem_error("tinydb::filesystem::directory_iterator::operator++",
                                           error_path,
                                           error_code(BOOST_ERRNO, system_category())));
                ec->assign(BOOST_ERRNO, system_category();
                return;
            } else if (ec != nullptr) ec->clear();

            if (it.m_imp->handle == nullptr) {
                it.m_imp.reset();
                return;
            }

            if (!(filename[0] == dot
                  && (filename.size() == 1
                      || (filename[1] == dot
                          && filename.size() == 2)))) {
                it.m_imp->dir_entry.replace_filename(
                        filename, file_stat, symlink_file_stat);
                return;
            }
        }
    }
}