#ifndef TINY_DB_ENGINE_FILESYSTEM_FSTREAM_HPP
#define TINY_DB_ENGINE_FILESYSTEM_FSTREAM_HPP

#include <boost/config.hpp>

#include "path.hpp"
#include <iosfwd>
#include <fstream>

#include <boost/config/abi_prefix.hpp>
# define TINY_DB_ENGINE_FILESYSTEM_C_STR c_str()

namespace tinydb::filesystem {
    template<class charT, class traits = std::char_traits<charT> >
    class basic_filebuf : public std::basic_filebuf<charT, traits> {
    private:
        basic_filebuf(const basic_filebuf &);

        const basic_filebuf &operator=(const basic_filebuf &);

    public:
        basic_filebuf() {}

        virtual ~basic_filebuf() {}

        basic_filebuf<charT, traits> *
        open(const path &p, std::ios_base::openmode mode) {
            return std::basic_filebuf<charT, traits>::open(p.TINY_DB_ENGINE_FILESYSTEM_C_STR, mode)
                   ? this : 0;
        }
    };


    template<class charT, class traits = std::char_traits<charT> >
    class basic_ifstream : public std::basic_ifstream<charT, traits> {
    private:
        basic_ifstream(const basic_ifstream &);

        const basic_ifstream &operator=(const basic_ifstream &);

    public:
        basic_ifstream() {}

        explicit basic_ifstream(const path &p)
                : std::basic_ifstream<charT, traits>(p.TINY_DB_ENGINE_FILESYSTEM_C_STR, std::ios_base::in) {}

        basic_ifstream(const path &p, std::ios_base::openmode mode)
                : std::basic_ifstream<charT, traits>(p.TINY_DB_ENGINE_FILESYSTEM_C_STR, mode) {}

        void open(const path &p) {
            std::basic_ifstream<charT, traits>::open(p.TINY_DB_ENGINE_FILESYSTEM_C_STR, std::ios_base::in);
        }

        void open(const path &p, std::ios_base::openmode mode) {
            std::basic_ifstream<charT, traits>::open(p.TINY_DB_ENGINE_FILESYSTEM_C_STR, mode);
        }

        virtual ~basic_ifstream() {}
    };


    template<class charT, class traits = std::char_traits<charT> >
    class basic_ofstream : public std::basic_ofstream<charT, traits> {
    private:
        basic_ofstream(const basic_ofstream &);

        const basic_ofstream &operator=(const basic_ofstream &);

    public:
        basic_ofstream() {}


        explicit basic_ofstream(const path &p)
                : std::basic_ofstream<charT, traits>(p.TINY_DB_ENGINE_FILESYSTEM_C_STR, std::ios_base::out) {}

        basic_ofstream(const path &p, std::ios_base::openmode mode)
                : std::basic_ofstream<charT, traits>(p.TINY_DB_ENGINE_FILESYSTEM_C_STR, mode) {}

        void open(const path &p) {
            std::basic_ofstream<charT, traits>::open(p.TINY_DB_ENGINE_FILESYSTEM_C_STR, std::ios_base::out);
        }

        void open(const path &p, std::ios_base::openmode mode) {
            std::basic_ofstream<charT, traits>::open(p.TINY_DB_ENGINE_FILESYSTEM_C_STR, mode);
        }

        virtual ~basic_ofstream() {}
    };


    template<class charT, class traits = std::char_traits<charT> >
    class basic_fstream : public std::basic_fstream<charT, traits> {
    private:
        basic_fstream(const basic_fstream &);

        const basic_fstream &operator=(const basic_fstream &);

    public:
        basic_fstream() {}


        explicit basic_fstream(const path &p)
                : std::basic_fstream<charT, traits>(p.TINY_DB_ENGINE_FILESYSTEM_C_STR,
                                                    std::ios_base::in | std::ios_base::out) {}

        basic_fstream(const path &p, std::ios_base::openmode mode)
                : std::basic_fstream<charT, traits>(p.TINY_DB_ENGINE_FILESYSTEM_C_STR, mode) {}

        void open(const path &p) {
            std::basic_fstream<charT, traits>::open(p.TINY_DB_ENGINE_FILESYSTEM_C_STR,
                                                    std::ios_base::in | std::ios_base::out);
        }

        void open(const path &p, std::ios_base::openmode mode) {
            std::basic_fstream<charT, traits>::open(p.TINY_DB_ENGINE_FILESYSTEM_C_STR, mode);
        }

        virtual ~basic_fstream() {}

    };


    typedef basic_filebuf<char> filebuf;
    typedef basic_ifstream<char> ifstream;
    typedef basic_ofstream<char> ofstream;
    typedef basic_fstream<char> fstream;

    typedef basic_filebuf<wchar_t> wfilebuf;
    typedef basic_ifstream<wchar_t> wifstream;
    typedef basic_ofstream<wchar_t> wofstream;
    typedef basic_fstream<wchar_t> wfstream;

}

#include <boost/config/abi_suffix.hpp>

#endif
