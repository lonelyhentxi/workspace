#include "../cxxstd/path_traits.hpp"
#include <boost/system/system_error.hpp>
#include <boost/scoped_array.hpp>
#include <locale>   
#include <cstring>  
#include <cwchar>   

namespace pt = tinydb::filesystem::path_traits;
namespace fs = tinydb::filesystem;
namespace bs = boost::system;

#ifndef BOOST_FILESYSTEM_CODECVT_BUF_SIZE
# define BOOST_FILESYSTEM_CODECVT_BUF_SIZE 256
#endif

namespace {

  const std::size_t default_codecvt_buf_size = BOOST_FILESYSTEM_CODECVT_BUF_SIZE;

  void convert_aux(
                   const char* from,
                   const char* from_end,
                   wchar_t* to, wchar_t* to_end,
                   std::wstring & target,
                   const pt::codecvt_type & cvt)
  {

    std::mbstate_t state  = std::mbstate_t();  
    const char* from_next;
    wchar_t* to_next;

    std::codecvt_base::result res;

    if ((res=cvt.in(state, from, from_end, from_next,
           to, to_end, to_next)) != std::codecvt_base::ok)
    {
      
      throw bs::system_error(res, fs::codecvt_error_category(),
        "tinydb::filesystem::path codecvt to wstring");
    }
    target.append(to, to_next); 
  }

  void convert_aux(
                   const wchar_t* from,
                   const wchar_t* from_end,
                   char* to, char* to_end,
                   std::string & target,
                   const pt::codecvt_type & cvt)
  {

    std::mbstate_t state  = std::mbstate_t();  
    const wchar_t* from_next;
    char* to_next;

    std::codecvt_base::result res;

    if ((res=cvt.out(state, from, from_end, from_next,
           to, to_end, to_next)) != std::codecvt_base::ok)
    {
      throw bs::system_error(res, fs::codecvt_error_category(),"tinydb::filesystem::path codecvt to string");
    }
    target.append(to, to_next); 
  }
  
}

namespace tinydb::filesystem::path_traits {

  void convert(const char* from,
                const char* from_end,    
                std::wstring & to,
                const codecvt_type & cvt)
  {
    BOOST_ASSERT(from);

    if (!from_end)  
    {
      from_end = from + std::strlen(from);
    }

    if (from == from_end) return;

    std::size_t buf_size = (from_end - from) * 3;  

    
    if (buf_size > default_codecvt_buf_size)
    {
      boost::scoped_array< wchar_t > buf(new wchar_t [buf_size]);
      convert_aux(from, from_end, buf.get(), buf.get()+buf_size, to, cvt);
    }
    else
    {
      wchar_t buf[default_codecvt_buf_size];
      convert_aux(from, from_end, buf, buf+default_codecvt_buf_size, to, cvt);
    }
  }

  void convert(const wchar_t* from,
                const wchar_t* from_end,  
                std::string & to,
                const codecvt_type & cvt)
  {
    BOOST_ASSERT(from);

    if (!from_end)  
    {
      from_end = from + std::wcslen(from);
    }

    if (from == from_end) return;

    std::size_t buf_size = (from_end - from) * 4;  
    buf_size += 4;  
    if (buf_size > default_codecvt_buf_size)
    {
      boost::scoped_array< char > buf(new char [buf_size]);
      convert_aux(from, from_end, buf.get(), buf.get()+buf_size, to, cvt);
    }
    else
    {
      char buf[default_codecvt_buf_size];
      convert_aux(from, from_end, buf, buf+default_codecvt_buf_size, to, cvt);
    }
  }
}
