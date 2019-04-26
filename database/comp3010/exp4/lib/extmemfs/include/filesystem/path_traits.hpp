#ifndef TINY_DB_ENGINE_FILESYSTEM_PATH_TRAITS_HPP
#define TINY_DB_ENGINE_FILESYSTEM_PATH_TRAITS_HPP

#include <boost/config.hpp>
#include <boost/utility/enable_if.hpp>
#include <boost/type_traits/is_array.hpp>
#include <boost/type_traits/decay.hpp>
#include <boost/system/error_code.hpp>
#include <cwchar>  
#include <string>
#include <vector>
#include <list>
#include <iterator>
#include <locale>
#include <boost/assert.hpp>

#include <boost/config/abi_prefix.hpp> 

namespace tinydb::filesystem {

  const boost::system::error_category& codecvt_error_category();

  class directory_entry;
  
namespace path_traits {
 
  typedef std::codecvt<wchar_t, char, std::mbstate_t> codecvt_type;

  

  template <class T>
  struct is_pathable { static const bool value = false; };

  template<> struct is_pathable<char*>                  { static const bool value = true; };
  template<> struct is_pathable<const char*>            { static const bool value = true; };
  template<> struct is_pathable<wchar_t*>               { static const bool value = true; };
  template<> struct is_pathable<const wchar_t*>         { static const bool value = true; };
  template<> struct is_pathable<std::string>            { static const bool value = true; };
  template<> struct is_pathable<std::wstring>           { static const bool value = true; };
  template<> struct is_pathable<std::vector<char> >     { static const bool value = true; };
  template<> struct is_pathable<std::vector<wchar_t> >  { static const bool value = true; };
  template<> struct is_pathable<std::list<char> >       { static const bool value = true; };
  template<> struct is_pathable<std::list<wchar_t> >    { static const bool value = true; };
  template<> struct is_pathable<directory_entry>        { static const bool value = true; };

  

  template <class Container> inline
    
    
    typename boost::disable_if<boost::is_array<Container>, bool>::type
      empty(const Container & c)
        { return c.begin() == c.end(); }

  template <class T> inline
    bool empty(T * const & c_str)
  {
    BOOST_ASSERT(c_str);
    return !*c_str;
  }

  template <typename T, size_t N> inline
     bool empty(T (&x)[N])
       { return !x[0]; }

  
  
  

  

  
    void convert(const char* from,
    const char* from_end,    
    std::wstring & to,
    const codecvt_type& cvt);

  
    void convert(const wchar_t* from,
    const wchar_t* from_end,  
    std::string & to,
    const codecvt_type& cvt);

  inline
    void convert(const char* from,
    std::wstring & to,
    const codecvt_type& cvt)
  {
    BOOST_ASSERT(from);
    convert(from, 0, to, cvt);
  }

  inline
    void convert(const wchar_t* from,
    std::string & to,
    const codecvt_type& cvt)
  {
    BOOST_ASSERT(from);
    convert(from, 0, to, cvt);
  }

  

  inline
    void convert(const char* from,
    const char* from_end,    
    std::wstring & to);

  inline
    void convert(const wchar_t* from,
    const wchar_t* from_end,  
    std::string & to);

  inline
    void convert(const char* from,
    std::wstring & to);

  inline
    void convert(const wchar_t* from,
    std::string & to);

  

  

  inline
    void convert(const char* from, const char* from_end, std::string & to,
    const codecvt_type&)
  {
    BOOST_ASSERT(from);
    BOOST_ASSERT(from_end);
    to.append(from, from_end);
  }

  inline
    void convert(const char* from,
    std::string & to,
    const codecvt_type&)
  {
    BOOST_ASSERT(from);
    to += from;
  }

  

  inline
    void convert(const wchar_t* from, const wchar_t* from_end, std::wstring & to,
    const codecvt_type&)
  {
    BOOST_ASSERT(from);
    BOOST_ASSERT(from_end);
    to.append(from, from_end);
  }

  inline
    void convert(const wchar_t* from,
    std::wstring & to,
    const codecvt_type&)
  {
    BOOST_ASSERT(from);
    to += from;
  }

  

  inline
    void convert(const char* from, const char* from_end, std::string & to)
  {
    BOOST_ASSERT(from);
    BOOST_ASSERT(from_end);
    to.append(from, from_end);
  }

  inline
    void convert(const char* from, std::string & to)
  {
    BOOST_ASSERT(from);
    to += from;
  }

  

  inline
    void convert(const wchar_t* from, const wchar_t* from_end, std::wstring & to)
  {
    BOOST_ASSERT(from);
    BOOST_ASSERT(from_end);
    to.append(from, from_end);
  }

  inline
    void convert(const wchar_t* from, std::wstring & to)
  {
    BOOST_ASSERT(from);
    to += from;
  }

  

  
  template <class U> inline
    void dispatch(const std::string& c, U& to, const codecvt_type& cvt)
  {
    if (c.size())
      convert(&*c.begin(), &*c.begin() + c.size(), to, cvt);
  }
  template <class U> inline
    void dispatch(const std::wstring& c, U& to, const codecvt_type& cvt)
  {
    if (c.size())
      convert(&*c.begin(), &*c.begin() + c.size(), to, cvt);
  }
  template <class U> inline
    void dispatch(const std::vector<char>& c, U& to, const codecvt_type& cvt)
  {
    if (c.size())
      convert(&*c.begin(), &*c.begin() + c.size(), to, cvt);
  }
  template <class U> inline
    void dispatch(const std::vector<wchar_t>& c, U& to, const codecvt_type& cvt)
  {
    if (c.size())
      convert(&*c.begin(), &*c.begin() + c.size(), to, cvt);
  }

  
  template <class U> inline
    void dispatch(const std::string& c, U& to)
  {
    if (c.size())
      convert(&*c.begin(), &*c.begin() + c.size(), to);
  }
  template <class U> inline
    void dispatch(const std::wstring& c, U& to)
  {
    if (c.size())
      convert(&*c.begin(), &*c.begin() + c.size(), to);
  }
  template <class U> inline
    void dispatch(const std::vector<char>& c, U& to)
  {
    if (c.size())
      convert(&*c.begin(), &*c.begin() + c.size(), to);
  }
  template <class U> inline
    void dispatch(const std::vector<wchar_t>& c, U& to)
  {
    if (c.size())
      convert(&*c.begin(), &*c.begin() + c.size(), to);
  }

  
  template <class Container, class U> inline
    
    
    typename boost::disable_if<boost::is_array<Container>, void>::type
    dispatch(const Container & c, U& to, const codecvt_type& cvt)
  {
    if (c.size())
    {
      std::basic_string<typename Container::value_type> s(c.begin(), c.end());
      convert(s.c_str(), s.c_str()+s.size(), to, cvt);
    }
  }

  
  template <class T, class U> inline
    void dispatch(T * const & c_str, U& to, const codecvt_type& cvt)
  {
    
    BOOST_ASSERT(c_str);
    convert(c_str, to, cvt);
  }

  
  

  
    void dispatch(const directory_entry & de,
    std::string & to,
    const codecvt_type&);

  
  template <class Container, class U> inline
    
    
    typename boost::disable_if<boost::is_array<Container>, void>::type
    dispatch(const Container & c, U& to)
  {
    if (c.size())
    {
      std::basic_string<typename Container::value_type> seq(c.begin(), c.end());
      convert(seq.c_str(), seq.c_str()+seq.size(), to);
    }
  }

  
  template <class T, class U> inline
    void dispatch(T * const & c_str, U& to)
  {
    
    BOOST_ASSERT(c_str);
    convert(c_str, to);
  }
  
  void dispatch(const directory_entry & de, std::string & to);


}} 

#include <boost/config/abi_suffix.hpp> 

#endif  
