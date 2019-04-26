#include <boost/system/api_config.hpp>  // for BOOST_POSIX_API or BOOST_WINDOWS_API
#include <cwchar>  // for mbstate_t

#ifdef BOOST_WINDOWS_API
#include "windows_file_codecvt.hpp"
#include <windows.h>

  std::codecvt_base::result windows_file_codecvt::do_in(
    std::mbstate_t &, 
    const char* from, const char* from_end, const char*& from_next,
    wchar_t* to, wchar_t* to_end, wchar_t*& to_next) const
  {
    UINT codepage = AreFileApisANSI() ? CP_ACP : CP_OEMCP;

    int count;
    if ((count = ::MultiByteToWideChar(codepage, MB_PRECOMPOSED, from,
      static_cast<int>(from_end - from), to, static_cast<int>(to_end - to))) == 0) 
    {
      return error;  // conversion failed
    }

    from_next = from_end;
    to_next = to + count;
    *to_next = L'\0';
    return ok;
 }

  std::codecvt_base::result windows_file_codecvt::do_out(
    std::mbstate_t &,
    const wchar_t* from, const wchar_t* from_end, const wchar_t*  & from_next,
    char* to, char* to_end, char* & to_next) const
  {
    UINT codepage = AreFileApisANSI() ? CP_ACP : CP_OEMCP;

    int count;
    if ((count = ::WideCharToMultiByte(codepage, WC_NO_BEST_FIT_CHARS, from,
      static_cast<int>(from_end - from), to, static_cast<int>(to_end - to), nullptr, nullptr)) == 0)
    {
      return error;  // conversion failed
    }

    from_next = from_end;
    to_next = to + count;
    *to_next = '\0';
    return ok;
  }
  # endif  // BOOST_WINDOWS_API

