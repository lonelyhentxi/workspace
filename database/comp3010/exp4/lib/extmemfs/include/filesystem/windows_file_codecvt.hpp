#ifndef TINY_DB_ENGINE_FILESYSTEM_WIN_FILE_CODECVT_HPP
#define TINY_DB_ENGINE_FILESYSTEM_WIN_FILE_CODECVT_HPP

#include <locale>

  class windows_file_codecvt
    : public std::codecvt< wchar_t, char, std::mbstate_t >  
  {
  public:
    explicit windows_file_codecvt(std::size_t refs = 0)
        : std::codecvt<wchar_t, char, std::mbstate_t>(refs) {}
  protected:

    bool do_always_noconv() const noexcept override { return false; }

    int do_encoding() const noexcept override { return 0; }

    std::codecvt_base::result do_in(std::mbstate_t& state,
      const char* from, const char* from_end, const char*& from_next,
      wchar_t* to, wchar_t* to_end, wchar_t*& to_next) const override;

    std::codecvt_base::result do_out(std::mbstate_t & state,
      const wchar_t* from, const wchar_t* from_end, const wchar_t*& from_next,
      char* to, char* to_end, char*& to_next) const override;

    std::codecvt_base::result do_unshift(std::mbstate_t&,
        char* /*from*/, char* /*to*/, char* & /*next*/) const override { return ok; }

    int do_length(std::mbstate_t&,
      const char* /*from*/, const char* /*from_end*/, std::size_t /*max*/) const override { return 0; }

    int do_max_length() const noexcept override { return 0; }
  };

#endif
