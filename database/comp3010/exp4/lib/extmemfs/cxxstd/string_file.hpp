#ifndef TINY_DB_ENGINE_FILESYSTEM_STRING_FILE_HPP
#define TINY_DB_ENGINE_FILESYSTEM_STRING_FILE_HPP

#include <string>
#include "fstream.hpp"
#include "operations.hpp"

namespace tinydb::filesystem
{
inline
void save_string_file(const path& p, const std::string& str)
{
  ofstream file;
  file.exceptions(std::ofstream::failbit | std::ofstream::badbit);
  file.open(p, std::ios_base::binary);
  file.write(str.c_str(), str.size());
}

inline
void load_string_file(const path& p, std::string& str)
{
  ifstream file;
  file.exceptions(std::ifstream::failbit | std::ifstream::badbit);
  file.open(p, std::ios_base::binary);
  auto sz = static_cast<std::size_t>(file_size(p));
  str.resize(sz, '\0');
  file.read(&str[0], sz);
}
}  

#endif  
