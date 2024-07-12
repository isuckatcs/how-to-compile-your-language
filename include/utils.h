#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_UTILS_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_UTILS_H

#include <string>

namespace yl {
struct SourceFile {
  std::string_view path;
  std::string buffer;
};

struct SourceLocation {
  std::string_view filepath;
  int line;
  int col;
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_UTILS_H
