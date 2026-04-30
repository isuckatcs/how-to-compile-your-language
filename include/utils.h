#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_UTILS_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_UTILS_H

#define varOrReturn(var, init)                                                 \
  auto var = (init);                                                           \
  if (!var)                                                                    \
    return nullptr;

#include <string>

namespace yl {

struct SourceFile {
  std::string_view path;
  std::string buffer;
};

struct SourceLocation {
  const SourceFile *file;
  int line;
  int col;
};

std::string indent(size_t level);
std::nullptr_t report(SourceLocation location,
                      std::string_view message,
                      bool isWarning = false);
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_UTILS_H
