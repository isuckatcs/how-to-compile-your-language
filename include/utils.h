#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_UTILS_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_UTILS_H

#define varOrReturn(var, init)                                                 \
  auto var = (init);                                                           \
  if (!var)                                                                    \
    return nullptr;

#include <string>

namespace yl {
struct Dumpable {
  [[nodiscard]] std::string indent(size_t level) const {
    return std::string(level * 2, ' ');
  }

  virtual ~Dumpable() = default;
  virtual void dump(size_t level = 0) const = 0;
};

struct SourceFile {
  std::string_view path;
  std::string buffer;
};

struct SourceLocation {
  std::string_view filepath;
  int line;
  int col;
};

std::nullptr_t report(SourceLocation location, std::string_view message,
                      bool isWarning = false);

} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_UTILS_H
