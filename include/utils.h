#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_UTILS_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_UTILS_H

#define varOrReturn(var, init)                                                 \
  auto var = (init);                                                           \
  if (!var)                                                                    \
    return nullptr;

#include <optional>
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

std::nullptr_t report(SourceLocation location, std::string_view message,
                      bool isWarning = false);

template <typename Ty> class ConstantValueContainer {
  std::optional<Ty> value = std::nullopt;

public:
  void setConstantValue(std::optional<Ty> val) { value = std::move(val); }
  std::optional<Ty> getConstantValue() const { return value; }
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_UTILS_H
