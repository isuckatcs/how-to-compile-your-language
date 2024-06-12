#ifndef A_LANGUAGE_UTILS_H
#define A_LANGUAGE_UTILS_H

#include <iostream>
#include <optional>
#include <string>

struct Dumpable {
  void indent(size_t level) {
    for (size_t i = 0; i < level; ++i)
      std::cerr << "  ";
  }

  virtual void dump(size_t level = 0) = 0;
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

std::nullptr_t error(SourceLocation location, std::string_view message);

template <typename Base, typename Ty> class ConstantValueContainer {
  std::optional<Ty> value = std::nullopt;

  ConstantValueContainer() = default;
  friend Base;

public:
  void setConstantValue(std::optional<Ty> val) { value = std::move(val); }
  std::optional<Ty> getConstantValue() const { return value; }
};

#endif // A_LANGUAGE_UTILS_H
