#include <cassert>
#include <iostream>

#include "utils.h"

std::nullptr_t report(SourceLocation location, std::string_view message,
                      bool isWarning) {
  const auto &[file, line, col] = location;

  assert(!file.empty() && line != 0 && col != 0);
  std::cerr << file << ':' << line << ':' << col << ':'
            << (isWarning ? " warning: " : " error: ") << message << '\n';

  return nullptr;
}
