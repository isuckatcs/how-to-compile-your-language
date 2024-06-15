#include <cassert>
#include <iostream>

#include "utils.h"

std::nullptr_t error(SourceLocation location, std::string_view message) {
  const auto &[file, line, col] = location;

  assert(!file.empty() && line != 0 && col != 0);
  std::cerr << file << ':' << line << ':' << col << ": error: " << message
            << '\n';

  return nullptr;
}
