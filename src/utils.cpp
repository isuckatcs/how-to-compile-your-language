#include "utils.h"

std::nullptr_t error(SourceLocation location, std::string_view message) {
  const auto &[file, line, col] = location;

  std::cerr << file << ':' << line << ':' << col << ": error: " << message
            << '\n';

  return nullptr;
}
