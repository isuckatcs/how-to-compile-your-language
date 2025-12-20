#include <cassert>
#include <cmath>
#include <iostream>

#include "utils.h"

namespace yl {
std::string indent(size_t level) { return std::string(level * 2, ' '); }

std::nullptr_t
report(SourceLocation location, std::string_view message, bool isWarning) {
  const auto &[file, line, col] = location;

  assert(file && line != 0 && col != 0);
  std::cerr << file->path << ':' << line << ':' << col << ':'
            << (isWarning ? " warning: " : " error: ") << message << '\n';

  std::string_view bufferView = file->buffer;
  size_t pos = 0;

  for (int i = 1; i < line; ++i) {
    pos = bufferView.find('\n', pos);

    if (pos == std::string_view::npos)
      break;

    ++pos;
  }

  std::cerr << line << ' ' << '|' << ' '
            << bufferView.substr(pos, bufferView.find('\n', pos) - pos) << '\n';
  std::cerr << std::string(std::floor(std::log10(line) + 1), ' ') << ' ' << '|'
            << ' ' << std::string(col - 1, ' ') << '^' << '\n';

  return nullptr;
}
} // namespace yl
