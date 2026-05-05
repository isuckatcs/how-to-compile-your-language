#include <cassert>
#include <cmath>
#include <iostream>

#include "diag.h"

namespace yl {

namespace diag {
void PrintingDiagnosticConsumer::consume(Diagnostic diagnostic) {
  const auto &[file, line, col] = diagnostic.location;
  assert(file && line != 0 && col != 0);

  std::cerr << file->path << ':' << line << ':' << col << ':';
  switch (diagnostic.severity) {
  case Diagnostic::Severity::Error:
    std::cerr << " error: ";
    break;
  case Diagnostic::Severity::Warning:
    std::cerr << " warning: ";
    break;
  }
  std::cerr << diagnostic.message << '\n';

  std::string_view bufferView = file->buffer;
  size_t pos = 0;

  for (int i = 1; i < line; ++i)
    pos = bufferView.find('\n', pos) + 1;

  std::cerr << line << ' ' << '|' << ' '
            << bufferView.substr(pos, bufferView.find('\n', pos) - pos) << '\n';
  std::cerr << std::string(std::floor(std::log10(line) + 1), ' ') << ' ' << '|'
            << ' ' << std::string(col - 1, ' ') << '^' << '\n';
}
} // namespace diag
} // namespace yl
