#ifndef A_COMPILER_LEXER_H
#define A_COMPILER_LEXER_H

#include <string>

#include "token.h"
#include "utils.h"

class TheLexer {
  const SourceFile *source;
  size_t idx = 0;

  int line = 1;
  int column = 0;

  SourceLocation getSourceLocation() const {
    return SourceLocation{source->path, line, column};
  }
  char peekNextChar() const { return source->buffer[idx]; }
  char eatNextChar() {
    ++column;

    if (source->buffer[idx] == '\n') {
      ++line;
      column = 0;
    }

    return source->buffer[idx++];
  }

public:
  explicit TheLexer(const SourceFile &source) : source(&source) {}
  Token getNextToken();
};

#endif // A_COMPILER_LEXER_H
