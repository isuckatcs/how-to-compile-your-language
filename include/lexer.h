#ifndef A_COMPILER_LEXER_H
#define A_COMPILER_LEXER_H

#include <optional>
#include <string>

#include "utils.h"

enum class TokenKind {
  eof,
  unk,

  lpar,
  rpar,
  lbrace,
  rbrace,
  colon,
  semi,
  comma,

  plus,
  minus,
  asterisk,
  slash,

  lt,
  gt,
  excl,
  equalequal,
  ampamp,
  pipepipe,

  equal,

  identifier,
  number,

  kw_fn,
  kw_number,
  kw_void,
  kw_if,
  kw_else,
  kw_let,
  kw_var
};

struct Token {
  SourceLocation location;
  TokenKind kind;
  std::optional<std::string> value;
};

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
