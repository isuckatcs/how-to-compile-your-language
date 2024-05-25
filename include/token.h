#ifndef A_COMPILER_TOKEN_H
#define A_COMPILER_TOKEN_H

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

  identifier,
  number,

  kw_fn,
  kw_number,
  kw_void,
};

struct Token {
  SourceLocation location;
  TokenKind kind;
  std::optional<std::string> value;
};

#endif // A_COMPILER_TOKEN_H
