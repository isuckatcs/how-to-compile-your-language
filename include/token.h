#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_TOKEN_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_TOKEN_H

#include <optional>
#include <string>

#include "utils.h"

namespace yl {
constexpr char singleCharTokens[] = {'\0', '(', ')', '{', '}', ':', ';', ',',
                                     '+',  '-', '*', '<', '>', '!', '.'};

enum class TokenKind : char {
  Unk = -128,
  Slash,

  Equal,
  EqualEqual,
  AmpAmp,
  PipePipe,

  Identifier,
  Number,

  KwFn,
  KwNumber,
  KwVoid,
  KwIf,
  KwElse,
  KwLet,
  KwVar,
  KwWhile,
  KwReturn,
  KwStruct,

  Eof = singleCharTokens[0],
  Lpar = singleCharTokens[1],
  Rpar = singleCharTokens[2],
  Lbrace = singleCharTokens[3],
  Rbrace = singleCharTokens[4],
  Colon = singleCharTokens[5],
  Semi = singleCharTokens[6],
  Comma = singleCharTokens[7],
  Plus = singleCharTokens[8],
  Minus = singleCharTokens[9],
  Asterisk = singleCharTokens[10],
  Lt = singleCharTokens[11],
  Gt = singleCharTokens[12],
  Excl = singleCharTokens[13],
  Dot = singleCharTokens[14],
};

struct Token {
  SourceLocation location;
  TokenKind kind;
  std::string value;
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_TOKEN_H
