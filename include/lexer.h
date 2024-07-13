#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_LEXER_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_LEXER_H

#include <optional>
#include <string>
#include <unordered_map>

#include "utils.h"

namespace yl {
constexpr char singleCharTokens[] = {'\0', '(', ')', '{', '}', ':', ';', ','};

enum class TokenKind : char {
  Unk = -128,

  Identifier,
  Number,

  KwFn,
  KwNumber,
  KwVoid,
  KwReturn,

  Eof = singleCharTokens[0],
  Lpar = singleCharTokens[1],
  Rpar = singleCharTokens[2],
  Lbrace = singleCharTokens[3],
  Rbrace = singleCharTokens[4],
  Colon = singleCharTokens[5],
  Semi = singleCharTokens[6],
  Comma = singleCharTokens[7],
};

const std::unordered_map<std::string_view, TokenKind> keywords = {
    {"void", TokenKind::KwVoid},
    {"fn", TokenKind::KwFn},
    {"number", TokenKind::KwNumber},
    {"return", TokenKind::KwReturn}};

struct Token {
  SourceLocation location;
  TokenKind kind;
  std::optional<std::string> value;
};

class Lexer {
  const SourceFile *source;
  size_t idx = 0;

  int line = 1;
  int column = 0;

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
  explicit Lexer(const SourceFile &source) : source(&source) {}
  Token getNextToken();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_LEXER_H
