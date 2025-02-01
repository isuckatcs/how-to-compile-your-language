#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_LEXER_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_LEXER_H

#include <cassert>
#include <optional>
#include <string>
#include <unordered_map>

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

const std::unordered_map<std::string_view, TokenKind> keywords = {
    {"void", TokenKind::KwVoid},     {"fn", TokenKind::KwFn},
    {"number", TokenKind::KwNumber}, {"if", TokenKind::KwIf},
    {"else", TokenKind::KwElse},     {"let", TokenKind::KwLet},
    {"var", TokenKind::KwVar},       {"while", TokenKind::KwWhile},
    {"return", TokenKind::KwReturn}, {"struct", TokenKind::KwStruct}};

struct Token {
  SourceLocation location;
  TokenKind kind;
  std::optional<std::string> value = std::nullopt;
};

class Lexer {
  const SourceFile *source;
  size_t idx = 0;

  int line = 1;
  int column = 0;

  char peekNextChar() const { return source->buffer[idx]; }
  char eatNextChar() {
    assert(idx <= source->buffer.size() &&
           "indexing past the end of the source buffer");

    ++column;

    if (source->buffer[idx] == '\n') {
      ++line;
      column = 0;
    }

    return source->buffer[idx++];
  }

public:
  explicit Lexer(const SourceFile &source)
      : source(&source) {}
  Token getNextToken();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_LEXER_H
