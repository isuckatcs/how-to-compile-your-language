#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_LEXER_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_LEXER_H

#include <llvm/Support/ErrorHandling.h>

#include <cassert>
#include <optional>
#include <string>
#include <unordered_map>

#include "utils.h"

namespace yl {
constexpr char singleCharTokens[] = {'\0', '(', ')', '{', '}', ':', ';', ',',
                                     '+',  '*', '<', '>', '!', '.', '@'};

enum class TokenKind : char {
  Unk = -128,
  Slash,

  Equal,
  EqualEqual,
  Amp,
  AmpAmp,
  PipePipe,
  Minus,
  Arrow,

  Identifier,
  Number,

  KwFn,
  KwNumber,
  KwUnit,
  KwIf,
  KwElse,
  KwLet,
  KwMut,
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
  Asterisk = singleCharTokens[9],
  Lt = singleCharTokens[10],
  Gt = singleCharTokens[11],
  Excl = singleCharTokens[12],
  Dot = singleCharTokens[13],
  At = singleCharTokens[14],
};

std::string_view getOpStr(TokenKind op);

const std::unordered_map<std::string_view, TokenKind> keywords = {
    {"unit", TokenKind::KwUnit},     {"fn", TokenKind::KwFn},
    {"number", TokenKind::KwNumber}, {"if", TokenKind::KwIf},
    {"else", TokenKind::KwElse},     {"let", TokenKind::KwLet},
    {"mut", TokenKind::KwMut},       {"while", TokenKind::KwWhile},
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
