#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_LEXER_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_LEXER_H

#include <llvm/Support/ErrorHandling.h>

#include <cassert>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include "utils.h"

namespace yl {
constexpr char singleCharTokens[] = {'(', ')', '{', '}', ';', ',', '+',
                                     '*', '<', '>', '!', '.', '@'};

enum class TokenKind : signed char {
  Unk = -128,
  Eof,
  Slash,

  Equal,
  EqualEqual,
  Amp,
  AmpAmp,
  PipePipe,
  Minus,
  Arrow,
  Colon,
  ColonColon,

  Identifier,
  Number,

  KwFn,
  KwNumber,
  KwUnit,
  KwBool,
  KwTrue,
  KwFalse,
  KwIf,
  KwElse,
  KwLet,
  KwMut,
  KwWhile,
  KwReturn,
  KwStruct,
  KwSelf,
  KwTrait,
  KwExtension,
  KwGC,
  KwAny,

  Lpar = singleCharTokens[0],
  Rpar = singleCharTokens[1],
  Lbrace = singleCharTokens[2],
  Rbrace = singleCharTokens[3],
  Semi = singleCharTokens[4],
  Comma = singleCharTokens[5],
  Plus = singleCharTokens[6],
  Asterisk = singleCharTokens[7],
  Lt = singleCharTokens[8],
  Gt = singleCharTokens[9],
  Excl = singleCharTokens[10],
  Dot = singleCharTokens[11],
  At = singleCharTokens[12],
};

std::string_view getOpStr(TokenKind op);

const std::unordered_map<std::string_view, TokenKind> keywords = {
    {"unit", TokenKind::KwUnit},
    {"fn", TokenKind::KwFn},
    {"number", TokenKind::KwNumber},
    {"if", TokenKind::KwIf},
    {"else", TokenKind::KwElse},
    {"let", TokenKind::KwLet},
    {"mut", TokenKind::KwMut},
    {"while", TokenKind::KwWhile},
    {"return", TokenKind::KwReturn},
    {"struct", TokenKind::KwStruct},
    {"Self", TokenKind::KwSelf},
    {"trait", TokenKind::KwTrait},
    {"extension", TokenKind::KwExtension},
    {"bool", TokenKind::KwBool},
    {"true", TokenKind::KwTrue},
    {"false", TokenKind::KwFalse},
    {"gc", TokenKind::KwGC},
    {"any", TokenKind::KwAny}};

const std::unordered_set<TokenKind> topLevelTokens = {
    TokenKind::KwTrait, TokenKind::KwStruct, TokenKind::KwFn,
    TokenKind::KwExtension, TokenKind::Eof};

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
  char peekSecondChar() const { return source->buffer[idx + 1]; }
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
