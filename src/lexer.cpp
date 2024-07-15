#include "lexer.h"

namespace {
bool isSpace(char c) {
  return c == ' ' || c == '\f' || c == '\n' || c == '\r' || c == '\t' ||
         c == '\v';
}

bool isAlpha(char c) { return 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z'; }
bool isNum(char c) { return '0' <= c && c <= '9'; }
bool isAlnum(char c) { return isAlpha(c) || isNum(c); }
} // namespace

namespace yl {
Token Lexer::getNextToken() {
  char currentChar = eatNextChar();

  while (isSpace(currentChar))
    currentChar = eatNextChar();

  SourceLocation tokenStartLocation{source->path, line, column};

  for (auto &&c : singleCharTokens)
    if (c == currentChar)
      return Token{tokenStartLocation, static_cast<TokenKind>(c)};

  if (currentChar == '/') {
    if (peekNextChar() != '/')
      return Token{tokenStartLocation, TokenKind::Slash};

    char c = eatNextChar();
    while (c != '\n' && c != '\0')
      c = eatNextChar();

    return getNextToken();
  }

  if (currentChar == '=' && peekNextChar() == '=') {
    eatNextChar();
    return Token{tokenStartLocation, TokenKind::EqualEqual};
  }

  if (currentChar == '&' && peekNextChar() == '&') {
    eatNextChar();
    return Token{tokenStartLocation, TokenKind::AmpAmp};
  }

  if (currentChar == '|' && peekNextChar() == '|') {
    eatNextChar();
    return Token{tokenStartLocation, TokenKind::PipePipe};
  }

  if (isAlpha(currentChar)) {
    std::string value{currentChar};

    while (isAlnum(peekNextChar()))
      value += eatNextChar();

    if (keywords.count(value))
      return Token{tokenStartLocation, keywords.at(value), std::move(value)};

    return Token{tokenStartLocation, TokenKind::Identifier, std::move(value)};
  }

  // [0-9]+ (. [0-9]+)?
  if (isNum(currentChar)) {
    std::string value{currentChar};

    while (isNum(peekNextChar()))
      value += eatNextChar();

    if (peekNextChar() != '.')
      return Token{tokenStartLocation, TokenKind::Number, value};

    value += eatNextChar();

    if (!isNum(peekNextChar()))
      return Token{tokenStartLocation, TokenKind::Unk};

    while (isNum(peekNextChar()))
      value += eatNextChar();

    return Token{tokenStartLocation, TokenKind::Number, value};
  }

  return Token{tokenStartLocation, TokenKind::Unk};
}
} // namespace yl
