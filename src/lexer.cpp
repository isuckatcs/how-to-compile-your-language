#include "lexer.h"

namespace {
bool isSpace(char c) {
  return c == ' ' || c == '\f' || c == '\n' || c == '\r' || c == '\t' ||
         c == '\v';
}

bool isAlpha(char c) {
  return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z');
}
bool isNum(char c) { return '0' <= c && c <= '9'; }
bool isAlnum(char c) { return isAlpha(c) || isNum(c); }
} // namespace

namespace yl {
std::string_view getOpStr(TokenKind op) {
  if (op == TokenKind::Plus)
    return "+";
  if (op == TokenKind::Minus)
    return "-";
  if (op == TokenKind::Asterisk)
    return "*";
  if (op == TokenKind::Slash)
    return "/";
  if (op == TokenKind::Equal)
    return "=";
  if (op == TokenKind::EqualEqual)
    return "==";
  if (op == TokenKind::Amp)
    return "&";
  if (op == TokenKind::AmpAmp)
    return "&&";
  if (op == TokenKind::PipePipe)
    return "||";
  if (op == TokenKind::Lt)
    return "<";
  if (op == TokenKind::Gt)
    return ">";
  if (op == TokenKind::Excl)
    return "!";

  llvm_unreachable("unexpected operator");
}

Token Lexer::getNextToken() {
  char currentChar = eatNextChar();

  while (isSpace(currentChar))
    currentChar = eatNextChar();

  SourceLocation tokenStartLocation{source, line, column};

  for (auto &&c : singleCharTokens)
    if (c == currentChar)
      return Token{tokenStartLocation, static_cast<TokenKind>(c)};

  if (currentChar == '/') {
    if (peekNextChar() != '/')
      return Token{tokenStartLocation, TokenKind::Slash};

    while (peekNextChar() != '\n' && peekNextChar() != '\0')
      eatNextChar();

    return getNextToken();
  }

  if (currentChar == '=') {
    if (peekNextChar() != '=')
      return Token{tokenStartLocation, TokenKind::Equal};

    eatNextChar();
    return Token{tokenStartLocation, TokenKind::EqualEqual};
  }

  if (currentChar == '&') {
    if (peekNextChar() == '&') {
      eatNextChar();
      return Token{tokenStartLocation, TokenKind::AmpAmp};
    }

    return Token{tokenStartLocation, TokenKind::Amp};
  }

  if (currentChar == '|' && peekNextChar() == '|') {
    eatNextChar();
    return Token{tokenStartLocation, TokenKind::PipePipe};
  }

  if (currentChar == '-') {
    if (peekNextChar() != '>')
      return Token{tokenStartLocation, TokenKind::Minus};

    eatNextChar();
    return Token{tokenStartLocation, TokenKind::Arrow};
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
