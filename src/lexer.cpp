#include <unordered_map>

#include "lexer.h"

namespace {
bool isSpace(char c) {
  return c == ' ' || c == '\f' || c == '\n' || c == '\r' || c == '\t' ||
         c == '\v';
}

bool isAlpha(char c) { return 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z'; }
bool isNum(char c) { return '0' <= c && c <= '9'; }
bool isAlnum(char c) { return isAlpha(c) || isNum(c); }

std::unordered_map<std::string_view, TokenKind> keywords = {
    {"void", TokenKind::KwVoid},     {"fn", TokenKind::KwFn},
    {"number", TokenKind::KwNumber}, {"if", TokenKind::KwIf},
    {"else", TokenKind::KwElse},     {"let", TokenKind::KwLet},
    {"var", TokenKind::KwVar},       {"while", TokenKind::KwWhile},
    {"return", TokenKind::KwReturn}};

} // namespace

Token TheLexer::getNextToken() {
  char currentChar = eatNextChar();

  while (isSpace(currentChar))
    currentChar = eatNextChar();

  SourceLocation tokenStartLocation = getSourceLocation();

  switch (currentChar) {
  case '(':
    return Token{tokenStartLocation, TokenKind::Lpar};
  case ')':
    return Token{tokenStartLocation, TokenKind::Rpar};
  case '{':
    return Token{tokenStartLocation, TokenKind::Lbrace};
  case '}':
    return Token{tokenStartLocation, TokenKind::Rbrace};
  case ':':
    return Token{tokenStartLocation, TokenKind::Colon};
  case ';':
    return Token{tokenStartLocation, TokenKind::Semi};
  case ',':
    return Token{tokenStartLocation, TokenKind::Comma};
  case '\0':
    return Token{tokenStartLocation, TokenKind::Eof};

  case '+':
    return Token{tokenStartLocation, TokenKind::Plus};
  case '-':
    return Token{tokenStartLocation, TokenKind::Minus};
  case '*':
    return Token{tokenStartLocation, TokenKind::Asterisk};
  case '/': {
    if (peekNextChar() != '/')
      return Token{tokenStartLocation, TokenKind::Slash};

    char c = eatNextChar();
    while (c != '\n' && c != '\0')
      c = eatNextChar();

    return getNextToken();
  }

  case '<':
    return Token{tokenStartLocation, TokenKind::Lt};
  case '>':
    return Token{tokenStartLocation, TokenKind::Gt};
  case '!':
    return Token{tokenStartLocation, TokenKind::Excl};

  case '=': {
    if (peekNextChar() != '=')
      return Token{tokenStartLocation, TokenKind::Equal};

    eatNextChar();
    return Token{tokenStartLocation, TokenKind::EqualEqual};
  }
  case '&': {
    if (peekNextChar() != '&')
      break;

    eatNextChar();
    return Token{tokenStartLocation, TokenKind::AmpAmp};
  }
  case '|': {
    if (peekNextChar() != '|')
      break;

    eatNextChar();
    return Token{tokenStartLocation, TokenKind::PipePipe};
  }
  default:
    break;
  }

  if (isAlpha(currentChar)) {
    std::string value{currentChar};

    while (isAlnum(peekNextChar()))
      value += eatNextChar();

    if (keywords.count(value))
      return Token{tokenStartLocation, keywords[value], std::move(value)};

    return Token{tokenStartLocation, TokenKind::Identifier, std::move(value)};
  }

  // [0-9]+ . [0-9]+
  if (isNum(currentChar)) {
    std::string value{currentChar};

    while (isNum(peekNextChar()))
      value += eatNextChar();

    if (peekNextChar() != '.')
      return Token{tokenStartLocation, TokenKind::Unk};

    value += eatNextChar();

    if (!isNum(peekNextChar()))
      return Token{tokenStartLocation, TokenKind::Unk};

    while (isNum(peekNextChar()))
      value += eatNextChar();

    return Token{tokenStartLocation, TokenKind::Number, value};
  }

  return Token{tokenStartLocation, TokenKind::Unk};
}
