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
    {"void", TokenKind::kw_void},     {"fn", TokenKind::kw_fn},
    {"number", TokenKind::kw_number}, {"if", TokenKind::kw_if},
    {"else", TokenKind::kw_else},     {"let", TokenKind::kw_let},
    {"var", TokenKind::kw_var}};

} // namespace

Token TheLexer::getNextToken() {
  char currentChar = eatNextChar();

  while (isSpace(currentChar))
    currentChar = eatNextChar();

  SourceLocation tokenStartLocation = getSourceLocation();

  // FIXME: refactor this using switch
  if (currentChar == '(')
    return Token{tokenStartLocation, TokenKind::lpar};
  if (currentChar == ')')
    return Token{tokenStartLocation, TokenKind::rpar};
  if (currentChar == '{')
    return Token{tokenStartLocation, TokenKind::lbrace};
  if (currentChar == '}')
    return Token{tokenStartLocation, TokenKind::rbrace};
  if (currentChar == ':')
    return Token{tokenStartLocation, TokenKind::colon};
  if (currentChar == ';')
    return Token{tokenStartLocation, TokenKind::semi};
  if (currentChar == ',')
    return Token{tokenStartLocation, TokenKind::comma};
  if (currentChar == '\0')
    return Token{tokenStartLocation, TokenKind::eof};

  if (currentChar == '+')
    return Token{tokenStartLocation, TokenKind::plus};
  if (currentChar == '-')
    return Token{tokenStartLocation, TokenKind::minus};
  if (currentChar == '*')
    return Token{tokenStartLocation, TokenKind::asterisk};
  if (currentChar == '/')
    return Token{tokenStartLocation, TokenKind::slash};

  if (currentChar == '<')
    return Token{tokenStartLocation, TokenKind::lt};
  if (currentChar == '>')
    return Token{tokenStartLocation, TokenKind::gt};
  if (currentChar == '!')
    return Token{tokenStartLocation, TokenKind::excl};

  if (currentChar == '=') {
    if (peekNextChar() == '=') {
      eatNextChar();
      return Token{tokenStartLocation, TokenKind::equalequal};
    }

    return Token{tokenStartLocation, TokenKind::equal};
  }

  if (currentChar == '&' && peekNextChar() == '&') {
    eatNextChar();
    return Token{tokenStartLocation, TokenKind::ampamp};
  }
  if (currentChar == '|' && peekNextChar() == '|') {
    eatNextChar();
    return Token{tokenStartLocation, TokenKind::pipepipe};
  }

  if (isAlpha(currentChar)) {
    std::string value{currentChar};

    while (isAlnum(peekNextChar()))
      value += eatNextChar();

    if (keywords.count(value))
      return Token{tokenStartLocation, keywords[value], std::move(value)};

    return Token{tokenStartLocation, TokenKind::identifier, std::move(value)};
  }

  // [0-9]+ . [0-9]+
  if (isNum(currentChar)) {
    std::string value{currentChar};

    while (isNum(peekNextChar()))
      value += eatNextChar();

    if (peekNextChar() != '.')
      return Token{tokenStartLocation, TokenKind::unk};

    value += eatNextChar();

    if (!isNum(peekNextChar()))
      return Token{tokenStartLocation, TokenKind::unk};

    while (isNum(peekNextChar()))
      value += eatNextChar();

    return Token{tokenStartLocation, TokenKind::number, value};
  }

  return Token{tokenStartLocation, TokenKind::unk};
}
