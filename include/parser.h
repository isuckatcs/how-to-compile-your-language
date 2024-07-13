#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_PARSER_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_PARSER_H

#include <memory>
#include <optional>
#include <utility>
#include <vector>

#include "ast.h"
#include "lexer.h"

namespace yl {
class Parser {
  Lexer *lexer;
  Token nextToken;
  bool incompleteAST = false;

  void eatNextToken() { nextToken = lexer->getNextToken(); }
  void synchronizeOn(TokenKind kind) {
    incompleteAST = true;

    while (nextToken.kind != kind && nextToken.kind != TokenKind::Eof)
      eatNextToken();
  }

  // AST node parser methods
  std::unique_ptr<FunctionDecl> parseFunctionDecl();
  std::unique_ptr<Block> parseBlock();
  std::optional<Type> parseType();

public:
  explicit Parser(Lexer &lexer)
      : lexer(&lexer), nextToken(lexer.getNextToken()) {}

  std::pair<std::vector<std::unique_ptr<FunctionDecl>>, bool> parseSourceFile();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_PARSER_H