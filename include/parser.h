#ifndef A_COMPILER_PARSER_H
#define A_COMPILER_PARSER_H

#include <memory>
#include <optional>
#include <vector>

#include "ast.h"
#include "lexer.h"

class TheParser {
  TheLexer *lexer;
  Token nextToken;

  void eatNextToken() { nextToken = lexer->getNextToken(); }

  // AST node parser methods
  std::unique_ptr<FunctionDecl> parseFunctionDecl();
  std::unique_ptr<ParamDecl> parseParamDecl();

  std::unique_ptr<Block> parseBlock();
  std::unique_ptr<Expr> parseExpr();
  std::unique_ptr<Expr> parsePrimary();

  // helper methods
  using ParameterList = std::vector<std::unique_ptr<ParamDecl>>;
  std::optional<ParameterList> parseParameterList();

  using ArgumentList = std::vector<std::unique_ptr<Expr>>;
  std::optional<ArgumentList> parseArgumentList();

  std::optional<std::string> parseType();

public:
  explicit TheParser(TheLexer &lexer)
      : lexer(&lexer), nextToken(lexer.getNextToken()) {}

  std::vector<std::unique_ptr<FunctionDecl>> parseSourceFile();
};

#endif // A_COMPILER_PARSER_H
