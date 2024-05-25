#ifndef A_COMPILER_PARSER_H
#define A_COMPILER_PARSER_H

#include <memory>
#include <vector>

#include "ast.h"
#include "lexer.h"

class TheParser {
  TheLexer *lexer;
  Token nextToken;

  std::nullptr_t error(SourceLocation location, std::string_view message);
  void eatNextToken() { nextToken = lexer->getNextToken(); }
  bool skipUntil(TokenKind kind);

  std::unique_ptr<Expr> parseIdentifierExpr();
  std::unique_ptr<Expr> parseExpr();
  std::unique_ptr<Stmt> parseStmt();

  std::unique_ptr<FunctionDecl> parseFunctionDecl();
  std::unique_ptr<ParamDecl> parseParamDecl();

  std::unique_ptr<Block> parseBlock();

public:
  explicit TheParser(TheLexer &lexer)
      : lexer(&lexer), nextToken(lexer.getNextToken()) {}

  std::vector<std::unique_ptr<FunctionDecl>> parseTopLevel();
};

#endif // A_COMPILER_PARSER_H
