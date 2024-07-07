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
  void synchronize();
  void synchronizeOn(TokenKind kind) {
    incompleteAST = true;

    while (nextToken.kind != kind && nextToken.kind != TokenKind::Eof)
      eatNextToken();
  }

  // AST node parser methods
  std::unique_ptr<FunctionDecl> parseFunctionDecl();
  std::unique_ptr<ParamDecl> parseParamDecl();
  std::unique_ptr<VarDecl> parseVarDecl(bool isLet);

  std::unique_ptr<Stmt> parseStmt();
  std::unique_ptr<IfStmt> parseIfStmt();
  std::unique_ptr<WhileStmt> parseWhileStmt();
  std::unique_ptr<Assignment>
  parseAssignmentRHS(std::unique_ptr<DeclRefExpr> lhs);
  std::unique_ptr<DeclStmt> parseDeclStmt();
  std::unique_ptr<ReturnStmt> parseReturnStmt();

  std::unique_ptr<Block> parseBlock();

  std::unique_ptr<Expr> parseExpr();
  std::unique_ptr<Expr> parseExprRHS(std::unique_ptr<Expr> lhs, int precedence);
  std::unique_ptr<Expr> parsePrefixExpr();
  std::unique_ptr<Expr> parsePrimary();

  // helper methods
  using ParameterList = std::vector<std::unique_ptr<ParamDecl>>;
  std::optional<ParameterList> parseParameterList();

  using ArgumentList = std::vector<std::unique_ptr<Expr>>;
  std::optional<ArgumentList> parseArgumentList();

  std::optional<Type> parseType();

public:
  explicit Parser(Lexer &lexer)
      : lexer(&lexer), nextToken(lexer.getNextToken()) {}

  std::pair<std::vector<std::unique_ptr<FunctionDecl>>, bool> parseSourceFile();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_PARSER_H
