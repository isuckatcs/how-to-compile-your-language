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
  std::unique_ptr<VarDecl> parseVarDecl(bool isLet);

  std::unique_ptr<Stmt> parseStmt();
  std::unique_ptr<IfStmt> parseIfStmt();
  std::unique_ptr<WhileStmt> parseWhileStmt();
  std::unique_ptr<Assignment>
  parseAssignmentRHS(std::unique_ptr<DeclRefExpr> lhs);
  std::unique_ptr<DeclStmt> parseDeclStmt();

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

  std::optional<std::string> parseType();

public:
  explicit TheParser(TheLexer &lexer)
      : lexer(&lexer), nextToken(lexer.getNextToken()) {}

  std::vector<std::unique_ptr<FunctionDecl>> parseSourceFile();
};

#endif // A_COMPILER_PARSER_H
