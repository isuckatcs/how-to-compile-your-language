#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_PARSER_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_PARSER_H

#include <memory>
#include <optional>
#include <unordered_set>
#include <utility>
#include <vector>

#include "ast.h"
#include "lexer.h"

namespace yl {
class Parser {
  Lexer *lexer;
  Token nextToken;
  bool incompleteAST = false;

  using RestrictionType = unsigned char;
  RestrictionType restrictions = 0;

  enum RestrictionKind : RestrictionType { StructNotAllowed = 1 };

  template <typename T>
  T withRestrictions(RestrictionType rests, T (Parser::*f)()) {
    restrictions |= rests;
    auto res = (this->*f)();
    restrictions &= ~rests;
    return res;
  }

  template <typename T> T withNoRestrictions(T (Parser::*f)()) {
    RestrictionType prevRestrictions = restrictions;
    restrictions = 0;
    auto res = (this->*f)();
    restrictions = prevRestrictions;
    return res;
  }

  void eatNextToken() { nextToken = lexer->getNextToken(); }
  void synchronize();
  void synchronizeOn(const std::unordered_set<TokenKind> &kind) {
    incompleteAST = true;

    while (!kind.count(nextToken.kind) && nextToken.kind != TokenKind::Eof)
      eatNextToken();
  }

  // AST node parser methods
  std::unique_ptr<FunctionDecl> parseFunctionDecl();
  std::unique_ptr<ParamDecl> parseParamDecl();
  std::unique_ptr<VarDecl> parseVarDecl(bool isLet);
  std::unique_ptr<StructDecl> parseStructDecl();
  std::unique_ptr<FieldDecl> parseFieldDecl();

  std::unique_ptr<Stmt> parseStmt();
  std::unique_ptr<IfStmt> parseIfStmt();
  std::unique_ptr<WhileStmt> parseWhileStmt();
  std::unique_ptr<Assignment>
  parseAssignmentRHS(std::unique_ptr<AssignableExpr> lhs);
  std::unique_ptr<DeclStmt> parseDeclStmt();
  std::unique_ptr<ReturnStmt> parseReturnStmt();
  std::unique_ptr<FieldInitStmt> parseFieldInitStmt();

  std::unique_ptr<Stmt> parseAssignmentOrExpr();

  std::unique_ptr<Block> parseBlock();

  std::unique_ptr<Expr> parseExpr();
  std::unique_ptr<Expr> parseExprRHS(std::unique_ptr<Expr> lhs, int precedence);
  std::unique_ptr<Expr> parsePrefixExpr();
  std::unique_ptr<Expr> parsePostfixExpr();
  std::unique_ptr<Expr> parsePrimary();

  // helper methods
  template <typename T, typename F>
  std::unique_ptr<std::vector<std::unique_ptr<T>>>
  parseListWithTrailingComma(std::pair<TokenKind, const char *> openingToken,
                             F parser,
                             std::pair<TokenKind, const char *> closingToken);

  std::optional<Type> parseType();

public:
  explicit Parser(Lexer &lexer)
      : lexer(&lexer),
        nextToken(lexer.getNextToken()) {}

  std::pair<std::vector<std::unique_ptr<Decl>>, bool> parseSourceFile();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_PARSER_H
