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
  std::unique_ptr<MemberDecl> parseMemberDecl();

  std::unique_ptr<Stmt> parseStmt();
  std::unique_ptr<IfStmt> parseIfStmt();
  std::unique_ptr<WhileStmt> parseWhileStmt();
  std::unique_ptr<Assignment>
  parseAssignmentRHS(std::unique_ptr<DeclRefExpr> lhs);
  std::unique_ptr<DeclStmt> parseDeclStmt();
  std::unique_ptr<ReturnStmt> parseReturnStmt();
  std::unique_ptr<MemberInitStmt> parseMemberInitStmt();

  std::unique_ptr<Stmt> parseAssignmentOrExpr();

  std::unique_ptr<Block> parseBlock();

  std::unique_ptr<Expr> parseExpr();
  std::unique_ptr<Expr> parseExprRHS(std::unique_ptr<Expr> lhs, int precedence);
  std::unique_ptr<Expr> parsePrefixExpr();
  std::unique_ptr<Expr> parsePostfixExpr();
  std::unique_ptr<Expr> parsePrimary();

  // helper methods
  using ParameterList = std::vector<std::unique_ptr<ParamDecl>>;
  std::unique_ptr<ParameterList> parseParameterList();

  using ArgumentList = std::vector<std::unique_ptr<Expr>>;
  std::unique_ptr<ArgumentList> parseArgumentList();

  using MemberList = std::vector<std::unique_ptr<MemberDecl>>;
  std::unique_ptr<MemberList> parseMemberList();

  using MemberInitList = std::vector<std::unique_ptr<MemberInitStmt>>;
  std::unique_ptr<MemberInitList> parseMemberInitList();

  std::optional<Type> parseType();

public:
  explicit Parser(Lexer &lexer)
      : lexer(&lexer),
        nextToken(lexer.getNextToken()) {}

  std::pair<std::vector<std::unique_ptr<Decl>>, bool> parseSourceFile();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_PARSER_H
