#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_PARSER_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_PARSER_H

#include <functional>
#include <memory>
#include <unordered_set>
#include <utility>
#include <vector>

#include "ast.h"
#include "diag.h"
#include "lexer.h"

namespace yl {
class Parser {
  diag::DiagnosticReporter *reporter;
  Lexer *lexer;

  Token nextToken;
  bool incompleteAST = false;

  using RestrictionType = unsigned char;
  RestrictionType restrictions = 0;

  enum RestrictionKind : RestrictionType {
    StructNotAllowed = (1 << 0),
    FunctionWithoutBodyAllowed = (1 << 1),
    ParamWithoutTypeAllowed = (1 << 2)
  };

  template <typename T>
  T withRestrictions(RestrictionType rests, T (Parser::*f)()) {
    RestrictionType prevRestrictions = restrictions;
    restrictions |= rests;
    auto res = (this->*f)();
    restrictions = prevRestrictions;
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
  std::unique_ptr<ast::FunctionDecl> parseFunctionDecl();
  std::unique_ptr<ast::ImplDecl> parseImplDecl();
  std::unique_ptr<ast::FunctionDecl> parseFunctionSignature();
  std::unique_ptr<ast::ParamDecl> parseParamDecl();
  std::unique_ptr<ast::VarDecl> parseVarDecl(bool isLet);
  std::unique_ptr<ast::StructDecl> parseStructDecl();
  std::unique_ptr<ast::TraitDecl> parseTraitDecl();
  std::unique_ptr<ast::FieldDecl> parseFieldDecl();
  std::unique_ptr<ast::TypeParamDecl> parseTypeParamDecl();

  std::unique_ptr<ast::Stmt> parseStmt();
  std::unique_ptr<ast::IfStmt> parseIfStmt();
  std::unique_ptr<ast::WhileStmt> parseWhileStmt();
  std::unique_ptr<ast::DeclStmt> parseDeclStmt();
  std::unique_ptr<ast::ReturnStmt> parseReturnStmt();
  std::unique_ptr<ast::FieldInitStmt> parseFieldInitStmt();

  std::unique_ptr<ast::Stmt> parseAssignmentOrExpr();

  std::unique_ptr<ast::Block> parseBlock();

  std::unique_ptr<ast::Expr> parseExpr();
  std::unique_ptr<ast::Expr> parseExprRHS(std::unique_ptr<ast::Expr> lhs,
                                          int precedence);
  std::unique_ptr<ast::Expr> parsePrefixExpr();
  std::unique_ptr<ast::Expr> parsePostfixExpr();
  std::unique_ptr<ast::Expr> parsePrimary();
  std::unique_ptr<ast::PathExpr> parsePathExpr();
  std::unique_ptr<ast::DeclRefExpr> parseDeclRefExpr();
  std::unique_ptr<ast::LambdaExpr> parseLambdaExpr();
  std::unique_ptr<ast::TypeArgumentList> parseTypeArgumentList();

  std::unique_ptr<ast::Type> parseType();

  template <typename T> std::unique_ptr<T> parseIdentifierWithTypelist();
  std::unique_ptr<ast::UserDefinedType> parseUserDefinedType();
  std::unique_ptr<ast::TraitInstance> parseTraitInstance();
  std::unique_ptr<ast::ImplSpecifier> parseImplSpecifier();

  // helper methods
  template <typename T>
  std::unique_ptr<std::vector<std::unique_ptr<T>>>
  parseListWithTrailingComma(std::function<std::unique_ptr<T>(Parser &)> parser,
                             TokenKind closingToken);
  std::unique_ptr<std::vector<std::unique_ptr<ast::TraitInstance>>>
  parseTraitList();
  std::unique_ptr<std::vector<std::unique_ptr<ast::TypeParamDecl>>>
  parseTypeParamList();
  std::unique_ptr<std::vector<std::unique_ptr<ast::Type>>> parseTypeList();

public:
  Parser(diag::DiagnosticReporter &reporter, Lexer &lexer)
      : reporter(&reporter),
        lexer(&lexer),
        nextToken(lexer.getNextToken()) {}

  std::pair<ast::Context, bool> parseSourceFile();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_PARSER_H
