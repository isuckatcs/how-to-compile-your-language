#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_SEMA_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_SEMA_H

#include <memory>
#include <optional>
#include <vector>

#include "ast.h"
#include "cfg.h"
#include "constexpr.h"
#include "res.h"

namespace yl {
class Sema {
  ConstantExpressionEvaluator cee;
  std::vector<std::unique_ptr<ast::Decl>> ast;
  std::vector<std::vector<res::Decl *>> scopes;

  res::FunctionDecl *currentFunction;

  class ScopeRAII {
    Sema *sema;

  public:
    explicit ScopeRAII(Sema *sema)
        : sema(sema) {
      sema->scopes.emplace_back();
    }
    ~ScopeRAII() { sema->scopes.pop_back(); }
  };

  std::optional<res::Type> resolveType(ast::Type parsedType);

  std::unique_ptr<res::UnaryOperator>
  resolveUnaryOperator(const ast::UnaryOperator &unary);
  std::unique_ptr<res::BinaryOperator>
  resolveBinaryOperator(const ast::BinaryOperator &binop);
  std::unique_ptr<res::GroupingExpr>
  resolveGroupingExpr(const ast::GroupingExpr &grouping);
  std::unique_ptr<res::DeclRefExpr>
  resolveDeclRefExpr(const ast::DeclRefExpr &declRefExpr,
                     bool isCallee = false);
  std::unique_ptr<res::CallExpr> resolveCallExpr(const ast::CallExpr &call);
  std::unique_ptr<res::StructInstantiationExpr> resolveStructInstantiation(
      const ast::StructInstantiationExpr &structInstantiation);
  std::unique_ptr<res::MemberExpr>
  resolveMemberExpr(const ast::MemberExpr &memberExpr);
  std::unique_ptr<res::AssignableExpr>
  resolveAssignableExpr(const ast::AssignableExpr &assignableExpr);
  std::unique_ptr<res::Expr> resolveExpr(const ast::Expr &expr);

  std::unique_ptr<res::Stmt> resolveStmt(const ast::Stmt &stmt);
  std::unique_ptr<res::IfStmt> resolveIfStmt(const ast::IfStmt &ifStmt);
  std::unique_ptr<res::WhileStmt>
  resolveWhileStmt(const ast::WhileStmt &whileStmt);
  std::unique_ptr<res::DeclStmt> resolveDeclStmt(const ast::DeclStmt &declStmt);
  std::unique_ptr<res::Assignment>
  resolveAssignment(const ast::Assignment &assignment);
  std::unique_ptr<res::ReturnStmt>
  resolveReturnStmt(const ast::ReturnStmt &returnStmt);

  std::unique_ptr<res::Block> resolveBlock(const ast::Block &block);

  std::unique_ptr<res::ParamDecl> resolveParamDecl(const ast::ParamDecl &param);
  std::unique_ptr<res::VarDecl> resolveVarDecl(const ast::VarDecl &varDecl);
  std::unique_ptr<res::FunctionDecl>
  resolveFunctionDecl(const ast::FunctionDecl &function);
  std::unique_ptr<res::StructDecl>
  resolveStructDecl(const ast::StructDecl &structDecl);

  bool resolveStructFields(res::StructDecl &resolvedStructDecl);

  bool insertDeclToCurrentScope(res::Decl &decl);
  template <typename T> std::pair<T *, int> lookupDecl(const std::string id);
  std::unique_ptr<res::FunctionDecl> createBuiltinPrintln();

  bool runFlowSensitiveChecks(const res::FunctionDecl &fn);
  bool checkReturnOnAllPaths(const res::FunctionDecl &fn, const CFG &cfg);
  bool checkVariableInitialization(const CFG &cfg);

public:
  explicit Sema(std::vector<std::unique_ptr<ast::Decl>> ast)
      : ast(std::move(ast)) {}

  std::vector<std::unique_ptr<res::Decl>> resolveAST();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_SEMA_H
