#ifndef A_COMPILER_SEMA_H
#define A_COMPILER_SEMA_H

#include <memory>
#include <optional>
#include <vector>

#include "ast.h"
#include "constexpr.h"

class Sema {
  ConstantExpressionEvaluator cee;
  std::vector<std::unique_ptr<FunctionDecl>> sourceFile;
  std::vector<std::vector<ResolvedDecl *>> scopes;

  ResolvedFunctionDecl *currentFunction;

  class ScopeRAII {
    Sema *sema;

  public:
    explicit ScopeRAII(Sema *sema) : sema(sema) { sema->scopes.emplace_back(); }
    ~ScopeRAII() { sema->scopes.pop_back(); }
  };

  std::optional<Type> resolveType(const std::string &typeSpecifier);

  std::unique_ptr<ResolvedUnaryOperator>
  resolveUnaryOperator(const UnaryOperator &unary);
  std::unique_ptr<ResolvedBinaryOperator>
  resolveBinaryOperator(const BinaryOperator &binop);
  std::unique_ptr<ResolvedGroupingExpr>
  resolveGroupingExpr(const GroupingExpr &grouping);
  std::unique_ptr<ResolvedDeclRefExpr>
  resolveDeclRefExpr(const DeclRefExpr &declRefExpr, bool inCall = false);
  std::unique_ptr<ResolvedCallExpr> resolveCallExpr(const CallExpr &call);
  std::unique_ptr<ResolvedExpr> resolveExpr(const Expr &expr);

  std::unique_ptr<ResolvedStmt> resolveStmt(const Stmt &stmt);
  std::unique_ptr<ResolvedIfStmt> resolveIfStmt(const IfStmt &ifStmt);
  std::unique_ptr<ResolvedWhileStmt>
  resolveWhileStmt(const WhileStmt &whileStmt);
  std::unique_ptr<ResolvedDeclStmt> resolveDeclStmt(const DeclStmt &declStmt);
  std::unique_ptr<ResolvedAssignment>
  resolveAssignment(const Assignment &assignment);
  std::unique_ptr<ResolvedReturnStmt>
  resolveReturnStmt(const ReturnStmt &returnStmt);

  std::unique_ptr<ResolvedBlock> resolveBlock(const Block &block);

  std::unique_ptr<ResolvedParamDecl> resolveParamDecl(const ParamDecl &param);
  std::unique_ptr<ResolvedVarDecl> resolveVarDecl(const VarDecl &varDecl);
  std::unique_ptr<ResolvedFunctionDecl>
  resolveFunctionWithoutBody(const FunctionDecl &function);

  bool insertDeclToCurrentScope(ResolvedDecl &decl);
  std::pair<ResolvedDecl *, int> lookupDecl(const std::string id);
  std::unique_ptr<ResolvedFunctionDecl> createBuiltinPrint();

public:
  explicit Sema(std::vector<std::unique_ptr<FunctionDecl>> sourceFile)
      : sourceFile(std::move(sourceFile)) {}

  std::vector<std::unique_ptr<ResolvedFunctionDecl>> resolveSourceFile();
};

#endif // A_COMPILER_SEMA_H
