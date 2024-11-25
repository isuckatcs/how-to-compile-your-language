#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_SEMA_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_SEMA_H

#include <memory>
#include <optional>
#include <vector>

#include "ast.h"
#include "cfg.h"
#include "constexpr.h"

namespace yl {
class Sema {
  ConstantExpressionEvaluator cee;
  std::vector<std::unique_ptr<Decl>> ast;
  std::vector<std::vector<ResolvedDecl *>> scopes;

  ResolvedFunctionDecl *currentFunction;

  class ScopeRAII {
    Sema *sema;

  public:
    explicit ScopeRAII(Sema *sema)
        : sema(sema) {
      sema->scopes.emplace_back();
    }
    ~ScopeRAII() { sema->scopes.pop_back(); }
  };

  std::optional<Type> resolveType(Type parsedType);

  std::unique_ptr<ResolvedUnaryOperator>
  resolveUnaryOperator(const UnaryOperator &unary);
  std::unique_ptr<ResolvedBinaryOperator>
  resolveBinaryOperator(const BinaryOperator &binop);
  std::unique_ptr<ResolvedGroupingExpr>
  resolveGroupingExpr(const GroupingExpr &grouping);
  std::unique_ptr<ResolvedDeclRefExpr>
  resolveDeclRefExpr(const DeclRefExpr &declRefExpr, bool isCallee = false);
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
  resolveFunctionDeclaration(const FunctionDecl &function);

  bool insertDeclToCurrentScope(ResolvedDecl &decl);
  std::pair<ResolvedDecl *, int> lookupDecl(const std::string id);
  std::unique_ptr<ResolvedFunctionDecl> createBuiltinPrintln();

  bool runFlowSensitiveChecks(const ResolvedFunctionDecl &fn);
  bool checkReturnOnAllPaths(const ResolvedFunctionDecl &fn, const CFG &cfg);
  bool checkVariableInitialization(const CFG &cfg);

public:
  explicit Sema(std::vector<std::unique_ptr<Decl>> ast)
      : ast(std::move(ast)) {}

  std::vector<std::unique_ptr<ResolvedFunctionDecl>> resolveAST();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_SEMA_H
