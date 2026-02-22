#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_SEMA_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_SEMA_H

#include <vector>

#include "ast.h"
#include "cfg.h"
#include "constexpr.h"
#include "res.h"

namespace yl {
class Sema {
  ConstantExpressionEvaluator *cee;
  const ast::Context *ast;

  res::Context ctx;
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

  unsigned char resolutionContext = 0;
  enum ResolutionContextKind : unsigned char {
    ParamList = (1 << 1),
    ArgList = (1 << 2)
  };

  class ResolutionContextRAII {
    Sema *sema;
    unsigned char previousContext;

  public:
    explicit ResolutionContextRAII(Sema *sema, ResolutionContextKind kind)
        : sema(sema),
          previousContext(sema->resolutionContext) {
      sema->resolutionContext |= kind;
    }
    ~ResolutionContextRAII() { sema->resolutionContext = previousContext; }
  };

  res::Type *resolveType(res::Context &ctx, const ast::Type &parsedType);

  res::UnaryOperator *resolveUnaryOperator(res::Context &ctx,
                                           const ast::UnaryOperator &unary);
  res::BinaryOperator *resolveBinaryOperator(res::Context &ctx,
                                             const ast::BinaryOperator &binop);
  res::GroupingExpr *resolveGroupingExpr(res::Context &ctx,
                                         const ast::GroupingExpr &grouping);
  res::DeclRefExpr *resolveDeclRefExpr(res::Context &ctx,
                                       const ast::DeclRefExpr &declRefExpr);
  res::CallExpr *resolveCallExpr(res::Context &ctx, const ast::CallExpr &call);
  res::StructInstantiationExpr *resolveStructInstantiation(
      res::Context &ctx,
      const ast::StructInstantiationExpr &structInstantiation);
  res::MemberExpr *resolveMemberExpr(res::Context &ctx,
                                     const ast::MemberExpr &memberExpr);
  res::Expr *resolveExpr(res::Context &ctx, const ast::Expr &expr);

  res::Stmt *resolveStmt(res::Context &ctx, const ast::Stmt &stmt);
  res::IfStmt *resolveIfStmt(res::Context &ctx, const ast::IfStmt &ifStmt);
  res::WhileStmt *resolveWhileStmt(res::Context &ctx,
                                   const ast::WhileStmt &whileStmt);
  res::DeclStmt *resolveDeclStmt(res::Context &ctx,
                                 const ast::DeclStmt &declStmt);
  res::Assignment *resolveAssignment(res::Context &ctx,
                                     const ast::Assignment &assignment);
  res::ReturnStmt *resolveReturnStmt(res::Context &ctx,
                                     const ast::ReturnStmt &returnStmt);

  res::Block *resolveBlock(res::Context &ctx, const ast::Block &block);

  res::VarDecl *resolveVarDecl(res::Context &ctx, const ast::VarDecl &varDecl);
  res::FunctionDecl *resolveFunctionDecl(res::Context &ctx,
                                         const ast::FunctionDecl &function);
  res::StructDecl *resolveStructDecl(res::Context &ctx,
                                     const ast::StructDecl &structDecl);
  bool resolveStructFields(res::Context &ctx,
                           res::StructDecl &decl,
                           const ast::StructDecl &astDecl);

  bool checkTypeParameterCount(SourceLocation loc,
                               size_t received,
                               size_t expected) const;
  std::vector<res::TypeParamDecl *> resolveTypeParameters(
      res::Context &ctx,
      const std::vector<std::unique_ptr<ast::TypeParamDecl>> &typeParamDecls);

  bool insertDeclToCurrentScope(res::Decl *decl);
  template <typename T> std::pair<T *, int> lookupDecl(const std::string id);
  res::FunctionDecl *createBuiltinPrintln(res::Context &ctx);

  bool runFlowSensitiveChecks(res::Context &ctx, const res::FunctionDecl &fn);
  bool checkReturnOnAllPaths(res::Context &ctx,
                             const res::FunctionDecl &fn,
                             const CFG &cfg);
  bool checkVariableInitialization(const res::Context &ctx, const CFG &cfg);
  bool hasSelfContainingStructs(const res::Context &ctx);

public:
  explicit Sema(ConstantExpressionEvaluator &cee, const ast::Context &ast)
      : cee(&cee),
        ast(&ast) {}

  res::Context *resolveAST();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_SEMA_H
