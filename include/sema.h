#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_SEMA_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_SEMA_H

#include <vector>

#include "ast.h"
#include "cfg.h"
#include "constexpr.h"
#include "diag.h"
#include "res.h"

namespace yl {
class Sema {
  static constexpr const char *implicitSelfId = "__Self";
  static constexpr const char *selfParamId = "self";
  static constexpr const char *selfTypeId = "Self";

  diag::DiagnosticReporter *reporter;
  ConstExprEvaluator *cee;
  const ast::Context *ast;

  res::TypeManager typeMgr;
  res::Context ctx;

  res::DeclContext *lexicalScope = nullptr;
  res::FunctionDecl *currentFunction = nullptr;
  res::Type *selfType = nullptr;

  class ScopeRAII {
    Sema *sema;
    res::DeclContext scope;

  public:
    explicit ScopeRAII(Sema *sema)
        : sema(sema),
          scope(sema->lexicalScope) {
      sema->lexicalScope = &scope;
    }
    ~ScopeRAII() { sema->lexicalScope = scope.parent; }
  };

  unsigned char resolutionContext = 0;
  enum ResolutionContextKind : unsigned char {
    ParamList = (1 << 1),
    ArgList = (1 << 2),
    Call = (1 << 3)
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
  template <typename Hint>
  res::PathExpr *resolvePathExpr(res::Context &ctx,
                                 const ast::PathExpr &pathExpr);
  template <typename Hint>
  res::DeclRefExpr *
  resolveDeclRefExpr(res::Context &ctx,
                     res::Type *parentTy,
                     const ast::DeclRefExpr *dre,
                     const ast::ImplSpecifier *impl = nullptr);
  res::DeclRefExpr *createDeclRefExpr(res::Context &ctx,
                                      const ast::DeclRefExpr *dre,
                                      res::Type *parentTy,
                                      res::Decl *decl,
                                      res::TraitType *trait);
  template <typename Hint>
  res::Decl *lookupSymbolWithFallback(res::DeclContext *scope,
                                      const ast::DeclRefExpr *dre);
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

  res::ImplDecl *resolveImplDecl(res::Context &ctx,
                                 const ast::ImplDecl &decl,
                                 res::StructDecl *parent);
  res::VarDecl *resolveVarDecl(res::Context &ctx, const ast::VarDecl &varDecl);
  res::FunctionDecl *
  resolveFunctionDecl(res::Context &ctx,
                      const ast::FunctionDecl &decl,
                      res::Decl *parent = nullptr,
                      res::FunctionDecl *implements = nullptr);
  res::FunctionDecl *resolveFunctionBody(res::Context &ctx,
                                         const ast::FunctionDecl &functionDecl,
                                         res::FunctionDecl *function);

  res::TraitInstance *resolveTraitInstance(res::Context &ctx,
                                           const ast::TraitInstance *trait);
  res::TraitDecl *resolveTraitDecl(res::Context &ctx,
                                   const ast::TraitDecl &decl);
  bool resolveTraitBody(res::Context &ctx,
                        res::TraitDecl &traitDecl,
                        const ast::TraitDecl &astDecl);
  bool resolveTraitFunctionBodies(res::Context &ctx,
                                  res::TraitDecl &traitDecl,
                                  const ast::TraitDecl &astDecl);

  res::StructDecl *resolveStructDecl(res::Context &ctx,
                                     const ast::StructDecl &decl);
  bool resolveStructBody(res::Context &ctx,
                         res::StructDecl &structDecl,
                         const ast::StructDecl &astDecl);
  bool resolveMemberFunctionBodies(res::Context &ctx,
                                   res::StructDecl &decl,
                                   const ast::StructDecl &astDecl);

  bool checkTypeParameterCount(SourceLocation loc,
                               size_t received,
                               size_t expected) const;

  std::vector<res::TypeParamDecl *> resolveTypeParamsWithoutBounds(
      res::Context &ctx,
      const std::vector<std::unique_ptr<ast::TypeParamDecl>> &typeParamDecls);
  bool resolveGenericParamsInCurrentScope(
      res::Context &ctx,
      const std::vector<res::TypeParamDecl *> &resParams,
      const std::vector<std::unique_ptr<ast::TypeParamDecl>> &astParams);

  std::vector<res::TraitInstance *> resolveTraitInstanceList(
      res::Context &ctx,
      const std::vector<std::unique_ptr<ast::TraitInstance>> &traitInstances);
  bool hasConflictingTraits(res::Context &ctx, std::vector<res::TraitType *>);
  bool implementsAllNecessaryTraitFunctions(res::Context &ctx,
                                            res::StructDecl *structDecl);

  bool insertDeclToScope(res::Decl *decl, res::DeclContext *scope);
  res::FunctionDecl *createBuiltinPrintln(res::Context &ctx);

  bool runFlowSensitiveChecks(res::Context &ctx, const res::FunctionDecl &fn);
  bool checkReturnOnAllPaths(res::Context &ctx,
                             const res::FunctionDecl &fn,
                             const CFG &cfg);
  bool checkVariableInitialization(const res::Context &ctx, const CFG &cfg);

  bool hasBuiltinFunctionCollisions(const res::FunctionDecl *fn);
  bool checkSelfParameter(res::ParamDecl *param, size_t idx);
  bool hasSelfContainingStructs(const res::Context &ctx);
  bool checkTraitInstances(res::Context &ctx);

public:
  explicit Sema(diag::DiagnosticReporter &reporter,
                ConstExprEvaluator &cee,
                const ast::Context &ast)
      : reporter(&reporter),
        cee(&cee),
        ast(&ast) {}

  res::Context *resolveAST();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_SEMA_H
