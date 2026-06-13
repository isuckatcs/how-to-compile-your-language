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
  static constexpr const char *lambdaFunctionId = "__builtin_lambda_call";

  diag::DiagnosticReporter *reporter;
  ConstExprEvaluator *cee;
  const ast::Context *ast;

  res::TypeManager typeMgr;
  res::Context ctx;

  class EnterScopeRAII {
    Sema *sema;
    res::DeclContext scope;

  public:
    explicit EnterScopeRAII(Sema *sema)
        : sema(sema),
          scope(sema->lexicalScope) {
      sema->lexicalScope = &scope;
    }
    ~EnterScopeRAII() { sema->lexicalScope = scope.parent; }
  };

  res::DeclContext *lexicalScope = nullptr;

  class WithSelfTypeRAII {
    Sema *sema;
    res::Type *oldSelfType;

  public:
    WithSelfTypeRAII(Sema *sema, res::Type *selfTy)
        : sema(sema),
          oldSelfType(sema->selfType) {
      sema->selfType = selfTy;
    }
    ~WithSelfTypeRAII() { sema->selfType = oldSelfType; }
  };

  res::Type *selfType = nullptr;

  enum Modifiers : unsigned char {
    UnaryAmpAllowed = 1 << 0,
    IsCallee = 1 << 1,
    MissingTypeAnnotationsAllowed = 1 << 2,
  };

  class WithModifiersRAII {
    Sema *sema;
    unsigned char oldModifiers;

  public:
    WithModifiersRAII(Sema *sema, unsigned char modifiers)
        : sema(sema),
          oldModifiers(sema->modifiers) {
      sema->modifiers |= modifiers;
    }
    ~WithModifiersRAII() { sema->modifiers = oldModifiers; }
  };

  unsigned char modifiers = 0;

  struct PendingLambdaDescriptor {
    res::LambdaExpr *lambda;
    const ast::LambdaExpr *astLambda;
    res::DeclContext snapshot;
  };

  struct FunctionInfo {
    res::FunctionDecl *function = nullptr;
    res::LambdaExpr *lambda = nullptr;
    res::DeclContext *lambdaParamScope = nullptr;
    std::vector<res::DeclRefExpr *> declReferences = {};
    std::vector<const ast::Expr *> pendingCaptureInits = {};
  };

  class WithFunctionInfoRAII {
    FunctionInfo functionInfo;
    Sema *sema;
    FunctionInfo *oldFunctionInfo;

  public:
    WithFunctionInfoRAII(Sema *sema, FunctionInfo functionInfo)
        : functionInfo(std::move(functionInfo)),
          sema(sema),
          oldFunctionInfo(sema->functionInfo) {
      sema->functionInfo = &this->functionInfo;
    }
    ~WithFunctionInfoRAII() { sema->functionInfo = oldFunctionInfo; }
  };

  FunctionInfo *functionInfo;

  res::Type *resolveType(res::Context &ctx,
                         const ast::Type &parsedType,
                         bool isPointee = false);

  res::UnaryOperator *resolveUnaryOperator(res::Context &ctx,
                                           const ast::UnaryOperator &unary);
  res::BinaryOperator *resolveBinaryOperator(res::Context &ctx,
                                             const ast::BinaryOperator &binop);
  res::GroupingExpr *resolveGroupingExpr(res::Context &ctx,
                                         const ast::GroupingExpr &grouping);
  template <typename Hint>
  res::DeclRefExpr *resolvePathExpr(res::Context &ctx,
                                    const ast::PathExpr &pathExpr);
  template <typename Hint>
  res::DeclRefExpr *resolveDeclRefExpr(res::Context &ctx,
                                       const ast::DeclRefExpr *dre,
                                       res::Type *in = nullptr,
                                       res::TraitInstance *traitHelp = nullptr);
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
  res::Expr *resolveExpr(res::Context &ctx,
                         const ast::Expr &expr,
                         res::Type *typeHint = nullptr);
  res::LambdaExpr *resolveLambdaExpr(res::Context &ctx,
                                     const ast::LambdaExpr &lambda,
                                     res::Type *typeHint = nullptr);

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

  res::ImplBlock *resolveImplBlock(res::Context &ctx,
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
  std::pair<res::ParamDecl *, bool>
  resolveParamDecl(res::Context &ctx, const ast::ParamDecl *param);

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

  res::Expr *coerceIfNeeded(res::Type *targetType, res::Expr *expr);

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
  res::FunctionDecl *createBuiltinGC(res::Context &ctx);
  res::FunctionDecl *createBuiltinGCMut(res::Context &ctx);
  res::FunctionDecl *createBuiltinGCCollect(res::Context &ctx);

  bool hasBuiltinFunctionCollisions(const res::FunctionDecl *fn);
  bool checkSelfParameter(res::ParamDecl *param, size_t idx);
  bool hasSelfContainingStructs(res::Context &ctx);
  bool checkTraitInstances(res::Context &ctx);
  bool checkTraitInstance(res::TraitInstance *traitInstance);
  bool isTraitVtableCompatible(res::TraitType *trait);

  // Post-body checks
  bool runPostFunctionBodyChecks();
  bool checkDeclRefTypes();
  bool checkReturnOnAllPaths(const CFG &cfg);
  bool checkVariableInitialization(const CFG &cfg);

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
