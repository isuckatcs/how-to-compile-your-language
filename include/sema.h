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
  static constexpr const char *selfParamId = "self";
  static constexpr const char *selfTypeId = "Self";
  static constexpr const char *lambdaFunctionId = "__builtin_lambda_call";

  diag::DiagnosticReporter *reporter;
  ConstExprEvaluator *cee;
  const ast::Context *ast;

  res::TypeManager typeMgr;
  res::Context ctx;

  class Scope {
    Scope *parent;
    res::GenericDeclContext *ctx;
    std::vector<res::Decl *> decls;

  public:
    Scope(Scope *parent, res::GenericDeclContext *ctx)
        : parent(parent),
          ctx(ctx) {}

    void addDecl(res::Decl *decl) { decls.emplace_back(decl); }
    std::vector<res::NamedDecl *> lookupSymbol(const std::string &id,
                                               bool recursive = true) const;

    Scope *getParent() const { return parent; }
    res::GenericDeclContext *getDeclContext() const;
    res::Type *getSelfType() const;
  };

  class EnterNewScopeRAII {
    Sema *sema;
    Scope scope;

  public:
    explicit EnterNewScopeRAII(Sema *sema,
                               res::GenericDeclContext *ctx = nullptr)
        : sema(sema),
          scope(sema->scope, ctx) {
      sema->scope = &scope;
    }

    ~EnterNewScopeRAII() { sema->scope = scope.getParent(); }
  };

  Scope *scope = nullptr;

  enum Modifiers : unsigned char {
    IsCallee = 1 << 0,
    AddressTaken = 1 << 1,
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

  struct FunctionInfo {
    res::FunctionDecl *function = nullptr;
    res::LambdaExpr *lambda = nullptr;
    Scope *lambdaParamScope = nullptr;
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

  bool shouldDelayUserDefinedTypeChecking = true;
  std::unordered_map<const ast::UserDefinedType *, res::Type *>
      delayedTypeChecks;

  res::TypeDecl *resolveTypeSymbol(const ast::UserDefinedType *udt);
  res::Type *resolveType(res::Context &ctx,
                         const ast::Type &parsedType,
                         bool allowTraitObject = false,
                         bool expectTrait = false,
                         res::Type *traitSelfType = nullptr);

  res::UnaryOperator *resolveUnaryOperator(res::Context &ctx,
                                           const ast::UnaryOperator &unary);
  res::BinaryOperator *resolveBinaryOperator(res::Context &ctx,
                                             const ast::BinaryOperator &binop);
  res::GroupingExpr *resolveGroupingExpr(res::Context &ctx,
                                         const ast::GroupingExpr &grouping);

  template <typename ExpectedDecl>
  res::DeclRefExpr *resolvePathExpr(res::Context &ctx,
                                    const ast::PathExpr &pathExpr);
  res::DeclRefExpr *createDeclRefExpr(res::Context &ctx,
                                      const ast::DeclRefExpr *dre,
                                      res::NamedDecl *decl,
                                      res::Substitution sub = {});

  std::vector<std::pair<res::NamedDecl *, res::Substitution>>
  lookupAssociatedDecls(std::string identifier,
                        res::Type *type,
                        res::TraitType *trait = nullptr);

  std::pair<res::Expr *, std::vector<res::Expr *>>
  resolveCallBase(res::Context &ctx, const ast::CallExpr &call);
  res::CallExpr *resolveCallExpr(res::Context &ctx, const ast::CallExpr &call);
  res::UnaryOperator *insertUnaryDeref(res::Context &ctx, res::Expr *val);
  res::StructInstantiationExpr *resolveStructInstantiation(
      res::Context &ctx,
      const ast::StructInstantiationExpr &structInstantiation);
  res::MemberExpr *resolveMemberExpr(res::Context &ctx,
                                     const ast::MemberExpr &memberExpr,
                                     bool isCallee = false);
  res::Expr *resolveExpr(res::Context &ctx,
                         const ast::Expr &expr,
                         res::Type *typeHint = nullptr);
  res::GCExpr *resolveGCExpr(res::Context &ctx, const ast::GCExpr &gc);
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

  res::TypeExtension *resolveTypeExtension(res::Context &ctx,
                                           const ast::TypeExtension &extension);
  bool resolveExtensionBody(res::Context &ctx,
                            res::TypeExtension *extension,
                            const ast::TypeExtension &astExtension);

  res::VarDecl *resolveVarDecl(res::Context &ctx, const ast::VarDecl &varDecl);
  res::FunctionDecl *resolveFunctionDecl(res::Context &ctx,
                                         const ast::FunctionDecl &decl);
  res::FunctionDecl *resolveFunctionBody(res::Context &ctx,
                                         const ast::FunctionDecl &functionDecl,
                                         res::FunctionDecl *function);
  std::pair<res::ParamDecl *, bool>
  resolveParamDecl(res::Context &ctx, const ast::ParamDecl *param);

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

  res::Expr *asTraitObjectIfNeeded(res::Type *targetType, res::Expr *expr);
  res::Expr *withPtrToRefDecay(res::Type *targetType, res::Expr *expr);
  res::Expr *withImplicitAsRef(res::Type *targetType, res::Expr *expr);

  std::vector<res::TypeParamDecl *> resolveTypeParamsWithoutBounds(
      res::Context &ctx,
      const std::vector<std::unique_ptr<ast::TypeParamDecl>> &typeParamDecls);
  bool resolveGenericParamsInCurrentScope(
      res::Context &ctx,
      const std::vector<res::TypeParamDecl *> &resParams,
      const std::vector<std::unique_ptr<ast::TypeParamDecl>> &astParams);

  res::TraitConformance *
  resolveTraitConformance(res::Context &ctx,
                          const ast::TraitConformance &conformance,
                          res::Type *type);
  bool hasConflictingTraits(res::Context &ctx, std::vector<res::TraitType *>);
  bool implementsAllNecessaryTraitFunctions(res::Context &ctx,
                                            res::TypeExtension *extension);

  bool insertDeclToCurrentScope(res::Decl *decl);
  res::FunctionDecl *createBuiltinPrintln(res::Context &ctx);
  res::FunctionDecl *createBuiltinGCCollect(res::Context &ctx);

  bool hasBuiltinFunctionCollisions(const res::FunctionDecl *fn);
  bool checkSelfParameter(res::ParamDecl *param, size_t idx);
  bool isSelfContainingTrait(res::TraitDecl *trait);
  bool hasSelfContainingStructs(res::Context &ctx);
  bool checkDelayedUserDefinedTypes(res::Context &ctx);
  res::Type *validatedUserDefinedType(const ast::UserDefinedType *astDecl,
                                      res::Type *type);
  bool checkVtableCompatibility(SourceLocation loc,
                                res::TraitType *trait,
                                std::set<std::string> &visited);

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

  std::pair<const res::Context *, const res::TypeManager *> resolveAST();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_SEMA_H
