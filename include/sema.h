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

  diag::DiagnosticReporter *reporter;
  ConstExprEvaluator *cee;
  const ast::SourceFile *ast;

  class Scope {
    Scope *parent;
    res::GenericDeclContext *ctx;
    std::vector<res::Decl *> decls;

  public:
    Scope(Scope *parent, res::GenericDeclContext *ctx)
        : parent(parent),
          ctx(ctx) {}

    void addDecl(res::Decl *decl) { decls.emplace_back(decl); }
    std::vector<res::Decl *> lookupSymbol(const std::string &id,
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
    std::vector<res::DeclRefExpr *> paths = {};
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

  res::NumberLiteral *resolveNumberLiteral(res::Context &ctx,
                                           const ast::NumberLiteral &number);

  res::UnaryOperator *resolveUnaryOperator(res::Context &ctx,
                                           const ast::UnaryOperator &unary);
  res::BinaryOperator *resolveBinaryOperator(res::Context &ctx,
                                             const ast::BinaryOperator &binop);

  res::Expr *resolvePathExpr(res::Context &ctx,
                             const ast::PathExpr &path,
                             res::Type *typeHint = nullptr);
  template <typename ExpectedDecl>
  res::DeclRefExpr *resolvePathDeclRef(res::Context &ctx,
                                       const ast::PathExpr &pathExpr);
  res::DeclRefExpr *resolveDeclRefExpr(res::Context &ctx,
                                       const ast::DeclRefExpr *dre,
                                       res::Decl *decl,
                                       res::Substitution sub = {});

  res::DeclRefExpr *resolveAssociatedDeclRef(res::Context &ctx,
                                             const ast::DeclRefExpr *dre,
                                             res::Type *type,
                                             res::TraitType *trait = nullptr);

  res::CallExpr *resolveCallExpr(res::Context &ctx, const ast::CallExpr &call);
  res::UnaryOperator *insertUnaryDeref(res::Context &ctx, res::Expr *val);
  res::StructInstantiationExpr *resolveStructInstantiation(
      res::Context &ctx,
      const ast::StructInstantiationExpr &structInstantiation);
  res::Expr *
  resolveMemberExpr(res::Context &ctx, const ast::MemberExpr &me, bool asCall);
  res::Expr *resolveExpr(res::Context &ctx,
                         const ast::Expr &expr,
                         res::Type *typeHint = nullptr);
  res::GCExpr *resolveGCExpr(res::Context &ctx, const ast::GCExpr &gc);

  bool shouldCaptureInCurrentLambda(res::DeclRefExpr *dre);
  res::MemberExpr *captureInCurrentLambda(res::Context &ctx,
                                          const ast::PathExpr &path,
                                          res::DeclRefExpr *dre);
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
  res::ParamDecl *resolveParamDecl(res::Context &ctx,
                                   const ast::ParamDecl *param,
                                   res::Type *typeHint = nullptr);

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

  bool checkTypeParameterCount(SourceLocation loc,
                               size_t received,
                               size_t expected) const;

  bool isTraitObjectOf(res::Context &ctx, res::Type *type, res::Type *any);
  res::Expr *tryCoerce(res::Context &ctx, res::Expr *expr, res::Type *to);

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
  bool implementsAllNecessaryTraitFunctions(res::TypeExtension *extension);

  bool insertDeclToCurrentScope(res::Decl *decl);
  res::FunctionDecl *createBuiltinPrintln(res::Context &ctx);
  res::FunctionDecl *createBuiltinGCCollect(res::Context &ctx);

  bool hasBuiltinFunctionCollisions(const res::FunctionDecl *fn);
  bool checkSelfParameter(res::Context &ctx, res::ParamDecl *param, size_t idx);
  bool isSelfContainingTrait(res::TraitDecl *trait);
  bool hasSelfContainingStructs(res::Context &ctx);
  bool checkDelayedUserDefinedTypes(res::Context &ctx);
  res::Type *validatedUserDefinedType(res::Context &ctx,
                                      const ast::UserDefinedType *astDecl,
                                      res::Type *type);
  bool checkVtableCompatibility(res::Context &ctx,
                                SourceLocation loc,
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
                const ast::SourceFile &ast)
      : reporter(&reporter),
        cee(&cee),
        ast(&ast) {}

  std::unique_ptr<res::Context> resolveAST();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_SEMA_H
