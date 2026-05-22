#include <cassert>
#include <iostream>
#include <map>
#include <set>
#include <stack>

#include "cfg.h"
#include "diag.h"
#include "sema.h"
#include "utils.h"

namespace yl {
bool Sema::runFlowSensitiveChecks(res::Context &ctx,
                                  const res::FunctionDecl &fn) {
  CFG cfg = CFGBuilder().build(fn);

  bool error = false;
  error |= checkReturnOnAllPaths(ctx, fn, cfg);
  error |= checkVariableInitialization(ctx, cfg);

  return error;
};

bool Sema::checkReturnOnAllPaths(res::Context &ctx,
                                 const res::FunctionDecl &fn,
                                 const CFG &cfg) {
  if (fn.getType()
          ->getAs<res::FunctionType>()
          ->getReturnType()
          ->getAs<res::BuiltinUnitType>())
    return false;

  int returnCount = 0;
  bool exitReached = false;

  std::set<int> visited;
  std::vector<int> worklist;
  worklist.emplace_back(cfg.entry);

  while (!worklist.empty()) {
    int bb = worklist.back();
    worklist.pop_back();

    if (!visited.emplace(bb).second)
      continue;

    exitReached |= bb == cfg.exit;

    const auto &[preds, succs, stmts] = cfg.basicBlocks[bb];

    if (!stmts.empty() && dynamic_cast<const res::ReturnStmt *>(stmts[0])) {
      ++returnCount;
      continue;
    }

    for (auto &&[succ, reachable] : succs)
      if (reachable)
        worklist.emplace_back(succ);
  }

  if (exitReached || returnCount == 0)
    (returnCount > 0
         ? err::expectedReturnValueOnEveryPath(fn.location).report(reporter)
         : err::expectedReturnValue(fn.location).report(reporter));

  return exitReached || returnCount == 0;
}

bool Sema::checkVariableInitialization(const res::Context &ctx,
                                       const CFG &cfg) {
  enum class State { Bottom, Unassigned, Assigned, Top };

  using Lattice = std::map<const res::Decl *, State>;

  auto joinStates = [](State s1, State s2) {
    if (s1 == s2)
      return s1;

    if (s1 == State::Bottom)
      return s2;

    if (s2 == State::Bottom)
      return s1;

    return State::Top;
  };

  std::vector<Lattice> curLattices(cfg.basicBlocks.size());
  std::vector<diag::DiagBuilder> pendingErrors;

  bool changed = true;
  while (changed) {
    changed = false;
    pendingErrors.clear();

    for (int bb = cfg.entry; bb != cfg.exit; --bb) {
      const auto &[preds, succs, stmts] = cfg.basicBlocks[bb];

      Lattice tmp;
      for (auto &&pred : preds)
        for (auto &&[decl, state] : curLattices[pred.first])
          tmp[decl] = joinStates(tmp[decl], state);

      for (auto it = stmts.rbegin(); it != stmts.rend(); ++it) {
        const res::Stmt *stmt = *it;

        if (auto *declStmt = dynamic_cast<const res::DeclStmt *>(stmt)) {
          const res::VarDecl *decl = declStmt->varDecl;
          tmp[decl] = decl->initializer ? State::Assigned : State::Unassigned;
          continue;
        }

        if (auto *assignment = dynamic_cast<const res::Assignment *>(stmt)) {
          const res::Expr *base = assignment->assignee;
          while (true) {
            if (const auto *me = dynamic_cast<const res::MemberExpr *>(base)) {
              base = me->base;
              continue;
            }

            if (const auto *g = dynamic_cast<const res::GroupingExpr *>(base)) {
              base = g->expr;
              continue;
            }

            break;
          }

          const auto *path = dynamic_cast<const res::PathExpr *>(base);
          if (!path)
            continue;

          const auto *decl =
              path->fragments.back()->decl->getAs<res::ValueDecl>();
          if (!decl->isMutable && tmp[decl] != State::Unassigned)
            pendingErrors.emplace_back(
                err::cannotBeMutated(assignment->location)
                    .with(decl->identifier));

          tmp[decl] = State::Assigned;
          continue;
        }

        if (const auto *path = dynamic_cast<const res::PathExpr *>(stmt)) {

          for (auto &&fragment : path->fragments) {
            for (auto &&typeArg : fragment->typeArgs) {
              if (!typeArg->getRootType()->getAs<res::UninferredType>())
                continue;

              pendingErrors.emplace_back(
                  err::annotationsNeeded(fragment->location)
                      .with(fragment->decl->identifier));
            }
          }

          const auto *dre = path->fragments.back();
          const auto *var = dre->decl->getAs<res::VarDecl>();
          if (var && tmp[var] != State::Assigned)
            pendingErrors.emplace_back(
                err::notInitialized(dre->location).with(var->identifier));

          continue;
        }
      }

      if (curLattices[bb] != tmp) {
        curLattices[bb] = tmp;
        changed = true;
      }
    }
  }

  for (auto &&[d, s] : curLattices[cfg.exit + 1])
    if (s == State::Unassigned && d->getType()->getAs<res::UninferredType>()) {
      err::unknownType(d->location).with(d->identifier).report(reporter);
      return true;
    }

  for (auto &&err : pendingErrors)
    err.report(reporter);

  return !pendingErrors.empty();
}

bool Sema::insertDeclToScope(res::Decl *decl, res::DeclContext *scope) {
  if (!decl)
    return false;

  if (!scope->insertDecl(decl)) {
    err::redeclaration(decl->location).with(decl->identifier).report(reporter);
    return false;
  }

  return true;
}

res::FunctionDecl *Sema::createBuiltinPrintln(res::Context &ctx) {
  SourceLocation loc{nullptr, 0, 0};

  auto *numTy = typeMgr.getBuiltinNumberType();
  auto *param = ctx.create<res::ParamDecl>(loc, numTy, "n", false);

  auto *fnTy = typeMgr.getFunctionType({numTy}, typeMgr.getBuiltinUnitType());
  auto *fn = ctx.create<res::FunctionDecl>(loc, fnTy, "println",
                                           std::vector<res::TypeParamDecl *>{},
                                           std::vector{param});
  fn->setBody(ctx.create<res::Block>(loc, std::vector<res::Stmt *>()));
  return fn;
};

res::StructDecl *Sema::createBuiltinGc(res::Context &ctx) {
  SourceLocation loc{nullptr, 0, 0};

  auto *typeParamTy = typeMgr.getNewUninferredType();
  auto *typeParamDecl = ctx.create<res::TypeParamDecl>(loc, typeParamTy, "T");
  typeMgr.unify(typeParamTy, typeMgr.getTypeParamType(*typeParamDecl));

  std::vector<res::TypeParamDecl *> typeParamDecls = {typeParamDecl};
  auto *structTy = typeMgr.getNewUninferredType();
  auto *structDecl = ctx.create<res::StructDecl>(loc, structTy, gcId, false,
                                                 true, typeParamDecls);
  typeMgr.unify(structTy, typeMgr.getStructType(*structDecl, {typeParamTy}));

  auto *fieldDecl = ctx.create<res::FieldDecl>(loc, typeParamTy, "val");
  structDecl->insertDecl(fieldDecl);

  return structDecl;
}

res::Type *Sema::resolveType(res::Context &ctx, const ast::Type &parsedType) {
  if (const auto *builtin =
          dynamic_cast<const ast::BuiltinType *>(&parsedType)) {
    switch (builtin->kind) {
    case ast::BuiltinType::Kind::Unit:
      return typeMgr.getBuiltinUnitType();
    case ast::BuiltinType::Kind::Number:
      return typeMgr.getBuiltinNumberType();
    case ast::BuiltinType::Kind::Bool:
      return typeMgr.getBuiltinBoolType();
    case ast::BuiltinType::Kind::Self:
      if (!selfType)
        return err::selfTyNotAllowed(parsedType.location).report(reporter);
      return selfType;
    }
  }

  if (const auto *udt =
          dynamic_cast<const ast::UserDefinedType *>(&parsedType)) {
    res::Decl *decl = lexicalScope->lookupDecl<res::TypeDecl>(udt->identifier);
    if (!decl)
      return err::failedToResolveType(udt->location)
          .with(udt->identifier)
          .report(reporter);

    if (auto *typeParamDecl = decl->getAs<res::TypeParamDecl>())
      return typeMgr.getTypeParamType(*typeParamDecl);

    auto *sd = decl->getAs<res::StructDecl>();
    assert(sd && "unexpected user defined type");

    varOrReturn(res, checkTypeParameterCount(udt->location,
                                             udt->typeArguments.size(),
                                             sd->typeParams.size()));

    std::vector<res::Type *> resolvedTypeArgs;
    for (auto &&astArg : udt->typeArguments) {
      varOrReturn(resolvedType, resolveType(ctx, *astArg));
      if (resolvedType->getAs<res::OutParamType>())
        return err::unexpectedAmpParam(astArg->location).report(reporter);

      resolvedTypeArgs.emplace_back(resolvedType);
    }

    return typeMgr.getStructType(*sd, std::move(resolvedTypeArgs));
  }

  if (const auto *function =
          dynamic_cast<const ast::FunctionType *>(&parsedType)) {
    std::vector<res::Type *> args;
    for (auto &&astArg : function->args) {
      WithModifiersRAII ampAllowed(this, UnaryAmpAllowed);
      if (auto *arg = resolveType(ctx, *astArg))
        args.emplace_back(arg);
    }

    auto *retTy = resolveType(ctx, *function->ret);
    if (args.size() != function->args.size() || !retTy)
      return nullptr;

    return typeMgr.getFunctionType(std::move(args), retTy);
  }

  if (const auto *out = dynamic_cast<const ast::OutParamType *>(&parsedType)) {
    if (!(modifiers & UnaryAmpAllowed))
      return err::unexpectedAmpParam(out->location).report(reporter);

    varOrReturn(paramType, resolveType(ctx, *out->paramType));
    assert(!paramType->getAs<res::OutParamType>() &&
           "grammar doesn't allow nested out param types");

    return typeMgr.getOutParamType(paramType);
  }

  llvm_unreachable("unexpected ast type encountered");
}

res::UnaryOperator *
Sema::resolveUnaryOperator(res::Context &ctx, const ast::UnaryOperator &unary) {
  varOrReturn(rhs, resolveExpr(ctx, *unary.operand));

  auto *rhsTy = rhs->getType();
  if (rhsTy->getAs<res::UninferredType>())
    return err::unaryOperandUnknown(rhs->location).report(reporter);

  if (unary.op == TokenKind::Excl && !rhsTy->getAs<res::BuiltinBoolType>())
    return err::expectedOperandTy(rhs->location).with("bool").report(reporter);

  if (unary.op == TokenKind::Minus && !rhsTy->getAs<res::BuiltinNumberType>())
    return err::expectedOperandTy(rhs->location)
        .with("number")
        .report(reporter);

  if (unary.op == TokenKind::Amp) {
    if (!(modifiers & UnaryAmpAllowed))
      return err::ampOutsideArgList(unary.location).report(reporter);

    if (!rhs->isMutable())
      return err::ampWrongCategory(rhs->location).report(reporter);

    rhsTy = typeMgr.getOutParamType(rhsTy);
  }

  return ctx.create<res::UnaryOperator>(unary.location, rhsTy, unary.op, rhs);
}

res::BinaryOperator *
Sema::resolveBinaryOperator(res::Context &ctx,
                            const ast::BinaryOperator &binop) {
  varOrReturn(lhs, resolveExpr(ctx, *binop.lhs));
  varOrReturn(rhs, resolveExpr(ctx, *binop.rhs));

  auto *lhsTy = lhs->getType();
  auto *rhsTy = rhs->getType();

  if (auto *uninferredLHS = lhsTy->getAs<res::UninferredType>();
      uninferredLHS || rhsTy->getAs<res::UninferredType>())
    return err::binopOperandUnknown((uninferredLHS ? lhs : rhs)->location)
        .with(uninferredLHS ? "LHS" : "RHS")
        .report(reporter);

  const auto &loc = binop.location;
  TokenKind op = binop.op;

  bool isLogicalOp = op == TokenKind::AmpAmp || op == TokenKind::PipePipe;
  bool isNumbericOp = !isLogicalOp && op != TokenKind::EqualEqual;

  bool typeError = !typeMgr.unify(lhsTy, rhsTy).empty();
  typeError |= isLogicalOp && !rhsTy->getAs<res::BuiltinBoolType>();
  typeError |= isNumbericOp && !rhsTy->getAs<res::BuiltinNumberType>();
  if (typeError)
    return err::binopIncompatibleOperands(loc)
        .with(lhsTy->getName())
        .with(rhsTy->getName())
        .report(reporter);

  res::Type *resTy = (op == TokenKind::EqualEqual || op == TokenKind::Lt ||
                      op == TokenKind::Gt)
                         ? typeMgr.getBuiltinBoolType()
                         : lhsTy;
  return ctx.create<res::BinaryOperator>(loc, resTy, binop.op, lhs, rhs);
}

res::GroupingExpr *
Sema::resolveGroupingExpr(res::Context &ctx,
                          const ast::GroupingExpr &grouping) {
  varOrReturn(expr, resolveExpr(ctx, *grouping.expr));
  return ctx.create<res::GroupingExpr>(grouping.location, expr);
}

template <typename Hint>
res::PathExpr *Sema::resolvePathExpr(res::Context &ctx,
                                     const ast::PathExpr &pathExpr) {
  const auto &path = pathExpr.path;
  std::vector<res::DeclRefExpr *> resFragments;

  for (auto &&fragment : path) {
    auto &&[impl, decl] = fragment;

    res::Type *parentTy = nullptr;
    if (!resFragments.empty())
      parentTy = resFragments.back()->getType();

    if (fragment != path.back()) {
      varOrReturn(dre, resolveDeclRefExpr<res::TypeDecl>(
                           ctx, parentTy, decl.get(), impl.get()));
      resFragments.emplace_back(dre);
      continue;
    }

    varOrReturn(
        dre, resolveDeclRefExpr<Hint>(ctx, parentTy, decl.get(), impl.get()));
    resFragments.emplace_back(dre);
  }

  return ctx.create<res::PathExpr>(std::move(resFragments));
}

template <typename Hint>
res::DeclRefExpr *Sema::resolveDeclRefExpr(res::Context &ctx,
                                           res::Type *parent,
                                           const ast::DeclRefExpr *dre,
                                           const ast::ImplSpecifier *impl) {
  if (impl) {
    assert(parent && "impl specifier without parent");

    varOrReturn(traitInstance,
                resolveTraitInstance(ctx, impl->traitInstance.get()));

    res::TraitType *traitTy = traitInstance->getType()->getAs<res::TraitType>();
    auto *checkTy =
        typeMgr.withObligation(typeMgr.getNewUninferredType(), traitTy);

    if (!typeMgr.unify(parent, checkTy).empty())
      return err::traitNotImplemented(impl->location)
          .with(parent->getName())
          .with(traitTy->getName())
          .report(reporter);

    if (auto *decl = lookupSymbolWithFallback<Hint>(traitInstance->decl, dre))
      return createDeclRefExpr(ctx, dre, parent, decl, traitTy);

    return err::traitMissingMember(dre->location)
        .with(traitInstance->decl->identifier)
        .with(dre->identifier)
        .report(reporter);
  }

  if (!parent && dre->identifier == selfTypeId) {
    if (!selfType)
      return err::selfTyNotAllowed(dre->location).report(reporter);

    if (auto *paramTy = selfType->getAs<res::TypeParamType>())
      return createDeclRefExpr(ctx, dre, nullptr, paramTy->decl, nullptr);

    if (auto *structTy = selfType->getAs<res::StructType>())
      return createDeclRefExpr(ctx, dre, nullptr, structTy->getDecl(), nullptr);

    llvm_unreachable("unexpected self type");
  }

  if (!parent) {
    if (auto *decl = lookupSymbolWithFallback<Hint>(lexicalScope, dre))
      return createDeclRefExpr(ctx, dre, nullptr, decl, nullptr);

    return err::missingSymbol(dre->location)
        .with(dre->identifier)
        .report(reporter);
  }

  auto *structTy = parent->getAs<res::StructType>();
  bool isLambda = structTy && structTy->getDecl()->isLambda;

  if (structTy && !isLambda)
    if (auto *decl = lookupSymbolWithFallback<Hint>(structTy->getDecl(), dre))
      return createDeclRefExpr(ctx, dre, parent, decl, nullptr);

  if (isLambda || !structTy && !parent->getAs<res::TypeParamType>())
    return err::cannotAccessMember(dre->location)
        .with(parent->getName())
        .report(reporter);

  res::Decl *decl = nullptr;
  res::TraitType *trait = nullptr;

  auto traits = typeMgr.getUpperBounds(parent);
  for (auto &&implementedTrait : traits) {
    if (auto *traitDecl =
            lookupSymbolWithFallback<Hint>(implementedTrait->getDecl(), dre)) {
      if (decl)
        return err::ambigousMemberFn(dre->location).report(reporter);

      decl = traitDecl;
      trait = implementedTrait;
    }
  }

  if (!decl)
    return err::lookupInTypeFailed(dre->location)
        .with(dre->identifier)
        .with(parent->getName())
        .report(reporter);
  return createDeclRefExpr(ctx, dre, parent, decl, trait);
}

res::DeclRefExpr *Sema::createDeclRefExpr(res::Context &ctx,
                                          const ast::DeclRefExpr *dre,
                                          res::Type *parentTy,
                                          res::Decl *decl,
                                          res::TraitType *trait) {
  res::Type *declTy = decl->getType();
  declTy = typeMgr.instantiate(declTy, typeMgr.extractSubstitutionFrom(trait));
  declTy =
      typeMgr.instantiate(declTy, typeMgr.extractSubstitutionFrom(parentTy));

  auto *valueDecl = decl->getAs<res::ValueDecl>();
  res::Expr::Kind kind = res::Expr::Kind::Lvalue;
  if (!valueDecl || decl->getAs<res::FunctionDecl>())
    kind = res::Expr::Kind::Rvalue;
  else if (valueDecl->isMutable)
    kind = res::Expr::Kind::MutLvalue;

  res::Substitution sub;
  std::vector<res::TypeParamDecl *> typeParams = decl->typeParams;
  std::vector<res::Type *> typeArgs;

  for (auto &&typeParam : typeParams) {
    bool isImplicitSelf = typeParam == typeParams.front() &&
                          typeParam->identifier == implicitSelfId;

    res::Type *typeParamTy = typeParam->getType();
    res::Type *subTy =
        isImplicitSelf ? parentTy : typeMgr.getNewUninferredType();

    sub[typeParamTy] = typeArgs.emplace_back(subTy);

    if (auto *u = subTy->getAs<res::UninferredType>())
      for (auto &&trait : typeMgr.getUpperBounds(typeParamTy))
        typeMgr.withObligation(
            u, typeMgr.instantiate(trait, sub)->getAs<res::TraitType>());
  }

  if (const auto *typeArgList = dre->typeArgumentList.get()) {
    if (!decl->isGeneric())
      return err::notGeneric(typeArgList->location)
          .with(decl->identifier)
          .report(reporter);

    bool hasImplicitSelf = typeParams.front()->identifier == implicitSelfId;
    varOrReturn(res, checkTypeParameterCount(
                         typeArgList->location, typeArgList->args.size(),
                         typeParams.size() - hasImplicitSelf));

    int idx = hasImplicitSelf ? 1 : 0;
    for (auto &&astArg : typeArgList->args) {
      varOrReturn(typeArgTy, resolveType(ctx, *astArg));

      if (const auto &errors = typeMgr.unify(typeArgTy, typeArgs[idx]);
          !errors.empty()) {
        for (auto &&error : errors)
          err::inferenceError(astArg->location).with(error).report(reporter);

        return nullptr;
      }

      ++idx;
    }
  }

  return ctx.create<res::DeclRefExpr>(dre->location,
                                      typeMgr.instantiate(declTy, sub), *decl,
                                      kind, typeArgs, parentTy, trait);
}

template <typename Hint>
res::Decl *Sema::lookupSymbolWithFallback(res::DeclContext *scope,
                                          const ast::DeclRefExpr *dre) {
  if (auto *decl = scope->lookupDecl<Hint>(dre->identifier))
    return decl;

  return scope->lookupDecl<res::Decl>(dre->identifier);
}

res::CallExpr *Sema::resolveCallExpr(res::Context &ctx,
                                     const ast::CallExpr &call) {
  res::Expr *callee = nullptr;
  res::Expr *selfArg = nullptr;

  {
    WithModifiersRAII isCallee(this, IsCallee);
    callee = resolveExpr(ctx, *call.callee);
    if (!callee)
      return nullptr;
  }

  res::Type *calleeType = callee->getType();
  auto *fnType = calleeType->getAs<res::FunctionType>();

  auto *lambdaTy = calleeType->getAs<res::StructType>();
  bool isLambda = lambdaTy && lambdaTy->getDecl()->isLambda;

  if (!fnType && !isLambda)
    return err::invalidCallTy(call.location)
        .with(calleeType->getName())
        .report(reporter);

  if (isLambda) {
    auto *fn =
        lambdaTy->getDecl()->lookupDecl<res::FunctionDecl>(lambdaFunctionId);
    fnType = fn->getType()->getAs<res::FunctionType>();

    auto *fnDre = ctx.create<res::DeclRefExpr>(
        callee->location, fnType, *fn, res::Expr::Kind::Rvalue,
        std::vector<res::Type *>{}, lambdaTy);
    callee = ctx.create<res::MemberExpr>(callee->location, callee, fnDre);
  }

  if (auto *me = dynamic_cast<res::MemberExpr *>(callee)) {
    res::Expr *base = me->base;
    res::DeclRefExpr *member = me->member;

    if (auto *function = member->decl->getAs<res::FunctionDecl>()) {
      res::ParamDecl *selfParam =
          function->params.empty() ? nullptr : function->params[0];
      if (!selfParam || selfParam->identifier != selfParamId)
        return err::classMethodCallOnInstance(call.location).report(reporter);

      SourceLocation baseLoc = base->location;
      res::Expr::Kind baseKind = base->kind;
      res::Type *baseTy = base->getType();

      selfArg = base;
      if (selfParam->getType()->getAs<res::OutParamType>()) {
        if (selfArg->isLvalue() && !selfArg->isMutable())
          return err::structImmutable(baseLoc).report(reporter);

        selfArg = ctx.create<res::UnaryOperator>(
            baseLoc, typeMgr.getOutParamType(baseTy), TokenKind::Amp, selfArg);
      }

      res::DeclRefExpr *baseDre = nullptr;
      if (auto *structTy = baseTy->getAs<res::StructType>())
        baseDre = ctx.create<res::DeclRefExpr>(baseLoc, structTy,
                                               *structTy->getDecl(), baseKind,
                                               structTy->getTypeArgs());

      if (auto *typeParamTy = baseTy->getAs<res::TypeParamType>())
        baseDre = ctx.create<res::DeclRefExpr>(baseLoc, typeParamTy,
                                               *typeParamTy->decl, baseKind);

      std::vector<res::DeclRefExpr *> fragments = {baseDre, member};
      callee = ctx.create<res::PathExpr>(std::move(fragments));
    }
  }

  std::vector<res::Type *> argTypes = fnType->getArgs();
  std::vector<res::Expr *> resolvedArgs;

  if (selfArg)
    resolvedArgs.emplace_back(selfArg);

  size_t fnTypeArgCnt = argTypes.size();
  size_t resolvedArgCnt = resolvedArgs.size();
  size_t astArgCnt = call.arguments.size();

  if ((astArgCnt + resolvedArgCnt) != fnTypeArgCnt)
    return err::wrongArgCount(call.location)
        .with(fnTypeArgCnt - resolvedArgCnt)
        .with(astArgCnt)
        .report(reporter);

  for (auto &&arg : call.arguments) {
    WithModifiersRAII unaryAmpAllowed(this, UnaryAmpAllowed);
    varOrReturn(resolvedArg, resolveExpr(ctx, *arg));

    res::Type *expectedTy = argTypes[resolvedArgs.size()];
    if (expectedTy->getAs<res::FunctionType>())
      resolvedArg = coerceIfLambda(resolvedArg);

    res::Type *actualTy = resolvedArg->getType();

    if (const auto &errors = typeMgr.unify(actualTy, expectedTy);
        !errors.empty()) {
      for (auto &&error : errors)
        err::inferenceError(resolvedArg->location).with(error).report(reporter);
      return nullptr;
    }

    resolvedArg->setConstantValue(cee->evaluate(*resolvedArg));
    resolvedArgs.emplace_back(resolvedArg);
  }

  return ctx.create<res::CallExpr>(call.location, fnType->getReturnType(),
                                   callee, std::move(resolvedArgs));
}

res::StructInstantiationExpr *Sema::resolveStructInstantiation(
    res::Context &ctx,
    const ast::StructInstantiationExpr &structInstantiation) {
  varOrReturn(path, resolvePathExpr<res::StructDecl>(
                        ctx, *structInstantiation.structRef));

  if (!path->fragments.back()->decl->getAs<res::StructDecl>())
    return err::notStructInstance(path->location).report(reporter);

  auto *structTy = path->getType()->getAs<res::StructType>();
  auto *sd = structTy->getDecl();

  std::vector<res::FieldInitStmt *> resolvedFieldInits;
  std::map<std::string_view, res::FieldInitStmt *> inits;

  std::map<std::string_view, res::FieldDecl *> fields;
  for (auto &&fieldDecl : sd->getAll<res::FieldDecl>())
    fields[fieldDecl->identifier] = fieldDecl;

  bool error = false;
  for (auto &&initStmt : structInstantiation.fieldInitializers) {
    std::string_view id = initStmt->identifier;
    const SourceLocation &loc = initStmt->location;

    if (inits.count(id)) {
      err::fieldAlreadyInitialized(loc).with(id).report(reporter);
      error = true;
      continue;
    }

    res::FieldDecl *fieldDecl = fields[id];
    if (!fieldDecl) {
      err::noFieldWithName(loc).with(sd->identifier).with(id).report(reporter);
      error = true;
      continue;
    }

    auto *resolvedInitExpr = resolveExpr(ctx, *initStmt->initializer);
    if (!resolvedInitExpr) {
      error = true;
      continue;
    }

    res::Type *fieldTy = typeMgr.instantiate(
        fieldDecl->getType(), typeMgr.extractSubstitutionFrom(structTy));
    if (fieldTy->getAs<res::FunctionType>())
      resolvedInitExpr = coerceIfLambda(resolvedInitExpr);

    res::Type *initTy = resolvedInitExpr->getType();
    if (const auto &msg = typeMgr.unify(initTy, fieldTy); !msg.empty()) {
      for (auto &&error : msg)
        err::inferenceError(resolvedInitExpr->location)
            .with(error)
            .report(reporter);
      error = true;
      continue;
    }

    inits[id] = resolvedFieldInits.emplace_back(
        ctx.create<res::FieldInitStmt>(loc, fieldDecl, resolvedInitExpr));
  }

  for (auto &&fieldDecl : sd->getAll<res::FieldDecl>()) {
    if (!inits.count(fieldDecl->identifier)) {
      err::fieldNotInitialized(structInstantiation.location)
          .with(fieldDecl->identifier)
          .report(reporter);
      error = true;
      continue;
    }

    auto *initExpr = inits[fieldDecl->identifier]->initializer;
    initExpr->setConstantValue(cee->evaluate(*initExpr));
  }

  if (error)
    return nullptr;

  return ctx.create<res::StructInstantiationExpr>(
      structInstantiation.location, structTy, path,
      std::move(resolvedFieldInits));
}

res::MemberExpr *Sema::resolveMemberExpr(res::Context &ctx,
                                         const ast::MemberExpr &memberExpr) {
  varOrReturn(base, resolveExpr(ctx, *memberExpr.base));
  varOrReturn(memberDre, resolveDeclRefExpr<res::ValueDecl>(
                             ctx, base->getType(), memberExpr.member.get()));

  if (memberDre->decl->getAs<res::FunctionDecl>() && !(modifiers & IsCallee))
    return err::expectedMethodCall(memberExpr.location).report(reporter);

  return ctx.create<res::MemberExpr>(memberExpr.location, base, memberDre);
}

res::LambdaExpr *Sema::resolveLambdaExpr(res::Context &ctx,
                                         const ast::LambdaExpr &lambdaExpr) {
  SourceLocation loc = lambdaExpr.location;

  std::stringstream structId;
  structId << "(lambda@<source>:" << loc.line << ':' << loc.col << ')';

  res::Type *lambdaTy = typeMgr.getNewUninferredType();
  auto *lambda =
      ctx.create<res::StructDecl>(loc, lambdaTy, structId.str(), true);
  typeMgr.unify(lambdaTy, typeMgr.getStructType(*lambda, {}));

  bool error = false;
  std::vector<res::Type *> paramTypes = {lambdaTy};
  std::vector<res::ParamDecl *> resolvedParams = {
      ctx.create<res::ParamDecl>(loc, lambdaTy, selfParamId, false)};

  {
    WithModifiersRAII lambdaParamList(this, UnaryAmpAllowed |
                                                MissingTypeAnnotationsAllowed);
    EnterScopeRAII paramScope(this);
    for (auto &&param : lambdaExpr.params) {

      auto [resolvedParam, err] = resolveParamDecl(ctx, param.get());

      paramTypes.emplace_back(resolvedParam->getType());
      resolvedParams.emplace_back(resolvedParam);

      error |= !insertDeclToScope(resolvedParam, lexicalScope);

      if (param->identifier == selfParamId) {
        err::selfParamNotAllowed(param->location).report(reporter);
        error = true;
      }
    }
  }

  res::Type *returnTy = lambdaExpr.returnType
                            ? resolveType(ctx, *lambdaExpr.returnType)
                            : typeMgr.getNewUninferredType();

  if (!returnTy || error)
    return nullptr;

  auto *fnTy = typeMgr.getFunctionType(std::move(paramTypes), returnTy);
  auto *fn = ctx.create<res::FunctionDecl>(loc, fnTy, lambdaFunctionId,
                                           std::vector<res::TypeParamDecl *>{},
                                           std::move(resolvedParams), lambda);
  lambda->insertDecl(fn);

  auto *resLambdaExpr = ctx.create<res::LambdaExpr>(loc, lambdaTy, lambda, fn);
  functionInfo->pendingLambdas.push_back(
      {resLambdaExpr, &lambdaExpr, *lexicalScope});
  return resLambdaExpr;
}

res::Expr *Sema::coerceIfLambda(res::Expr *expr) {
  res::StructType *structTy = expr->getType()->getAs<res::StructType>();
  if (!structTy)
    return expr;

  res::StructDecl *decl = structTy->getDecl();
  if (!decl->isLambda)
    return expr;

  auto *fn = decl->lookupDecl<res::FunctionDecl>(lambdaFunctionId);
  assert(fn && "lambda without method!?");

  auto *fnTy = fn->getType()->getAs<res::FunctionType>();

  std::vector<res::Type *> coercedArgs = fnTy->getArgs();
  coercedArgs.erase(coercedArgs.begin());

  auto *coercedFnTy =
      typeMgr.getFunctionType(coercedArgs, fnTy->getReturnType());
  return functionInfo->pedingCoercions.emplace_back(
      ctx.create<res::ImplicitCoerceExpr>(expr->location, coercedFnTy, expr));
}

bool Sema::checkLambdaCoercions() {
  bool error = false;

  for (auto &&coercion : functionInfo->pedingCoercions) {
    auto *lambda = coercion->lambdaExpr->getType()->getAs<res::StructType>();
    bool capturing = !lambda->getDecl()->getAll<res::FieldDecl>().empty();

    if (capturing) {
      err::capturingLambdaFunctionCoercion(coercion->location)
          .with(lambda->getName())
          .with(coercion->getType()->getName())
          .report(reporter);
      error = true;
    }
  }

  return !error;
}

bool Sema::resolvePendingLambdaBodies() {
  bool error = false;

  for (auto &&[res, ast, captureScope] : functionInfo->pendingLambdas) {
    res::FunctionDecl *fn = res->method;
    auto *fnTy = fn->getType()->getAs<res::FunctionType>();

    lexicalScope = &captureScope;

    EnterScopeRAII paramScope(this);
    for (auto &&param : fn->params)
      insertDeclToScope(param, lexicalScope);

    std::vector<const ast::Expr *> pendingCaptureInits;
    {
      WithFunctionInfoRAII lambdaInfo(this, {fn, res, lexicalScope});

      if (res::Block *block = resolveBlock(ctx, *ast->body)) {
        fn->setBody(block);

        res::Type *retTy = fnTy->getReturnType();
        if (retTy->getAs<res::UninferredType>())
          typeMgr.unify(retTy, typeMgr.getBuiltinUnitType());

        error |= runFlowSensitiveChecks(ctx, *fn);
      }

      error |= !fn->isComplete;
      pendingCaptureInits = std::move(functionInfo->pendingCaptureInits);
    }

    for (auto &&pendingInit : pendingCaptureInits) {
      res::Expr *initExpr = resolveExpr(ctx, *pendingInit);
      initExpr->setConstantValue(cee->evaluate(*initExpr));
      res->fieldInits.emplace_back(initExpr);
    }
  }

  for (auto &&lambdaDescriptor : functionInfo->pendingLambdas) {
    for (auto &&param : lambdaDescriptor.lambda->method->params) {
      if (param->getType()->getAs<res::UninferredType>()) {
        err::annotationsNeeded(param->location)
            .with(param->identifier)
            .report(reporter);
        error = true;
      }
    }
  }

  return !error;
}

res::Stmt *Sema::resolveStmt(res::Context &ctx, const ast::Stmt &stmt) {
  if (auto *expr = dynamic_cast<const ast::Expr *>(&stmt))
    return resolveExpr(ctx, *expr);

  if (auto *ifStmt = dynamic_cast<const ast::IfStmt *>(&stmt))
    return resolveIfStmt(ctx, *ifStmt);

  if (auto *assignment = dynamic_cast<const ast::Assignment *>(&stmt))
    return resolveAssignment(ctx, *assignment);

  if (auto *declStmt = dynamic_cast<const ast::DeclStmt *>(&stmt))
    return resolveDeclStmt(ctx, *declStmt);

  if (auto *whileStmt = dynamic_cast<const ast::WhileStmt *>(&stmt))
    return resolveWhileStmt(ctx, *whileStmt);

  if (auto *returnStmt = dynamic_cast<const ast::ReturnStmt *>(&stmt))
    return resolveReturnStmt(ctx, *returnStmt);

  llvm_unreachable("unexpected statement");
}

res::IfStmt *Sema::resolveIfStmt(res::Context &ctx, const ast::IfStmt &ifStmt) {
  varOrReturn(cond, resolveExpr(ctx, *ifStmt.condition));
  if (!typeMgr.unify(cond->getType(), typeMgr.getBuiltinBoolType()).empty())
    return err::expectedBoolCondition(cond->location).report(reporter);

  varOrReturn(trueBlock, resolveBlock(ctx, *ifStmt.trueBlock));

  res::Block *falseBlock = nullptr;
  if (ifStmt.falseBlock) {
    falseBlock = resolveBlock(ctx, *ifStmt.falseBlock);
    if (!falseBlock)
      return nullptr;
  }

  cond->setConstantValue(cee->evaluate(*cond));
  return ctx.create<res::IfStmt>(ifStmt.location, cond, trueBlock, falseBlock);
}

res::WhileStmt *Sema::resolveWhileStmt(res::Context &ctx,
                                       const ast::WhileStmt &whileStmt) {
  varOrReturn(cond, resolveExpr(ctx, *whileStmt.condition));
  if (!typeMgr.unify(cond->getType(), typeMgr.getBuiltinBoolType()).empty())
    return err::expectedBoolCondition(cond->location).report(reporter);

  varOrReturn(body, resolveBlock(ctx, *whileStmt.body));

  cond->setConstantValue(cee->evaluate(*cond));
  return ctx.create<res::WhileStmt>(whileStmt.location, cond, body);
}

res::DeclStmt *Sema::resolveDeclStmt(res::Context &ctx,
                                     const ast::DeclStmt &declStmt) {
  varOrReturn(varDecl, resolveVarDecl(ctx, *declStmt.varDecl));

  if (!insertDeclToScope(varDecl, lexicalScope))
    return nullptr;

  return ctx.create<res::DeclStmt>(declStmt.location, varDecl);
}

res::Assignment *Sema::resolveAssignment(res::Context &ctx,
                                         const ast::Assignment &assignment) {
  varOrReturn(rhs, resolveExpr(ctx, *assignment.expr));
  varOrReturn(lhs, resolveExpr(ctx, *assignment.assignee));

  if (!lhs->isLvalue())
    return err::rvalueAssignment(lhs->location).report(reporter);

  auto *lhsTy = lhs->getType();
  if (lhsTy->getAs<res::FunctionType>())
    rhs = coerceIfLambda(rhs);

  auto *rhsTy = rhs->getType();
  if (const auto &errors = typeMgr.unify(lhsTy, rhsTy); !errors.empty()) {
    for (auto &&error : errors)
      err::inferenceError(rhs->location).with(error).report(reporter);

    return err::incompatibleAssignment(rhs->location)
        .with(lhsTy->getName())
        .with(rhsTy->getName())
        .report(reporter);
  }

  rhs->setConstantValue(cee->evaluate(*rhs));
  return ctx.create<res::Assignment>(assignment.location, lhs, rhs);
}

res::ReturnStmt *Sema::resolveReturnStmt(res::Context &ctx,
                                         const ast::ReturnStmt &returnStmt) {
  assert(functionInfo && "return stmt outside a function");

  auto *fnTy = functionInfo->function->getType()->getAs<res::FunctionType>();
  auto *retTy = fnTy->getReturnType();
  if (!retTy->getAs<res::BuiltinUnitType>() && !returnStmt.expr)
    return err::noReturnValue(returnStmt.location).report(reporter);

  res::Expr *expr = nullptr;
  if (returnStmt.expr) {
    expr = resolveExpr(ctx, *returnStmt.expr);
    if (!expr)
      return nullptr;

    if (retTy->getAs<res::FunctionType>())
      expr = coerceIfLambda(expr);

    res::Type *exprTy = expr->getType();
    if (!typeMgr.unify(retTy, exprTy).empty())
      return err::invalidReturnValue(expr->location)
          .with(exprTy->getName())
          .with(retTy->getName())
          .report(reporter);

    expr->setConstantValue(cee->evaluate(*expr));
  }

  return ctx.create<res::ReturnStmt>(returnStmt.location, expr);
}

res::Expr *Sema::resolveExpr(res::Context &ctx, const ast::Expr &expr) {
  if (const auto *number = dynamic_cast<const ast::NumberLiteral *>(&expr))
    return ctx.create<res::NumberLiteral>(number->location,
                                          typeMgr.getBuiltinNumberType(),
                                          std::stod(number->value));

  if (const auto *boolLiteral = dynamic_cast<const ast::BoolLiteral *>(&expr))
    return ctx.create<res::BoolLiteral>(boolLiteral->location,
                                        typeMgr.getBuiltinBoolType(),
                                        boolLiteral->value == "true");

  if (const auto *unit = dynamic_cast<const ast::UnitLiteral *>(&expr))
    return ctx.create<res::UnitLiteral>(unit->location,
                                        typeMgr.getBuiltinUnitType());

  if (const auto *callExpr = dynamic_cast<const ast::CallExpr *>(&expr))
    return resolveCallExpr(ctx, *callExpr);

  if (const auto *groupingExpr = dynamic_cast<const ast::GroupingExpr *>(&expr))
    return resolveGroupingExpr(ctx, *groupingExpr);

  if (const auto *binaryOperator =
          dynamic_cast<const ast::BinaryOperator *>(&expr))
    return resolveBinaryOperator(ctx, *binaryOperator);

  if (const auto *unaryOperator =
          dynamic_cast<const ast::UnaryOperator *>(&expr))
    return resolveUnaryOperator(ctx, *unaryOperator);

  if (const auto *structInstantiation =
          dynamic_cast<const ast::StructInstantiationExpr *>(&expr))
    return resolveStructInstantiation(ctx, *structInstantiation);

  if (const auto *memberExpr = dynamic_cast<const ast::MemberExpr *>(&expr))
    return resolveMemberExpr(ctx, *memberExpr);

  if (const auto *lambda = dynamic_cast<const ast::LambdaExpr *>(&expr))
    return resolveLambdaExpr(ctx, *lambda);

  if (const auto *path = dynamic_cast<const ast::PathExpr *>(&expr)) {
    varOrReturn(resPath, resolvePathExpr<res::ValueDecl>(ctx, *path));

    const res::Decl *decl = resPath->fragments.back()->decl;
    bool isFunctionDecl = decl->getAs<res::FunctionDecl>();

    if (decl->getAs<res::TypeParamDecl>())
      return err::unexpectedTypeParam(resPath->location).report(reporter);

    if (decl->getAs<res::StructDecl>())
      return err::expectedInstance(resPath->location)
          .with(decl->identifier)
          .report(reporter);

    if (resPath->fragments.size() > 1 && !isFunctionDecl)
      return err::memberFnLookupFailed(resPath->location)
          .with(decl->identifier)
          .with(resPath->fragments[resPath->fragments.size() - 2]
                    ->decl->identifier)
          .report(reporter);

    auto *outType = resPath->getType()->getAs<res::OutParamType>();

    if (functionInfo && functionInfo->lambda && !isFunctionDecl) {
      res::Decl *insideDecl =
          lexicalScope->lookupDecl<res::Decl>(decl->identifier);
      res::Decl *outsideDecl =
          functionInfo->lambdaParamScope->parent->lookupDecl<res::Decl>(
              decl->identifier);

      if (outsideDecl == insideDecl) {
        if (outType)
          return err::outParamCapture(resPath->location)
              .with(decl->identifier)
              .report(reporter);

        auto *lambda = functionInfo->lambda;

        auto *field =
            lambda->lambda->lookupDecl<res::FieldDecl>(decl->identifier);
        if (!field) {
          field = ctx.create<res::FieldDecl>(
              lambda->location, resPath->getType(), decl->identifier);
          lambda->lambda->insertDecl(field);
          functionInfo->pendingCaptureInits.emplace_back(&expr);
        }

        auto *selfDre = ctx.create<res::PathExpr>(
            std::vector<res::DeclRefExpr *>{ctx.create<res::DeclRefExpr>(
                lambda->location, lambda->method->params[0]->getType(),
                *lambda->method->params[0], res::Expr::Kind::Lvalue)});

        auto *fieldDre =
            ctx.create<res::DeclRefExpr>(lambda->location, field->getType(),
                                         *field, res::Expr::Kind::Lvalue);

        return ctx.create<res::MemberExpr>(lambda->location, selfDre, fieldDre);
      }
    }

    if (outType)
      return ctx.create<res::ImplicitDerefExpr>(
          resPath->location, outType->getParamType(), resPath);

    return resPath;
  }

  llvm_unreachable("unexpected expression");
}

res::Block *Sema::resolveBlock(res::Context &ctx, const ast::Block &block) {
  std::vector<res::Stmt *> resolvedStatements;

  bool error = false;
  int reportUnreachableCount = 0;

  EnterScopeRAII blockScope(this);
  for (auto &&stmt : block.statements) {
    auto *resolvedStmt = resolveStmt(ctx, *stmt);

    error |= !resolvedStatements.emplace_back(resolvedStmt);
    if (error)
      continue;

    if (reportUnreachableCount == 1) {
      wrn::unreachableStmt(stmt->location).report(reporter);
      ++reportUnreachableCount;
    }

    if (dynamic_cast<ast::ReturnStmt *>(stmt.get()))
      ++reportUnreachableCount;
  }

  if (!resolvePendingLambdaBodies() || !checkLambdaCoercions() || error)
    return nullptr;

  return ctx.create<res::Block>(block.location, std::move(resolvedStatements));
}

res::ImplBlock *Sema::resolveImplBlock(res::Context &ctx,
                                       const ast::ImplDecl &decl,
                                       res::StructDecl *parent) {
  varOrReturn(traitInstance, resolveTraitInstance(ctx, decl.trait.get()));

  auto *traitTy = traitInstance->getType()->getAs<res::TraitType>();
  typeMgr.addUpperBound(parent, traitTy);

  auto *resImpl = ctx.create<res::ImplBlock>(decl.location, traitInstance);

  for (auto &&astFunction : decl.functions) {
    auto *traitFn = traitTy->getDecl()->lookupDecl<res::FunctionDecl>(
        astFunction->identifier);
    if (!traitFn) {
      err::memberFnLookupFailed(astFunction->location)
          .with(astFunction->identifier)
          .with(traitTy->getDecl()->identifier)
          .report(reporter);
      continue;
    }

    auto *implFn = resolveFunctionDecl(ctx, *astFunction, parent, traitFn);
    if (!implFn)
      continue;

    auto traitFnTypeParams = traitFn->typeParams;
    auto implTypeParams = implFn->typeParams;

    if (!checkTypeParameterCount(implFn->location, implTypeParams.size(),
                                 traitFnTypeParams.size() - 1))
      continue;

    res::Substitution sub;
    res::Substitution reverseSub;

    for (size_t i = 0; i < implTypeParams.size(); ++i) {
      res::Type *traitParamTy = traitFn->typeParams[i + 1]->getType();
      res::Type *implParamTy = implFn->typeParams[i]->getType();

      auto *checkTy = typeMgr.getNewUninferredType();
      sub[implParamTy] = traitParamTy;
      reverseSub[implParamTy] = checkTy;

      for (auto &&trait : typeMgr.getUpperBounds(implParamTy))
        typeMgr.withObligation(
            checkTy, typeMgr.instantiate(trait, sub)->getAs<res::TraitType>());

      if (const auto &errors = typeMgr.unify(traitParamTy, checkTy);
          !errors.empty()) {

        for (auto &&error : errors)
          err::inferenceError(implFn->typeParams[i]->location)
              .with(error)
              .report(reporter);

        err::stricterParamTy(implFn->typeParams[i]->location)
            .with(traitParamTy->getName())
            .with(implParamTy->getName())
            .report(reporter);
      }
    }

    auto traitSub = typeMgr.extractSubstitutionFrom(traitTy);
    sub[traitFn->typeParams[0]->getType()] = parent->getType();

    res::Type *expectedType = typeMgr.instantiate(
        typeMgr.instantiate(traitFn->getType(), traitSub), sub);
    res::Type *actualType = implFn->getType();

    if (!typeMgr.unify(expectedType, actualType).empty())
      err::fnSignatureMismatch(implFn->location)
          .with(expectedType->getName())
          .with(actualType->getName())
          .report(reporter);

    if (!resImpl->insertDecl(implFn))
      err::alreadyImplementedFn(implFn->location)
          .with(implFn->identifier)
          .with(traitTy->getName())
          .report(reporter);
  }

  return resImpl;
}

res::VarDecl *Sema::resolveVarDecl(res::Context &ctx,
                                   const ast::VarDecl &varDecl) {
  res::Type *declTy = varDecl.type ? resolveType(ctx, *varDecl.type)
                                   : typeMgr.getNewUninferredType();
  if (!declTy)
    return nullptr;

  res::Expr *initializer = nullptr;
  if (varDecl.initializer) {
    varOrReturn(init, resolveExpr(ctx, *varDecl.initializer));

    if (declTy->getAs<res::FunctionType>())
      init = coerceIfLambda(init);

    auto *initTy = init->getType();
    if (!typeMgr.unify(declTy, initTy).empty())
      return err::initTyMismatch(init->location)
          .with(initTy->getName())
          .with(declTy->getName())
          .report(reporter);

    init->setConstantValue(cee->evaluate(*init));
    initializer = init;
  }

  return ctx.create<res::VarDecl>(varDecl.location, declTy, varDecl.identifier,
                                  varDecl.isMutable, initializer);
}

bool Sema::checkTypeParameterCount(SourceLocation loc,
                                   size_t received,
                                   size_t expected) const {
  if (received != expected) {
    err::typeArgCntMismatch(loc).with(expected).with(received).report(reporter);
    return false;
  }

  return true;
}

std::vector<res::TypeParamDecl *> Sema::resolveTypeParamsWithoutBounds(
    res::Context &ctx,
    const std::vector<std::unique_ptr<ast::TypeParamDecl>> &typeParamDecls) {
  std::vector<res::TypeParamDecl *> resTypeParams;

  for (auto &&tp : typeParamDecls) {
    auto *tpTy = typeMgr.getNewUninferredType();
    auto *resTP =
        ctx.create<res::TypeParamDecl>(tp->location, tpTy, tp->identifier);
    typeMgr.unify(tpTy, typeMgr.getTypeParamType(*resTP));
    resTypeParams.emplace_back(resTP);
  }

  return resTypeParams;
}

bool Sema::resolveGenericParamsInCurrentScope(
    res::Context &ctx,
    const std::vector<res::TypeParamDecl *> &resParams,
    const std::vector<std::unique_ptr<ast::TypeParamDecl>> &astParams) {
  bool error = false;

  for (size_t i = 0; i < resParams.size(); ++i) {
    res::TypeParamDecl *resParam = resParams[i];
    error |= !insertDeclToScope(resParam, lexicalScope);

    const auto &restrictions = astParams[i]->restrictions;
    auto traits = resolveTraitInstanceList(ctx, restrictions);
    error |= traits.size() != restrictions.size();

    for (auto &&trait : traits) {
      resParam->traits.emplace_back(trait);
      typeMgr.addUpperBound(resParam,
                            trait->getType()->getAs<res::TraitType>());
    }
  }

  return !error;
}

std::vector<res::TraitInstance *> Sema::resolveTraitInstanceList(
    res::Context &ctx,
    const std::vector<std::unique_ptr<ast::TraitInstance>> &traitInstances) {
  std::vector<res::TraitInstance *> resolvedTraits;

  for (auto &&trait : traitInstances) {
    auto *resTrait = resolveTraitInstance(ctx, trait.get());
    if (!resTrait)
      continue;

    resolvedTraits.emplace_back(resTrait);
  }

  return resolvedTraits;
}

bool Sema::implementsAllNecessaryTraitFunctions(res::Context &ctx,
                                                res::StructDecl *structDecl) {
  bool error = false;

  for (auto &&trait : typeMgr.getUpperBounds(structDecl->getType())) {
    res::DeclContext *implCtx = nullptr;
    for (auto &&impl : structDecl->implBlocks) {
      if (typeMgr.unify(impl->traitInstance->getType(), trait).empty()) {
        implCtx = impl;
        break;
      }
    }
    assert(implCtx && "failed to find impl block");

    for (auto &&fn : trait->getDecl()->getAll<res::FunctionDecl>()) {
      if (fn->body || implCtx->lookupDecl<res::FunctionDecl>(fn->identifier))
        continue;

      err::missingTraitFn(fn->location)
          .with(structDecl->identifier)
          .with(fn->identifier)
          .with(trait->getName())
          .report(reporter);
      error = true;
    }
  }

  return !error;
}

res::FunctionDecl *Sema::resolveFunctionDecl(res::Context &ctx,
                                             const ast::FunctionDecl &decl,
                                             res::Decl *parent,
                                             res::FunctionDecl *implements) {
  EnterScopeRAII typeParamScope(this);

  auto typeParams = resolveTypeParamsWithoutBounds(ctx, decl.typeParameters);
  bool error =
      !resolveGenericParamsInCurrentScope(ctx, typeParams, decl.typeParameters);
  for (auto &&tp : typeParams)
    if (lexicalScope->parent->lookupDecl<res::TypeParamDecl>(tp->identifier)) {
      err::typeParamShadowed(tp->location)
          .with(tp->identifier)
          .report(reporter);
      error = true;
    }

  res::Type *currentSelfType = selfType;
  res::TypeParamDecl *implicitSelf = nullptr;
  if (parent && parent->getAs<res::TraitDecl>()) {
    selfType = typeMgr.getNewUninferredType();
    implicitSelf =
        ctx.create<res::TypeParamDecl>(decl.location, selfType, implicitSelfId);

    typeMgr.unify(selfType, typeMgr.getTypeParamType(*implicitSelf));
    typeMgr.addUpperBound(implicitSelf,
                          parent->getType()->getAs<res::TraitType>());

    insertDeclToScope(implicitSelf, lexicalScope);
  }
  if (implicitSelf)
    typeParams.emplace(typeParams.begin(), implicitSelf);

  std::vector<res::Type *> paramTypes;
  std::vector<res::ParamDecl *> resolvedParams;

  EnterScopeRAII paramScope(this);
  for (auto &&param : decl.params) {
    auto [resolvedParam, err] = resolveParamDecl(ctx, param.get());

    paramTypes.emplace_back(resolvedParam->getType());
    resolvedParams.emplace_back(resolvedParam);

    error |= err;
    error |= !insertDeclToScope(resolvedParam, lexicalScope);
    error |= !checkSelfParameter(resolvedParam, resolvedParams.size() - 1);
  }

  res::Type *retTy =
      decl.type ? resolveType(ctx, *decl.type) : typeMgr.getBuiltinUnitType();
  error |= !retTy;

  selfType = currentSelfType;
  if (error)
    return nullptr;

  auto *fnTy = typeMgr.getFunctionType(std::move(paramTypes), retTy);
  return ctx.create<res::FunctionDecl>(decl.location, fnTy, decl.identifier,
                                       typeParams, std::move(resolvedParams),
                                       parent, implements);
}

res::FunctionDecl *
Sema::resolveFunctionBody(res::Context &ctx,
                          const ast::FunctionDecl &functionDecl,
                          res::FunctionDecl *function) {
  if (!functionDecl.body)
    return function;

  WithFunctionInfoRAII currentFnInfo(this, {function, nullptr, nullptr, {}});

  EnterScopeRAII typeParamScope(this);
  for (auto &&typeParam : function->typeParams)
    insertDeclToScope(typeParam, lexicalScope);

  EnterScopeRAII paramScope(this);
  for (auto &&param : function->params)
    insertDeclToScope(param, lexicalScope);

  auto *body = resolveBlock(ctx, *functionDecl.body);
  if (!body) {
    function->setBody(ctx.create<res::Block>(functionDecl.location,
                                             std::vector<res::Stmt *>{}));
    return nullptr;
  }

  function->setBody(body);
  if (runFlowSensitiveChecks(ctx, *function))
    return nullptr;

  return function;
}

std::pair<res::ParamDecl *, bool>
Sema::resolveParamDecl(res::Context &ctx, const ast::ParamDecl *param) {
  assert((param->type || modifiers & MissingTypeAnnotationsAllowed) &&
         "param without type annotations outside lambda");

  WithModifiersRAII ampAllowed(this, UnaryAmpAllowed);

  res::Type *paramTy = nullptr;
  bool error = false;

  if (param->type) {
    paramTy = resolveType(ctx, *param->type);
    error |= !paramTy;
  }

  if (!paramTy)
    paramTy = typeMgr.getNewUninferredType();

  bool isOutputType = paramTy->getAs<res::OutParamType>();
  if (isOutputType && param->isMutable) {
    err::mutableAmp(param->location).report(reporter);
    error = true;
  }

  return std::make_pair(
      ctx.create<res::ParamDecl>(param->location, paramTy, param->identifier,
                                 param->isMutable || isOutputType),
      error);
}

res::TraitInstance *
Sema::resolveTraitInstance(res::Context &ctx, const ast::TraitInstance *trait) {
  SourceLocation location = trait->location;
  std::string identifier = trait->identifier;
  const auto &typeArguments = trait->typeArguments;

  auto *traitDecl = lexicalScope->lookupDecl<res::TraitDecl>(identifier);
  if (!traitDecl)
    return err::notATrait(location).with(identifier).report(reporter);

  std::vector<res::Type *> resTypeArgs;

  if (!checkTypeParameterCount(location, typeArguments.size(),
                               traitDecl->typeParams.size()))
    return nullptr;

  std::vector<SourceLocation> resTypeArgsLocs;
  for (auto &&typeArg : typeArguments)
    if (auto *resTypeArg = resolveType(ctx, *typeArg.get())) {
      resTypeArgs.emplace_back(resTypeArg);
      resTypeArgsLocs.emplace_back(typeArg->location);
    }

  if (typeArguments.size() != resTypeArgs.size())
    return nullptr;

  auto *traitTy = typeMgr.getTraitType(*traitDecl, resTypeArgs);
  return ctx.create<res::TraitInstance>(location, traitTy, traitDecl,
                                        std::move(resTypeArgs),
                                        std::move(resTypeArgsLocs));
}

res::TraitDecl *Sema::resolveTraitDecl(res::Context &ctx,
                                       const ast::TraitDecl &decl) {
  auto *traitTy = typeMgr.getNewUninferredType();
  auto *traitDecl = ctx.create<res::TraitDecl>(
      decl.location, traitTy, decl.identifier,
      resolveTypeParamsWithoutBounds(ctx, decl.typeParameters));

  std::vector<res::Type *> typeParamTys;
  for (auto &&typeParam : traitDecl->typeParams)
    typeParamTys.emplace_back(typeParam->getType());

  typeMgr.unify(traitTy, typeMgr.getTraitType(*traitDecl, typeParamTys));
  return traitDecl;
}

bool Sema::resolveTraitBody(res::Context &ctx,
                            res::TraitDecl &traitDecl,
                            const ast::TraitDecl &astDecl) {
  EnterScopeRAII typeParamScope(this);
  bool error = !resolveGenericParamsInCurrentScope(ctx, traitDecl.typeParams,
                                                   astDecl.typeParameters);

  auto traits = resolveTraitInstanceList(ctx, astDecl.requirements);
  error |= astDecl.requirements.size() != traits.size();

  for (auto &&trait : traits) {
    traitDecl.traits.emplace_back(trait);
    typeMgr.addUpperBound(&traitDecl,
                          trait->getType()->getAs<res::TraitType>());
  }

  for (auto &&fn : astDecl.traitFunctions)
    error |= !insertDeclToScope(resolveFunctionDecl(ctx, *fn, &traitDecl),
                                &traitDecl);

  return !error;
}

bool Sema::resolveTraitFunctionBodies(res::Context &ctx,
                                      res::TraitDecl &traitDecl,
                                      const ast::TraitDecl &astDecl) {
  EnterScopeRAII typeParamScope(this);
  for (auto &&typeParamDecl : traitDecl.typeParams)
    insertDeclToScope(typeParamDecl, lexicalScope);

  bool error = false;
  int idx = 0;
  for (auto &&fn : traitDecl.getAll<res::FunctionDecl>()) {
    selfType = fn->typeParams[0]->getType();
    error |= !resolveFunctionBody(ctx, *astDecl.traitFunctions[idx], fn);
    selfType = nullptr;
    ++idx;
  }

  return !error;
}

res::StructDecl *Sema::resolveStructDecl(res::Context &ctx,
                                         const ast::StructDecl &decl) {
  auto *structTy = typeMgr.getNewUninferredType();
  auto *structDecl = ctx.create<res::StructDecl>(
      decl.location, structTy, decl.identifier, false, false,
      resolveTypeParamsWithoutBounds(ctx, decl.typeParameters));

  std::vector<res::Type *> typeParamTys;
  for (auto &&typeParam : structDecl->typeParams)
    typeParamTys.emplace_back(typeParam->getType());

  typeMgr.unify(structTy, typeMgr.getStructType(*structDecl, typeParamTys));
  return structDecl;
}

bool Sema::resolveStructBody(res::Context &ctx,
                             res::StructDecl &structDecl,
                             const ast::StructDecl &astDecl) {
  EnterScopeRAII typeParamScope(this);
  bool error = !resolveGenericParamsInCurrentScope(ctx, structDecl.typeParams,
                                                   astDecl.typeParameters);

  std::vector<res::TraitInstance *> traitInstances;

  selfType = structDecl.getType();
  for (auto &&decl : astDecl.decls) {
    if (auto *field = dynamic_cast<ast::FieldDecl *>(decl.get())) {
      res::Type *fieldTy = resolveType(ctx, *field->type);
      if (!fieldTy) {
        error = true;
        continue;
      }

      auto *fieldDecl = ctx.create<res::FieldDecl>(field->location, fieldTy,
                                                   field->identifier);
      error |= !insertDeclToScope(fieldDecl, &structDecl);
      continue;
    }

    if (auto *memberFunction = dynamic_cast<ast::FunctionDecl *>(decl.get())) {
      auto *memberFn = resolveFunctionDecl(ctx, *memberFunction, &structDecl);
      if (!memberFn) {
        error = true;
        continue;
      }

      error |= !insertDeclToScope(memberFn, &structDecl);
    }

    if (auto *implDecl = dynamic_cast<ast::ImplDecl *>(decl.get())) {
      auto *resImpl = resolveImplBlock(ctx, *implDecl, &structDecl);
      if (!resImpl) {
        error = true;
        continue;
      }

      res::Type *resImplTraitTy = resImpl->traitInstance->getType();
      for (auto &&implBlock : structDecl.implBlocks) {
        res::Type *implBlockTraitTy = implBlock->traitInstance->getType();
        if (!typeMgr.unify(resImplTraitTy, implBlockTraitTy).empty())
          continue;

        err::alreadyImplementedTrait(resImpl->location)
            .with(resImplTraitTy->getName())
            .with(structDecl.identifier)
            .report(reporter);

        error = true;
        break;
      }

      if (resImpl->decls.size() != implDecl->functions.size())
        error = true;

      structDecl.implBlocks.emplace_back(resImpl);
    }
  }

  auto impls = structDecl.implBlocks;
  for (auto &&impl : impls) {
    auto *traitTy = impl->traitInstance->getType()->getAs<res::TraitType>();
    res::Substitution sub = typeMgr.extractSubstitutionFrom(traitTy);

    for (auto &&moreSpecificImpl : impls) {
      res::Type *moreSpecificTy =
          typeMgr.instantiate(moreSpecificImpl->traitInstance->getType(), sub);

      if (typeMgr.moreGeneral(traitTy, moreSpecificTy)) {
        err::conflictingTrait(impl->location)
            .with(traitTy->getName())
            .with(moreSpecificTy->getName())
            .report(reporter);
        error = true;
      }
    }

    for (res::Type *req :
         typeMgr.getUpperBounds(impl->traitInstance->decl->getType())) {
      req = typeMgr.instantiate(req, sub);

      bool found = false;
      for (auto &&impl : impls)
        found |= typeMgr.unify(impl->traitInstance->getType(), req).empty();

      if (!found) {
        err::missingRequirement(impl->location)
            .with(traitTy->getName())
            .with(req->getName())
            .report(reporter);
        error = true;
      }
    }
  }

  selfType = nullptr;
  return !error;
}

bool Sema::resolveMemberFunctionBodies(res::Context &ctx,
                                       res::StructDecl &decl,
                                       const ast::StructDecl &astDecl) {
  EnterScopeRAII typeParamScope(this);
  for (auto &&typeParamDecl : decl.typeParams)
    insertDeclToScope(typeParamDecl, lexicalScope);

  selfType = decl.getType();
  bool error = false;

  for (auto &&memberDecl : astDecl.decls) {
    if (const auto *memberFn =
            dynamic_cast<const ast::FunctionDecl *>(memberDecl.get())) {
      error |= !resolveFunctionBody(
          ctx, *memberFn,
          decl.lookupDecl<res::FunctionDecl>(memberFn->identifier));
      continue;
    }

    if (const auto *implBlock =
            dynamic_cast<const ast::ImplDecl *>(memberDecl.get())) {
      auto *traitInstance = resolveTraitInstance(ctx, implBlock->trait.get());

      res::ImplBlock *resImplBlock = nullptr;
      for (auto &&impl : decl.implBlocks) {
        if (typeMgr
                .unify(impl->traitInstance->getType(), traitInstance->getType())
                .empty()) {
          resImplBlock = impl;
          break;
        }
      }
      assert(resImplBlock && "failed to find resolved impl block");

      for (auto &&fn : implBlock->functions)
        error |= !resolveFunctionBody(
            ctx, *fn,
            resImplBlock->lookupDecl<res::FunctionDecl>(fn->identifier));
    }
  }

  selfType = nullptr;
  return !error;
}

res::Context *Sema::resolveAST() {
  EnterScopeRAII globalScope(this);
  bool error = false;

  std::vector<std::pair<res::Decl *, const ast::Decl *>> resDecls;

  insertDeclToScope(createBuiltinGc(ctx), lexicalScope);
  for (auto &&decl : ast->decls) {
    res::Decl *rd = nullptr;
    if (const auto *sd = dynamic_cast<const ast::StructDecl *>(decl.get()))
      rd = resolveStructDecl(ctx, *sd);

    if (const auto *td = dynamic_cast<const ast::TraitDecl *>(decl.get()))
      rd = resolveTraitDecl(ctx, *td);

    if (!rd)
      continue;

    error |= !insertDeclToScope(rd, lexicalScope);
    resDecls.emplace_back(rd, decl.get());
  }

  for (auto &&[resDecl, astDecl] : resDecls) {
    if (auto *resSD = resDecl->getAs<res::StructDecl>())
      error |= !resolveStructBody(
          ctx, *resSD, *static_cast<const ast::StructDecl *>(astDecl));

    if (auto *resTD = resDecl->getAs<res::TraitDecl>())
      error |= !resolveTraitBody(ctx, *resTD,
                                 *static_cast<const ast::TraitDecl *>(astDecl));
  }

  insertDeclToScope(createBuiltinPrintln(ctx), lexicalScope);
  for (auto &&fn : ast->functions) {
    auto *rf = resolveFunctionDecl(ctx, *fn);
    error |= !insertDeclToScope(rf, lexicalScope);
    error |= hasBuiltinFunctionCollisions(rf);
    resDecls.emplace_back(rf, fn);
  }

  error |= hasSelfContainingStructs(ctx);
  error |= !checkTraitInstances(ctx);
  if (error)
    return nullptr;

  for (auto &&[resDecl, astDecl] : resDecls) {
    if (auto *rs = resDecl->getAs<res::StructDecl>()) {
      error |= !resolveMemberFunctionBodies(
          ctx, *rs, *static_cast<const ast::StructDecl *>(astDecl));
      error |= !implementsAllNecessaryTraitFunctions(ctx, rs);
    }

    if (auto *rt = resDecl->getAs<res::TraitDecl>())
      error |= !resolveTraitFunctionBodies(
          ctx, *rt, *static_cast<const ast::TraitDecl *>(astDecl));

    if (auto *resFN = resDecl->getAs<res::FunctionDecl>())
      error |= !resolveFunctionBody(
          ctx, *static_cast<const ast::FunctionDecl *>(astDecl), resFN);
  }

  if (error)
    return nullptr;

  return &ctx;
}

bool Sema::hasBuiltinFunctionCollisions(const res::FunctionDecl *fnDecl) {
  if (!fnDecl)
    return false;

  if (fnDecl->identifier == "main") {
    if (!fnDecl->getType()
             ->getAs<res::FunctionType>()
             ->getReturnType()
             ->getAs<res::BuiltinUnitType>()) {
      err::wrongMainReturnTy(fnDecl->location).report(reporter);
      return true;
    }

    if (!fnDecl->params.empty()) {
      err::wrongMainArgCount(fnDecl->location).report(reporter);
      return true;
    }

    if (!fnDecl->typeParams.empty()) {
      err::mainIsGeneric(fnDecl->location).report(reporter);
      return true;
    }
  }

  if (fnDecl->identifier == "printf") {
    err::reservedPrintf(fnDecl->location).report(reporter);
    return true;
  }

  return false;
}

bool Sema::checkSelfParameter(res::ParamDecl *param, size_t idx) {
  if (param->identifier != selfParamId)
    return true;

  if (!selfType) {
    err::selfParamNotAllowed(param->location).report(reporter);
    return false;
  }

  if (idx != 0) {
    err::selfWrongPosition(param->location).report(reporter);
    return false;
  }

  auto *type = param->getType();
  auto *outTy = type->getAs<res::OutParamType>();

  if (!typeMgr.unify(type, selfType).empty() &&
      !(outTy && typeMgr.unify(outTy->getParamType(), selfType).empty())) {
    err::selfWrongType(param->location).report(reporter);
    return false;
  }

  return true;
}

bool Sema::hasSelfContainingStructs(res::Context &ctx) {
  std::stack<res::StructType *> worklist;
  std::set<res::StructDecl *> selfContaining;

  for (auto &&sd : ctx.getStructs()) {
    std::vector<res::StructType *> seen;
    worklist.emplace(sd->getType()->getAs<res::StructType>());

    while (!worklist.empty()) {
      res::StructType *ty = worklist.top();
      worklist.pop();

      res::StructDecl *decl = ty->getDecl();
      if (decl->identifier == gcId)
        continue;

      for (auto &&seenTy : seen)
        if (typeMgr.unify(seenTy, ty).empty())
          selfContaining.emplace(decl);

      if (selfContaining.count(decl))
        continue;

      seen.emplace_back(ty);

      res::Substitution sub = typeMgr.extractSubstitutionFrom(ty);
      for (auto &&field : decl->getAll<res::FieldDecl>())
        if (auto *structTy = typeMgr.instantiate(field->getType(), sub)
                                 ->getAs<res::StructType>())
          worklist.emplace(structTy);
    }
  }

  for (auto &&sd : selfContaining)
    err::selfContainingStruct(sd->location)
        .with(sd->identifier)
        .report(reporter);

  return !selfContaining.empty();
}

bool Sema::checkTraitInstances(res::Context &ctx) {
  bool error = false;

  for (auto &&traitInstance : ctx.getTraitInstances()) {
    auto sub = typeMgr.extractSubstitutionFrom(traitInstance->getType());

    for (size_t i = 0; i < traitInstance->typeArgs.size(); ++i) {
      auto *subTy = typeMgr.getNewUninferredType();

      for (auto &&trait : typeMgr.getUpperBounds(
               traitInstance->decl->typeParams[i]->getType()))
        typeMgr.withObligation(
            subTy, typeMgr.instantiate(trait, sub)->getAs<res::TraitType>());

      if (const auto &msg = typeMgr.unify(traitInstance->typeArgs[i], subTy);
          !msg.empty()) {
        for (auto &&error : msg)
          err::inferenceError(traitInstance->typeLocations[i])
              .with(error)
              .report(reporter);

        error = true;
      }
    }
  }

  return !error;
}
} // namespace yl
