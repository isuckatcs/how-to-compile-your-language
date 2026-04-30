#include <cassert>
#include <map>
#include <set>
#include <sstream>
#include <stack>

#include "cfg.h"
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
  if (typeMgr.getType(&fn)
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

  if (exitReached || returnCount == 0) {
    report(fn.location,
           returnCount > 0 ? "expected function to return a value on every path"
                           : "expected function to return a value");
  }

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
  std::vector<std::pair<SourceLocation, std::string>> pendingErrors;

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
          if (!decl->isMutable && tmp[decl] != State::Unassigned) {
            std::string msg = '\'' + decl->identifier + "' cannot be mutated";
            pendingErrors.emplace_back(assignment->location, std::move(msg));
          }

          tmp[decl] = State::Assigned;
          continue;
        }

        if (const auto *path = dynamic_cast<const res::PathExpr *>(stmt)) {

          for (auto &&fragment : path->fragments) {
            for (auto &&typeArg : fragment->typeArgs) {
              if (!typeArg->getRootType()->getAs<res::UninferredType>())
                continue;

              std::string msg = "explicit type annotations are needed to infer "
                                "the type of '" +
                                fragment->decl->identifier + "'";
              pendingErrors.emplace_back(fragment->location, std::move(msg));
            }
          }

          const auto *dre = path->fragments.back();
          const auto *var = dre->decl->getAs<res::VarDecl>();
          if (var && tmp[var] != State::Assigned) {
            std::string msg = '\'' + var->identifier + "' is not initialized";
            pendingErrors.emplace_back(dre->location, std::move(msg));
          }

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
    if (s == State::Unassigned &&
        typeMgr.getType(d)->getAs<res::UninferredType>())
      report(d->location, "the type of '" + d->identifier + "' is unknown");

  for (auto &&[loc, msg] : pendingErrors)
    report(loc, msg);

  return !pendingErrors.empty();
}

bool Sema::insertDeclToScope(res::Decl *decl, res::DeclContext *scope) {
  if (!decl)
    return false;

  if (!scope->insertDecl(decl)) {
    report(decl->location, "redeclaration of '" + decl->identifier + '\'');
    return false;
  }

  return true;
}

res::FunctionDecl *Sema::createBuiltinPrintln(res::Context &ctx) {
  SourceLocation loc{nullptr, 0, 0};

  auto *numTy = typeMgr.getBuiltinNumberType();
  auto *param = ctx.createAndBind<res::ParamDecl>(numTy, loc, "n", false);

  auto *fn = ctx.create<res::FunctionDecl>(
      loc, "println", std::vector<res::TypeParamDecl *>{}, std::vector{param});
  fn->setBody(ctx.create<res::Block>(loc, std::vector<res::Stmt *>()));

  typeMgr.bind(fn,
               typeMgr.getFunctionType({numTy}, typeMgr.getBuiltinUnitType()));
  return fn;
};

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
        return report(parsedType.location,
                      "'Self' is only allowed inside structs and traits");
      return selfType;
    }
  }

  if (const auto *udt =
          dynamic_cast<const ast::UserDefinedType *>(&parsedType)) {
    res::Decl *decl = lexicalScope->lookupDecl<res::TypeDecl>(udt->identifier);
    if (!decl)
      return report(udt->location,
                    "failed to resolve type '" + udt->identifier + "'");

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
      resolvedTypeArgs.emplace_back(resolvedType);
    }

    return typeMgr.getStructType(*sd, std::move(resolvedTypeArgs));
  }

  if (const auto *function =
          dynamic_cast<const ast::FunctionType *>(&parsedType)) {
    std::vector<res::Type *> args;
    for (auto &&astArg : function->args) {
      ResolutionContextRAII paramCtx(this, ParamList);
      varOrReturn(argTy, resolveType(ctx, *astArg));
      args.emplace_back(argTy);
    }

    varOrReturn(retTy, resolveType(ctx, *function->ret));
    return typeMgr.getFunctionType(std::move(args), retTy);
  }

  if (const auto *out = dynamic_cast<const ast::OutParamType *>(&parsedType)) {
    if (!(resolutionContext & ParamList))
      return report(out->location, "only parameters can have '&' type");

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

  auto *rhsTy = typeMgr.getType(rhs);
  if (rhsTy->getAs<res::UninferredType>())
    return report(rhs->location,
                  "type of operand to unary operator is unknown");

  if (unary.op == TokenKind::Excl && !rhsTy->getAs<res::BuiltinBoolType>())
    return report(rhs->location, "expected 'bool' operand");

  if (unary.op == TokenKind::Minus && !rhsTy->getAs<res::BuiltinNumberType>())
    return report(rhs->location, "expected 'number' operand");

  if (unary.op == TokenKind::Amp) {
    if (!(resolutionContext & ArgList))
      return report(unary.location,
                    "'&' can only be used to pass arguments to '&' parameters");

    if (!rhs->isMutable())
      return report(rhs->location,
                    "only mutable lvalues can be passed to '&' parameters");

    rhsTy = typeMgr.getOutParamType(rhsTy);
  }

  return ctx.createAndBind<res::UnaryOperator>(rhsTy, unary.location, unary.op,
                                               rhs);
}

res::BinaryOperator *
Sema::resolveBinaryOperator(res::Context &ctx,
                            const ast::BinaryOperator &binop) {
  varOrReturn(lhs, resolveExpr(ctx, *binop.lhs));
  varOrReturn(rhs, resolveExpr(ctx, *binop.rhs));

  auto *lhsTy = typeMgr.getType(lhs);
  auto *rhsTy = typeMgr.getType(rhs);

  if (auto *uninferredLHS = lhsTy->getAs<res::UninferredType>();
      uninferredLHS || rhsTy->getAs<res::UninferredType>())
    return report((uninferredLHS ? lhs : rhs)->location,
                  "type of " + std::string(uninferredLHS ? "LHS" : "RHS") +
                      " to binary operator is unknown");

  const auto &loc = binop.location;
  TokenKind op = binop.op;

  bool isLogicalOp = op == TokenKind::AmpAmp || op == TokenKind::PipePipe;
  bool isNumbericOp = !isLogicalOp && op != TokenKind::EqualEqual;

  bool typeError = !typeMgr.unify(lhsTy, rhsTy).empty();
  typeError |= isLogicalOp && !rhsTy->getAs<res::BuiltinBoolType>();
  typeError |= isNumbericOp && !rhsTy->getAs<res::BuiltinNumberType>();
  if (typeError)
    return report(loc, "incompatible operands to binary operator ('" +
                           lhsTy->getName() + "' and '" + rhsTy->getName() +
                           "')");

  res::Type *resTy = (op == TokenKind::EqualEqual || op == TokenKind::Lt ||
                      op == TokenKind::Gt)
                         ? typeMgr.getBuiltinBoolType()
                         : lhsTy;
  return ctx.createAndBind<res::BinaryOperator>(resTy, loc, binop.op, lhs, rhs);
}

res::GroupingExpr *
Sema::resolveGroupingExpr(res::Context &ctx,
                          const ast::GroupingExpr &grouping) {
  varOrReturn(expr, resolveExpr(ctx, *grouping.expr));
  return ctx.createAndBind<res::GroupingExpr>(typeMgr.getType(expr),
                                              grouping.location, expr);
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
      parentTy = typeMgr.getType(resFragments.back());

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

  return ctx.createAndBind<res::PathExpr>(
      ctx.getTypeMgr().getType(resFragments.back()), std::move(resFragments));
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

    res::TraitType *traitTy =
        typeMgr.getType(traitInstance)->getAs<res::TraitType>();

    auto *checkTy =
        typeMgr.withObligation(typeMgr.getNewUninferredType(), traitTy);

    if (!typeMgr.unify(parent, checkTy).empty())
      return report(impl->location, "'" + parent->getName() +
                                        "' doesn't implement trait '" +
                                        traitTy->getName() + "'");

    if (auto *decl = lookupSymbolWithFallback<Hint>(traitInstance->decl, dre))
      return createDeclRefExpr(ctx, dre, parent, decl, traitTy);

    return report(dre->location, "trait '" + traitInstance->decl->identifier +
                                     "' has no member '" + dre->identifier +
                                     "'");
  }

  if (!parent && dre->identifier == selfTypeId) {
    auto *structTy = selfType ? selfType->getAs<res::StructType>() : nullptr;
    if (structTy)
      return createDeclRefExpr(ctx, dre, nullptr, structTy->getDecl(), nullptr);

    auto *paramTy = selfType ? selfType->getAs<res::TypeParamType>() : nullptr;
    if (paramTy)
      return createDeclRefExpr(ctx, dre, nullptr, paramTy->decl, nullptr);

    return report(dre->location,
                  "'Self' is only allowed inside structs and traits");
  }

  if (!parent) {
    if (auto *decl = lookupSymbolWithFallback<Hint>(lexicalScope, dre))
      return createDeclRefExpr(ctx, dre, nullptr, decl, nullptr);

    return report(dre->location, "symbol '" + dre->identifier + "' not found");
  }

  auto *structTy = parent->getAs<res::StructType>();
  if (structTy)
    if (auto *decl = lookupSymbolWithFallback<Hint>(structTy->getDecl(), dre))
      return createDeclRefExpr(ctx, dre, parent, decl, nullptr);

  if (!structTy && !parent->getAs<res::TypeParamType>())
    return report(dre->location,
                  "cannot access member of '" + parent->getName() + '\'');

  res::Decl *decl = nullptr;
  res::TraitType *trait = nullptr;

  auto traits = typeMgr.getUpperBounds(parent);
  for (auto &&implementedTrait : traits) {
    if (auto *traitDecl =
            lookupSymbolWithFallback<Hint>(implementedTrait->getDecl(), dre)) {
      if (decl)
        return report(dre->location, "ambigous member function reference");

      decl = traitDecl;
      trait = implementedTrait;
    }
  }

  if (!decl)
    return report(dre->location, "failed to find member '" + dre->identifier +
                                     "' in '" + parent->getName() + "'");
  return createDeclRefExpr(ctx, dre, parent, decl, trait);
}

res::DeclRefExpr *Sema::createDeclRefExpr(res::Context &ctx,
                                          const ast::DeclRefExpr *dre,
                                          res::Type *parentTy,
                                          res::Decl *decl,
                                          res::TraitType *trait) {
  res::Type *declTy = typeMgr.getType(decl);
  declTy = typeMgr.instantiate(declTy, typeMgr.extractSubstitutionFrom(trait));
  declTy =
      typeMgr.instantiate(declTy, typeMgr.extractSubstitutionFrom(parentTy));

  res::Expr::Kind kind = res::Expr::Kind::Lvalue;
  if (decl->getAs<res::FunctionDecl>() || decl->getAs<res::TypeDecl>() ||
      decl->getAs<res::TraitDecl>())
    kind = res::Expr::Kind::Rvalue;
  else if (decl->getAs<res::ValueDecl>()->isMutable)
    kind = res::Expr::Kind::MutLvalue;

  res::Substitution sub;
  std::vector<res::TypeParamDecl *> typeParams = decl->typeParams;
  std::vector<res::Type *> typeArgs;

  for (auto &&typeParam : typeParams) {
    bool isImplicitSelf = typeParam == typeParams.front() &&
                          typeParam->identifier == implicitSelfId;

    res::Type *typeParamTy = typeMgr.getType(typeParam);
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
      return report(typeArgList->location,
                    "'" + decl->identifier + "' is not a generic");

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
          report(astArg->location, error);

        return nullptr;
      }

      ++idx;
    }
  }

  return ctx.createAndBind<res::DeclRefExpr>(typeMgr.instantiate(declTy, sub),
                                             dre->location, *decl, kind,
                                             typeArgs, parentTy, trait);
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
    ResolutionContextRAII callCtx(this, Call);
    callee = resolveExpr(ctx, *call.callee);
    if (!callee)
      return nullptr;
  }

  res::Type *calleeType = typeMgr.getType(callee);
  auto *fnType = calleeType->getAs<res::FunctionType>();
  if (!fnType)
    return report(call.location, "calling expression of type '" +
                                     calleeType->getName() + '\'');

  if (auto *me = dynamic_cast<res::MemberExpr *>(callee)) {
    res::Expr *base = me->base;
    res::DeclRefExpr *member = me->member;

    if (auto *function = member->decl->getAs<res::FunctionDecl>()) {
      res::ParamDecl *selfParam =
          function->params.empty() ? nullptr : function->params[0];
      if (!selfParam || selfParam->identifier != selfParamId)
        return report(call.location,
                      "class level methods cannot be called on an instance");

      SourceLocation baseLoc = base->location;
      res::Expr::Kind baseKind = base->kind;
      res::Type *baseTy = typeMgr.getType(base);

      selfArg = base;
      if (typeMgr.getType(selfParam)->getAs<res::OutParamType>()) {
        if (selfArg->isLvalue() && !selfArg->isMutable())
          return report(baseLoc, "expected mutable struct instance");

        selfArg = ctx.createAndBind<res::UnaryOperator>(
            typeMgr.getOutParamType(baseTy), baseLoc, TokenKind::Amp, selfArg);
      }

      res::DeclRefExpr *baseDre = nullptr;
      if (auto *structTy = baseTy->getAs<res::StructType>())
        baseDre = ctx.createAndBind<res::DeclRefExpr>(
            structTy, baseLoc, *structTy->getDecl(), baseKind,
            structTy->getTypeArgs());

      if (auto *typeParamTy = baseTy->getAs<res::TypeParamType>())
        baseDre = ctx.createAndBind<res::DeclRefExpr>(
            typeParamTy, baseLoc, *typeParamTy->decl, baseKind);

      std::vector<res::DeclRefExpr *> fragments = {baseDre, member};
      callee = ctx.createAndBind<res::PathExpr>(fnType, std::move(fragments));
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
    return report(call.location,
                  "wrong number of arguments in function call, expected " +
                      std::to_string(fnTypeArgCnt - resolvedArgCnt) +
                      ", but received " + std::to_string(astArgCnt));

  for (auto &&arg : call.arguments) {
    ResolutionContextRAII argCtx(this, ArgList);
    varOrReturn(resolvedArg, resolveExpr(ctx, *arg));

    res::Type *expectedTy = argTypes[resolvedArgs.size()];
    res::Type *actualTy = typeMgr.getType(resolvedArg);

    if (const auto &errors = typeMgr.unify(actualTy, expectedTy);
        !errors.empty()) {
      for (auto &&error : errors)
        report(resolvedArg->location, error);
      return nullptr;
    }

    cee->evaluate(*resolvedArg);
    resolvedArgs.emplace_back(resolvedArg);
  }

  return ctx.createAndBind<res::CallExpr>(
      fnType->getReturnType(), call.location, callee, std::move(resolvedArgs));
}

res::StructInstantiationExpr *Sema::resolveStructInstantiation(
    res::Context &ctx,
    const ast::StructInstantiationExpr &structInstantiation) {
  varOrReturn(path, resolvePathExpr<res::StructDecl>(
                        ctx, *structInstantiation.structRef));

  if (!path->fragments.back()->decl->getAs<res::StructDecl>())
    return report(path->location, "expected struct declaration to instantiate");

  auto *structTy = typeMgr.getType(path)->getAs<res::StructType>();
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
      report(loc, "field '" + std::string{id} + "' is already initialized");
      error = true;
      continue;
    }

    res::FieldDecl *fieldDecl = fields[id];
    if (!fieldDecl) {
      report(loc, "'" + sd->identifier + "' has no field named '" +
                      std::string{id} + "'");
      error = true;
      continue;
    }

    auto *resolvedInitExpr = resolveExpr(ctx, *initStmt->initializer);
    if (!resolvedInitExpr) {
      error = true;
      continue;
    }

    res::Type *initTy = typeMgr.getType(resolvedInitExpr);
    res::Type *fieldTy = typeMgr.instantiate(
        typeMgr.getType(fieldDecl), typeMgr.extractSubstitutionFrom(structTy));

    if (const auto &msg = typeMgr.unify(initTy, fieldTy); !msg.empty()) {
      for (auto &&error : msg) {
        report(resolvedInitExpr->location, error);
      }
      error = true;
      continue;
    }

    inits[id] = resolvedFieldInits.emplace_back(
        ctx.create<res::FieldInitStmt>(loc, fieldDecl, resolvedInitExpr));
  }

  for (auto &&fieldDecl : sd->getAll<res::FieldDecl>()) {
    if (!inits.count(fieldDecl->identifier)) {
      report(structInstantiation.location,
             "field '" + fieldDecl->identifier + "' is not initialized");
      error = true;
      continue;
    }

    cee->evaluate(*inits[fieldDecl->identifier]->initializer);
  }

  if (error)
    return nullptr;

  return ctx.createAndBind<res::StructInstantiationExpr>(
      structTy, structInstantiation.location, path,
      std::move(resolvedFieldInits));
}

res::MemberExpr *Sema::resolveMemberExpr(res::Context &ctx,
                                         const ast::MemberExpr &memberExpr) {
  varOrReturn(base, resolveExpr(ctx, *memberExpr.base));
  varOrReturn(memberDre,
              resolveDeclRefExpr<res::ValueDecl>(ctx, typeMgr.getType(base),
                                                 memberExpr.member.get()));

  if (memberDre->decl->getAs<res::FunctionDecl>() &&
      !(resolutionContext & Call))
    return report(memberExpr.location, "expected to call method");

  return ctx.createAndBind<res::MemberExpr>(
      typeMgr.getType(memberDre), memberExpr.location, base, memberDre);
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
  if (!typeMgr.unify(typeMgr.getType(cond), typeMgr.getBuiltinBoolType())
           .empty())
    return report(cond->location, "expected 'bool' in condition");

  varOrReturn(trueBlock, resolveBlock(ctx, *ifStmt.trueBlock));

  res::Block *falseBlock = nullptr;
  if (ifStmt.falseBlock) {
    falseBlock = resolveBlock(ctx, *ifStmt.falseBlock);
    if (!falseBlock)
      return nullptr;
  }

  cee->evaluate(*cond);
  return ctx.create<res::IfStmt>(ifStmt.location, cond, trueBlock, falseBlock);
}

res::WhileStmt *Sema::resolveWhileStmt(res::Context &ctx,
                                       const ast::WhileStmt &whileStmt) {
  varOrReturn(cond, resolveExpr(ctx, *whileStmt.condition));
  if (!typeMgr.unify(typeMgr.getType(cond), typeMgr.getBuiltinBoolType())
           .empty())
    return report(cond->location, "expected 'bool' in condition");

  varOrReturn(body, resolveBlock(ctx, *whileStmt.body));

  cee->evaluate(*cond);
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
    return report(lhs->location, "cannot assign to rvalue");

  auto *lhsTy = typeMgr.getType(lhs);
  auto *rhsTy = typeMgr.getType(rhs);

  if (const auto &errors = typeMgr.unify(lhsTy, rhsTy); !errors.empty()) {
    for (auto &&error : errors)
      report(rhs->location, error);

    return report(rhs->location, "expected to assign '" + lhsTy->getName() +
                                     "' but received '" + rhsTy->getName() +
                                     "' instead");
  }

  cee->evaluate(*rhs);
  return ctx.create<res::Assignment>(assignment.location, lhs, rhs);
}

res::ReturnStmt *Sema::resolveReturnStmt(res::Context &ctx,
                                         const ast::ReturnStmt &returnStmt) {
  assert(currentFunction && "return stmt outside a function");

  auto *fnTy = typeMgr.getType(currentFunction)->getAs<res::FunctionType>();

  auto *retTy = fnTy->getReturnType();
  if (!retTy->getAs<res::BuiltinUnitType>() && !returnStmt.expr)
    return report(returnStmt.location, "expected a return value");

  res::Expr *expr = nullptr;
  if (returnStmt.expr) {
    expr = resolveExpr(ctx, *returnStmt.expr);
    if (!expr)
      return nullptr;

    res::Type *exprTy = typeMgr.getType(expr);
    if (!typeMgr.unify(retTy, exprTy).empty()) {
      exprTy = typeMgr.getType(expr);
      retTy = fnTy->getReturnType();

      return report(expr->location, "cannot return '" + exprTy->getName() +
                                        "' from a function returning '" +
                                        retTy->getName() + "'");
    }

    cee->evaluate(*expr);
  }

  return ctx.create<res::ReturnStmt>(returnStmt.location, expr);
}

res::Expr *Sema::resolveExpr(res::Context &ctx, const ast::Expr &expr) {
  if (const auto *number = dynamic_cast<const ast::NumberLiteral *>(&expr))
    return ctx.createAndBind<res::NumberLiteral>(typeMgr.getBuiltinNumberType(),
                                                 number->location,
                                                 std::stod(number->value));

  if (const auto *boolLiteral = dynamic_cast<const ast::BoolLiteral *>(&expr))
    return ctx.createAndBind<res::BoolLiteral>(typeMgr.getBuiltinBoolType(),
                                               boolLiteral->location,
                                               boolLiteral->value == "true");

  if (const auto *unit = dynamic_cast<const ast::UnitLiteral *>(&expr))
    return ctx.createAndBind<res::UnitLiteral>(typeMgr.getBuiltinUnitType(),
                                               unit->location);

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

  if (const auto *path = dynamic_cast<const ast::PathExpr *>(&expr)) {
    varOrReturn(resPath, resolvePathExpr<res::ValueDecl>(ctx, *path));

    const res::Decl *decl = resPath->fragments.back()->decl;

    if (decl->getAs<res::TypeParamDecl>())
      return report(resPath->location, "expected value, found type parameter");

    if (decl->getAs<res::StructDecl>())
      return report(resPath->location,
                    "expected an instance of '" + decl->identifier + '\'');

    if (resPath->fragments.size() > 1 && !decl->getAs<res::FunctionDecl>())
      return report(resPath->location,
                    "failed to find member function '" + decl->identifier +
                        "' in '" +
                        resPath->fragments[resPath->fragments.size() - 2]
                            ->decl->identifier +
                        "'");

    auto *outType = typeMgr.getType(resPath)->getAs<res::OutParamType>();
    if (outType)
      return ctx.createAndBind<res::ImplicitDerefExpr>(
          outType->getParamType(), resPath->location, resPath);

    return resPath;
  }

  if (const auto *memberExpr = dynamic_cast<const ast::MemberExpr *>(&expr))
    return resolveMemberExpr(ctx, *memberExpr);

  llvm_unreachable("unexpected expression");
}

res::Block *Sema::resolveBlock(res::Context &ctx, const ast::Block &block) {
  std::vector<res::Stmt *> resolvedStatements;

  bool error = false;
  int reportUnreachableCount = 0;

  ScopeRAII blockScope(this);
  for (auto &&stmt : block.statements) {
    auto *resolvedStmt = resolveStmt(ctx, *stmt);

    error |= !resolvedStatements.emplace_back(resolvedStmt);
    if (error)
      continue;

    if (reportUnreachableCount == 1) {
      report(stmt->location, "unreachable statement", true);
      ++reportUnreachableCount;
    }

    if (dynamic_cast<ast::ReturnStmt *>(stmt.get()))
      ++reportUnreachableCount;
  }

  if (error)
    return nullptr;

  return ctx.create<res::Block>(block.location, std::move(resolvedStatements));
}

res::ImplDecl *Sema::resolveImplDecl(res::Context &ctx,
                                     const ast::ImplDecl &decl,
                                     res::StructDecl *parent) {
  varOrReturn(traitInstance, resolveTraitInstance(ctx, decl.trait.get()));

  auto *traitTy = typeMgr.getType(traitInstance)->getAs<res::TraitType>();
  typeMgr.addUpperBound(parent, traitTy);

  auto *resDecl = ctx.createAndBind<res::ImplDecl>(
      traitTy, decl.location, traitTy->getName(), traitInstance);

  for (auto &&astFunction : decl.functions) {
    auto *traitFn = traitTy->getDecl()->lookupDecl<res::FunctionDecl>(
        astFunction->identifier);
    if (!traitFn) {
      report(astFunction->location, "'" + traitTy->getDecl()->identifier +
                                        "' has no member function called '" +
                                        astFunction->identifier + "'");
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
      res::Type *traitParamTy = typeMgr.getType(traitFn->typeParams[i + 1]);
      res::Type *implParamTy = typeMgr.getType(implFn->typeParams[i]);

      auto *checkTy = typeMgr.getNewUninferredType();
      sub[implParamTy] = traitParamTy;
      reverseSub[implParamTy] = checkTy;

      for (auto &&trait : typeMgr.getUpperBounds(implParamTy))
        typeMgr.withObligation(
            checkTy, typeMgr.instantiate(trait, sub)->getAs<res::TraitType>());

      if (const auto &errors = typeMgr.unify(traitParamTy, checkTy);
          !errors.empty()) {

        for (auto &&error : errors)
          report(implFn->typeParams[i]->location, error);

        report(implFn->typeParams[i]->location,
               "cannot replace parameter of type '" + traitParamTy->getName() +
                   "' with stricter implementation type '" +
                   implParamTy->getName() + "'");
      }
    }

    auto traitSub = typeMgr.extractSubstitutionFrom(traitTy);
    sub[typeMgr.getType(traitFn->typeParams[0])] = typeMgr.getType(parent);

    res::Type *expectedType = typeMgr.instantiate(
        typeMgr.instantiate(typeMgr.getType(traitFn), traitSub), sub);
    res::Type *actualType = typeMgr.getType(implFn);

    if (!typeMgr.unify(expectedType, actualType).empty())
      report(implFn->location,
             "trait function declaration has '" + expectedType->getName() +
                 "' signature, but the given implementation is '" +
                 actualType->getName() + "'");

    if (!resDecl->insertDecl(implFn))
      report(implFn->location, "function '" + implFn->identifier +
                                   "' is already implemented for trait '" +
                                   traitTy->getName() + "'");
  }

  return resDecl;
}

res::VarDecl *Sema::resolveVarDecl(res::Context &ctx,
                                   const ast::VarDecl &varDecl) {
  res::Expr *initializer = nullptr;
  if (varDecl.initializer) {
    initializer = resolveExpr(ctx, *varDecl.initializer);
    if (!initializer)
      return nullptr;
  }

  auto *decl = ctx.createAndBind<res::VarDecl>(
      typeMgr.getNewUninferredType(), varDecl.location, varDecl.identifier,
      varDecl.isMutable, initializer);

  if (varDecl.type) {
    varOrReturn(type, resolveType(ctx, *varDecl.type));
    typeMgr.unify(typeMgr.getType(decl), type);
  }

  if (initializer) {
    auto *declTy = typeMgr.getType(decl);
    auto *initTy = typeMgr.getType(initializer);

    if (!typeMgr.unify(declTy, initTy).empty()) {
      return report(decl->initializer->location,
                    "an expression of type '" + initTy->getName() +
                        "' cannot be used to initialize a variable of type '" +
                        declTy->getName() + "'");
    }

    cee->evaluate(*initializer);
  }

  return decl;
}

bool Sema::checkTypeParameterCount(SourceLocation loc,
                                   size_t received,
                                   size_t expected) const {
  if (received != expected) {
    std::stringstream msg;
    msg << "expected '" << expected << "' type argument";
    if (expected != 1)
      msg << 's';
    msg << " but received '" << received << '\'';

    report(loc, msg.str());
    return false;
  }

  return true;
}

std::vector<res::TypeParamDecl *> Sema::resolveTypeParamsWithoutBounds(
    res::Context &ctx,
    const std::vector<std::unique_ptr<ast::TypeParamDecl>> &typeParamDecls) {
  std::vector<res::TypeParamDecl *> resTypeParams;

  for (auto &&tp : typeParamDecls) {
    auto *resTP = ctx.create<res::TypeParamDecl>(tp->location, tp->identifier);
    typeMgr.bind(resTP, typeMgr.getTypeParamType(*resTP));
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
                            typeMgr.getType(trait)->getAs<res::TraitType>());
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

  for (auto &&trait : typeMgr.getUpperBounds(typeMgr.getType(structDecl))) {
    res::DeclContext *implCtx =
        structDecl->lookupDecl<res::ImplDecl>(trait->getName());

    for (auto &&fn : trait->getDecl()->getAll<res::FunctionDecl>()) {
      if (fn->body || implCtx->lookupDecl<res::FunctionDecl>(fn->identifier))
        continue;

      report(fn->location, "struct '" + structDecl->identifier +
                               "' must implement function '" + fn->identifier +
                               "' from trait '" + trait->getName() + "'");
      error = true;
    }
  }

  return !error;
}

res::FunctionDecl *Sema::resolveFunctionDecl(res::Context &ctx,
                                             const ast::FunctionDecl &decl,
                                             res::Decl *parent,
                                             res::FunctionDecl *implements) {
  ScopeRAII typeParamScope(this);

  auto typeParams = resolveTypeParamsWithoutBounds(ctx, decl.typeParameters);
  bool error =
      !resolveGenericParamsInCurrentScope(ctx, typeParams, decl.typeParameters);
  for (auto &&tp : typeParams)
    if (lexicalScope->parent->lookupDecl<res::TypeParamDecl>(tp->identifier)) {
      report(tp->location,
             "declaring '" + tp->identifier + "' shadows outer type parameter");
      error = true;
    }

  res::Type *currentSelfType = selfType;
  res::TypeParamDecl *implicitSelf = nullptr;
  if (parent && parent->getAs<res::TraitDecl>()) {
    implicitSelf =
        ctx.create<res::TypeParamDecl>(decl.location, implicitSelfId);
    selfType = typeMgr.getTypeParamType(*implicitSelf);

    typeMgr.bind(implicitSelf, selfType);
    typeMgr.addUpperBound(implicitSelf,
                          typeMgr.getType(parent)->getAs<res::TraitType>());

    insertDeclToScope(implicitSelf, lexicalScope);
  }
  if (implicitSelf)
    typeParams.emplace(typeParams.begin(), implicitSelf);

  std::vector<res::Type *> paramTypes;
  std::vector<res::ParamDecl *> resolvedParams;

  ScopeRAII paramScope(this);
  for (auto &&param : decl.params) {
    ResolutionContextRAII paramCtx(this, ParamList);

    auto *type = paramTypes.emplace_back(resolveType(ctx, *param->type));
    error |= !type;

    bool isOutputType = type && type->getAs<res::OutParamType>();
    if (isOutputType && param->isMutable) {
      report(param->location,
             "unexpected 'mut' specifier, a '&' parameter is always mutable");
      error = true;
    }

    auto *resolvedParam = resolvedParams.emplace_back(
        ctx.create<res::ParamDecl>(param->location, param->identifier,
                                   param->isMutable || isOutputType));
    if (type)
      typeMgr.bind(resolvedParam, type);

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
  return ctx.createAndBind<res::FunctionDecl>(
      fnTy, decl.location, decl.identifier, typeParams,
      std::move(resolvedParams), parent, implements);
}

res::FunctionDecl *
Sema::resolveFunctionBody(res::Context &ctx,
                          const ast::FunctionDecl &functionDecl,
                          res::FunctionDecl *function) {
  if (!functionDecl.body)
    return function;

  currentFunction = function;

  ScopeRAII typeParamScope(this);
  for (auto &&typeParam : currentFunction->typeParams)
    insertDeclToScope(typeParam, lexicalScope);

  ScopeRAII paramScope(this);
  for (auto &&param : currentFunction->params)
    insertDeclToScope(param, lexicalScope);

  auto *body = resolveBlock(ctx, *functionDecl.body);
  if (!body) {
    currentFunction->setBody(ctx.create<res::Block>(
        functionDecl.location, std::vector<res::Stmt *>{}));
    return nullptr;
  }

  currentFunction->setBody(body);
  if (runFlowSensitiveChecks(ctx, *currentFunction)) {
    currentFunction = nullptr;
    return nullptr;
  }

  currentFunction = nullptr;
  return function;
}

res::TraitInstance *
Sema::resolveTraitInstance(res::Context &ctx, const ast::TraitInstance *trait) {
  SourceLocation location = trait->location;
  std::string identifier = trait->identifier;
  const auto &typeArguments = trait->typeArguments;

  auto *traitDecl = lexicalScope->lookupDecl<res::TraitDecl>(identifier);
  if (!traitDecl)
    return report(location, identifier + " is not a trait");

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
  return ctx.createAndBind<res::TraitInstance>(traitTy, location, traitDecl,
                                               std::move(resTypeArgs),
                                               std::move(resTypeArgsLocs));
}

res::TraitDecl *Sema::resolveTraitDecl(res::Context &ctx,
                                       const ast::TraitDecl &decl) {
  auto *traitDecl = ctx.create<res::TraitDecl>(
      decl.location, decl.identifier,
      resolveTypeParamsWithoutBounds(ctx, decl.typeParameters));

  std::vector<res::Type *> typeParamTys;
  for (auto &&typeParam : traitDecl->typeParams)
    typeParamTys.emplace_back(typeMgr.getType(typeParam));

  typeMgr.bind(traitDecl, typeMgr.getTraitType(*traitDecl, typeParamTys));
  return traitDecl;
}

bool Sema::resolveTraitBody(res::Context &ctx,
                            res::TraitDecl &traitDecl,
                            const ast::TraitDecl &astDecl) {
  ScopeRAII typeParamScope(this);
  bool error = !resolveGenericParamsInCurrentScope(ctx, traitDecl.typeParams,
                                                   astDecl.typeParameters);

  auto traits = resolveTraitInstanceList(ctx, astDecl.requirements);
  error |= astDecl.requirements.size() != traits.size();

  for (auto &&trait : traits) {
    traitDecl.traits.emplace_back(trait);
    typeMgr.addUpperBound(&traitDecl,
                          typeMgr.getType(trait)->getAs<res::TraitType>());
  }

  for (auto &&fn : astDecl.traitFunctions)
    error |= !insertDeclToScope(resolveFunctionDecl(ctx, *fn, &traitDecl),
                                &traitDecl);

  return !error;
}

bool Sema::resolveTraitFunctionBodies(res::Context &ctx,
                                      res::TraitDecl &traitDecl,
                                      const ast::TraitDecl &astDecl) {
  ScopeRAII typeParamScope(this);
  for (auto &&typeParamDecl : traitDecl.typeParams)
    insertDeclToScope(typeParamDecl, lexicalScope);

  bool error = false;
  int idx = 0;
  for (auto &&fn : traitDecl.getAll<res::FunctionDecl>()) {
    selfType = typeMgr.getType(fn->typeParams[0]);
    error |= !resolveFunctionBody(ctx, *astDecl.traitFunctions[idx], fn);
    selfType = nullptr;
    ++idx;
  }

  return !error;
}

res::StructDecl *Sema::resolveStructDecl(res::Context &ctx,
                                         const ast::StructDecl &decl) {
  auto *rs = ctx.create<res::StructDecl>(
      decl.location, decl.identifier,
      resolveTypeParamsWithoutBounds(ctx, decl.typeParameters));

  std::vector<res::Type *> typeParamTys;
  for (auto &&typeParam : rs->typeParams)
    typeParamTys.emplace_back(typeMgr.getType(typeParam));
  typeMgr.bind(rs, typeMgr.getStructType(*rs, std::move(typeParamTys)));
  return rs;
}

bool Sema::resolveStructBody(res::Context &ctx,
                             res::StructDecl &structDecl,
                             const ast::StructDecl &astDecl) {
  ScopeRAII typeParamScope(this);
  bool error = !resolveGenericParamsInCurrentScope(ctx, structDecl.typeParams,
                                                   astDecl.typeParameters);

  std::vector<res::TraitInstance *> traitInstances;

  selfType = typeMgr.getType(&structDecl);
  for (auto &&decl : astDecl.decls) {
    if (auto *field = dynamic_cast<ast::FieldDecl *>(decl.get())) {
      res::Type *fieldTy = resolveType(ctx, *field->type);
      if (!fieldTy) {
        error = true;
        continue;
      }

      auto *fieldDecl = ctx.createAndBind<res::FieldDecl>(
          fieldTy, field->location, field->identifier);
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
      auto *resImpl = resolveImplDecl(ctx, *implDecl, &structDecl);
      if (!resImpl) {
        error = true;
        continue;
      }

      if (!structDecl.insertDecl(resImpl)) {
        report(resImpl->location, "trait '" +
                                      typeMgr.getType(resImpl)->getName() +
                                      "' is already implemented for struct '" +
                                      structDecl.identifier + "'");
        error = true;
      }

      if (resImpl->decls.size() != implDecl->functions.size())
        error = true;
    }
  }

  auto impls = structDecl.getAll<res::ImplDecl>();
  for (auto &&impl : impls) {
    auto *traitTy = typeMgr.getType(impl)->getAs<res::TraitType>();
    res::Substitution sub = typeMgr.extractSubstitutionFrom(traitTy);

    for (auto &&moreSpecificImpl : impls) {
      res::Type *moreSpecificTy =
          typeMgr.instantiate(typeMgr.getType(moreSpecificImpl), sub);

      if (typeMgr.moreGeneral(traitTy, moreSpecificTy)) {
        report(impl->location,
               "implementing trait '" + traitTy->getName() +
                   "' conflicts with more specific implementation '" +
                   moreSpecificTy->getName() + "'");
        error = true;
      }
    }

    for (res::Type *req :
         typeMgr.getUpperBounds(typeMgr.getType(impl->traitInstance->decl))) {
      req = typeMgr.instantiate(req, sub);

      bool found = false;
      for (auto &&impl : impls)
        found |= typeMgr.unify(typeMgr.getType(impl), req).empty();

      if (!found) {
        report(impl->location, "implementing trait '" + traitTy->getName() +
                                   "' requires implementing trait '" +
                                   req->getName() + "'");
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
  ScopeRAII typeParamScope(this);
  for (auto &&typeParamDecl : decl.typeParams)
    insertDeclToScope(typeParamDecl, lexicalScope);

  selfType = typeMgr.getType(&decl);
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

      for (auto &&fn : implBlock->functions) {
        res::FunctionDecl *implFn =
            decl.lookupDecl<res::ImplDecl>(
                    typeMgr.getType(traitInstance)->getName())
                ->lookupDecl<res::FunctionDecl>(fn->identifier);

        error |= !resolveFunctionBody(ctx, *fn, implFn);
      }
    }
  }

  selfType = nullptr;
  return !error;
}

res::Context *Sema::resolveAST() {
  ScopeRAII globalScope(this);
  bool error = false;

  std::vector<std::pair<res::Decl *, const ast::Decl *>> resDecls;

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
    if (!typeMgr.getType(fnDecl)
             ->getAs<res::FunctionType>()
             ->getReturnType()
             ->getAs<res::BuiltinUnitType>()) {
      report(fnDecl->location, "'main' function is expected to return 'unit'");
      return true;
    }

    if (!fnDecl->params.empty()) {
      report(fnDecl->location,
             "'main' function is expected to take no arguments");
      return true;
    }

    if (!fnDecl->typeParams.empty()) {
      report(fnDecl->location, "'main' function cannot be generic");
      return true;
    }
  }

  if (fnDecl->identifier == "printf") {
    report(fnDecl->location,
           "'printf' is a reserved function name and cannot be used for "
           "user-defined functions");
    return true;
  }

  return false;
}

bool Sema::checkSelfParameter(const res::ParamDecl *param, size_t idx) {
  if (param->identifier != selfParamId)
    return true;

  if (!selfType) {
    report(param->location, "'self' parameter is only allowed in methods");
    return false;
  }

  if (idx != 0) {
    report(param->location, "'self' can only be the first parameter");
    return false;
  }

  auto *type = typeMgr.getType(param);
  if (!type)
    return true;

  auto *outTy = type->getAs<res::OutParamType>();
  if (!typeMgr.unify(type, selfType).empty() &&
      !(outTy && typeMgr.unify(outTy->getParamType(), selfType).empty())) {
    report(param->location, "the type of 'self' must reference 'Self'");
    return false;
  }

  return true;
}

bool Sema::hasSelfContainingStructs(const res::Context &ctx) {
  std::stack<const res::StructDecl *> worklist;
  std::set<const res::StructDecl *> selfContaining;

  for (auto &&sd : ctx.getStructs()) {
    std::set<const res::StructDecl *> seen;
    worklist.emplace(sd);

    while (!worklist.empty()) {
      const res::StructDecl *decl = worklist.top();
      worklist.pop();

      if (!seen.emplace(decl).second) {
        selfContaining.emplace(decl);
        continue;
      }

      for (auto &&field : decl->getAll<res::FieldDecl>())
        if (const auto *structTy =
                typeMgr.getType(field)->getAs<res::StructType>())
          worklist.emplace(structTy->getDecl());
    }
  }

  for (auto &&sd : selfContaining)
    report(sd->location, "struct '" + sd->identifier + "' contains itself");

  return !selfContaining.empty();
}

bool Sema::checkTraitInstances(res::Context &ctx) {
  bool error = false;

  for (auto &&traitInstance : ctx.getTraitInstances()) {
    auto sub = typeMgr.extractSubstitutionFrom(typeMgr.getType(traitInstance));

    for (size_t i = 0; i < traitInstance->typeArgs.size(); ++i) {
      auto *subTy = typeMgr.getNewUninferredType();

      for (auto &&trait : typeMgr.getUpperBounds(
               typeMgr.getType(traitInstance->decl->typeParams[i])))
        typeMgr.withObligation(
            subTy, typeMgr.instantiate(trait, sub)->getAs<res::TraitType>());

      if (const auto &msg = typeMgr.unify(traitInstance->typeArgs[i], subTy);
          !msg.empty()) {
        for (auto &&error : msg) {
          report(traitInstance->typeLocations[i], error);
        }

        error = true;
      }
    }
  }

  return !error;
}
} // namespace yl
