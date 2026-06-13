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

res::FunctionDecl *Sema::createBuiltinGC(res::Context &ctx) {
  SourceLocation loc{nullptr, 0, 0};

  auto *typeParamTy = typeMgr.getNewUninferredType();
  auto *typeParamDecl = ctx.create<res::TypeParamDecl>(loc, typeParamTy, "T");
  typeMgr.unify(typeParamTy, typeMgr.getTypeParamType(*typeParamDecl));

  auto *param = ctx.create<res::ParamDecl>(loc, typeParamTy, "t", false);

  auto *fnTy = typeMgr.getFunctionType(
      {typeParamTy}, typeMgr.getPointerType(typeParamTy, false));
  auto *fn = ctx.create<res::FunctionDecl>(
      loc, fnTy, "gc", std::vector<res::TypeParamDecl *>{typeParamDecl},
      std::vector{param});
  fn->setBody(ctx.create<res::Block>(loc, std::vector<res::Stmt *>()));

  return fn;
}

res::FunctionDecl *Sema::createBuiltinGCMut(res::Context &ctx) {
  SourceLocation loc{nullptr, 0, 0};

  auto *typeParamTy = typeMgr.getNewUninferredType();
  auto *typeParamDecl = ctx.create<res::TypeParamDecl>(loc, typeParamTy, "T");
  typeMgr.unify(typeParamTy, typeMgr.getTypeParamType(*typeParamDecl));

  auto *param = ctx.create<res::ParamDecl>(loc, typeParamTy, "t", false);

  auto *fnTy = typeMgr.getFunctionType(
      {typeParamTy}, typeMgr.getPointerType(typeParamTy, true));
  auto *fn = ctx.create<res::FunctionDecl>(
      loc, fnTy, "gcMut", std::vector<res::TypeParamDecl *>{typeParamDecl},
      std::vector{param});
  fn->setBody(ctx.create<res::Block>(loc, std::vector<res::Stmt *>()));

  return fn;
}

res::FunctionDecl *Sema::createBuiltinGCCollect(res::Context &ctx) {
  SourceLocation loc{nullptr, 0, 0};

  auto *fnTy = typeMgr.getFunctionType({}, typeMgr.getBuiltinUnitType());
  auto *fn = ctx.create<res::FunctionDecl>(loc, fnTy, "gcCollect");
  fn->setBody(ctx.create<res::Block>(loc, std::vector<res::Stmt *>()));

  return fn;
}

res::Type *Sema::resolveType(res::Context &ctx,
                             const ast::Type &parsedType,
                             bool isPointee) {
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

  if (const auto *impl = dynamic_cast<const ast::ImplType *>(&parsedType)) {
    if (!isPointee)
      return err::traitObjectNotPointee(impl->location).report(reporter);

    auto resolvedTraits = resolveTraitInstanceList(ctx, impl->traits);
    bool error = resolvedTraits.size() != impl->traits.size();

    std::vector<res::TraitType *> traitTys;
    for (auto &&trait : resolvedTraits) {
      if (!isTraitVtableCompatible(trait->getType()->getAs<res::TraitType>())) {
        err::traitObjectTemplateMemberFn(trait->location).report(reporter);
        error = true;
      }

      if (functionInfo)
        error |= !checkTraitInstance(trait);
      traitTys.emplace_back(trait->getType()->getAs<res::TraitType>());
    }

    if (error)
      return nullptr;

    auto *implType = typeMgr.getImplType(traitTys);
    for (auto &&trait : traitTys)
      typeMgr.addUpperBound(implType, trait);

    return implType;
  }

  if (const auto *ptr = dynamic_cast<const ast::PointerType *>(&parsedType)) {
    varOrReturn(pointeeType, resolveType(ctx, *ptr->pointeeType, true));
    if (pointeeType->getAs<res::OutParamType>())
      return err::outParamPointer(ptr->location).report(reporter);

    return typeMgr.getPointerType(pointeeType, ptr->isMut);
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

  res::Expr::Kind kind = res::Expr::Kind::Rvalue;
  if (unary.op == TokenKind::Asterisk) {
    auto *ptr = rhsTy->getAs<res::PointerType>();
    if (!ptr)
      return err::expectedPointerOperand(rhs->location).report(reporter);

    kind =
        ptr->isMutable() ? res::Expr::Kind::MutLvalue : res::Expr::Kind::Lvalue;
    rhsTy = ptr->getPointeeType();
  }

  return ctx.create<res::UnaryOperator>(unary.location, rhsTy, unary.op, rhs,
                                        kind);
}

res::BinaryOperator *
Sema::resolveBinaryOperator(res::Context &ctx,
                            const ast::BinaryOperator &binop) {
  varOrReturn(lhs, resolveExpr(ctx, *binop.lhs));
  varOrReturn(rhs, resolveExpr(ctx, *binop.rhs, lhs->getType()));

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
res::DeclRefExpr *Sema::resolvePathExpr(res::Context &ctx,
                                        const ast::PathExpr &pathExpr) {
  res::Type *parentType = nullptr;
  res::TraitInstance *parentTrait = nullptr;

  if (auto *traitSpecifier = pathExpr.traitSpecifier.get()) {
    varOrReturn(type, resolveType(ctx, *traitSpecifier->type));
    varOrReturn(trait, resolveTraitInstance(
                           ctx, traitSpecifier->impl->traitInstance.get()));

    res::TraitType *traitTy = trait->getType()->getAs<res::TraitType>();
    auto *checkTy =
        typeMgr.withObligation(typeMgr.getNewUninferredType(), traitTy);

    if (!typeMgr.unify(type, checkTy).empty())
      return err::traitNotImplemented(trait->location)
          .with(type->getName())
          .with(traitTy->getName())
          .report(reporter);

    parentType = type;
    parentTrait = trait;
  }

  std::vector<res::DeclRefExpr *> resolvedFragments;
  res::Type *prevType = parentType;
  res::TraitInstance *traitHelp = parentTrait;

  for (auto &&fragment : pathExpr.fragments) {
    bool isLast = fragment != pathExpr.fragments.back();
    varOrReturn(resolvedDre,
                isLast ? resolveDeclRefExpr<res::TypeDecl>(
                             ctx, fragment.get(), prevType, parentTrait)
                       : resolveDeclRefExpr<Hint>(ctx, fragment.get(), prevType,
                                                  parentTrait));

    resolvedFragments.emplace_back(resolvedDre);
    prevType = resolvedDre->getType();
    traitHelp = nullptr;
  }

  return resolvedFragments.back();
}

template <typename Hint>
res::DeclRefExpr *Sema::resolveDeclRefExpr(res::Context &ctx,
                                           const ast::DeclRefExpr *dre,
                                           res::Type *in,
                                           res::TraitInstance *traitHelp) {
  res::DeclContext *scope = nullptr;
  res::TraitType *traitHelpTy =
      traitHelp ? traitHelp->getType()->getAs<res::TraitType>() : nullptr;

  if (!in)
    scope = lexicalScope;
  else if (traitHelp)
    scope = traitHelp->decl;
  else if (auto *st = in->getAs<res::StructType>())
    scope = st->getDecl();

  // FIXME: error out if 'in' is a trait type

  if (!in && dre->identifier == selfTypeId) {
    if (!selfType)
      return err::selfTyNotAllowed(dre->location).report(reporter);

    res::Decl *decl = nullptr;
    if (auto *paramTy = selfType->getAs<res::TypeParamType>())
      decl = paramTy->decl;
    else
      decl = selfType->getAs<res::StructType>()->getDecl();

    return createDeclRefExpr(ctx, dre, in, decl, traitHelpTy);
  }

  if (scope)
    if (res::Decl *decl = lookupSymbolWithFallback<Hint>(scope, dre))
      return createDeclRefExpr(ctx, dre, in, decl, traitHelpTy);

  if (in) {
    res::TraitType *candidateTrait = nullptr;
    res::Decl *candidateDecl = nullptr;

    for (auto &&trait : typeMgr.getUpperBounds(in)) {
      if (candidateTrait && typeMgr.unify(trait, candidateTrait).empty())
        continue;

      auto *declInTrait = lookupSymbolWithFallback<Hint>(trait->getDecl(), dre);
      if (!declInTrait)
        continue;

      if (candidateDecl)
        return err::ambigousMemberFn(dre->location).report(reporter);

      candidateTrait = trait;
      candidateDecl = declInTrait;
    }

    if (candidateDecl)
      return createDeclRefExpr(ctx, dre, in, candidateDecl, candidateTrait);

    if (traitHelp) {
      in = traitHelp->getType();
    }
    return err::lookupInTypeFailed(dre->location)
        .with(dre->identifier)
        .with(in->getName())
        .report(reporter);
  }

  return err::missingSymbol(dre->location)
      .with(dre->identifier)
      .report(reporter);
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

  return functionInfo->declReferences.emplace_back(ctx.create<res::DeclRefExpr>(
      dre->location, typeMgr.instantiate(declTy, sub), decl, kind, typeArgs,
      parentTy, trait));
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
        callee->location, fnType, fn, res::Expr::Kind::Rvalue,
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
            baseLoc, typeMgr.getOutParamType(baseTy), TokenKind::Amp, selfArg,
            res::Expr::Kind::Rvalue);
      } else if (baseTy->getAs<res::ImplType>())
        return err::traitObjectSelf(baseLoc).report(reporter);

      auto referencesSelf = [&](const res::Type *paramTy) {
        while (true) {
          if (auto *ptrTy = paramTy->getAs<res::PointerType>()) {
            paramTy = ptrTy->getPointeeType();
            continue;
          }

          if (auto *outTy = paramTy->getAs<res::OutParamType>()) {
            paramTy = outTy->getParamType();
            continue;
          }

          return paramTy->getRootType() ==
                 function->typeParams[0]->getType()->getRootType();
        }
      };

      if (baseTy->getAs<res::ImplType>()) {
        for (auto &&param : function->params) {
          if (param->identifier == selfParamId)
            continue;

          if (referencesSelf(param->getType()))
            return err::traitObjectSelfParam(baseLoc).report(reporter);
        }

        if (referencesSelf(function->getType()
                               ->getAs<res::FunctionType>()
                               ->getReturnType()))
          return err::traitObjectSelfReturn(baseLoc).report(reporter);
      }

      res::DeclRefExpr *baseDre = nullptr;
      if (auto *structTy = baseTy->getAs<res::StructType>())
        baseDre =
            ctx.create<res::DeclRefExpr>(baseLoc, structTy, structTy->getDecl(),
                                         baseKind, structTy->getTypeArgs());

      if (auto *typeParamTy = baseTy->getAs<res::TypeParamType>())
        baseDre = ctx.create<res::DeclRefExpr>(baseLoc, typeParamTy,
                                               typeParamTy->decl, baseKind);

      std::vector<res::DeclRefExpr *> fragments;
      if (baseDre)
        fragments.emplace_back(baseDre);
      fragments.emplace_back(member);

      callee = fragments.back();
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

    res::Type *expectedTy = argTypes[resolvedArgs.size()];
    varOrReturn(resolvedArg, resolveExpr(ctx, *arg, expectedTy));
    varOrReturn(coercedArg, coerceIfNeeded(expectedTy, resolvedArg));

    res::Type *actualTy = coercedArg->getType();

    if (const auto &errors = typeMgr.unify(actualTy, expectedTy);
        !errors.empty()) {
      for (auto &&error : errors)
        err::inferenceError(coercedArg->location).with(error).report(reporter);
      return nullptr;
    }

    coercedArg->setConstantValue(cee->evaluate(*coercedArg));
    resolvedArgs.emplace_back(coercedArg);
  }

  return ctx.create<res::CallExpr>(call.location, fnType->getReturnType(),
                                   callee, std::move(resolvedArgs));
}

res::StructInstantiationExpr *Sema::resolveStructInstantiation(
    res::Context &ctx,
    const ast::StructInstantiationExpr &structInstantiation) {
  varOrReturn(path, resolvePathExpr<res::StructDecl>(
                        ctx, *structInstantiation.structRef));

  if (!path->decl->getAs<res::StructDecl>())
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

    res::Type *fieldTy = typeMgr.instantiate(
        fieldDecl->getType(), typeMgr.extractSubstitutionFrom(structTy));

    auto *resolvedInitExpr = resolveExpr(ctx, *initStmt->initializer, fieldTy);
    if (!resolvedInitExpr) {
      error = true;
      continue;
    }

    res::Expr *coercedInitExpr = coerceIfNeeded(fieldTy, resolvedInitExpr);
    if (!coercedInitExpr) {
      error = true;
      continue;
    }

    res::Type *initTy = coercedInitExpr->getType();
    if (const auto &msg = typeMgr.unify(initTy, fieldTy); !msg.empty()) {
      for (auto &&error : msg)
        err::inferenceError(coercedInitExpr->location)
            .with(error)
            .report(reporter);
      error = true;
      continue;
    }

    inits[id] = resolvedFieldInits.emplace_back(
        ctx.create<res::FieldInitStmt>(loc, fieldDecl, coercedInitExpr));
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

  res::Type *baseType = base->getType();
  if (auto *ptr = baseType->getAs<res::PointerType>())
    base = ctx.create<res::UnaryOperator>(
        memberExpr.location, ptr->getPointeeType(), TokenKind::Asterisk, base,
        ptr->isMutable() ? res::Expr::Kind::MutLvalue
                         : res::Expr::Kind::Lvalue);

  varOrReturn(memberDre, resolveDeclRefExpr<res::ValueDecl>(
                             ctx, memberExpr.member.get(), base->getType()));

  if (memberDre->decl->getAs<res::FunctionDecl>() && !(modifiers & IsCallee))
    return err::expectedMethodCall(memberExpr.location).report(reporter);

  return ctx.create<res::MemberExpr>(memberExpr.location, base, memberDre);
}

res::LambdaExpr *Sema::resolveLambdaExpr(res::Context &ctx,
                                         const ast::LambdaExpr &lambdaExpr,
                                         res::Type *typeHint) {
  SourceLocation loc = lambdaExpr.location;

  res::FunctionType *expectedFnType = nullptr;
  if (typeHint) {
    expectedFnType = typeHint->getAs<res::FunctionType>();

    if (!expectedFnType && !typeHint->getAs<res::UninferredType>())
      return err::unexpectedLambda(loc)
          .with(typeHint->getName())
          .report(reporter);
  }

  std::stringstream structId;
  structId << "(closure@<source>:" << loc.line << ':' << loc.col << ')';

  res::Type *closureTy = typeMgr.getNewUninferredType();
  auto *closure =
      ctx.create<res::StructDecl>(loc, closureTy, structId.str(),
                                  std::vector<res::TypeParamDecl *>{}, true);
  typeMgr.unify(closureTy, typeMgr.getStructType(*closure, {}));

  bool error = false;
  std::vector<res::Type *> paramTypes = {};
  std::vector<res::ParamDecl *> resolvedParams = {};

  EnterScopeRAII paramScope(this);
  {
    WithModifiersRAII lambdaParamList(this, UnaryAmpAllowed |
                                                MissingTypeAnnotationsAllowed);
    int i = 0;
    for (auto &&param : lambdaExpr.params) {
      auto [resolvedParam, err] = resolveParamDecl(ctx, param.get());

      if (resolvedParam->getType()->getAs<res::UninferredType>() &&
          expectedFnType && i < expectedFnType->getArgs().size())
        typeMgr.unify(resolvedParam->getType(), expectedFnType->getArgs()[i]);

      if (resolvedParam->getType()->getAs<res::UninferredType>()) {
        err::annotationsNeeded(param->location)
            .with(param->identifier)
            .report(reporter);
        error = true;
      }

      paramTypes.emplace_back(resolvedParam->getType());
      resolvedParams.emplace_back(resolvedParam);

      error |= !insertDeclToScope(resolvedParam, lexicalScope);

      if (param->identifier == selfParamId) {
        err::selfParamNotAllowed(param->location).report(reporter);
        error = true;
      }

      ++i;
    }
  }

  res::Type *returnTy = lambdaExpr.returnType
                            ? resolveType(ctx, *lambdaExpr.returnType)
                            : typeMgr.getNewUninferredType();
  if (returnTy && returnTy->getAs<res::UninferredType>() && expectedFnType)
    typeMgr.unify(returnTy, expectedFnType->getReturnType());

  if (!returnTy || error)
    return nullptr;

  auto *lambdaTy = typeMgr.getFunctionType(paramTypes, returnTy);
  if (expectedFnType) {
    auto msgs = typeMgr.unify(expectedFnType, lambdaTy);
    if (!msgs.empty()) {
      for (auto &&msg : msgs)
        err::inferenceError(loc).with(msg).report(reporter);
      return nullptr;
    }
  }

  paramTypes.emplace_back(typeMgr.getPointerType(closureTy, false));
  resolvedParams.emplace_back(ctx.create<res::ParamDecl>(
      loc, typeMgr.getPointerType(closureTy, false), "closure", false));

  auto *fnTy = typeMgr.getFunctionType(paramTypes, returnTy);
  auto *fn = ctx.create<res::FunctionDecl>(loc, fnTy, lambdaFunctionId,
                                           std::vector<res::TypeParamDecl *>{},
                                           std::move(resolvedParams), closure);
  closure->insertDecl(fn);

  auto *resLambdaExpr = ctx.create<res::LambdaExpr>(loc, lambdaTy, closure, fn);

  std::vector<const ast::Expr *> pendingCaptureInits;
  {
    WithFunctionInfoRAII lambdaInfo(this, {fn, resLambdaExpr, lexicalScope});

    if (res::Block *block = resolveBlock(ctx, *lambdaExpr.body)) {
      fn->setBody(block);

      res::Type *retTy = fnTy->getReturnType();
      if (retTy->getAs<res::UninferredType>())
        typeMgr.unify(retTy, typeMgr.getBuiltinUnitType());

      error |= !runPostFunctionBodyChecks();
    }

    error |= !fn->isComplete;
    pendingCaptureInits = std::move(functionInfo->pendingCaptureInits);
  }

  if (error)
    return nullptr;

  for (auto &&pendingInit : pendingCaptureInits) {
    res::Expr *initExpr = resolveExpr(ctx, *pendingInit);
    initExpr->setConstantValue(cee->evaluate(*initExpr));
    resLambdaExpr->fieldInits.emplace_back(initExpr);
  }

  return resLambdaExpr;
}

res::Expr *Sema::coerceIfNeeded(res::Type *targetType, res::Expr *expr) {
  res::Type *exprType = expr->getType();
  auto &&[isItPossible, errors] = typeMgr.tryCoerce(targetType, exprType);

  if (!isItPossible)
    return expr;

  if (!errors.empty()) {
    for (auto &&error : errors)
      err::inferenceError(expr->location).with(error).report(reporter);
    return nullptr;
  }

  return ctx.create<res::ImplicitCoerceExpr>(expr->location, targetType, expr);
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
  varOrReturn(
      cond, resolveExpr(ctx, *ifStmt.condition, typeMgr.getBuiltinBoolType()));
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
  varOrReturn(cond, resolveExpr(ctx, *whileStmt.condition,
                                typeMgr.getBuiltinBoolType()));
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
  varOrReturn(lhs, resolveExpr(ctx, *assignment.assignee));
  varOrReturn(rhs, resolveExpr(ctx, *assignment.expr, lhs->getType()));

  if (!lhs->isLvalue())
    return err::rvalueAssignment(lhs->location).report(reporter);
  auto *lhsTy = lhs->getType();

  varOrReturn(coercedRhs, coerceIfNeeded(lhsTy, rhs));
  auto *rhsTy = coercedRhs->getType();

  if (const auto &errors = typeMgr.unify(lhsTy, rhsTy); !errors.empty()) {
    for (auto &&error : errors)
      err::inferenceError(coercedRhs->location).with(error).report(reporter);

    return err::incompatibleAssignment(coercedRhs->location)
        .with(lhsTy->getName())
        .with(rhsTy->getName())
        .report(reporter);
  }

  coercedRhs->setConstantValue(cee->evaluate(*coercedRhs));
  return ctx.create<res::Assignment>(assignment.location, lhs, coercedRhs);
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
    expr = resolveExpr(ctx, *returnStmt.expr, retTy);
    if (!expr)
      return nullptr;

    varOrReturn(coercedExpr, coerceIfNeeded(retTy, expr));
    expr = coercedExpr;

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

res::Expr *Sema::resolveExpr(res::Context &ctx,
                             const ast::Expr &expr,
                             res::Type *typeHint) {
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
    return resolveLambdaExpr(ctx, *lambda, typeHint);

  if (const auto *path = dynamic_cast<const ast::PathExpr *>(&expr)) {
    varOrReturn(resPath, resolvePathExpr<res::ValueDecl>(ctx, *path));

    const res::Decl *decl = resPath->decl;
    bool isFunctionDecl = decl->getAs<res::FunctionDecl>();

    if (decl->getAs<res::TypeParamDecl>())
      return err::unexpectedTypeParam(resPath->location).report(reporter);

    if (decl->getAs<res::StructDecl>())
      return err::expectedInstance(resPath->location)
          .with(decl->identifier)
          .report(reporter);

    if (resPath->owningType && !isFunctionDecl)
      return err::memberFnLookupFailed(resPath->location)
          .with(decl->identifier)
          .with(resPath->owningType->getName())
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
            lambda->closure->lookupDecl<res::FieldDecl>(decl->identifier);
        if (!field) {
          field = ctx.create<res::FieldDecl>(
              lambda->location, resPath->getType(), decl->identifier);
          lambda->closure->insertDecl(field);
          functionInfo->pendingCaptureInits.emplace_back(&expr);
        }

        res::Expr *base = ctx.create<res::DeclRefExpr>(
            lambda->location, lambda->method->params.back()->getType(),
            lambda->method->params.back(), res::Expr::Kind::Lvalue);

        base = ctx.create<res::UnaryOperator>(
            lambda->location, lambda->closure->getType(), TokenKind::Asterisk,
            base, res::Expr::Kind::Lvalue);

        auto *fieldDre = ctx.create<res::DeclRefExpr>(
            lambda->location, field->getType(), field, res::Expr::Kind::Lvalue);

        return ctx.create<res::MemberExpr>(lambda->location, base, fieldDre);
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

  if (error)
    return nullptr;

  return ctx.create<res::Block>(block.location, std::move(resolvedStatements));
}

res::ImplBlock *Sema::resolveImplBlock(res::Context &ctx,
                                       const ast::ImplDecl &decl,
                                       res::StructDecl *parent) {
  varOrReturn(traitInstance, resolveTraitInstance(ctx, decl.trait.get()));

  auto *traitTy = traitInstance->getType()->getAs<res::TraitType>();
  typeMgr.addUpperBound(parent->getType(), traitTy);

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
    varOrReturn(init, resolveExpr(ctx, *varDecl.initializer, declTy));

    varOrReturn(coercedInit, coerceIfNeeded(declTy, init));
    init = coercedInit;
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
      typeMgr.addUpperBound(resParam->getType(),
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
    typeMgr.addUpperBound(implicitSelf->getType(),
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
  if (!runPostFunctionBodyChecks())
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
    typeMgr.addUpperBound(traitDecl.getType(),
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
      decl.location, structTy, decl.identifier,
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

  insertDeclToScope(createBuiltinGC(ctx), lexicalScope);
  insertDeclToScope(createBuiltinGCMut(ctx), lexicalScope);
  insertDeclToScope(createBuiltinGCCollect(ctx), lexicalScope);
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
  std::stack<std::pair<res::StructType *, int>> worklist;
  std::set<res::StructDecl *> selfContaining;

  for (auto &&sd : ctx.getStructs()) {
    std::vector<std::pair<res::StructType *, int>> seen;
    worklist.emplace(sd->getType()->getAs<res::StructType>(), 0);

    while (!worklist.empty()) {
      auto &&[ty, level] = worklist.top();
      worklist.pop();

      res::StructDecl *decl = ty->getDecl();
      res::Substitution sub = typeMgr.extractSubstitutionFrom(ty);

      for (auto &&[seenTy, seenLevel] : seen)
        if (seenLevel < level && typeMgr.unify(seenTy, ty).empty())
          selfContaining.emplace(decl);

      if (selfContaining.count(decl))
        continue;

      seen.emplace_back(ty, level);

      for (auto &&field : decl->getAll<res::FieldDecl>())
        if (auto *structTy = typeMgr.instantiate(field->getType(), sub)
                                 ->getAs<res::StructType>())
          worklist.emplace(structTy, level + 1);
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

  for (auto &&traitInstance : ctx.getTraitInstances())
    error |= !checkTraitInstance(traitInstance);

  return !error;
}

bool Sema::checkTraitInstance(res::TraitInstance *traitInstance) {
  auto sub = typeMgr.extractSubstitutionFrom(traitInstance->getType());

  for (size_t i = 0; i < traitInstance->typeArgs.size(); ++i) {
    auto *subTy = typeMgr.getNewUninferredType();

    for (auto &&trait :
         typeMgr.getUpperBounds(traitInstance->decl->typeParams[i]->getType()))
      typeMgr.withObligation(
          subTy, typeMgr.instantiate(trait, sub)->getAs<res::TraitType>());

    if (const auto &msg = typeMgr.unify(traitInstance->typeArgs[i], subTy);
        !msg.empty()) {
      for (auto &&error : msg)
        err::inferenceError(traitInstance->typeLocations[i])
            .with(error)
            .report(reporter);

      return false;
    }
  }

  return true;
}

bool Sema::isTraitVtableCompatible(res::TraitType *trait) {
  for (auto &&fn : trait->getDecl()->getAll<res::FunctionDecl>()) {
    bool hasSelfParam =
        fn->params.size() > 0 && fn->params[0]->identifier == selfParamId;

    if (!fn->isGeneric() || !hasSelfParam || fn->typeParams.size() == 1)
      continue;

    return false;
  }

  for (auto &&parentTrait : typeMgr.getUpperBounds(trait))
    if (!isTraitVtableCompatible(parentTrait))
      return false;

  return true;
}

bool Sema::runPostFunctionBodyChecks() {
  assert(functionInfo && "expected function info");

  CFG cfg = CFGBuilder().build(*functionInfo->function);
  bool error = false;

  error |= !checkDeclRefTypes();
  error |= !checkReturnOnAllPaths(cfg);
  error |= !checkVariableInitialization(cfg);

  return !error;
}

bool Sema::checkDeclRefTypes() {
  bool error = false;
  for (auto &&dre : functionInfo->declReferences)
    for (auto &&typeArg : dre->typeArgs) {
      if (!typeArg->getRootType()->getAs<res::UninferredType>())
        continue;

      err::annotationsNeeded(dre->location)
          .with(dre->decl->identifier)
          .report(reporter);
      error = true;
      break;
    };

  return !error;
}

bool Sema::checkReturnOnAllPaths(const CFG &cfg) {
  const res::FunctionDecl *fn = cfg.fn;
  if (fn->getType()
          ->getAs<res::FunctionType>()
          ->getReturnType()
          ->getAs<res::BuiltinUnitType>())
    return true;

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
    (returnCount > 0
         ? err::expectedReturnValueOnEveryPath(fn->location).report(reporter)
         : err::expectedReturnValue(fn->location).report(reporter));
    return false;
  }

  return true;
}

bool Sema::checkVariableInitialization(const CFG &cfg) {
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

            if (const auto *u =
                    dynamic_cast<const res::UnaryOperator *>(base)) {
              if (u->op == TokenKind::Asterisk && !u->isMutable())
                pendingErrors.emplace_back(
                    err::pointeeCannotBeMutated(assignment->location)
                        .with(u->operand->getType()->getName()));
              break;
            }

            break;
          }

          const auto *path = dynamic_cast<const res::DeclRefExpr *>(base);
          if (!path)
            continue;

          const auto *decl = path->decl->getAs<res::ValueDecl>();
          if (!decl->isMutable && tmp[decl] != State::Unassigned)
            pendingErrors.emplace_back(
                err::cannotBeMutated(assignment->location)
                    .with(decl->identifier));

          tmp[decl] = State::Assigned;
          continue;
        }

        if (const auto *path = dynamic_cast<const res::DeclRefExpr *>(stmt)) {
          const auto *dre = path;
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
      return false;
    }

  for (auto &&err : pendingErrors)
    err.report(reporter);

  return pendingErrors.empty();
}
} // namespace yl
