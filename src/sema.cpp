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
res::GenericDeclContext *Sema::Scope::getDeclContext() const {
  if (ctx)
    return ctx;

  if (parent)
    return parent->getDeclContext();

  return nullptr;
}

res::Type *Sema::Scope::getSelfType() const {
  auto *declContext = getDeclContext();
  while (declContext) {
    if (auto *s = dynamic_cast<res::StructDecl *>(declContext))
      return s->getType();

    if (auto *t = dynamic_cast<res::TraitDecl *>(declContext))
      return t->typeParams[0]->getType();

    if (auto *e = dynamic_cast<res::TypeExtension *>(declContext))
      return e->type;

    declContext = declContext->parent;
  }

  return nullptr;
}

std::vector<res::Decl *> Sema::Scope::lookupSymbol(const std::string &id,
                                                   bool recursive) const {
  std::vector<res::Decl *> results;

  for (auto &&decl : decls)
    if (decl->identifier == id)
      results.emplace_back(decl);

  if (!recursive || !parent)
    return results;

  for (auto &&res : parent->lookupSymbol(id))
    results.emplace_back(res);

  return results;
}

bool Sema::insertDeclToCurrentScope(res::Decl *decl) {
  if (!decl)
    return false;

  const auto &results = scope->lookupSymbol(decl->identifier, false);
  if (results.empty()) {
    scope->addDecl(decl);
    return true;
  }

  bool resultIsValue = results[0]->getAs<res::ValueDecl>() != nullptr;
  bool declIsValue = decl->getAs<res::ValueDecl>() != nullptr;

  if (results.size() > 1 || resultIsValue == declIsValue) {
    err::redeclaration(decl->location).with(decl->identifier).report(reporter);
    return false;
  }

  scope->addDecl(decl);
  return true;
}

res::FunctionDecl *Sema::createBuiltinPrintln(res::Context &ctx) {
  SourceLocation loc{nullptr, 0, 0};
  auto *numTy = res::BuiltinNumberType::create(ctx);

  auto *fn =
      res::FunctionDecl::create(ctx, loc, "println", scope->getDeclContext(),
                                std::vector<res::TypeParamDecl *>{});
  fn->setType(res::FunctionType::create(ctx, std::vector<res::Type *>{numTy},
                                        res::BuiltinUnitType::create(ctx)));

  auto *param = res::ParamDecl::create(ctx, loc, "n", fn, false);
  param->setType(numTy);
  fn->setParams({param});

  fn->setBody(res::Block::create(ctx, loc, std::vector<res::Stmt *>()));
  return fn;
};

res::FunctionDecl *Sema::createBuiltinGCCollect(res::Context &ctx) {
  SourceLocation loc{nullptr, 0, 0};

  auto *fn =
      res::FunctionDecl::create(ctx, loc, "gcCollect", scope->getDeclContext());
  fn->setType(res::FunctionType::create(ctx, std::vector<res::Type *>{},
                                        res::BuiltinUnitType::create(ctx)));
  fn->setBody(res::Block::create(ctx, loc, std::vector<res::Stmt *>()));

  return fn;
}

res::TypeDecl *Sema::resolveTypeSymbol(const ast::UserDefinedType *udt) {
  for (auto &&d : scope->lookupSymbol(udt->identifier))
    if (auto *td = d->getAs<res::TypeDecl>())
      return td;

  return err::failedToResolveType(udt->location)
      .with(udt->identifier)
      .report(reporter);
}

res::Type *Sema::resolveType(res::Context &ctx,
                             const ast::Type &parsedType,
                             bool allowTraitObject,
                             bool expectTrait,
                             res::Type *traitSelfType) {
  if (const auto *builtin =
          dynamic_cast<const ast::BuiltinType *>(&parsedType)) {
    switch (builtin->kind) {
    case ast::BuiltinType::Kind::Unit:
      return res::BuiltinUnitType::create(ctx);
    case ast::BuiltinType::Kind::Number:
      return res::BuiltinNumberType::create(ctx);
    case ast::BuiltinType::Kind::Bool:
      return res::BuiltinBoolType::create(ctx);
    case ast::BuiltinType::Kind::Self:
      if (auto *selfType = scope->getSelfType())
        return selfType;
      return err::selfTyNotAllowed(parsedType.location).report(reporter);
    }
  }

  if (const auto *udt =
          dynamic_cast<const ast::UserDefinedType *>(&parsedType)) {
    varOrReturn(decl, resolveTypeSymbol(udt));
    bool isTraitDecl = decl->getAs<res::TraitDecl>();

    if (isTraitDecl && !expectTrait)
      return err::rawTrait(udt->location)
          .with(udt->identifier)
          .report(reporter);

    if (!isTraitDecl && expectTrait)
      return err::notATrait(udt->location)
          .with(udt->identifier)
          .report(reporter);

    if (auto *typeParamDecl = decl->getAs<res::TypeParamDecl>())
      return res::TypeParamType::create(ctx, typeParamDecl);

    auto *gdc = dynamic_cast<res::GenericDeclContext *>(decl);
    assert(gdc && "expected generic decl context");

    int offset = isTraitDecl ? 1 : 0;
    const auto &typeParams = gdc->typeParams;

    varOrReturn(res, checkTypeParameterCount(udt->location,
                                             udt->typeArguments.size(),
                                             typeParams.size() - offset));

    std::vector<res::Type *> resolvedTypeArgs;
    for (auto &&astArg : udt->typeArguments)
      if (auto *resolvedType = resolveType(ctx, *astArg))
        resolvedTypeArgs.emplace_back(resolvedType);

    if (resolvedTypeArgs.size() != udt->typeArguments.size())
      return nullptr;

    if (isTraitDecl) {
      auto *td = decl->getAs<res::TraitDecl>();
      if (!traitSelfType)
        return validatedUserDefinedType(
            udt,
            res::AnyTraitType::create(ctx, td, std::move(resolvedTypeArgs)));

      resolvedTypeArgs.emplace(resolvedTypeArgs.begin(), traitSelfType);
      return validatedUserDefinedType(
          udt, res::TraitType::create(ctx, td, std::move(resolvedTypeArgs)));
    }

    return validatedUserDefinedType(
        udt, res::StructType::create(ctx, decl->getAs<res::StructDecl>(),
                                     std::move(resolvedTypeArgs)));
  }

  if (const auto *function =
          dynamic_cast<const ast::FunctionType *>(&parsedType)) {
    std::vector<res::Type *> args;
    for (auto &&astArg : function->args)
      if (auto *arg = resolveType(ctx, *astArg))
        args.emplace_back(arg);

    auto *retTy = resolveType(ctx, *function->ret);
    if (args.size() != function->args.size() || !retTy)
      return nullptr;

    return res::FunctionType::create(ctx, std::move(args), retTy);
  }

  if (const auto *any = dynamic_cast<const ast::AnyType *>(&parsedType)) {
    if (!allowTraitObject)
      return err::traitObjectNotPointee(any->location).report(reporter);

    varOrReturn(type, resolveType(ctx, *any->type, false, true));
    auto *anyTraitType = type->getAs<res::AnyTraitType>();

    SourceLocation loc = any->type->location;
    std::set<std::string> visited;
    if (!checkVtableCompatibility(
            loc,
            anyTraitType->withSelfType(&ctx, res::UninferredType::create(ctx)),
            visited))
      return err::traitNotTraitObjectCompatible(loc)
          .with(anyTraitType->getDecl()->identifier)
          .report(reporter);

    return anyTraitType;
  }

  if (const auto *ptr = dynamic_cast<const ast::PointerType *>(&parsedType)) {
    varOrReturn(pointeeType, resolveType(ctx, *ptr->pointeeType, true));
    return res::PointerType::create(ctx, pointeeType, ptr->isMut);
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

  res::Expr::Kind kind = res::Expr::Kind::Rvalue;
  if (unary.op == TokenKind::Asterisk) {
    auto *ptr = rhsTy->getAs<res::PointerType>();
    if (!ptr)
      return err::expectedPointerOperand(rhs->location).report(reporter);

    kind =
        ptr->isMutable() ? res::Expr::Kind::MutLvalue : res::Expr::Kind::Lvalue;
    rhsTy = ptr->getPointeeType();

    if (rhsTy->getAs<res::AnyTraitType>())
      return err::traitObjectPtrDereference(rhs->location).report(reporter);
  }

  auto *resolvedUnaryOp =
      res::UnaryOperator::create(ctx, unary.location, unary.op, rhs, kind);
  resolvedUnaryOp->setType(rhsTy);

  return resolvedUnaryOp;
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
  bool isNumericOp = !isLogicalOp && op != TokenKind::EqualEqual;

  bool typeError = !ctx.unify(lhsTy, rhsTy).empty();
  typeError |= isLogicalOp && !rhsTy->getAs<res::BuiltinBoolType>();
  typeError |= isNumericOp && !rhsTy->getAs<res::BuiltinNumberType>();
  typeError |=
      op == TokenKind::EqualEqual &&
      (rhsTy->getAs<res::StructType>() || rhsTy->getAs<res::TypeParamType>());
  if (typeError)
    return err::binopIncompatibleOperands(loc)
        .with(lhsTy->getName())
        .with(rhsTy->getName())
        .report(reporter);

  bool isCmpOp =
      op == TokenKind::EqualEqual || op == TokenKind::Lt || op == TokenKind::Gt;

  auto *resBinop = res::BinaryOperator::create(ctx, loc, binop.op, lhs, rhs);
  resBinop->setType(isCmpOp ? res::BuiltinBoolType::create(ctx) : lhsTy);

  return resBinop;
}

res::GroupingExpr *
Sema::resolveGroupingExpr(res::Context &ctx,
                          const ast::GroupingExpr &grouping) {
  varOrReturn(expr, resolveExpr(ctx, *grouping.expr));

  auto *g = res::GroupingExpr::create(ctx, grouping.location, expr);
  g->setType(expr->getType());
  return g;
}

template <typename ExpectedDecl>
res::DeclRefExpr *Sema::resolvePathExpr(res::Context &ctx,
                                        const ast::PathExpr &pathExpr) {
  const auto &fragments = pathExpr.fragments;
  int idx = 0;

  std::vector<std::pair<res::Decl *, res::Substitution>> candidates;

  if (auto *traitSpec = pathExpr.traitSpecifier.get()) {
    varOrReturn(type, resolveType(ctx, *traitSpec->type, true));
    varOrReturn(t, resolveType(ctx, *traitSpec->trait, false, true, type));
    res::TraitType *trait = t->getAs<res::TraitType>();

    if (!ctx.solveConformance(type, trait).empty())
      return err::traitNotImplemented(traitSpec->trait->location)
          .with(type->getName())
          .with(trait->getName())
          .report(reporter);

    const ast::DeclRefExpr *fragment = fragments[idx].get();
    candidates = lookupAssociatedDecls(fragment->identifier, type, trait);

    if (candidates.empty())
      return err::memberLookupFailed(fragment->location)
          .with(fragment->identifier)
          .with(trait->getName())
          .report(reporter);

    ++idx;
  }

  for (; idx != fragments.size(); ++idx) {
    const ast::DeclRefExpr *fragment = fragments[idx].get();
    if (!candidates.empty()) {
      assert(idx > 0 && "unexpected fragment index");

      res::Type *type = nullptr;
      for (auto &&candidate : candidates) {
        if (candidate.first->getAs<res::TypeDecl>()) {
          varOrReturn(dre,
                      resolveDeclRefExpr(ctx, fragments[idx - 1].get(),
                                         candidate.first, candidate.second));
          type = dre->getType();
          break;
        }
      }

      if (!type)
        return err::memberAccessInValue(fragment->location).report(reporter);

      if (type->getAs<res::TraitType>())
        return err::memberAccessInRawTrait(fragment->location).report(reporter);

      candidates = lookupAssociatedDecls(fragment->identifier, type);
      if (candidates.empty())
        return err::memberLookupFailed(fragment->location)
            .with(fragment->identifier)
            .with(type->getName())
            .report(reporter);

      continue;
    }

    assert(idx == 0 && "unexpected fragment index");

    if (fragment->identifier == selfTypeId) {
      auto *selfType = scope->getSelfType();
      if (!selfType)
        return err::selfTyNotAllowed(fragment->location).report(reporter);

      if (auto *paramType = selfType->getAs<res::TypeParamType>()) {
        candidates.emplace_back(paramType->getDecl(), paramType->getSub());
        continue;
      }

      auto *structType = selfType->getAs<res::StructType>();
      assert(structType && "unexpect self type");

      candidates.emplace_back(structType->getDecl(), structType->getSub());
      continue;
    }

    auto symbolsInScope = scope->lookupSymbol(fragment->identifier);
    if (symbolsInScope.empty())
      return err::missingSymbol(fragment->location)
          .with(fragment->identifier)
          .report(reporter);

    for (auto &&symbol : symbolsInScope)
      candidates.emplace_back(symbol, res::Substitution{});
  }

  std::vector<std::pair<res::Decl *, res::Substitution>> expectedCandidates;
  for (auto &&candidate : candidates)
    if (dynamic_cast<ExpectedDecl *>(candidate.first))
      expectedCandidates.emplace_back(std::move(candidate));

  if (expectedCandidates.empty())
    return err::wrongDeclKind(fragments.back()->location).report(reporter);

  if (expectedCandidates.size() > 1 && fragments.size() > 1)
    return err::ambigousMemberFn(fragments.back()->location).report(reporter);

  auto &&[decl, sub] = expectedCandidates.front();
  return resolveDeclRefExpr(ctx, fragments.back().get(), decl, sub);
}

res::DeclRefExpr *Sema::resolveDeclRefExpr(res::Context &ctx,
                                           const ast::DeclRefExpr *dre,
                                           res::Decl *decl,
                                           res::Substitution sub) {
  auto *valueDecl = decl->getAs<res::ValueDecl>();
  res::Expr::Kind kind = res::Expr::Kind::Lvalue;
  if (!valueDecl || decl->getAs<res::FunctionDecl>())
    kind = res::Expr::Kind::Rvalue;
  else if (valueDecl->isMutable)
    kind = res::Expr::Kind::MutLvalue;

  auto *gdc = decl->getAs<res::GenericDeclContext>();
  if (gdc) {
    for (auto &&typeParam : gdc->typeParams) {
      auto *tpType = typeParam->getType();
      auto *subType = res::UninferredType::create(ctx);

      sub[tpType] = subType;

      for (auto &&trait : ctx.getDirectConformance(tpType)) {
        auto *instTrait = ctx.instantiate(trait, sub)->getAs<res::TraitType>();
        subType->addObligation(instTrait);
      }
    }
  }

  if (auto *typeArgList = dre->typeArgumentList.get()) {
    if (!gdc || gdc->typeParams.empty())
      return err::notGeneric(typeArgList->location)
          .with(decl->identifier)
          .report(reporter);

    const auto &args = typeArgList->args;
    varOrReturn(res, checkTypeParameterCount(typeArgList->location, args.size(),
                                             gdc->typeParams.size()));

    for (int i = 0; i < args.size(); ++i) {
      varOrReturn(arg, resolveType(ctx, *args[i]));
      auto *expectedType = sub[gdc->typeParams[i]->getType()];

      if (const auto &errs = ctx.unify(expectedType, arg); !errs.empty()) {
        for (auto &&err : errs)
          err::inferenceError(args[i]->location).with(err).report(reporter);

        return nullptr;
      }
    }
  }

  auto *resDre = res::DeclRefExpr::create(ctx, dre->location, decl, kind, sub);
  resDre->setType(ctx.instantiate(decl->getType(), sub));

  if (modifiers & AddressTaken)
    resDre->decl->setStorageNeeded();

  return functionInfo->declReferences.emplace_back(resDre);
}

std::vector<std::pair<res::Decl *, res::Substitution>>
Sema::lookupAssociatedDecls(std::string identifier,
                            res::Type *type,
                            res::TraitType *trait) {
  std::vector<std::pair<res::Decl *, res::Substitution>> candidates;

  if (trait) {
    for (auto &&decl : trait->getDecl()->lookupDirect(identifier))
      candidates.emplace_back(decl, trait->getSub());

    return candidates;
  }

  if (auto *s = type->getAs<res::StructType>())
    for (auto &&decl : s->getDecl()->lookupDirect(identifier))
      candidates.emplace_back(decl, type->getSub());

  if (!candidates.empty())
    return candidates;

  for (auto &&trait : ctx.getEveryConformance(type))
    for (auto &&decl : trait->getDecl()->lookupDirect(identifier))
      candidates.emplace_back(decl, trait->getSub());

  if (!candidates.empty())
    return candidates;

  auto extensions = ctx.getExtensions(type, trait);

  for (auto &&[extension, sub] : extensions) {
    auto *trait = extension->trait;
    for (auto &&decl : trait->getDecl()->lookupDirect(identifier))
      candidates.emplace_back(decl, ctx.instantiate(trait->getSub(), sub));
  }

  return candidates;
}

std::pair<res::Expr *, std::vector<res::Expr *>>
Sema::resolveCallBase(res::Context &ctx, const ast::CallExpr &call) {
  const auto *me = dynamic_cast<const ast::MemberExpr *>(call.callee.get());
  if (!me) {
    WithModifiersRAII callee(this, IsCallee);
    return {resolveExpr(ctx, *call.callee), {}};
  }

  res::MemberExpr *resMemberExpr = resolveMemberExpr(ctx, *me, true);
  if (!resMemberExpr)
    return {nullptr, {}};

  const auto *method = resMemberExpr->member->decl->getAs<res::FunctionDecl>();
  if (!method)
    return {resMemberExpr, {}};

  if (method->params.empty() || method->params[0]->identifier != selfParamId)
    return {err::classMethodCallOnInstance(call.location).report(reporter), {}};

  res::Expr *selfArg = resMemberExpr->base;
  if (auto *deref = dynamic_cast<res::ImplicitDerefExpr *>(selfArg))
    selfArg = deref->dre;

  if (!selfArg->isLvalue()) {
    auto *mte =
        res::MaterializeTemporaryExpr::create(ctx, selfArg->location, selfArg);
    mte->setType(selfArg->getType());
    selfArg = mte;
  }

  auto *targetType =
      resMemberExpr->getType()->getAs<res::FunctionType>()->getArgs()[0];
  selfArg = withPtrToRefDecay(targetType, selfArg);
  selfArg = withImplicitAsRef(targetType, selfArg);

  return {resMemberExpr->member, {selfArg}};
}

res::CallExpr *Sema::resolveCallExpr(res::Context &ctx,
                                     const ast::CallExpr &call) {
  auto &&[callee, args] = resolveCallBase(ctx, call);
  if (!callee)
    return nullptr;

  auto *fnType = callee->getType()->getAs<res::FunctionType>();
  if (!fnType)
    return err::invalidCallTy(call.location)
        .with(callee->getType()->getName())
        .report(reporter);

  if (!args.empty()) {
    auto msgs = ctx.unify(args[0]->getType(), fnType->getArgs()[0]);
    if (!msgs.empty()) {
      for (auto &&msg : msgs)
        err::inferenceError(args[0]->location).with(msg).report(reporter);
      return nullptr;
    }
  }

  std::vector<res::Type *> argTypes = fnType->getArgs();

  size_t expectedArgCnt = argTypes.size();
  size_t implicitArgCnt = args.size();
  size_t sourceSpelledArgCnt = call.arguments.size();

  if ((sourceSpelledArgCnt + implicitArgCnt) != expectedArgCnt)
    return err::wrongArgCount(call.location)
        .with(expectedArgCnt - implicitArgCnt)
        .with(sourceSpelledArgCnt)
        .report(reporter);

  for (auto &&arg : call.arguments) {
    res::Type *expectedTy = argTypes[args.size()];

    WithModifiersRAII unaryAmpAllowed(
        this, expectedTy->getAs<res::RefType>() ? AddressTaken : 0);

    varOrReturn(resolvedArg, resolveExpr(ctx, *arg, expectedTy));
    varOrReturn(coercedArg, asTraitObjectIfNeeded(expectedTy, resolvedArg));
    varOrReturn(promotedArg, withImplicitAsRef(expectedTy, coercedArg));
    promotedArg = withPtrToRefDecay(expectedTy, promotedArg);

    res::Type *actualTy = promotedArg->getType();

    if (const auto &errors = ctx.unify(actualTy, expectedTy); !errors.empty()) {
      for (auto &&error : errors)
        err::inferenceError(promotedArg->location).with(error).report(reporter);
      return nullptr;
    }

    promotedArg->setConstantValue(cee->evaluate(*promotedArg));
    args.emplace_back(promotedArg);
  }

  auto *ce = res::CallExpr::create(ctx, call.location, callee, std::move(args));
  ce->setType(fnType->getReturnType());
  return ce;
}

res::StructInstantiationExpr *Sema::resolveStructInstantiation(
    res::Context &ctx,
    const ast::StructInstantiationExpr &structInstantiation) {
  varOrReturn(path, resolvePathExpr<res::StructDecl>(
                        ctx, *structInstantiation.structRef));

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

    res::Type *fieldTy =
        ctx.instantiate(fieldDecl->getType(), structTy->getSub());

    auto *resolvedInitExpr = resolveExpr(ctx, *initStmt->initializer, fieldTy);
    if (!resolvedInitExpr) {
      error = true;
      continue;
    }

    res::Expr *coercedInitExpr =
        asTraitObjectIfNeeded(fieldTy, resolvedInitExpr);
    if (!coercedInitExpr) {
      error = true;
      continue;
    }

    res::Type *initTy = coercedInitExpr->getType();
    if (const auto &msg = ctx.unify(initTy, fieldTy); !msg.empty()) {
      for (auto &&error : msg)
        err::inferenceError(coercedInitExpr->location)
            .with(error)
            .report(reporter);
      error = true;
      continue;
    }

    inits[id] = resolvedFieldInits.emplace_back(
        res::FieldInitStmt::create(ctx, loc, fieldDecl, coercedInitExpr));
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

  auto *sie = res::StructInstantiationExpr::create(
      ctx, structInstantiation.location, path, std::move(resolvedFieldInits));
  sie->setType(structTy);
  return sie;
}

res::UnaryOperator *Sema::insertUnaryDeref(res::Context &ctx, res::Expr *val) {
  res::PointerType *ptrType = val->getType()->getAs<res::PointerType>();

  res::Expr::Kind kind = ptrType->isMutable() ? res::Expr::Kind::MutLvalue
                                              : res::Expr::Kind::Lvalue;

  auto *uo = res::UnaryOperator::create(ctx, val->location, TokenKind::Asterisk,
                                        val, kind);
  uo->setType(ptrType->getPointeeType());
  return uo;
}

res::MemberExpr *Sema::resolveMemberExpr(res::Context &ctx,
                                         const ast::MemberExpr &memberExpr,
                                         bool isCallee) {
  WithModifiersRAII mods(this, isCallee ? AddressTaken : 0);
  varOrReturn(base, resolveExpr(ctx, *memberExpr.base));

  auto *baseType = base->getType();
  auto *ptrType = baseType->getAs<res::PointerType>();

  const ast::DeclRefExpr *dre = memberExpr.member.get();
  auto *lookupType = ptrType ? ptrType->getPointeeType() : baseType;

  auto candidates = lookupAssociatedDecls(dre->identifier, lookupType);
  if (ptrType && candidates.empty())
    candidates = lookupAssociatedDecls(dre->identifier, ptrType);

  if (candidates.empty())
    return err::memberLookupFailed(dre->location)
        .with(dre->identifier)
        .with(baseType->getName())
        .report(reporter);

  if (candidates.size() > 1)
    return err::ambigousMemberFn(dre->location).report(reporter);

  auto &&[decl, sub] = candidates.back();

  varOrReturn(memberDre, resolveDeclRefExpr(ctx, dre, decl, sub));

  if (!isCallee) {
    if (memberDre->decl->getAs<res::FunctionDecl>())
      return err::expectedMethodCall(memberExpr.location).report(reporter);

    if (ptrType)
      base = insertUnaryDeref(ctx, base);
  }

  auto *me = res::MemberExpr::create(ctx, memberExpr.location, base, memberDre);
  me->setType(memberDre->getType());
  return me;
}

res::GCExpr *Sema::resolveGCExpr(res::Context &ctx, const ast::GCExpr &gc) {
  varOrReturn(expr, resolveExpr(ctx, *gc.expr));
  expr->setConstantValue(cee->evaluate(*expr));

  auto *gce = res::GCExpr::create(ctx, gc.location, expr);
  gce->setType(res::PointerType::create(ctx, expr->getType(), gc.isMut));
  return gce;
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

  auto *closure =
      res::StructDecl::create(ctx, loc, structId.str(), scope->getDeclContext(),
                              std::vector<res::TypeParamDecl *>{}, true);
  closure->setType(res::StructType::create(ctx, closure));

  bool error = false;
  std::vector<res::Type *> paramTypes = {};
  std::vector<res::ParamDecl *> resolvedParams = {};

  EnterNewScopeRAII paramScope(this);
  {
    WithModifiersRAII lambdaParamList(this, MissingTypeAnnotationsAllowed);
    int i = 0;
    for (auto &&param : lambdaExpr.params) {
      auto [resolvedParam, err] = resolveParamDecl(ctx, param.get());

      if (resolvedParam->getType()->getAs<res::UninferredType>() &&
          expectedFnType && i < expectedFnType->getArgs().size())
        ctx.unify(resolvedParam->getType(), expectedFnType->getArgs()[i]);

      if (resolvedParam->getType()->getAs<res::UninferredType>()) {
        err::annotationsNeeded(param->location)
            .with(param->identifier)
            .report(reporter);
        error = true;
      }

      paramTypes.emplace_back(resolvedParam->getType());
      resolvedParams.emplace_back(resolvedParam);

      error |= !insertDeclToCurrentScope(resolvedParam);

      if (param->identifier == selfParamId) {
        err::selfParamNotAllowed(param->location).report(reporter);
        error = true;
      }

      ++i;
    }
  }

  res::Type *returnTy = lambdaExpr.returnType
                            ? resolveType(ctx, *lambdaExpr.returnType)
                            : res::UninferredType::create(ctx);
  if (returnTy && returnTy->getAs<res::UninferredType>() && expectedFnType)
    ctx.unify(returnTy, expectedFnType->getReturnType());

  if (!returnTy || error)
    return nullptr;

  auto *lambdaTy = res::FunctionType::create(ctx, paramTypes, returnTy);
  if (expectedFnType) {
    auto msgs = ctx.unify(expectedFnType, lambdaTy);
    if (!msgs.empty()) {
      for (auto &&msg : msgs)
        err::inferenceError(loc).with(msg).report(reporter);
      return nullptr;
    }
  }

  auto *paramType = res::PointerType::create(ctx, closure->getType(), false);
  paramTypes.emplace_back(paramType);

  auto *fn = res::FunctionDecl::create(ctx, loc, lambdaFunctionId, closure,
                                       std::vector<res::TypeParamDecl *>{});
  fn->setType(res::FunctionType::create(ctx, paramTypes, returnTy));
  closure->insertDecl(fn);

  auto *p = res::ParamDecl::create(ctx, loc, "closure", fn, false);
  p->setType(paramType);
  resolvedParams.emplace_back(p);
  fn->setParams(std::move(resolvedParams));

  auto *resLambdaExpr = res::LambdaExpr::create(ctx, loc, closure, fn);
  resLambdaExpr->setType(lambdaTy);

  std::vector<const ast::Expr *> pendingCaptureInits;
  {
    WithFunctionInfoRAII lambdaInfo(this, {fn, resLambdaExpr, scope});

    if (res::Block *block = resolveBlock(ctx, *lambdaExpr.body)) {
      fn->setBody(block);

      res::Type *retTy =
          fn->getType()->getAs<res::FunctionType>()->getReturnType();
      if (retTy->getAs<res::UninferredType>())
        ctx.unify(retTy, res::BuiltinUnitType::create(ctx));

      error |= !runPostFunctionBodyChecks();
    }

    error |= !fn->body;
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

// FIXME: should '&' types be allowed as well?
res::Expr *Sema::asTraitObjectIfNeeded(res::Type *targetType, res::Expr *expr) {
  auto *targetPtr = targetType->getAs<res::PointerType>();
  if (!targetPtr)
    return expr;

  auto *exprPtr = expr->getType()->getAs<res::PointerType>();
  if (!exprPtr)
    return expr;

  auto *exprPointee = exprPtr->getPointeeType();
  auto *targetAny = targetPtr->getPointeeType()->getAs<res::AnyTraitType>();

  if (exprPointee->getAs<res::AnyTraitType>() || !targetAny ||
      targetPtr->isMutable() != exprPtr->isMutable())
    return expr;

  auto *requiredTrait = targetAny->withSelfType(&ctx, exprPointee);
  auto errors = ctx.solveConformance(exprPointee, requiredTrait);

  if (errors.empty()) {
    auto *top = res::TraitObjectPromoExpr::create(ctx, expr->location, expr);
    top->setType(targetType);
    return top;
  }

  for (auto &&error : errors)
    err::inferenceError(expr->location).with(error).report(reporter);
  return nullptr;
}

res::Expr *Sema::withPtrToRefDecay(res::Type *targetType, res::Expr *expr) {
  auto *targetRefType = targetType->getAs<res::RefType>();
  auto *currentPtrType = expr->getType()->getAs<res::PointerType>();

  if (!targetRefType || !currentPtrType)
    return expr;

  if (targetRefType->isMutable() && !currentPtrType->isMutable())
    return expr;

  res::Type *referencedType = targetRefType->getReferencedType();
  res::Type *pointerType = currentPtrType->getPointeeType();
  if (!ctx.unify(referencedType, pointerType).empty())
    return expr;

  auto *p2b = res::ImplicitPtrToRefDecay::create(ctx, expr->location, expr);
  p2b->setType(targetRefType);
  return p2b;
}

res::Expr *Sema::withImplicitAsRef(res::Type *targetType, res::Expr *expr) {
  auto *targetRefType = targetType->getAs<res::RefType>();
  if (!targetRefType)
    return expr;

  if (expr->getType()->getAs<res::RefType>())
    return expr;

  if (!expr->isLvalue())
    return err::rvalueRef(expr->location).report(reporter);

  if (targetRefType->isMutable() && !expr->isMutable())
    return expr;

  if (!ctx.unify(targetRefType->getReferencedType(), expr->getType()).empty())
    return expr;

  auto *be = res::ImplicitAsRefExpr::create(ctx, expr->location, expr);
  be->setType(
      res::RefType::create(ctx, expr->getType(), targetRefType->isMutable()));
  return be;
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
  varOrReturn(cond, resolveExpr(ctx, *ifStmt.condition,
                                res::BuiltinBoolType::create(ctx)));
  if (!ctx.unify(cond->getType(), res::BuiltinBoolType::create(ctx)).empty())
    return err::expectedBoolCondition(cond->location).report(reporter);

  varOrReturn(trueBlock, resolveBlock(ctx, *ifStmt.trueBlock));

  res::Block *falseBlock = nullptr;
  if (ifStmt.falseBlock) {
    falseBlock = resolveBlock(ctx, *ifStmt.falseBlock);
    if (!falseBlock)
      return nullptr;
  }

  cond->setConstantValue(cee->evaluate(*cond));
  return res::IfStmt::create(ctx, ifStmt.location, cond, trueBlock, falseBlock);
}

res::WhileStmt *Sema::resolveWhileStmt(res::Context &ctx,
                                       const ast::WhileStmt &whileStmt) {
  varOrReturn(cond, resolveExpr(ctx, *whileStmt.condition,
                                res::BuiltinBoolType::create(ctx)));
  if (!ctx.unify(cond->getType(), res::BuiltinBoolType::create(ctx)).empty())
    return err::expectedBoolCondition(cond->location).report(reporter);

  varOrReturn(body, resolveBlock(ctx, *whileStmt.body));

  cond->setConstantValue(cee->evaluate(*cond));
  return res::WhileStmt::create(ctx, whileStmt.location, cond, body);
}

res::DeclStmt *Sema::resolveDeclStmt(res::Context &ctx,
                                     const ast::DeclStmt &declStmt) {
  varOrReturn(varDecl, resolveVarDecl(ctx, *declStmt.varDecl));

  if (!insertDeclToCurrentScope(varDecl))
    return nullptr;

  return res::DeclStmt::create(ctx, declStmt.location, varDecl);
}

res::Assignment *Sema::resolveAssignment(res::Context &ctx,
                                         const ast::Assignment &assignment) {
  varOrReturn(lhs, resolveExpr(ctx, *assignment.assignee));
  varOrReturn(rhs, resolveExpr(ctx, *assignment.expr, lhs->getType()));

  if (!lhs->isLvalue())
    return err::rvalueAssignment(lhs->location).report(reporter);
  auto *lhsTy = lhs->getType();

  varOrReturn(coercedRhs, asTraitObjectIfNeeded(lhsTy, rhs));
  auto *rhsTy = coercedRhs->getType();

  if (const auto &errors = ctx.unify(lhsTy, rhsTy); !errors.empty()) {
    for (auto &&error : errors)
      err::inferenceError(coercedRhs->location).with(error).report(reporter);

    return err::incompatibleAssignment(coercedRhs->location)
        .with(lhsTy->getName())
        .with(rhsTy->getName())
        .report(reporter);
  }

  coercedRhs->setConstantValue(cee->evaluate(*coercedRhs));
  return res::Assignment::create(ctx, assignment.location, lhs, coercedRhs);
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

    varOrReturn(coercedExpr, asTraitObjectIfNeeded(retTy, expr));
    expr = coercedExpr;

    res::Type *exprTy = expr->getType();

    if (!ctx.unify(retTy, exprTy).empty())
      return err::invalidReturnValue(expr->location)
          .with(exprTy->getName())
          .with(retTy->getName())
          .report(reporter);

    expr->setConstantValue(cee->evaluate(*expr));
  }

  return res::ReturnStmt::create(ctx, returnStmt.location, expr);
}

res::Expr *Sema::resolveExpr(res::Context &ctx,
                             const ast::Expr &expr,
                             res::Type *typeHint) {
  if (const auto *number = dynamic_cast<const ast::NumberLiteral *>(&expr)) {
    auto *nl = res::NumberLiteral::create(ctx, number->location,
                                          std::stod(number->value));
    nl->setType(res::BuiltinNumberType::create(ctx));
    return nl;
  }

  if (const auto *boolLiteral = dynamic_cast<const ast::BoolLiteral *>(&expr)) {
    auto *bl = res::BoolLiteral::create(ctx, boolLiteral->location,
                                        boolLiteral->value == "true");
    bl->setType(res::BuiltinBoolType::create(ctx));
    return bl;
  }

  if (const auto *unit = dynamic_cast<const ast::UnitLiteral *>(&expr)) {
    auto *ul = res::UnitLiteral::create(ctx, unit->location);
    ul->setType(res::BuiltinUnitType::create(ctx));
    return ul;
  }

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

  if (const auto *gc = dynamic_cast<const ast::GCExpr *>(&expr))
    return resolveGCExpr(ctx, *gc);

  if (const auto *lambda = dynamic_cast<const ast::LambdaExpr *>(&expr))
    return resolveLambdaExpr(ctx, *lambda, typeHint);

  if (const auto *path = dynamic_cast<const ast::PathExpr *>(&expr)) {
    varOrReturn(resPath, resolvePathExpr<res::ValueDecl>(ctx, *path));

    const res::Decl *decl = resPath->decl;
    bool isFunctionDecl = decl->getAs<res::FunctionDecl>();

    if (decl->getAs<res::FieldDecl>())
      return err::fieldReference(resPath->location)
          .with(decl->identifier)
          .report(reporter);

    if (resPath->getReceiverType() &&
        resPath->getReceiverType()->getAs<res::AnyTraitType>() &&
        isFunctionDecl && !(modifiers & IsCallee))
      return err::traitObjectMethodNotCalled(resPath->location)
          .report(reporter);

    auto *refType = resPath->getType()->getAs<res::RefType>();

    if (functionInfo && functionInfo->lambda && !isFunctionDecl) {
      res::Decl *insideDecl = nullptr;
      if (auto r = scope->lookupSymbol(decl->identifier); !r.empty())
        insideDecl = r.front();

      res::Decl *outsideDecl = nullptr;
      if (auto r = functionInfo->lambdaParamScope->getParent()->lookupSymbol(
              decl->identifier);
          !r.empty())
        outsideDecl = r.front();

      if (outsideDecl == insideDecl) {
        if (refType)
          return err::refParamCapture(resPath->location)
              .with(decl->identifier)
              .report(reporter);

        auto *lambda = functionInfo->lambda;

        res::FieldDecl *field = nullptr;
        if (auto r = lambda->closure->lookupDirect(decl->identifier);
            !r.empty())
          field = r.front()->getAs<res::FieldDecl>();

        if (!field) {
          field = res::FieldDecl::create(ctx, lambda->location,
                                         decl->identifier, lambda->closure);
          field->setType(resPath->getType());
          lambda->closure->insertDecl(field);
          functionInfo->pendingCaptureInits.emplace_back(&expr);
        }

        res::Expr *base = res::DeclRefExpr::create(
            ctx, lambda->location, lambda->method->params.back(),
            res::Expr::Kind::Lvalue, res::Substitution{});
        base->setType(lambda->method->params.back()->getType());

        base = res::UnaryOperator::create(ctx, lambda->location,
                                          TokenKind::Asterisk, base,
                                          res::Expr::Kind::Lvalue);
        base->setType(lambda->closure->getType());

        auto *fieldDre = res::DeclRefExpr::create(ctx, lambda->location, field,
                                                  res::Expr::Kind::Lvalue,
                                                  res::Substitution{});
        fieldDre->setType(field->getType());

        auto *me =
            res::MemberExpr::create(ctx, lambda->location, base, fieldDre);
        me->setType(fieldDre->getType());
        return me;
      }
    }

    if (refType && (!typeHint || !typeHint->getAs<res::RefType>())) {
      auto *ide =
          res::ImplicitDerefExpr::create(ctx, resPath->location, resPath);
      ide->setType(refType->getReferencedType());
      return ide;
    }

    return resPath;
  }

  llvm_unreachable("unexpected expression");
}

res::Block *Sema::resolveBlock(res::Context &ctx, const ast::Block &block) {
  std::vector<res::Stmt *> resolvedStatements;

  bool error = false;
  int reportUnreachableCount = 0;

  EnterNewScopeRAII blockScope(this);
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

  return res::Block::create(ctx, block.location, std::move(resolvedStatements));
}

res::TypeExtension *
Sema::resolveTypeExtension(res::Context &ctx,
                           const ast::TypeExtension &extension) {
  EnterNewScopeRAII typeParamScope(this);

  auto typeParams = resolveTypeParamsWithoutBounds(ctx, extension.typeParams);
  if (!resolveGenericParamsInCurrentScope(ctx, typeParams,
                                          extension.typeParams))
    return nullptr;

  varOrReturn(type, resolveType(ctx, *extension.type));
  varOrReturn(trait, resolveType(ctx, *extension.trait, false, true, type));

  auto *traitType = trait->getAs<res::TraitType>();

  res::Substitution probeSub;
  for (auto &&typeParam : typeParams)
    probeSub[typeParam->getType()] = res::UninferredType::create(ctx);

  auto conflictingExtensions = ctx.getExtensions(
      ctx.instantiate(type, probeSub),
      ctx.instantiate(traitType, probeSub)->getAs<res::TraitType>());
  if (!conflictingExtensions.empty()) {
    for (auto &&[extensionDecl, sub] : conflictingExtensions)
      err::conflictingExtension(extension.location)
          .with(type->getName())
          .with(traitType->getName())
          .with(extensionDecl->type->getName())
          .with(extensionDecl->trait->getName())
          .report(reporter);

    return nullptr;
  }

  auto *typeExtension = res::TypeExtension::create(
      ctx, extension.location, std::move(typeParams), type, traitType);
  ctx.translationUnit.extensions.emplace_back(typeExtension);

  EnterNewScopeRAII extensionScope(this, typeExtension);
  for (auto &&fn : extension.functions) {
    res::FunctionDecl *traitFn = nullptr;
    if (auto r = traitType->getDecl()->lookupDirect(fn->identifier); !r.empty())
      traitFn = r.front()->getAs<res::FunctionDecl>();

    if (!traitFn) {
      err::memberFnLookupFailed(fn->location)
          .with(fn->identifier)
          .with(traitType->getDecl()->identifier)
          .report(reporter);
      continue;
    }

    auto *implFn = resolveFunctionDecl(ctx, *fn);
    if (!implFn)
      continue;

    auto traitFnTypeParams = traitFn->typeParams;
    auto implTypeParams = implFn->typeParams;

    if (!checkTypeParameterCount(implFn->location, implTypeParams.size(),
                                 traitFnTypeParams.size()))
      continue;

    res::Substitution sub;
    bool error = false;

    for (size_t i = 0; i < implTypeParams.size(); ++i) {
      res::Type *traitParamTy = traitFn->typeParams[i]->getType();
      res::Type *implParamTy = implFn->typeParams[i]->getType();

      sub[implParamTy] = traitParamTy;
      for (auto &&trait : ctx.getDirectConformance(implParamTy)) {
        trait = ctx.instantiate(trait, sub)->getAs<res::TraitType>();

        auto errors = ctx.solveConformance(traitParamTy, trait);
        if (errors.empty())
          continue;

        for (auto &&error : errors)
          err::inferenceError(implFn->typeParams[i]->location)
              .with(error)
              .report(reporter);

        err::stricterParamTy(implFn->typeParams[i]->location)
            .with(traitParamTy->getName())
            .with(implParamTy->getName())
            .report(reporter);
        error = true;
      }
    }
    if (error)
      continue;

    auto traitSub = traitType->getSub();

    res::Type *expectedType =
        ctx.instantiate(ctx.instantiate(traitFn->getType(), traitSub), sub);
    res::Type *actualType = implFn->getType();

    if (!ctx.unify(expectedType, actualType).empty()) {
      err::fnSignatureMismatch(implFn->location)
          .with(expectedType->getName())
          .with(actualType->getName())
          .report(reporter);
      continue;
    }

    if (insertDeclToCurrentScope(implFn))
      typeExtension->insertDecl(implFn);
  }

  if (typeExtension->decls.size() != extension.functions.size())
    return nullptr;

  return typeExtension;
}

bool Sema::resolveExtensionBody(res::Context &ctx,
                                res::TypeExtension *extension,
                                const ast::TypeExtension &astExtension) {
  bool error = false;
  auto *trait = extension->trait;

  for (auto &&requirement : ctx.getDirectConformance(trait)) {
    res::Type *type = extension->type;
    if (ctx.getExtensions(type, requirement).empty()) {
      err::missingRequirement(extension->location)
          .with(type->getName())
          .with(trait->getName())
          .with(type->getName())
          .with(requirement->getName())
          .report(reporter);
      error = true;
    }
  }

  EnterNewScopeRAII extensionScope(this, extension);
  for (int i = 0; i < astExtension.functions.size(); ++i)
    error |=
        !resolveFunctionBody(ctx, *astExtension.functions[i],
                             extension->decls[i]->getAs<res::FunctionDecl>());

  error |= !implementsAllNecessaryTraitFunctions(ctx, extension);
  return !error;
}

res::VarDecl *Sema::resolveVarDecl(res::Context &ctx,
                                   const ast::VarDecl &varDecl) {
  res::Type *declTy = varDecl.type ? resolveType(ctx, *varDecl.type)
                                   : res::UninferredType::create(ctx);
  if (!declTy)
    return nullptr;

  res::Expr *initializer = nullptr;
  if (varDecl.initializer) {
    varOrReturn(init, resolveExpr(ctx, *varDecl.initializer, declTy));

    varOrReturn(coercedInit, asTraitObjectIfNeeded(declTy, init));
    init = coercedInit;
    auto *initTy = init->getType();

    for (auto &&err : ctx.unify(declTy, initTy)) {
      std::cout << err << '\n';
    }

    if (!ctx.unify(declTy, initTy).empty())
      return err::initTyMismatch(init->location)
          .with(initTy->getName())
          .with(declTy->getName())
          .report(reporter);

    init->setConstantValue(cee->evaluate(*init));
    initializer = init;
  }

  auto *vd = res::VarDecl::create(ctx, varDecl.location, varDecl.identifier,
                                  scope->getDeclContext(), varDecl.isMutable,
                                  initializer);
  vd->setType(declTy);
  return vd;
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
    auto *resTP = res::TypeParamDecl::create(ctx, tp->location, tp->identifier);
    resTP->setType(res::TypeParamType::create(ctx, resTP));
    resTypeParams.emplace_back(resTP);
  }

  return resTypeParams;
}

bool Sema::resolveGenericParamsInCurrentScope(
    res::Context &ctx,
    const std::vector<res::TypeParamDecl *> &resParams,
    const std::vector<std::unique_ptr<ast::TypeParamDecl>> &astParams) {
  bool error = false;
  int offset = 0;

  for (size_t i = 0; i < resParams.size(); ++i) {
    if (resParams[i]->isImplicitSelf) {
      assert(i == 0 && "implicit self can only be the first parameter");
      insertDeclToCurrentScope(resParams[0]);
      offset = 1;
      continue;
    }

    res::TypeParamDecl *resParam = resParams[i];
    error |= !insertDeclToCurrentScope(resParam);

    if (auto *astConformance = astParams[i - offset]->traitConformance.get()) {
      resParam->conformance =
          resolveTraitConformance(ctx, *astConformance, resParam->getType());

      if (!resParam->conformance)
        return false;
    }
  }

  return !error;
}

bool Sema::implementsAllNecessaryTraitFunctions(res::Context &ctx,
                                                res::TypeExtension *extension) {
  bool error = false;

  for (auto &&fn : extension->trait->getDecl()->getAll<res::FunctionDecl>()) {
    if (!fn->mustImplement || !extension->lookupDirect(fn->identifier).empty())
      continue;

    err::missingTraitFn(fn->location)
        .with(extension->type->getName())
        .with(fn->identifier)
        .with(extension->trait->getName())
        .report(reporter);
    error = true;
  }

  return !error;
}

res::FunctionDecl *Sema::resolveFunctionDecl(res::Context &ctx,
                                             const ast::FunctionDecl &decl) {
  EnterNewScopeRAII typeParamScope(this);

  auto typeParams = resolveTypeParamsWithoutBounds(ctx, decl.typeParameters);
  bool error =
      !resolveGenericParamsInCurrentScope(ctx, typeParams, decl.typeParameters);
  for (auto &&tp : typeParams) {
    for (auto &&decl : scope->getParent()->lookupSymbol(tp->identifier)) {
      if (decl->getAs<res::TypeParamDecl>()) {
        err::typeParamShadowed(tp->location)
            .with(tp->identifier)
            .report(reporter);
        error = true;
        break;
      }
    }
  }

  std::vector<res::Type *> paramTypes;
  std::vector<res::ParamDecl *> resolvedParams;

  EnterNewScopeRAII paramScope(this);
  for (auto &&param : decl.params) {
    auto [resolvedParam, err] = resolveParamDecl(ctx, param.get());

    paramTypes.emplace_back(resolvedParam->getType());
    resolvedParams.emplace_back(resolvedParam);

    error |= err;
    error |= !insertDeclToCurrentScope(resolvedParam);
    error |= !checkSelfParameter(resolvedParam, resolvedParams.size() - 1);
  }

  res::Type *retTy = decl.type ? resolveType(ctx, *decl.type)
                               : res::BuiltinUnitType::create(ctx);
  error |= !retTy;

  if (error)
    return nullptr;

  auto *fn = res::FunctionDecl::create(ctx, decl.location, decl.identifier,
                                       scope->getDeclContext(), typeParams);
  fn->setType(res::FunctionType::create(ctx, std::move(paramTypes), retTy));
  fn->setParams(std::move(resolvedParams));
  return fn;
}

res::FunctionDecl *
Sema::resolveFunctionBody(res::Context &ctx,
                          const ast::FunctionDecl &functionDecl,
                          res::FunctionDecl *function) {
  if (!functionDecl.body)
    return function;

  WithFunctionInfoRAII currentFnInfo(this, {function, nullptr, nullptr, {}});

  EnterNewScopeRAII typeParamScope(this, function);
  for (auto &&typeParam : function->typeParams)
    insertDeclToCurrentScope(typeParam);

  EnterNewScopeRAII paramScope(this);
  for (auto &&param : function->params)
    insertDeclToCurrentScope(param);

  auto *body = resolveBlock(ctx, *functionDecl.body);
  if (!body) {
    function->setBody(res::Block::create(ctx, functionDecl.location,
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

  res::Type *paramTy = nullptr;
  bool error = false;

  if (param->type) {
    paramTy = resolveType(ctx, *param->type, param->refModifier != nullptr);
    error |= !paramTy;
  }

  if (!paramTy)
    paramTy = res::UninferredType::create(ctx);

  if (auto *ref = param->refModifier.get())
    paramTy = res::RefType::create(ctx, paramTy, ref->isMut);

  auto *referenceType = paramTy->getAs<res::RefType>();
  if (referenceType && param->isMutable) {
    err::mutRefParameter(param->location).report(reporter);
    error = true;
  }

  auto *p = res::ParamDecl::create(
      ctx, param->location, param->identifier, scope->getDeclContext(),
      param->isMutable || referenceType && referenceType->isMutable());
  p->setType(paramTy);
  return std::make_pair(p, error);
}

res::TraitConformance *
Sema::resolveTraitConformance(res::Context &ctx,
                              const ast::TraitConformance &conformance,
                              res::Type *type) {
  std::vector<res::TraitType *> traits;

  for (auto &&trait : conformance.traits) {
    varOrReturn(resTrait, resolveType(ctx, *trait, false, true, type));
    traits.emplace_back(resTrait->getAs<res::TraitType>());
  }

  return res::TraitConformance::create(ctx, conformance.location, type,
                                       std::move(traits));
}

res::TraitDecl *Sema::resolveTraitDecl(res::Context &ctx,
                                       const ast::TraitDecl &decl) {
  auto *self = res::TypeParamDecl::create(ctx, decl.location, selfTypeId, true);
  auto *selfType = res::TypeParamType::create(ctx, self);
  self->setType(selfType);

  std::vector<res::TypeParamDecl *> typeParams = {self};
  for (auto &&tp : resolveTypeParamsWithoutBounds(ctx, decl.typeParameters))
    typeParams.emplace_back(tp);

  auto *trait = res::TraitDecl::create(ctx, decl.location, decl.identifier,
                                       nullptr, std::move(typeParams));

  std::vector<res::Type *> typeParamTys;
  for (auto &&typeParam : trait->typeParams)
    typeParamTys.emplace_back(typeParam->getType());

  auto *traitType = res::TraitType::create(ctx, trait, typeParamTys);
  trait->setType(traitType);

  self->conformance = res::TraitConformance::create(
      ctx, decl.location, selfType, std::vector<res::TraitType *>{traitType});

  return trait;
}

bool Sema::resolveTraitBody(res::Context &ctx,
                            res::TraitDecl &traitDecl,
                            const ast::TraitDecl &astDecl) {
  EnterNewScopeRAII traitParamScope(this, &traitDecl);
  bool error = !resolveGenericParamsInCurrentScope(ctx, traitDecl.typeParams,
                                                   astDecl.typeParameters);

  if (astDecl.traitConformance) {
    traitDecl.conformance = resolveTraitConformance(
        ctx, *astDecl.traitConformance, traitDecl.typeParams[0]->getType());
    error |= !traitDecl.conformance;
  }

  EnterNewScopeRAII traitBodyScope(this);
  for (auto &&fn : astDecl.traitFunctions) {
    auto *resolvedFn = resolveFunctionDecl(ctx, *fn);
    if (!insertDeclToCurrentScope(resolvedFn)) {
      error = true;
      continue;
    }

    resolvedFn->setMustImplement(!fn->body);
    traitDecl.insertDecl(resolvedFn);
  }

  return !error;
}

bool Sema::resolveTraitFunctionBodies(res::Context &ctx,
                                      res::TraitDecl &traitDecl,
                                      const ast::TraitDecl &astDecl) {
  EnterNewScopeRAII traitParamScope(this, &traitDecl);
  for (auto &&typeParamDecl : traitDecl.typeParams)
    insertDeclToCurrentScope(typeParamDecl);

  EnterNewScopeRAII traitBodyScope(this);

  bool error = false;
  int idx = 0;
  for (auto &&fn : traitDecl.getAll<res::FunctionDecl>()) {
    error |= !resolveFunctionBody(ctx, *astDecl.traitFunctions[idx], fn);
    ++idx;
  }

  return !error;
}

res::StructDecl *Sema::resolveStructDecl(res::Context &ctx,
                                         const ast::StructDecl &decl) {
  auto *structDecl = res::StructDecl::create(
      ctx, decl.location, decl.identifier, scope->getDeclContext(),
      resolveTypeParamsWithoutBounds(ctx, decl.typeParameters));

  std::vector<res::Type *> typeParamTys;
  for (auto &&typeParam : structDecl->typeParams)
    typeParamTys.emplace_back(typeParam->getType());

  structDecl->setType(res::StructType::create(ctx, structDecl, typeParamTys));
  return structDecl;
}

bool Sema::resolveStructBody(res::Context &ctx,
                             res::StructDecl &structDecl,
                             const ast::StructDecl &astDecl) {
  EnterNewScopeRAII structParamScope(this, &structDecl);
  bool error = !resolveGenericParamsInCurrentScope(ctx, structDecl.typeParams,
                                                   astDecl.typeParameters);

  EnterNewScopeRAII structBodyScope(this);
  for (auto &&decl : astDecl.decls) {
    res::Decl *memberDecl = nullptr;

    if (auto *field = dynamic_cast<ast::FieldDecl *>(decl.get())) {
      if (res::Type *fieldTy = resolveType(ctx, *field->type)) {
        memberDecl = res::FieldDecl::create(ctx, field->location,
                                            field->identifier, &structDecl);
        memberDecl->setType(fieldTy);
      }
    }

    if (auto *memberFunction = dynamic_cast<ast::FunctionDecl *>(decl.get()))
      memberDecl = resolveFunctionDecl(ctx, *memberFunction);

    if (!insertDeclToCurrentScope(memberDecl)) {
      error = true;
      continue;
    }

    structDecl.insertDecl(memberDecl);
  }

  return !error;
}

bool Sema::resolveMemberFunctionBodies(res::Context &ctx,
                                       res::StructDecl &decl,
                                       const ast::StructDecl &astDecl) {
  EnterNewScopeRAII structParamScope(this, &decl);
  for (auto &&typeParamDecl : decl.typeParams)
    insertDeclToCurrentScope(typeParamDecl);

  EnterNewScopeRAII structBodyScope(this);
  bool error = false;

  for (auto &&memberDecl : astDecl.decls) {
    if (const auto *memberFn =
            dynamic_cast<const ast::FunctionDecl *>(memberDecl.get())) {
      for (auto &&d : decl.lookupDirect(memberFn->identifier))
        if (auto *fd = d->getAs<res::FunctionDecl>())
          error |= !resolveFunctionBody(ctx, *memberFn, fd);

      continue;
    }
  }

  return !error;
}

res::Context *Sema::resolveAST() {
  EnterNewScopeRAII globalScope(this, &ctx.translationUnit);
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

    error |= !insertDeclToCurrentScope(rd);
    resDecls.emplace_back(rd, decl.get());
  }

  for (auto &&[resDecl, astDecl] : resDecls) {
    if (auto *resSD = resDecl->getAs<res::StructDecl>()) {
      ctx.translationUnit.insertDecl(resSD);
      error |= !resolveStructBody(
          ctx, *resSD, *static_cast<const ast::StructDecl *>(astDecl));
    }

    if (auto *resTD = resDecl->getAs<res::TraitDecl>()) {
      ctx.translationUnit.insertDecl(resTD);
      error |= !resolveTraitBody(ctx, *resTD,
                                 *static_cast<const ast::TraitDecl *>(astDecl));
    }
  }

  for (auto &&trait : ctx.translationUnit.getAll<res::TraitDecl>())
    error |= isSelfContainingTrait(trait);

  error |= hasSelfContainingStructs(ctx);

  for (auto &&extension : ast->extensions)
    error |= !resolveTypeExtension(ctx, *extension);
  error |= !checkDelayedUserDefinedTypes(ctx);

  auto *builtinGCCollect = createBuiltinGCCollect(ctx);
  insertDeclToCurrentScope(builtinGCCollect);
  ctx.translationUnit.insertDecl(builtinGCCollect);

  auto *builtinPrintln = createBuiltinPrintln(ctx);
  insertDeclToCurrentScope(builtinPrintln);
  ctx.translationUnit.insertDecl(builtinPrintln);

  for (auto &&fn : ast->functions) {
    auto *rf = resolveFunctionDecl(ctx, *fn);
    error |= !insertDeclToCurrentScope(rf);
    error |= hasBuiltinFunctionCollisions(rf);
    resDecls.emplace_back(rf, fn);
    ctx.translationUnit.insertDecl(rf);
  }

  if (error)
    return nullptr;

  const auto &astExtensions = ast->extensions;
  auto resExtensions = ctx.translationUnit.extensions;

  for (int i = 0; i < astExtensions.size(); ++i)
    error |= !resolveExtensionBody(ctx, resExtensions[i], *astExtensions[i]);

  for (auto &&[resDecl, astDecl] : resDecls) {
    if (auto *rs = resDecl->getAs<res::StructDecl>())
      error |= !resolveMemberFunctionBodies(
          ctx, *rs, *static_cast<const ast::StructDecl *>(astDecl));

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

  res::Type *selfType = scope->getSelfType();
  if (!selfType) {
    err::selfParamNotAllowed(param->location).report(reporter);
    return false;
  }

  if (idx != 0) {
    err::selfWrongPosition(param->location).report(reporter);
    return false;
  }

  auto *refType = param->getType()->getAs<res::RefType>();
  if (!refType || !ctx.unify(refType->getReferencedType(), selfType).empty()) {
    err::selfWrongType(param->location).report(reporter);
    return false;
  }

  return true;
}

bool Sema::isSelfContainingTrait(res::TraitDecl *trait) {
  std::stack<res::TraitDecl *> stack;

  stack.emplace(trait);
  while (!stack.empty()) {
    res::TraitDecl *decl = stack.top();
    stack.pop();

    auto *conformance = decl->conformance;
    if (!conformance)
      continue;

    for (auto &&requirement : conformance->traits)
      if (stack.emplace(requirement->getDecl()) == trait) {
        err::selfRequiringTrait(trait->location)
            .with(trait->identifier)
            .report(reporter);
        return true;
      }
  }

  return false;
}

bool Sema::hasSelfContainingStructs(res::Context &ctx) {
  std::stack<std::pair<res::StructType *, int>> worklist;
  std::set<res::StructDecl *> selfContaining;

  for (auto &&sd : ctx.translationUnit.getAll<res::StructDecl>()) {
    std::vector<std::pair<res::StructType *, int>> seen;
    worklist.emplace(sd->getType()->getAs<res::StructType>(), 0);

    while (!worklist.empty()) {
      auto &&[ty, level] = worklist.top();
      worklist.pop();

      res::StructDecl *decl = ty->getDecl();
      res::Substitution sub = ty->getSub();

      for (auto &&[seenTy, seenLevel] : seen)
        if (seenLevel < level && ctx.unify(seenTy, ty).empty())
          selfContaining.emplace(decl);

      if (selfContaining.count(decl))
        continue;

      seen.emplace_back(ty, level);

      for (auto &&field : decl->getAll<res::FieldDecl>())
        if (auto *structTy = ctx.instantiate(field->getType(), sub)
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

bool Sema::checkDelayedUserDefinedTypes(res::Context &ctx) {
  shouldDelayUserDefinedTypeChecking = false;

  bool error = false;
  for (auto &&[ast, res] : delayedTypeChecks)
    error |= !validatedUserDefinedType(ast, res);

  return !error;
}

res::Type *Sema::validatedUserDefinedType(const ast::UserDefinedType *astDecl,
                                          res::Type *type) {
  if (shouldDelayUserDefinedTypeChecking) {
    delayedTypeChecks[astDecl] = type;
    return type;
  }

  res::GenericDeclContext *gdc = nullptr;
  auto sub = type->getSub();

  if (auto *st = type->getAs<res::StructType>())
    gdc = st->getDecl();
  else if (auto *t = type->getAs<res::TraitType>())
    gdc = t->getDecl();
  else if (auto *a = type->getAs<res::AnyTraitType>())
    gdc = a->getDecl();

  assert(gdc && "unexpected type param type");

  auto astIt = astDecl->typeArguments.begin();
  for (auto &&typeParam : gdc->typeParams) {
    // AnyTraitType doesn't have a Self mapping, so nothing to check here.
    if (typeParam->isImplicitSelf)
      continue;

    auto *typeParamType = typeParam->getType();
    auto *typeArg = ctx.instantiate(typeParamType, sub);

    for (auto &&trait : ctx.getDirectConformance(typeParamType)) {
      trait = ctx.instantiate(trait, sub)->getAs<res::TraitType>();
      if (auto errs = ctx.solveConformance(typeArg, trait); !errs.empty()) {
        for (auto &&error : errs)
          err::inferenceError((*astIt)->location).with(error).report(reporter);

        return nullptr;
      }
    }

    ++astIt;
  }

  return type;
}

bool Sema::checkVtableCompatibility(SourceLocation loc,
                                    res::TraitType *trait,
                                    std::set<std::string> &visited) {
  if (!visited.emplace(trait->getName()).second)
    return true;

  bool error = false;
  for (auto &&fn : trait->getDecl()->getAll<res::FunctionDecl>()) {
    SourceLocation fnLoc = fn->location;

    if (fn->typeParams.size() > 0) {
      err::traitObjectTemplateMemberFn(fnLoc)
          .with(trait->getName())
          .report(reporter);
      error = true;
      continue;
    }

    if (fn->params.empty() || fn->params[0]->identifier != selfParamId) {
      err::traitObjectStaticMemberFn(fnLoc)
          .with(trait->getName())
          .report(reporter);
      error = true;
      continue;
    }

    res::Type *selfTPType = trait->getDecl()->typeParams[0]->getType();
    res::Substitution testSub;
    testSub[selfTPType] = res::BuiltinUnitType::create(ctx);

    auto *fnType = fn->getType()->getAs<res::FunctionType>();
    for (int i = 1; i < fn->params.size(); ++i) {
      const auto &param = fn->params[i];
      res::Type *paramType = param->getType();

      if (!ctx.unify(paramType, ctx.instantiate(paramType, testSub)).empty()) {
        err::traitObjectSelfParam(param->location)
            .with(trait->getName())
            .report(reporter);
        error = true;
        break;
      }
    }

    res::Type *retType =
        fn->getType()->getAs<res::FunctionType>()->getReturnType();
    if (!ctx.unify(retType, ctx.instantiate(retType, testSub)).empty()) {
      err::traitObjectSelfReturn(fnLoc).with(trait->getName()).report(reporter);
      error = true;
    }
  }

  for (auto &&parentTrait : ctx.getDirectConformance(trait))
    if (!checkVtableCompatibility(loc, parentTrait, visited)) {
      err::superTraitNotTraitObjectCompatible(loc)
          .with(parentTrait->getName())
          .with(trait->getName())
          .report(reporter);
      error = true;
    }

  return !error;
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
  for (auto &&dre : functionInfo->declReferences) {
    auto *gdc = dre->decl->getAs<res::GenericDeclContext>();
    if (!gdc || gdc->typeParams.empty())
      continue;

    for (auto &&tp : gdc->typeParams) {
      if (dre->sub[tp->getType()]->getAs<res::UninferredType>()) {
        err::annotationsNeeded(dre->location)
            .with(dre->decl->identifier)
            .report(reporter);
        error = true;
        break;
      }
    }
  }

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

            if (const auto *i =
                    dynamic_cast<const res::ImplicitDerefExpr *>(base)) {
              base = i->dre;
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
