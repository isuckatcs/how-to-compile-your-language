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

    if (auto *e = dynamic_cast<res::ExtensionDecl *>(declContext))
      return e->type;

    declContext = declContext->parent;
  }

  return nullptr;
}

std::vector<res::NamedDecl *> Sema::Scope::lookupSymbol(const std::string &id,
                                                        bool recursive) const {
  std::vector<res::NamedDecl *> results;

  for (auto &&decl : decls)
    if (auto *nd = decl->getAs<res::NamedDecl>(); nd && nd->identifier == id)
      results.emplace_back(nd);

  if (!recursive || !parent)
    return results;

  for (auto &&res : parent->lookupSymbol(id))
    results.emplace_back(res);

  return results;
}

bool Sema::insertDeclToCurrentScope(res::Decl *decl) {
  if (!decl)
    return false;

  if (auto *nd = decl->getAs<res::NamedDecl>()) {
    const auto &results = scope->lookupSymbol(nd->identifier, false);
    if (results.empty()) {
      scope->addDecl(decl);
      return true;
    }

    bool resultIsValue = results[0]->getAs<res::ValueDecl>() != nullptr;
    bool declIsValue = decl->getAs<res::ValueDecl>() != nullptr;

    if (results.size() > 1 || resultIsValue == declIsValue) {
      err::redeclaration(decl->location).with(nd->identifier).report(reporter);
      return false;
    }
  }

  scope->addDecl(decl);
  return true;
}

res::FunctionDecl *Sema::createBuiltinPrintln(res::Context &ctx) {
  SourceLocation loc{nullptr, 0, 0};
  auto *numTy = typeMgr.getBuiltinNumberType();

  auto *fn =
      ctx.create<res::FunctionDecl>(loc, "println", scope->getDeclContext(),
                                    std::vector<res::TypeParamDecl *>{});
  fn->setType(typeMgr.getFunctionType({numTy}, typeMgr.getBuiltinUnitType()));

  auto *param = ctx.create<res::ParamDecl>(loc, "n", fn, false);
  param->setType(numTy);
  fn->setParams({param});

  fn->setBody(ctx.create<res::Block>(loc, std::vector<res::Stmt *>()));
  return fn;
};

res::FunctionDecl *Sema::createBuiltinGCCollect(res::Context &ctx) {
  SourceLocation loc{nullptr, 0, 0};

  auto *fn =
      ctx.create<res::FunctionDecl>(loc, "gcCollect", scope->getDeclContext());
  fn->setType(typeMgr.getFunctionType({}, typeMgr.getBuiltinUnitType()));
  fn->setBody(ctx.create<res::Block>(loc, std::vector<res::Stmt *>()));

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
      return typeMgr.getBuiltinUnitType();
    case ast::BuiltinType::Kind::Number:
      return typeMgr.getBuiltinNumberType();
    case ast::BuiltinType::Kind::Bool:
      return typeMgr.getBuiltinBoolType();
    case ast::BuiltinType::Kind::Self:
      if (auto *selfType = scope->getSelfType())
        return selfType;
      return err::selfTyNotAllowed(parsedType.location).report(reporter);
    }
  }

  if (const auto *udt =
          dynamic_cast<const ast::UserDefinedType *>(&parsedType)) {
    varOrReturn(decl, resolveTypeSymbol(udt));

    if (auto *typeParamDecl = decl->getAs<res::TypeParamDecl>())
      return typeMgr.getTypeParamType(*typeParamDecl);

    auto *gdc = dynamic_cast<res::GenericDeclContext *>(decl);
    assert(gdc && "expected generic decl context");

    bool isTraitDecl = decl->getAs<res::TraitDecl>();
    int offset = isTraitDecl ? 1 : 0;
    const auto &typeParams = gdc->typeParams;

    if (isTraitDecl && !expectTrait)
      return err::rawTrait(udt->location)
          .with(udt->identifier)
          .report(reporter);

    if (!isTraitDecl && expectTrait)
      return err::notATrait(udt->location)
          .with(udt->identifier)
          .report(reporter);

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
            udt, typeMgr.getAnyTraitType(*td, std::move(resolvedTypeArgs)));

      resolvedTypeArgs.emplace(resolvedTypeArgs.begin(), traitSelfType);
      return validatedUserDefinedType(
          udt, typeMgr.getTraitType(*td, std::move(resolvedTypeArgs)));
    }

    return validatedUserDefinedType(
        udt, typeMgr.getStructType(*decl->getAs<res::StructDecl>(),
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

    return typeMgr.getFunctionType(std::move(args), retTy);
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
            typeMgr.withSelfType(anyTraitType, typeMgr.getNewUninferredType()),
            visited))
      return err::traitNotTraitObjectCompatible(loc)
          .with(anyTraitType->getDecl()->identifier)
          .report(reporter);

    return anyTraitType;
  }

  if (const auto *ptr = dynamic_cast<const ast::PointerType *>(&parsedType)) {
    varOrReturn(pointeeType, resolveType(ctx, *ptr->pointeeType, true));
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
      ctx.create<res::UnaryOperator>(unary.location, unary.op, rhs, kind);
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

  bool typeError = !typeMgr.unify(lhsTy, rhsTy).empty();
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

  auto *resBinop = ctx.create<res::BinaryOperator>(loc, binop.op, lhs, rhs);
  resBinop->setType(isCmpOp ? typeMgr.getBuiltinBoolType() : lhsTy);

  return resBinop;
}

res::GroupingExpr *
Sema::resolveGroupingExpr(res::Context &ctx,
                          const ast::GroupingExpr &grouping) {
  varOrReturn(expr, resolveExpr(ctx, *grouping.expr));

  auto *g = ctx.create<res::GroupingExpr>(grouping.location, expr);
  g->setType(expr->getType());
  return g;
}

template <typename ExpectedDecl>
res::DeclRefExpr *Sema::resolvePathExpr(res::Context &ctx,
                                        const ast::PathExpr &pathExpr) {
  res::Type *parentType = nullptr;
  res::TraitType *parentTrait = nullptr;

  if (auto *traitSpec = pathExpr.traitSpecifier.get()) {
    varOrReturn(specType, resolveType(ctx, *traitSpec->type, true));
    varOrReturn(traitType,
                resolveType(ctx, *traitSpec->trait, false, true, specType));

    parentType = specType;
    parentTrait = traitType->getAs<res::TraitType>();

    if (!typeMgr.conformsTo(parentType, parentTrait) &&
        typeMgr.getExtensions(parentType, parentTrait).empty())
      return err::traitNotImplemented(traitSpec->trait->location)
          .with(parentType->getName())
          .with(parentTrait->getName())
          .report(reporter);
  }

  std::vector<res::DeclRefExpr *> results;

  for (auto &&fragment : pathExpr.fragments) {
    if (!results.empty()) {
      parentType = nullptr;
      parentTrait = nullptr;

      for (auto &&result : results)
        if (result->decl->getAs<res::TypeDecl>()) {
          parentType = result->getType();
          break;
        }

      // FIXME: report ambigous associated items?

      if (!parentType)
        return err::memberAccessInValue(results[0]->location).report(reporter);

      if (parentType->getAs<res::TraitType>())
        return err::memberAccessInRawTrait(results[0]->location)
            .report(reporter);

      results.clear();
    }

    if (parentType) {
      // FIXME: could simplify these branches if both lookup methods returned
      // the same type... introduce struct LookupResult...
      auto candidates =
          lookupAssociatedDecls(fragment->identifier, parentType, parentTrait);

      if (candidates.empty())
        return err::lookupInTypeFailed(fragment->location)
            .with(fragment->identifier)
            .with(parentTrait ? parentTrait->getName() : parentType->getName())
            .report(reporter);

      assert(candidates.size() == 1 || !parentTrait);

      for (auto &&[candidate, sub] : candidates) {
        varOrReturn(dre,
                    createDeclRefExpr(ctx, fragment.get(), candidate, sub));
        results.emplace_back(dre);
      }

      continue;
    }

    if (fragment->identifier == selfTypeId) {
      auto *selfType = scope->getSelfType();
      if (!selfType)
        return err::selfTyNotAllowed(fragment->location).report(reporter);

      res::NamedDecl *decl = nullptr;
      res::Substitution sub;

      if (auto *paramType = selfType->getAs<res::TypeParamType>())
        decl = paramType->decl;
      else if (auto *structType = selfType->getAs<res::StructType>()) {
        decl = structType->getDecl();
        sub = typeMgr.extractSubstitutionFrom(structType);
      }

      assert(decl && "unexpect self type");

      varOrReturn(dre, createDeclRefExpr(ctx, fragment.get(), decl, sub));
      results.emplace_back(dre);
      continue;
    }

    auto candidates = scope->lookupSymbol(fragment->identifier);
    if (candidates.empty())
      return err::missingSymbol(fragment->location)
          .with(fragment->identifier)
          .report(reporter);

    bool hasNonValue = false;
    bool hasValue = false;

    // FIXME: once the parent type and trait are part of the decl, this can be
    // simplified
    for (auto &&candidate : candidates) {
      if (candidate->getAs<res::ValueDecl>()) {
        if (hasValue)
          continue;

        varOrReturn(dre, createDeclRefExpr(ctx, fragment.get(), candidate, {}));
        results.emplace_back(dre);
        hasValue = true;
        continue;
      }

      if (hasNonValue)
        continue;

      varOrReturn(dre, createDeclRefExpr(ctx, fragment.get(), candidate, {}));
      results.emplace_back(dre);
      hasNonValue = true;
    }
  }

  res::DeclRefExpr *result = nullptr;
  for (auto &&res : results) {
    if (res->decl->getAs<ExpectedDecl>()) {
      if (result)
        return err::ambigousMemberFn(res->location).report(reporter);

      result = res;
    }
  }

  if (result)
    return result;

  // FIXME: should just return all candidates and handle errors in wrappers?
  if constexpr (std::is_same_v<ExpectedDecl, res::StructDecl>)
    return err::notStructInstance(pathExpr.fragments.back()->location)
        .report(reporter);

  if constexpr (std::is_same_v<ExpectedDecl, res::ValueDecl>) {
    if (results[0]->decl->getAs<res::StructDecl>())
      return err::expectedInstance(pathExpr.fragments.back()->location)
          .with(results[0]->decl->identifier)
          .report(reporter);

    if (results[0]->decl->getAs<res::TypeParamDecl>())
      return err::unexpectedTypeParam(pathExpr.fragments.back()->location)
          .report(reporter);
  }

  // FIXME: be more descriptive
  return err::missingSymbol(pathExpr.fragments.back()->location)
      .with(pathExpr.fragments.back()->identifier)
      .report(reporter);
}

res::DeclRefExpr *Sema::createDeclRefExpr(res::Context &ctx,
                                          const ast::DeclRefExpr *dre,
                                          res::NamedDecl *decl,
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
      auto *subType = typeMgr.getNewUninferredType();

      sub[tpType] = subType;

      for (auto &&trait : typeMgr.getDirectConformance(tpType)) {
        auto *instTrait =
            typeMgr.instantiate(trait, sub)->getAs<res::TraitType>();
        typeMgr.createObligation(subType, instTrait);
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

      if (const auto &errs = typeMgr.unify(expectedType, arg); !errs.empty()) {
        for (auto &&err : errs)
          err::inferenceError(args[i]->location).with(err).report(reporter);

        return nullptr;
      }
    }
  }

  auto *resDre = ctx.create<res::DeclRefExpr>(dre->location, decl, kind, sub);
  resDre->setType(typeMgr.instantiate(decl->getType(), sub));

  if (modifiers & AddressTaken)
    resDre->decl->setStorageNeeded();

  return functionInfo->declReferences.emplace_back(resDre);
}

std::vector<std::pair<res::NamedDecl *, res::Substitution>>
Sema::lookupAssociatedDecls(std::string identifier,
                            res::Type *type,
                            res::TraitType *trait) {
  std::vector<std::pair<res::NamedDecl *, res::Substitution>> candidates;

  if (!trait) {
    if (auto *s = type->getAs<res::StructType>())
      for (auto &&decl : s->getDecl()->lookupDirect(identifier))
        candidates.emplace_back(decl, typeMgr.extractSubstitutionFrom(type));

    if (!candidates.empty())
      return candidates;

    for (auto &&trait : typeMgr.getEveryConformance(type))
      for (auto &&decl : trait->getDecl()->lookupDirect(identifier))
        candidates.emplace_back(decl, typeMgr.extractSubstitutionFrom(trait));
  } else if (typeMgr.conformsTo(type, trait)) {
    for (auto &&decl : trait->getDecl()->lookupDirect(identifier))
      candidates.emplace_back(decl, typeMgr.extractSubstitutionFrom(trait));

    return candidates;
  }

  auto extensions = typeMgr.getExtensions(type, trait);

  for (auto &&[extension, sub] : extensions) {
    for (auto &&decl : extension->trait->getDecl()->lookupDirect(identifier))
      candidates.emplace_back(
          // FIXME: add an API for substitution composition?
          decl, typeMgr.extractSubstitutionFrom(
                    typeMgr.instantiate(extension->trait, sub)));
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
        ctx.create<res::MaterializeTemporaryExpr>(selfArg->location, selfArg);
    mte->setType(selfArg->getType());
    selfArg = mte;
  }

  auto *targetType =
      resMemberExpr->getType()->getAs<res::FunctionType>()->getArgs()[0];
  selfArg = withPtrToBorrowDecay(targetType, selfArg);
  selfArg = withImplicitBorrow(targetType, selfArg);

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
    auto msgs = typeMgr.unify(args[0]->getType(), fnType->getArgs()[0]);
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
        this, expectedTy->getAs<res::BorrowedType>() ? AddressTaken : 0);

    varOrReturn(resolvedArg, resolveExpr(ctx, *arg, expectedTy));
    varOrReturn(coercedArg, asTraitObjectIfNeeded(expectedTy, resolvedArg));
    varOrReturn(promotedArg, withImplicitBorrow(expectedTy, coercedArg));
    promotedArg = withPtrToBorrowDecay(expectedTy, promotedArg);

    res::Type *actualTy = promotedArg->getType();

    if (const auto &errors = typeMgr.unify(actualTy, expectedTy);
        !errors.empty()) {
      for (auto &&error : errors)
        err::inferenceError(promotedArg->location).with(error).report(reporter);
      return nullptr;
    }

    promotedArg->setConstantValue(cee->evaluate(*promotedArg));
    args.emplace_back(promotedArg);
  }

  auto *ce = ctx.create<res::CallExpr>(call.location, callee, std::move(args));
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

    res::Type *fieldTy = typeMgr.instantiate(
        fieldDecl->getType(), typeMgr.extractSubstitutionFrom(structTy));

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

  auto *sie = ctx.create<res::StructInstantiationExpr>(
      structInstantiation.location, path, std::move(resolvedFieldInits));
  sie->setType(structTy);
  return sie;
}

res::UnaryOperator *Sema::insertUnaryDeref(res::Context &ctx, res::Expr *val) {
  res::PointerType *ptrType = val->getType()->getAs<res::PointerType>();

  res::Expr::Kind kind = ptrType->isMutable() ? res::Expr::Kind::MutLvalue
                                              : res::Expr::Kind::Lvalue;

  auto *uo = ctx.create<res::UnaryOperator>(val->location, TokenKind::Asterisk,
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
    return err::lookupInTypeFailed(dre->location)
        .with(dre->identifier)
        .with(baseType->getName())
        .report(reporter);

  if (candidates.size() > 1)
    return err::ambigousMemberFn(dre->location).report(reporter);

  auto &&[decl, sub] = candidates.back();

  varOrReturn(memberDre, createDeclRefExpr(ctx, dre, decl, sub));

  if (!isCallee) {
    if (memberDre->decl->getAs<res::FunctionDecl>())
      return err::expectedMethodCall(memberExpr.location).report(reporter);

    if (ptrType)
      base = insertUnaryDeref(ctx, base);
  }

  auto *me = ctx.create<res::MemberExpr>(memberExpr.location, base, memberDre);
  me->setType(memberDre->getType());
  return me;
}

res::GCExpr *Sema::resolveGCExpr(res::Context &ctx, const ast::GCExpr &gc) {
  varOrReturn(expr, resolveExpr(ctx, *gc.expr));
  expr->setConstantValue(cee->evaluate(*expr));

  auto *gce = ctx.create<res::GCExpr>(gc.location, expr);
  gce->setType(typeMgr.getPointerType(expr->getType(), gc.isMut));
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
      ctx.create<res::StructDecl>(loc, structId.str(), scope->getDeclContext(),
                                  std::vector<res::TypeParamDecl *>{}, true);
  closure->setType(typeMgr.getStructType(*closure, {}));

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
        typeMgr.unify(resolvedParam->getType(), expectedFnType->getArgs()[i]);

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

  auto *paramType = typeMgr.getPointerType(closure->getType(), false);
  paramTypes.emplace_back(paramType);

  auto *fn = ctx.create<res::FunctionDecl>(loc, lambdaFunctionId, closure,
                                           std::vector<res::TypeParamDecl *>{});
  fn->setType(typeMgr.getFunctionType(paramTypes, returnTy));
  closure->insertDecl(fn);

  auto *p = ctx.create<res::ParamDecl>(loc, "closure", fn, false);
  p->setType(paramType);
  resolvedParams.emplace_back(p);
  fn->setParams(std::move(resolvedParams));

  auto *resLambdaExpr = ctx.create<res::LambdaExpr>(loc, closure, fn);
  resLambdaExpr->setType(lambdaTy);

  std::vector<const ast::Expr *> pendingCaptureInits;
  {
    WithFunctionInfoRAII lambdaInfo(this, {fn, resLambdaExpr, scope});

    if (res::Block *block = resolveBlock(ctx, *lambdaExpr.body)) {
      fn->setBody(block);

      res::Type *retTy =
          fn->getType()->getAs<res::FunctionType>()->getReturnType();
      if (retTy->getAs<res::UninferredType>())
        typeMgr.unify(retTy, typeMgr.getBuiltinUnitType());

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

  auto *targetAny = targetPtr->getPointeeType()->getAs<res::AnyTraitType>();
  if (!targetAny)
    return expr;

  auto *exprPtr = expr->getType()->getAs<res::PointerType>();
  if (!exprPtr)
    return expr;

  auto *exprPointee = exprPtr->getPointeeType();
  if (exprPointee->getAs<res::AnyTraitType>() ||
      targetPtr->isMutable() != exprPtr->isMutable())
    return expr;

  auto *tmpType = typeMgr.getNewUninferredType();
  typeMgr.createObligation(tmpType, typeMgr.withSelfType(targetAny, tmpType));

  const auto &errors = typeMgr.unify(tmpType, exprPointee);
  if (errors.empty()) {
    auto *top = ctx.create<res::TraitObjectPromoExpr>(expr->location, expr);
    top->setType(targetType);
    return top;
  }

  for (auto &&error : errors)
    err::inferenceError(expr->location).with(error).report(reporter);
  return nullptr;
}

res::Expr *Sema::withPtrToBorrowDecay(res::Type *targetType, res::Expr *expr) {
  auto *targetBorrowType = targetType->getAs<res::BorrowedType>();
  auto *currentPtrType = expr->getType()->getAs<res::PointerType>();

  if (!targetBorrowType || !currentPtrType)
    return expr;

  if (targetBorrowType->isMutable() && !currentPtrType->isMutable())
    return expr;

  res::Type *borrowedType = targetBorrowType->getBorrowedType();
  res::Type *pointerrType = currentPtrType->getPointeeType();
  if (!typeMgr.unify(borrowedType, pointerrType).empty())
    return expr;

  auto *p2b = ctx.create<res::ImplicitPtrToBorrowDecay>(expr->location, expr);
  p2b->setType(targetBorrowType);
  return p2b;
}

res::Expr *Sema::withImplicitBorrow(res::Type *targetType, res::Expr *expr) {
  auto *targetRefType = targetType->getAs<res::BorrowedType>();
  if (!targetRefType)
    return expr;

  if (expr->getType()->getAs<res::BorrowedType>())
    return expr;

  if (!expr->isLvalue())
    return err::rvalueBorrow(expr->location).report(reporter);

  if (targetRefType->isMutable() && !expr->isMutable())
    return expr;

  if (!typeMgr.unify(targetRefType->getBorrowedType(), expr->getType()).empty())
    return expr;

  auto *be = ctx.create<res::ImplicitBorrowExpr>(expr->location, expr);
  be->setType(
      typeMgr.getBorrowedType(expr->getType(), targetRefType->isMutable()));
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

  if (!insertDeclToCurrentScope(varDecl))
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

  varOrReturn(coercedRhs, asTraitObjectIfNeeded(lhsTy, rhs));
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

    varOrReturn(coercedExpr, asTraitObjectIfNeeded(retTy, expr));
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
  if (const auto *number = dynamic_cast<const ast::NumberLiteral *>(&expr)) {
    auto *nl = ctx.create<res::NumberLiteral>(number->location,
                                              std::stod(number->value));
    nl->setType(typeMgr.getBuiltinNumberType());
    return nl;
  }

  if (const auto *boolLiteral = dynamic_cast<const ast::BoolLiteral *>(&expr)) {
    auto *bl = ctx.create<res::BoolLiteral>(boolLiteral->location,
                                            boolLiteral->value == "true");
    bl->setType(typeMgr.getBuiltinBoolType());
    return bl;
  }

  if (const auto *unit = dynamic_cast<const ast::UnitLiteral *>(&expr)) {
    auto *ul = ctx.create<res::UnitLiteral>(unit->location);
    ul->setType(typeMgr.getBuiltinUnitType());
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

    const res::NamedDecl *decl = resPath->decl;
    bool isFunctionDecl = decl->getAs<res::FunctionDecl>();

    // FIXME: check these
    if (decl->getAs<res::FieldDecl>())
      return err::memberFnLookupFailed(resPath->location)
          .with(decl->identifier)
          // FIXME: remove this once every parent is a decl
          .with(((res::StructDecl *)resPath->decl->declContext)->identifier)
          .report(reporter);

    // FIXME: remove this limitation
    if (resPath->getReceiverType() &&
        resPath->getReceiverType()->getAs<res::AnyTraitType>() &&
        isFunctionDecl && !(modifiers & IsCallee))
      return err::traitObjectMethodNotCalled(resPath->location)
          .report(reporter);

    auto *outType = resPath->getType()->getAs<res::BorrowedType>();

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
        if (outType)
          return err::outParamCapture(resPath->location)
              .with(decl->identifier)
              .report(reporter);

        auto *lambda = functionInfo->lambda;

        res::FieldDecl *field = nullptr;
        if (auto r = lambda->closure->lookupDirect(decl->identifier);
            !r.empty())
          field = r.front()->getAs<res::FieldDecl>();

        if (!field) {
          field = ctx.create<res::FieldDecl>(lambda->location, decl->identifier,
                                             lambda->closure);
          field->setType(resPath->getType());
          lambda->closure->insertDecl(field);
          functionInfo->pendingCaptureInits.emplace_back(&expr);
        }

        res::Expr *base = ctx.create<res::DeclRefExpr>(
            lambda->location, lambda->method->params.back(),
            res::Expr::Kind::Lvalue, res::Substitution{});
        base->setType(lambda->method->params.back()->getType());

        base = ctx.create<res::UnaryOperator>(lambda->location,
                                              TokenKind::Asterisk, base,
                                              res::Expr::Kind::Lvalue);
        base->setType(lambda->closure->getType());

        auto *fieldDre = ctx.create<res::DeclRefExpr>(lambda->location, field,
                                                      res::Expr::Kind::Lvalue,
                                                      res::Substitution{});
        fieldDre->setType(field->getType());

        auto *me =
            ctx.create<res::MemberExpr>(lambda->location, base, fieldDre);
        me->setType(fieldDre->getType());
        return me;
      }
    }

    if (outType && (!typeHint || !typeHint->getAs<res::BorrowedType>())) {
      auto *ide =
          ctx.create<res::ImplicitDerefExpr>(resPath->location, resPath);
      ide->setType(outType->getBorrowedType());
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

  return ctx.create<res::Block>(block.location, std::move(resolvedStatements));
}

res::ExtensionDecl *
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
    probeSub[typeParam->getType()] = typeMgr.getNewUninferredType();

  auto conflictingExtensions = typeMgr.getExtensions(
      typeMgr.instantiate(type, probeSub),
      typeMgr.instantiate(traitType, probeSub)->getAs<res::TraitType>());
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

  auto *typeExtension = ctx.create<res::ExtensionDecl>(
      extension.location, scope->getDeclContext(), std::move(typeParams), type,
      traitType);
  typeMgr.addExtension(typeExtension);

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
    res::Substitution reverseSub;

    bool error = false;
    for (size_t i = 0; i < implTypeParams.size(); ++i) {
      res::Type *traitParamTy = traitFn->typeParams[i]->getType();
      res::Type *implParamTy = implFn->typeParams[i]->getType();

      auto *checkTy = typeMgr.getNewUninferredType();
      sub[implParamTy] = traitParamTy;
      reverseSub[implParamTy] = checkTy;

      for (auto &&trait : typeMgr.getDirectConformance(implParamTy))
        typeMgr.createObligation(
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
        error = true;
      }
    }
    if (error)
      continue;

    auto traitSub = typeMgr.extractSubstitutionFrom(traitType);

    res::Type *expectedType = typeMgr.instantiate(
        typeMgr.instantiate(traitFn->getType(), traitSub), sub);
    res::Type *actualType = implFn->getType();

    if (!typeMgr.unify(expectedType, actualType).empty()) {
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
                                res::ExtensionDecl *extension,
                                const ast::TypeExtension &astExtension) {
  bool error = false;
  auto *trait = extension->trait;

  for (auto &&requirement : typeMgr.getDirectConformance(trait)) {
    res::Type *type = extension->type;
    if (typeMgr.getExtensions(type, requirement).empty()) {
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
                                   : typeMgr.getNewUninferredType();
  if (!declTy)
    return nullptr;

  res::Expr *initializer = nullptr;
  if (varDecl.initializer) {
    varOrReturn(init, resolveExpr(ctx, *varDecl.initializer, declTy));

    varOrReturn(coercedInit, asTraitObjectIfNeeded(declTy, init));
    init = coercedInit;
    auto *initTy = init->getType();

    for (auto &&err : typeMgr.unify(declTy, initTy)) {
      std::cout << err << '\n';
    }

    if (!typeMgr.unify(declTy, initTy).empty())
      return err::initTyMismatch(init->location)
          .with(initTy->getName())
          .with(declTy->getName())
          .report(reporter);

    init->setConstantValue(cee->evaluate(*init));
    initializer = init;
  }

  auto *vd = ctx.create<res::VarDecl>(varDecl.location, varDecl.identifier,
                                      scope->getDeclContext(),
                                      varDecl.isMutable, initializer);
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
    auto *resTP = ctx.create<res::TypeParamDecl>(tp->location, tp->identifier);
    resTP->setType(typeMgr.getTypeParamType(*resTP));
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
                                                res::ExtensionDecl *extension) {
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

  res::Type *retTy =
      decl.type ? resolveType(ctx, *decl.type) : typeMgr.getBuiltinUnitType();
  error |= !retTy;

  if (error)
    return nullptr;

  auto *fn = ctx.create<res::FunctionDecl>(decl.location, decl.identifier,
                                           scope->getDeclContext(), typeParams);
  fn->setType(typeMgr.getFunctionType(std::move(paramTypes), retTy));
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

  res::Type *paramTy = nullptr;
  bool error = false;

  if (param->type) {
    paramTy =
        resolveType(ctx, *param->type, param->borrowedModifier != nullptr);
    error |= !paramTy;
  }

  if (!paramTy)
    paramTy = typeMgr.getNewUninferredType();

  if (auto *borrowed = param->borrowedModifier.get())
    paramTy = typeMgr.getBorrowedType(paramTy, borrowed->isMut);

  auto *referenceType = paramTy->getAs<res::BorrowedType>();
  if (referenceType && param->isMutable) {
    err::mutBorrowParameter(param->location).report(reporter);
    error = true;
  }

  auto *p = ctx.create<res::ParamDecl>(
      param->location, param->identifier, scope->getDeclContext(),
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

  return ctx.create<res::TraitConformance>(conformance.location, type,
                                           std::move(traits));
}

res::TraitDecl *Sema::resolveTraitDecl(res::Context &ctx,
                                       const ast::TraitDecl &decl) {
  auto typeParams = resolveTypeParamsWithoutBounds(ctx, decl.typeParameters);

  auto *self = ctx.create<res::TypeParamDecl>(decl.location, selfTypeId, true);
  auto *selfType = typeMgr.getTypeParamType(*self);
  self->setType(selfType);

  // FIXME: something is wrong with the design
  typeParams.emplace(typeParams.begin(), self);

  auto *trait = ctx.create<res::TraitDecl>(decl.location, decl.identifier,
                                           nullptr, std::move(typeParams));

  std::vector<res::Type *> typeParamTys;
  for (auto &&typeParam : trait->typeParams)
    typeParamTys.emplace_back(typeParam->getType());

  auto *traitType = typeMgr.getTraitType(*trait, typeParamTys);
  trait->setType(traitType);

  self->conformance = ctx.create<res::TraitConformance>(
      decl.location, selfType, std::vector<res::TraitType *>{traitType});

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
  auto *structDecl = ctx.create<res::StructDecl>(
      decl.location, decl.identifier, scope->getDeclContext(),
      resolveTypeParamsWithoutBounds(ctx, decl.typeParameters));

  std::vector<res::Type *> typeParamTys;
  for (auto &&typeParam : structDecl->typeParams)
    typeParamTys.emplace_back(typeParam->getType());

  structDecl->setType(typeMgr.getStructType(*structDecl, typeParamTys));
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
    res::NamedDecl *memberDecl = nullptr;

    if (auto *field = dynamic_cast<ast::FieldDecl *>(decl.get())) {
      if (res::Type *fieldTy = resolveType(ctx, *field->type)) {
        memberDecl = ctx.create<res::FieldDecl>(field->location,
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

std::pair<const res::Context *, const res::TypeManager *> Sema::resolveAST() {
  EnterNewScopeRAII globalScope(this);
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
    if (auto *resSD = resDecl->getAs<res::StructDecl>())
      error |= !resolveStructBody(
          ctx, *resSD, *static_cast<const ast::StructDecl *>(astDecl));

    if (auto *resTD = resDecl->getAs<res::TraitDecl>())
      error |= !resolveTraitBody(ctx, *resTD,
                                 *static_cast<const ast::TraitDecl *>(astDecl));
  }
  error |= hasSelfContainingStructs(ctx);

  for (auto &&extension : ast->extensions)
    error |= !resolveTypeExtension(ctx, *extension);
  error |= !checkDelayedUserDefinedTypes(ctx);

  insertDeclToCurrentScope(createBuiltinGCCollect(ctx));
  insertDeclToCurrentScope(createBuiltinPrintln(ctx));

  for (auto &&fn : ast->functions) {
    auto *rf = resolveFunctionDecl(ctx, *fn);
    error |= !insertDeclToCurrentScope(rf);
    error |= hasBuiltinFunctionCollisions(rf);
    resDecls.emplace_back(rf, fn);
  }

  if (error)
    return {nullptr, nullptr};

  const auto &astExtensions = ast->extensions;
  auto resExtensions = ctx.getTypeExtensions();

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
    return {nullptr, nullptr};

  return {&ctx, &typeMgr};
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

  auto *refType = param->getType()->getAs<res::BorrowedType>();
  if (!refType ||
      !typeMgr.unify(refType->getBorrowedType(), selfType).empty()) {
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
  auto sub = typeMgr.extractSubstitutionFrom(type);

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

    auto *probeType = typeMgr.getNewUninferredType();
    for (auto &&trait : typeMgr.getDirectConformance(typeParam->getType()))
      typeMgr.createObligation(
          probeType, typeMgr.instantiate(trait, sub)->getAs<res::TraitType>());

    if (auto errs = typeMgr.unify(
            typeMgr.instantiate(typeParam->getType(), sub), probeType);
        !errs.empty()) {
      for (auto &&err : errs)
        err::inferenceError(astIt->get()->location).with(err).report(reporter);

      return nullptr;
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
    testSub[selfTPType] = typeMgr.getBuiltinUnitType();

    auto *fnType = fn->getType()->getAs<res::FunctionType>();
    for (int i = 1; i < fn->params.size(); ++i) {
      const auto &param = fn->params[i];
      res::Type *paramType = param->getType();

      if (!typeMgr.unify(paramType, typeMgr.instantiate(paramType, testSub))
               .empty()) {
        err::traitObjectSelfParam(param->location)
            .with(trait->getName())
            .report(reporter);
        error = true;
        break;
      }
    }

    res::Type *retType =
        fn->getType()->getAs<res::FunctionType>()->getReturnType();
    if (!typeMgr.unify(retType, typeMgr.instantiate(retType, testSub))
             .empty()) {
      err::traitObjectSelfReturn(fnLoc).with(trait->getName()).report(reporter);
      error = true;
    }
  }

  for (auto &&parentTrait : typeMgr.getDirectConformance(trait))
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
  for (auto &&dre : functionInfo->declReferences)
    for (auto &&[from, to] : dre->sub) {
      if (!to->getRootType()->getAs<res::UninferredType>())
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

  using Lattice = std::map<const res::NamedDecl *, State>;

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
