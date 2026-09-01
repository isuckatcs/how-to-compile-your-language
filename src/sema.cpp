#include <cassert>
#include <charconv>
#include <limits>
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

  return parent->getDeclContext();
}

res::Type *Sema::Scope::getSelfType() const {
  auto *declContext = getDeclContext();
  while (declContext) {
    if (auto *s = dynamic_cast<res::StructDecl *>(declContext))
      return s->getType();

    if (auto *t = dynamic_cast<res::TraitDecl *>(declContext))
      return t->typeParams[0]->getType();

    if (auto *e = dynamic_cast<res::TypeExtension *>(declContext))
      if (auto *st = e->type->getAs<res::StructType>();
          !st || !st->getDecl()->isLambda)
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
    err::redeclaration()
        .at(decl->location)
        .with(decl->identifier)
        .report(reporter);
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
  fn->setBuiltin(true);

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
  fn->setBuiltin(true);

  fn->setType(res::FunctionType::create(ctx, std::vector<res::Type *>{},
                                        res::BuiltinUnitType::create(ctx)));
  fn->setBody(res::Block::create(ctx, loc, std::vector<res::Stmt *>()));

  return fn;
}

res::TypeDecl *Sema::resolveTypeSymbol(const ast::UserDefinedType *udt) {
  for (auto &&d : scope->lookupSymbol(udt->identifier))
    if (auto *td = d->getAs<res::TypeDecl>())
      return td;

  return err::failedToResolveType()
      .at(udt->location)
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
      return err::selfTyNotAllowed().at(parsedType.location).report(reporter);
    }
  }

  if (const auto *udt =
          dynamic_cast<const ast::UserDefinedType *>(&parsedType)) {
    varOrReturn(decl, resolveTypeSymbol(udt));
    bool isTraitDecl = decl->getAs<res::TraitDecl>();

    if (isTraitDecl && !expectTrait)
      return err::rawTrait()
          .at(udt->location)
          .with(udt->identifier)
          .report(reporter);

    if (!isTraitDecl && expectTrait)
      return err::notATrait()
          .at(udt->location)
          .with(udt->identifier)
          .report(reporter);

    if (auto *typeParamDecl = decl->getAs<res::TypeParamDecl>()) {
      typeParamDecl->setUsed(true);
      return res::TypeParamType::create(ctx, typeParamDecl);
    }

    auto *gdc = dynamic_cast<res::GenericDeclContext *>(decl);
    assert(gdc && "expected generic decl context");

    int offset = isTraitDecl ? 1 : 0;
    const auto &typeParams = gdc->typeParams;

    if (!checkTypeParameterCount(udt->location, udt->typeArguments.size(),
                                 typeParams.size() - offset))
      return nullptr;

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
            ctx, udt,
            res::AnyTraitType::create(ctx, td, std::move(resolvedTypeArgs)));

      resolvedTypeArgs.emplace(resolvedTypeArgs.begin(), traitSelfType);
      return validatedUserDefinedType(
          ctx, udt,
          res::TraitType::create(ctx, td, std::move(resolvedTypeArgs)));
    }

    auto *structType = res::StructType::create(
        ctx, decl->getAs<res::StructDecl>(), std::move(resolvedTypeArgs));

    if (ctx.isInfiniteStructType(structType))
      return err::infiniteStructType()
          .at(parsedType.location)
          .with(decl->getType()->getName())
          .with(structType->getName())
          .report(reporter);

    return validatedUserDefinedType(ctx, udt, structType);
  }

  if (const auto *arg = dynamic_cast<const ast::ArgumentType *>(&parsedType)) {
    varOrReturn(type, resolveType(ctx, *arg->type));

    if (const auto *ref = arg->refModifier.get())
      type = res::RefType::create(ctx, type, ref->isMut);

    return type;
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
      return err::traitObjectNotPointee().at(any->location).report(reporter);

    varOrReturn(type, resolveType(ctx, *any->type, false, true));
    return validatedAnyTraitType(ctx, any, type->getAs<res::AnyTraitType>());
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
    return err::unaryOperandUnknown().at(rhs->location).report(reporter);

  if (unary.op == TokenKind::Excl && !rhsTy->getAs<res::BuiltinBoolType>())
    return err::expectedOperandTy()
        .at(rhs->location)
        .with("bool")
        .report(reporter);

  if (unary.op == TokenKind::Minus && !rhsTy->getAs<res::BuiltinNumberType>())
    return err::expectedOperandTy()
        .at(rhs->location)
        .with("number")
        .report(reporter);

  res::Expr::ValueCategory valueCategory = res::Expr::ValueCategory::Rvalue;
  if (unary.op == TokenKind::Asterisk) {
    auto *ptr = rhsTy->getAs<res::PointerType>();
    if (!ptr)
      return err::expectedPointerOperand().at(rhs->location).report(reporter);

    valueCategory = ptr->isMutable() ? res::Expr::ValueCategory::MutLvalue
                                     : res::Expr::ValueCategory::Lvalue;
    rhsTy = ptr->getPointeeType();

    if (rhsTy->getAs<res::AnyTraitType>())
      return err::traitObjectPtrDereference()
          .at(rhs->location)
          .report(reporter);
  }

  auto *resolvedUnaryOp = res::UnaryOperator::create(
      ctx, unary.location, unary.op, rhs, valueCategory);
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
    return err::binopOperandUnknown()
        .at((uninferredLHS ? lhs : rhs)->location)
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
    return err::binopIncompatibleOperands()
        .at(loc)
        .with(lhsTy->getName())
        .with(rhsTy->getName())
        .report(reporter);

  bool isCmpOp =
      op == TokenKind::EqualEqual || op == TokenKind::Lt || op == TokenKind::Gt;

  auto *resBinop = res::BinaryOperator::create(ctx, loc, binop.op, lhs, rhs);
  resBinop->setType(isCmpOp ? res::BuiltinBoolType::create(ctx) : lhsTy);

  return resBinop;
}

bool Sema::shouldCaptureInCurrentLambda(res::DeclRefExpr *dre) {
  if (!functionInfo->lambda || !dre->isLvalue())
    return false;

  res::Decl *decl = dre->decl;
  auto r = functionInfo->lambdaParamScope->getParent()->lookupSymbol(
      decl->identifier);

  return !r.empty() && decl == r.front();
}

res::MemberExpr *Sema::captureInCurrentLambda(res::Context &ctx,
                                              const ast::PathExpr &path,
                                              res::DeclRefExpr *dre) {
  const std::string &id = dre->decl->identifier;

  res::LambdaExpr *lambda = functionInfo->lambda;
  res::StructDecl *closureDecl = lambda->closure;

  SourceLocation lambdaLoc = lambda->location;
  SourceLocation dreLoc = dre->location;

  res::FieldDecl *fieldDecl = closureDecl->lookupField(id);
  if (!fieldDecl) {
    fieldDecl = res::FieldDecl::create(ctx, lambdaLoc, id, closureDecl);
    fieldDecl->setType(dre->getType());
    closureDecl->fields.emplace_back(fieldDecl);
    functionInfo->pendingCaptureInits.emplace_back(&path);
  }

  res::ParamDecl *closureParam = lambda->getFunction()->params.back();
  auto emptySub = res::Substitution{};
  auto lvalueKind = res::Expr::ValueCategory::Lvalue;

  res::Expr *closure =
      res::DeclRefExpr::create(ctx, dreLoc, closureParam, lvalueKind, emptySub);
  closure->setType(closureParam->getType());

  closure = res::UnaryOperator::create(ctx, dreLoc, TokenKind::Asterisk,
                                       closure, lvalueKind);
  closure->setType(closureDecl->getType());

  auto *field =
      res::DeclRefExpr::create(ctx, dreLoc, fieldDecl, lvalueKind, emptySub);
  field->setType(fieldDecl->getType());

  auto *memberExpr = res::MemberExpr::create(ctx, dreLoc, closure, field);
  memberExpr->setType(field->getType());
  return memberExpr;
}

res::Expr *Sema::resolvePathExpr(res::Context &ctx,
                                 const ast::PathExpr &path,
                                 res::Type *typeHint) {
  varOrReturn(dre, resolvePathDeclRef<res::ValueDecl>(ctx, path));

  if (auto *selfType = dre->sub.getSelfType();
      selfType && selfType->getAs<res::AnyTraitType>() &&
      !(modifiers & IsCallee))
    return err::traitObjectMethodNotCalled().at(dre->location).report(reporter);

  auto *refType = dre->getType()->getAs<res::RefType>();

  if (shouldCaptureInCurrentLambda(dre)) {
    if (refType)
      return err::refParamCapture().at(dre->location).report(reporter);

    return captureInCurrentLambda(ctx, path, dre);
  }

  if (refType && (!typeHint || !typeHint->getAs<res::RefType>())) {
    auto *ide = res::ImplicitDerefExpr::create(ctx, dre->location, dre);
    ide->setType(refType->getReferencedType());
    return ide;
  }

  return dre;
}

template <typename ExpectedDecl>
res::DeclRefExpr *Sema::resolvePathDeclRef(res::Context &ctx,
                                           const ast::PathExpr &pathExpr) {
  std::vector<res::DeclRefExpr *> resFragments;
  const auto &fragments = pathExpr.fragments;
  size_t idx = 0;

  if (auto *traitSpec = pathExpr.traitSpecifier.get()) {
    varOrReturn(type, resolveType(ctx, *traitSpec->type, true));
    varOrReturn(t, resolveType(ctx, *traitSpec->trait, false, true, type));
    res::TraitType *trait = t->getAs<res::TraitType>();

    auto result = ctx.querySatisfyingTraits(type, trait);
    if (result.state != res::Context::QueryState::Success) {
      for (auto &&err : result.diags)
        err.at(traitSpec->trait->location).report(reporter);

      return nullptr;
    }

    const ast::DeclRefExpr *fragment = fragments[idx].get();
    varOrReturn(dre, resolveAssociatedDeclRef(ctx, fragment, type, trait));
    resFragments.emplace_back(dre);

    ++idx;
  }

  for (; idx != fragments.size(); ++idx) {
    const ast::DeclRefExpr *fragment = fragments[idx].get();
    if (!resFragments.empty()) {
      assert(idx > 0 && "unexpected fragment index");

      res::Type *type = resFragments.back()->getType();
      if (type->getAs<res::TraitType>())
        return err::memberAccessInRawTrait()
            .at(fragment->location)
            .report(reporter);

      varOrReturn(dre, resolveAssociatedDeclRef(ctx, fragment, type));
      resFragments.emplace_back(dre);
      continue;
    }

    assert(idx == 0 && "unexpected fragment index");

    if (fragment->identifier == selfTypeId) {
      auto *selfType = scope->getSelfType();
      if (!selfType)
        return err::selfTyNotAllowed().at(fragment->location).report(reporter);

      if (auto *paramType = selfType->getAs<res::TypeParamType>()) {
        auto *dre = resolveDeclRefExpr(ctx, fragment, paramType->getDecl(),
                                       paramType->getSub());
        assert(dre && "self type not resolved");
        resFragments.emplace_back(dre);
        continue;
      }

      auto *structType = selfType->getAs<res::StructType>();
      assert(structType && "unexpect self type");

      varOrReturn(dre, resolveDeclRefExpr(ctx, fragment, structType->getDecl(),
                                          structType->getSub()));
      resFragments.emplace_back(dre);
      continue;
    }

    auto symbolsInScope = scope->lookupSymbol(fragment->identifier);
    if (symbolsInScope.empty())
      return err::missingSymbol()
          .at(fragment->location)
          .with(fragment->identifier)
          .report(reporter);

    if (fragments.size() > 1) {
      for (auto &&decl : symbolsInScope) {
        if (decl->getAs<res::TypeDecl>()) {
          varOrReturn(dre, resolveDeclRefExpr(ctx, fragment, decl));
          resFragments.emplace_back(dre);
          break;
        }
      }

      if (resFragments.empty())
        return err::memberAccessInValue()
            .at(fragments[idx + 1]->location)
            .report(reporter);

      continue;
    }

    for (auto &&decl : symbolsInScope) {
      if (decl->getAs<ExpectedDecl>()) {
        varOrReturn(dre, resolveDeclRefExpr(ctx, fragment, decl));
        resFragments.emplace_back(dre);
        break;
      }
    }

    if (resFragments.empty())
      return err::wrongDeclKind().at(fragment->location).report(reporter);
  }

  auto *dre = resFragments.back();
  functionInfo->paths.emplace_back(dre);

  resFragments.pop_back();
  dre->setPath(std::move(resFragments));
  return dre;
}

res::DeclRefExpr *Sema::resolveDeclRefExpr(res::Context &ctx,
                                           const ast::DeclRefExpr *dre,
                                           res::Decl *decl,
                                           res::Substitution sub) {
  auto *valueDecl = decl->getAs<res::ValueDecl>();
  res::Expr::ValueCategory valueCategory = res::Expr::ValueCategory::Lvalue;
  if (!valueDecl || decl->getAs<res::FunctionDecl>())
    valueCategory = res::Expr::ValueCategory::Rvalue;
  else if (valueDecl->isMutable)
    valueCategory = res::Expr::ValueCategory::MutLvalue;

  auto *gdc = decl->getAs<res::GenericDeclContext>();
  if (gdc)
    for (auto &&[from, to] : ctx.getUninferredInstantiation(gdc))
      sub[from] = to;

  if (auto *typeArgList = dre->typeArgumentList.get()) {
    if (!gdc || gdc->typeParams.empty())
      return err::notGeneric()
          .at(typeArgList->location)
          .with(decl->identifier)
          .report(reporter);

    const auto &args = typeArgList->args;
    if (!checkTypeParameterCount(typeArgList->location, args.size(),
                                 gdc->typeParams.size()))
      return nullptr;

    for (size_t i = 0; i < args.size(); ++i) {
      varOrReturn(arg, resolveType(ctx, *args[i]));
      auto *expectedType =
          sub[gdc->typeParams[i]->getType()->getAs<res::TypeParamType>()];

      if (auto errs = ctx.unify(expectedType, arg); !errs.empty()) {
        for (auto &&err : errs)
          err.at(args[i]->location).report(reporter);

        return nullptr;
      }
    }
  }

  auto *resDre =
      res::DeclRefExpr::create(ctx, dre->location, decl, valueCategory, sub);
  resDre->setType(ctx.instantiate(decl->getType(), sub));

  if (modifiers & AddressTaken)
    resDre->decl->setStorageNeeded();

  return resDre;
}

res::DeclRefExpr *Sema::resolveAssociatedDeclRef(res::Context &ctx,
                                                 const ast::DeclRefExpr *dre,
                                                 res::Type *type,
                                                 res::TraitType *trait) {
  auto result = ctx.queryAssociatedDecls(dre->identifier, type, trait);
  if (result.state != res::Context::QueryState::Success) {
    for (auto &&err : result.diags)
      err.at(dre->location).report(reporter);
    return nullptr;
  }

  auto &&[hint, decl, sub] = result.items.front();
  if (hint)
    ctx.unify(type, hint);
  return resolveDeclRefExpr(ctx, dre, decl, sub);
}

res::NumberLiteral *
Sema::resolveNumberLiteral(res::Context &ctx,
                           const ast::NumberLiteral &number) {
  const std::string &value = number.value;
  double result = 0;
  auto [ptr, ec] =
      std::from_chars(value.data(), value.data() + value.size(), result);

  if (ec == std::errc::result_out_of_range) {
    auto limits = std::numeric_limits<double>{};
    return err::numberLiteralOutOfRange()
        .at(number.location)
        .with(limits.min())
        .with(limits.max())
        .report(reporter);
  }

  auto *nl = res::NumberLiteral::create(ctx, number.location, result);
  nl->setType(res::BuiltinNumberType::create(ctx));
  return nl;
}

res::CallExpr *Sema::resolveCallExpr(res::Context &ctx,
                                     const ast::CallExpr &call) {
  SourceLocation callLoc = call.location;
  const auto *callee = call.callee.get();
  const auto &arguments = call.arguments;

  res::CallExpr *resCall = nullptr;

  if (auto *me = dynamic_cast<const ast::MemberExpr *>(callee)) {
    varOrReturn(call, resolveMemberExpr(ctx, *me, true));
    resCall = static_cast<res::CallExpr *>(call);
  } else {
    WithModifiersRAII isCallee(this, IsCallee);
    varOrReturn(resCallee, resolveExpr(ctx, *callee));

    auto *functionType = resCallee->getType()->getAs<res::FunctionType>();
    if (!functionType)
      return err::invalidCallTy()
          .at(callLoc)
          .with(resCallee->getType()->getName())
          .report(reporter);

    resCall = res::CallExpr::create(ctx, callLoc, resCallee);
    resCall->setType(functionType->getReturnType());
  }

  std::vector<res::Type *> argumentTypes =
      resCall->callee->getType()->getAs<res::FunctionType>()->getArgs();

  size_t expectedArgCnt = argumentTypes.size();
  size_t implicitArgCnt = resCall->arguments.size();
  size_t sourceSpelledArgCnt = arguments.size();

  if ((sourceSpelledArgCnt + implicitArgCnt) != expectedArgCnt)
    return err::wrongArgCount()
        .at(callLoc)
        .with(expectedArgCnt - implicitArgCnt)
        .with(sourceSpelledArgCnt)
        .report(reporter);

  size_t argIdx = implicitArgCnt;
  for (auto &&argument : arguments) {
    res::Type *expectedType = argumentTypes[argIdx++];
    WithModifiersRAII addrTaken(
        this, expectedType->getAs<res::RefType>() ? AddressTaken : 0);

    auto *resolvedArgument = resolveExpr(ctx, *argument, expectedType);
    if (!resolvedArgument)
      continue;

    if (expectedType->getAs<res::RefType>() && !resolvedArgument->isLvalue()) {
      err::rvalueRef().at(argument->location).report(reporter);
      continue;
    }

    auto *coercedArgument = tryCoerce(ctx, resolvedArgument, expectedType);
    if (!coercedArgument)
      continue;

    coercedArgument->setConstantValue(cee->evaluate(*coercedArgument));
    resCall->addArg(coercedArgument);
  }

  if (resCall->arguments.size() != expectedArgCnt)
    return nullptr;

  return resCall;
}

res::StructInstantiationExpr *Sema::resolveStructInstantiation(
    res::Context &ctx,
    const ast::StructInstantiationExpr &structInstantiation) {
  varOrReturn(path, resolvePathDeclRef<res::StructDecl>(
                        ctx, *structInstantiation.structRef));

  auto *structTy = path->getType()->getAs<res::StructType>();
  auto *sd = structTy->getDecl();

  std::vector<res::FieldInitStmt *> resolvedFieldInits;
  std::map<std::string_view, res::FieldInitStmt *> inits;

  std::map<std::string_view, res::FieldDecl *> fields;
  for (auto &&fieldDecl : sd->fields)
    fields[fieldDecl->identifier] = fieldDecl;

  bool error = false;
  for (auto &&initStmt : structInstantiation.fieldInitializers) {
    std::string_view id = initStmt->identifier;
    const SourceLocation &loc = initStmt->location;

    if (inits.count(id)) {
      err::fieldAlreadyInitialized().at(loc).with(id).report(reporter);
      error = true;
      continue;
    }

    res::FieldDecl *fieldDecl = fields[id];
    if (!fieldDecl) {
      err::noFieldWithName()
          .at(loc)
          .with(sd->identifier)
          .with(id)
          .report(reporter);
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

    auto *coercedInitExpr = tryCoerce(ctx, resolvedInitExpr, fieldTy);
    if (!coercedInitExpr) {
      error = true;
      continue;
    }

    inits[id] = resolvedFieldInits.emplace_back(
        res::FieldInitStmt::create(ctx, loc, fieldDecl, coercedInitExpr));
  }

  for (auto &&fieldDecl : sd->fields) {
    if (!inits.count(fieldDecl->identifier)) {
      err::fieldNotInitialized()
          .at(structInstantiation.location)
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

  res::Expr::ValueCategory valueCategory =
      ptrType->isMutable() ? res::Expr::ValueCategory::MutLvalue
                           : res::Expr::ValueCategory::Lvalue;

  auto *uo = res::UnaryOperator::create(ctx, val->location, TokenKind::Asterisk,
                                        val, valueCategory);
  uo->setType(ptrType->getPointeeType());
  return uo;
}

res::Expr *Sema::resolveMemberExpr(res::Context &ctx,
                                   const ast::MemberExpr &me,
                                   bool asCall) {
  WithModifiersRAII mods(this, asCall ? AddressTaken : 0);
  varOrReturn(base, resolveExpr(ctx, *me.base));
  if (base->getType()->getAs<res::UninferredType>())
    return err::memberBaseUnknown().at(base->location).report(reporter);

  if (!base->isLvalue()) {
    auto *mte =
        res::MaterializeTemporaryExpr::create(ctx, base->location, base);
    mte->setType(base->getType());
    base = mte;
  }

  auto *baseType = base->getType();
  auto *basePtrType = baseType->getAs<res::PointerType>();
  auto *lookupType = basePtrType ? basePtrType->getPointeeType() : baseType;

  const auto *member = me.member.get();
  const SourceLocation &memberLoc = member->location;
  std::string memberId = member->identifier;

  if (!asCall) {
    auto *st = lookupType->getAs<res::StructType>();
    if (!st)
      return err::fieldLookupBaseInvalid()
          .at(memberLoc)
          .with(memberId)
          .with(baseType->getName())
          .report(reporter);

    auto *field = st->getDecl()->lookupField(memberId);
    if (!field)
      return err::fieldLookupFailed()
          .at(memberLoc)
          .with(memberId)
          .with(lookupType->getName())
          .report(reporter);

    varOrReturn(dre, resolveDeclRefExpr(ctx, member, field, st->getSub()));
    if (basePtrType)
      base = insertUnaryDeref(ctx, base);

    auto *resMemberExpr = res::MemberExpr::create(ctx, me.location, base, dre);
    resMemberExpr->setType(dre->getType());
    return resMemberExpr;
  }

  auto result =
      ctx.queryAssociatedDecls(member->identifier, lookupType, nullptr);
  if (result.state == res::Context::QueryState::Error && basePtrType)
    result = ctx.queryAssociatedDecls(member->identifier, basePtrType, nullptr);

  if (result.state != res::Context::QueryState::Success) {
    for (auto &&err : result.diags)
      err.at(member->location).report(reporter);
    return nullptr;
  }

  auto &&[hint, decl, sub] = result.items.front();
  varOrReturn(dre, resolveDeclRefExpr(ctx, member, decl, sub));
  functionInfo->paths.emplace_back(dre);

  auto *fnDecl = decl->getAs<res::FunctionDecl>();
  auto *functionType = dre->getType()->getAs<res::FunctionType>();

  assert(fnDecl && "expected function decl");

  if (fnDecl->params.empty() || fnDecl->params[0]->identifier != selfParamId)
    return err::classMethodCallOnInstance().at(memberLoc).report(reporter);

  auto *call = res::CallExpr::create(ctx, me.location, dre);
  call->setType(functionType->getReturnType());

  auto *selfType = functionType->getArgs()[0];
  varOrReturn(coercedBase, tryCoerce(ctx, base, selfType));

  call->addArg(coercedBase);
  return call;
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
  std::vector<res::Type *> paramHints;
  res::Type *returnHint = nullptr;

  if (auto *hint = typeHint ? typeHint->getAs<res::FunctionType>() : nullptr) {
    paramHints = hint->getArgs();
    returnHint = hint->getReturnType();
  }

  std::vector<res::Type *> paramTypes = {};
  std::vector<res::ParamDecl *> params = {};

  EnterNewScopeRAII paramScope(this);
  for (size_t i = 0; i < lambdaExpr.params.size(); ++i) {
    const ast::ParamDecl *astParam = lambdaExpr.params[i].get();
    res::Type *paramHint = i < paramHints.size() ? paramHints[i] : nullptr;

    auto *param = resolveParamDecl(ctx, astParam, paramHint);
    res::Type *paramType = param->getType();

    if (paramType && paramType->getAs<res::UninferredType>())
      paramType = err::annotationsNeeded()
                      .at(astParam->location)
                      .with(astParam->identifier)
                      .report(reporter);

    if (param->identifier == selfParamId)
      paramType = err::lamdaSelfParam().at(astParam->location).report(reporter);

    if (!insertDeclToCurrentScope(param) || !paramType)
      continue;

    paramTypes.emplace_back(paramType);
    params.emplace_back(param);
  }

  res::Type *returnType = returnHint;
  if (auto *astReturnType = lambdaExpr.returnType.get()) {
    varOrReturn(resolvedType, resolveType(ctx, *astReturnType));
    returnType = resolvedType;
  }

  if (!returnType)
    returnType = res::UninferredType::create(ctx);

  if (params.size() != lambdaExpr.params.size())
    return nullptr;

  SourceLocation loc = lambdaExpr.location;

  auto *lambdaExprType = res::FunctionType::create(ctx, paramTypes, returnType);

  std::stringstream closureId;
  closureId << "(closure@<source>:" << loc.line << ':' << loc.col << ')';
  auto noTypeParams = std::vector<res::TypeParamDecl *>{};

  auto *closure = res::StructDecl::create(
      ctx, loc, closureId.str(), scope->getDeclContext(), noTypeParams, true);
  res::Type *closureType = res::StructType::create(ctx, closure);
  closure->setType(closureType);

  auto *closureExtension = res::TypeExtension::create(
      ctx, loc, noTypeParams, scope->getDeclContext(), closureType, nullptr);

  auto *closureParamType = res::PointerType::create(ctx, closureType, false);
  paramTypes.emplace_back(closureParamType);
  auto *lambdaFnType = res::FunctionType::create(ctx, paramTypes, returnType);

  auto *lambdaFn = res::FunctionDecl::create(ctx, loc, "__builtin_lambda_call",
                                             closureExtension, noTypeParams);
  lambdaFn->setType(lambdaFnType);
  closureExtension->functions.emplace_back(lambdaFn);

  auto *param = res::ParamDecl::create(ctx, loc, "closure", lambdaFn, false);
  param->setType(closureParamType);
  params.emplace_back(param);
  lambdaFn->setParams(std::move(params));

  auto *resLambdaExpr =
      res::LambdaExpr::create(ctx, loc, closure, closureExtension);
  resLambdaExpr->setType(lambdaExprType);

  std::vector<const ast::Expr *> pendingCaptureInits;
  {
    WithFunctionInfoRAII lambdaInfo(this, {lambdaFn, resLambdaExpr, scope});
    EnterNewScopeRAII lambdaScope(this, lambdaFn);

    varOrReturn(block, resolveBlock(ctx, *lambdaExpr.body));
    lambdaFn->setBody(block);

    if (returnType->getAs<res::UninferredType>())
      ctx.unify(returnType, res::BuiltinUnitType::create(ctx));

    if (!runPostFunctionBodyChecks())
      return nullptr;

    pendingCaptureInits = std::move(functionInfo->pendingCaptureInits);
  }

  for (auto &&pendingInit : pendingCaptureInits) {
    res::Expr *initExpr = resolveExpr(ctx, *pendingInit);
    initExpr->setConstantValue(cee->evaluate(*initExpr));
    resLambdaExpr->fieldInits.emplace_back(initExpr);
  }

  return resLambdaExpr;
}

res::Expr *Sema::tryCoerce(res::Context &ctx, res::Expr *expr, res::Type *to) {
  auto *toRef = to->getAs<res::RefType>();
  res::Type *from = expr->getType();

  res::Expr *coerced = expr;
  if (toRef && !from->getAs<res::RefType>() &&
      (expr->isMutable() || !toRef->isMutable())) {
    if (auto *fromPtr = from->getAs<res::PointerType>();
        fromPtr && !toRef->getReferencedType()->getAs<res::PointerType>()) {
      // *type -> &type
      coerced = res::ImplicitPtrToRefDecay::create(ctx, expr->location, expr);
      coerced->setType(res::RefType::create(ctx, fromPtr->getPointeeType(),
                                            toRef->isMutable()));
    } else if (ctx.unify(from, toRef->getReferencedType()).empty() ||
               toRef->getReferencedType()->getAs<res::AnyTraitType>()) {
      // type -> &type
      coerced = res::ImplicitAsRefExpr::create(ctx, expr->location, expr);
      coerced->setType(res::RefType::create(ctx, from, toRef->isMutable()));
    }
  }

  // *type -> *any
  // &type -> &any
  res::Type *exprType = coerced->getType();

  res::Type *promoFrom = nullptr;
  res::AnyTraitType *anyType = nullptr;

  if (auto *ptr = to->getAs<res::PointerType>()) {
    anyType = ptr->getPointeeType()->getAs<res::AnyTraitType>();

    auto *fromPtr = exprType->getAs<res::PointerType>();
    if (fromPtr && fromPtr->isMutable() == ptr->isMutable())
      promoFrom = fromPtr->getPointeeType();
  }

  if (auto *ref = to->getAs<res::RefType>()) {
    anyType = ref->getReferencedType()->getAs<res::AnyTraitType>();

    auto *fromRef = exprType->getAs<res::RefType>();
    if (fromRef && coerced->isMutable() == ref->isMutable())
      promoFrom = fromRef->getReferencedType();
  }

  if (promoFrom && anyType && !ctx.eq(promoFrom, anyType) &&
      !promoFrom->getAs<res::AnyTraitType>()) {

    res::TraitType *trait = anyType->withSelfType(&ctx, promoFrom);
    auto result = ctx.querySatisfyingTraits(promoFrom, trait);

    if (result.state == res::Context::QueryState::Success) {
      ctx.unify(result.items.front(), trait);
      coerced =
          res::TraitObjectPromoExpr::create(ctx, coerced->location, coerced);
      coerced->setType(to);
    } else {
      for (auto &&diag : result.diags)
        diag.at(expr->location).report(reporter);
      return nullptr;
    }
  }

  auto errors = ctx.unify(coerced->getType(), to);
  if (!errors.empty()) {
    for (auto &&error : errors)
      error.at(expr->location).report(reporter);

    return nullptr;
  }

  return coerced;
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
    return err::expectedBoolCondition().at(cond->location).report(reporter);

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
    return err::expectedBoolCondition().at(cond->location).report(reporter);

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
    return err::rvalueAssignment().at(lhs->location).report(reporter);

  auto *lhsTy = lhs->getType();
  varOrReturn(coercedRhs, tryCoerce(ctx, rhs, lhsTy));

  if (auto *fnType = lhsTy->getAs<res::FunctionType>()) {
    auto *retTypeUninferred =
        fnType->getReturnType()->getAs<res::UninferredType>();

    for (auto &&argType : fnType->getArgs())
      if (retTypeUninferred || argType->getAs<res::UninferredType>())
        return err::unknownFunctionAssignment()
            .at(lhs->location)
            .with(lhsTy->getName())
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
    return err::noReturnValue().at(returnStmt.location).report(reporter);

  res::Expr *expr = nullptr;
  if (returnStmt.expr) {
    varOrReturn(resolvedExpr, resolveExpr(ctx, *returnStmt.expr, retTy));
    varOrReturn(coercedExpr, tryCoerce(ctx, resolvedExpr, retTy));

    coercedExpr->setConstantValue(cee->evaluate(*coercedExpr));
    expr = coercedExpr;
  }

  return res::ReturnStmt::create(ctx, returnStmt.location, expr);
}

res::Expr *Sema::resolveExpr(res::Context &ctx,
                             const ast::Expr &expr,
                             res::Type *typeHint) {
  if (const auto *number = dynamic_cast<const ast::NumberLiteral *>(&expr))
    return resolveNumberLiteral(ctx, *number);

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
    return resolveExpr(ctx, *groupingExpr->expr, typeHint);

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
    return resolveMemberExpr(ctx, *memberExpr, false);

  if (const auto *gc = dynamic_cast<const ast::GCExpr *>(&expr))
    return resolveGCExpr(ctx, *gc);

  if (const auto *lambda = dynamic_cast<const ast::LambdaExpr *>(&expr))
    return resolveLambdaExpr(ctx, *lambda, typeHint);

  if (const auto *path = dynamic_cast<const ast::PathExpr *>(&expr))
    return resolvePathExpr(ctx, *path, typeHint);

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
      wrn::unreachableStmt().at(stmt->location).report(reporter);
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

  for (auto &&tp : typeParams)
    tp->setUsed(false);

  varOrReturn(type, resolveType(ctx, *extension.type));

  res::TraitType *trait = nullptr;
  if (auto *astTrait = extension.trait.get()) {
    varOrReturn(resTrait, resolveType(ctx, *astTrait, false, true, type));
    trait = resTrait->getAs<res::TraitType>();
  }

  if (!trait) {
    if (type->getAs<res::TypeParamType>())
      return err::universalTypeExtension()
          .at(extension.type->location)
          .report(reporter);

    if (!type->getAs<res::StructType>())
      return err::nonStructTypeExtension()
          .with(type->getName())
          .at(extension.type->location)
          .report(reporter);
  }

  bool error = false;

  for (auto &&tp : typeParams) {
    if (!tp->used) {
      err::extensionTypeParamUnused()
          .with(tp->identifier)
          .at(tp->location)
          .report(reporter);
      error = true;
    }
  }

  if (error)
    return nullptr;

  return res::TypeExtension::create(ctx, extension.location,
                                    std::move(typeParams),
                                    scope->getDeclContext(), type, trait);
}

bool Sema::resolveExtensionBody(res::Context &ctx,
                                res::TypeExtension *extension,
                                const ast::TypeExtension &astExtension) {
  bool error = false;
  res::Type *type = extension->type;
  res::TraitType *trait = extension->trait;

  EnterNewScopeRAII typeParamScope(this);
  for (auto &&tp : extension->typeParams)
    insertDeclToCurrentScope(tp);

  EnterNewScopeRAII extensionScope(this, extension);
  for (auto &&fn : astExtension.functions) {
    if (!trait) {
      res::Substitution sub = ctx.getUninferredInstantiation(extension);

      bool redeclared = false;
      for (auto &&extension :
           ctx.queryExtensions(ctx.instantiate(type, sub), nullptr).items)
        redeclared |= extension->getFunction(fn->identifier) != nullptr;

      if (redeclared) {
        err::redeclaration()
            .at(fn->location)
            .with(fn->identifier)
            .report(reporter);
        continue;
      }

      if (auto *memberFn = resolveFunctionDecl(ctx, *fn))
        extension->functions.emplace_back(memberFn);

      continue;
    }

    res::FunctionDecl *traitFn =
        trait->getDecl()->lookupFunction(fn->identifier);

    if (!traitFn) {
      err::memberFnLookupFailed()
          .at(fn->location)
          .with(fn->identifier)
          .with(trait->getDecl()->identifier)
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

    res::Substitution implInstSub = ctx.getUninferredInstantiation(implFn);
    bool implError = false;

    for (size_t i = 0; i < implTypeParams.size(); ++i) {
      res::Type *traitParamTy = traitFn->typeParams[i]->getType();
      res::Type *implParamTy =
          ctx.instantiate(implFn->typeParams[i]->getType(), implInstSub);

      auto errors = ctx.unify(traitParamTy, implParamTy);
      if (errors.empty())
        continue;

      for (auto &&error : errors)
        error.at(implFn->typeParams[i]->location).report(reporter);

      err::stricterParamTy()
          .at(implFn->typeParams[i]->location)
          .with(traitParamTy->getName())
          .with(implFn->typeParams[i]->getType()->getName())
          .report(reporter);
      implError = true;
    }
    if (implError)
      continue;

    auto traitSub = trait->getSub();

    res::Type *expectedType = ctx.instantiate(traitFn->getType(), traitSub);
    res::Type *actualType = ctx.instantiate(implFn->getType(), implInstSub);

    if (!ctx.eq(expectedType, actualType)) {
      err::fnSignatureMismatch()
          .at(implFn->location)
          .with(expectedType->getName())
          .with(actualType->getName())
          .report(reporter);
      continue;
    }

    if (insertDeclToCurrentScope(implFn))
      extension->functions.emplace_back(implFn);
  }

  error |= extension->functions.size() != astExtension.functions.size();

  if (trait) {
    res::Substitution testSub = ctx.getUninferredInstantiation(extension);

    res::Type *testType = ctx.instantiate(type, testSub);
    auto *testTrait = ctx.instantiate(trait, testSub)->getAs<res::TraitType>();

    auto extensionQuery = ctx.queryExtensions(testType, testTrait);
    assert(extensionQuery.items.size() > 0 && "failed to find extensions");

    if (extensionQuery.items[0] != extension) {
      res::TypeExtension *conflict = extensionQuery.items[0];

      auto extensionSub = ctx.getUninferredInstantiation(extension);
      auto conflictSub = ctx.getUninferredInstantiation(conflict);

      res::Type *forType = ctx.instantiate(type, extensionSub);
      ctx.unify(forType, ctx.instantiate(conflict->type, conflictSub));
      ctx.unify(ctx.instantiate(trait, extensionSub),
                ctx.instantiate(conflict->trait, conflictSub));

      err::conflictingExtensionForType()
          .at(extension->location)
          .with(type->getName())
          .with(trait->getName())
          .with(conflict->type->getName())
          .with(conflict->trait->getName())
          .with(forType->getName())
          .report(reporter);
      error = true;
    }

    for (auto &&requirement : ctx.getDirectConformance(trait)) {
      if (ctx.queryExtensions(type, requirement).state !=
          res::Context::QueryState::Error)
        continue;

      err::missingRequirement()
          .at(extension->location)
          .with(type->getName())
          .with(trait->getName())
          .with(type->getName())
          .with(requirement->getName())
          .report(reporter);
      error = true;
    }

    error |= !implementsAllNecessaryTraitFunctions(extension);
  }

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
    varOrReturn(coercedInit, tryCoerce(ctx, init, declTy));

    coercedInit->setConstantValue(cee->evaluate(*coercedInit));
    initializer = coercedInit;
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
    err::typeArgCntMismatch().at(loc).with(expected).with(received).report(
        reporter);
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

      error |= !resParam->conformance;
    }

    for (auto &&decl : scope->getParent()->lookupSymbol(resParam->identifier)) {
      if (decl->getAs<res::TypeParamDecl>()) {
        err::typeParamShadowed()
            .at(resParam->location)
            .with(resParam->identifier)
            .report(reporter);
        error = true;
        break;
      }
    }
  }

  return !error;
}

bool Sema::implementsAllNecessaryTraitFunctions(res::TypeExtension *extension) {
  bool error = false;

  for (auto &&fn : extension->trait->getDecl()->functions) {
    if (!fn->mustImplement || extension->getFunction(fn->identifier))
      continue;

    err::missingTraitFn()
        .at(fn->location)
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
  if (!resolveGenericParamsInCurrentScope(ctx, typeParams, decl.typeParameters))
    return nullptr;

  std::vector<res::Type *> paramTypes;
  std::vector<res::ParamDecl *> resolvedParams;

  EnterNewScopeRAII paramScope(this);
  for (auto &&param : decl.params) {
    res::ParamDecl *resolvedParam = resolveParamDecl(ctx, param.get());
    res::Type *paramType = resolvedParam->getType();

    bool error = !paramType;
    error |= !checkSelfParameter(ctx, resolvedParam, resolvedParams.size());
    error |= !insertDeclToCurrentScope(resolvedParam);

    if (!error) {
      paramTypes.emplace_back(resolvedParam->getType());
      resolvedParams.emplace_back(resolvedParam);
    }
  }

  res::Type *retTy = decl.type ? resolveType(ctx, *decl.type)
                               : res::BuiltinUnitType::create(ctx);

  if (!retTy || resolvedParams.size() != decl.params.size())
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

res::ParamDecl *Sema::resolveParamDecl(res::Context &ctx,
                                       const ast::ParamDecl *param,
                                       res::Type *typeHint) {
  res::Type *paramType;

  if (param->type)
    paramType = resolveType(ctx, *param->type, param->refModifier != nullptr);
  else if (typeHint)
    paramType = typeHint;
  else
    paramType = res::UninferredType::create(ctx);

  bool isMut = param->isMutable;

  if (paramType) {
    if (auto *ref = param->refModifier.get())
      paramType = res::RefType::create(ctx, paramType, ref->isMut);

    if (auto *refType = paramType->getAs<res::RefType>()) {
      if (isMut)
        paramType = err::mutRefParameter().at(param->location).report(reporter);

      isMut = refType->isMutable();
    }
  }

  auto *resParam = res::ParamDecl::create(
      ctx, param->location, param->identifier, scope->getDeclContext(), isMut);
  resParam->setType(paramType);
  return resParam;
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
    traitDecl.functions.emplace_back(resolvedFn);
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
  for (auto &&fn : traitDecl.functions) {
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
  if (!resolveGenericParamsInCurrentScope(ctx, structDecl.typeParams,
                                          astDecl.typeParameters))
    return false;

  EnterNewScopeRAII structBodyScope(this);
  for (auto &&field : astDecl.fields) {
    res::Type *fieldTy = resolveType(ctx, *field->type);
    if (!fieldTy)
      continue;

    auto *resField = res::FieldDecl::create(ctx, field->location,
                                            field->identifier, &structDecl);
    resField->setType(fieldTy);

    if (insertDeclToCurrentScope(resField))
      structDecl.fields.emplace_back(resField);
  }

  return structDecl.fields.size() == astDecl.fields.size();
}

std::unique_ptr<res::Context> Sema::resolveAST() {
  auto ctx = std::make_unique<res::Context>();

  EnterNewScopeRAII globalScope(this, ctx->getTU());
  bool error = false;

  std::vector<std::pair<res::Decl *, const ast::Decl *>> resDecls;
  std::vector<std::pair<res::TypeExtension *, const ast::TypeExtension *>>
      resExtensions;

  for (auto &&node : ast->topLevel) {
    const ast::Decl *ad = dynamic_cast<const ast::Decl *>(node.get());
    res::Decl *rd = nullptr;
    if (const auto *sd = dynamic_cast<const ast::StructDecl *>(node.get()))
      rd = resolveStructDecl(*ctx, *sd);

    if (const auto *td = dynamic_cast<const ast::TraitDecl *>(node.get()))
      rd = resolveTraitDecl(*ctx, *td);

    if (!rd)
      continue;

    error |= !insertDeclToCurrentScope(rd);
    resDecls.emplace_back(rd, ad);
  }

  for (auto &&[resDecl, astDecl] : resDecls) {
    if (auto *resSD = resDecl->getAs<res::StructDecl>()) {
      ctx->getTU()->structs.emplace_back(resSD);
      error |= !resolveStructBody(
          *ctx, *resSD, *static_cast<const ast::StructDecl *>(astDecl));
    }

    if (auto *resTD = resDecl->getAs<res::TraitDecl>()) {
      ctx->getTU()->traits.emplace_back(resTD);
      error |= !resolveTraitBody(*ctx, *resTD,
                                 *static_cast<const ast::TraitDecl *>(astDecl));
    }
  }

  for (auto &&trait : ctx->getTU()->traits)
    error |= isSelfContainingTrait(trait);

  error |= hasSelfContainingStructs(*ctx);

  for (auto &&node : ast->topLevel) {
    auto *extension = dynamic_cast<const ast::TypeExtension *>(node.get());
    if (!extension)
      continue;

    if (auto *resExtension = resolveTypeExtension(*ctx, *extension)) {
      ctx->getTU()->extensions.emplace_back(resExtension);
      resExtensions.emplace_back(resExtension, extension);
      continue;
    }

    error = true;
  }

  for (auto &&[resExtension, extension] : resExtensions)
    error |= !resolveExtensionBody(*ctx, resExtension, *extension);

  error |= !runDeferredTypeChecks(*ctx);

  auto *builtinGCCollect = createBuiltinGCCollect(*ctx);
  insertDeclToCurrentScope(builtinGCCollect);
  ctx->getTU()->functions.emplace_back(builtinGCCollect);

  auto *builtinPrintln = createBuiltinPrintln(*ctx);
  insertDeclToCurrentScope(builtinPrintln);
  ctx->getTU()->functions.emplace_back(builtinPrintln);

  bool hasMainFunction = false;

  for (auto &&node : ast->topLevel) {
    if (auto *fn = dynamic_cast<const ast::FunctionDecl *>(node.get())) {
      auto *rf = resolveFunctionDecl(*ctx, *fn);
      error |= !insertDeclToCurrentScope(rf);
      error |= hasBuiltinFunctionCollisions(rf);
      resDecls.emplace_back(rf, fn);
      ctx->getTU()->functions.emplace_back(rf);

      hasMainFunction |= fn->identifier == "main";
    }
  }

  if (!hasMainFunction)
    return err::mainNotFound().at(ast->location).report(reporter);

  if (error)
    return nullptr;

  for (auto &&[resExt, astExt] : resExtensions) {
    EnterNewScopeRAII extensionParamScope(this, resExt);
    for (auto &&tp : resExt->typeParams)
      insertDeclToCurrentScope(tp);

    EnterNewScopeRAII extensionScope(this);
    for (size_t i = 0; i < astExt->functions.size(); ++i)
      error |= !resolveFunctionBody(*ctx, *astExt->functions[i],
                                    resExt->functions[i]);
  }

  for (auto &&[resDecl, astDecl] : resDecls) {
    if (auto *rt = resDecl->getAs<res::TraitDecl>())
      error |= !resolveTraitFunctionBodies(
          *ctx, *rt, *static_cast<const ast::TraitDecl *>(astDecl));

    if (auto *resFN = resDecl->getAs<res::FunctionDecl>())
      error |= !resolveFunctionBody(
          *ctx, *static_cast<const ast::FunctionDecl *>(astDecl), resFN);
  }

  if (error)
    return nullptr;

  return ctx;
}

bool Sema::hasBuiltinFunctionCollisions(const res::FunctionDecl *fnDecl) {
  if (!fnDecl)
    return false;

  if (fnDecl->identifier == "main") {
    if (!fnDecl->getType()
             ->getAs<res::FunctionType>()
             ->getReturnType()
             ->getAs<res::BuiltinUnitType>()) {
      err::wrongMainReturnTy().at(fnDecl->location).report(reporter);
      return true;
    }

    if (!fnDecl->params.empty()) {
      err::wrongMainArgCount().at(fnDecl->location).report(reporter);
      return true;
    }

    if (!fnDecl->typeParams.empty()) {
      err::mainIsGeneric().at(fnDecl->location).report(reporter);
      return true;
    }
  }

  return false;
}

bool Sema::checkSelfParameter(res::Context &ctx,
                              res::ParamDecl *param,
                              size_t idx) {
  if (param->identifier != selfParamId)
    return true;

  res::Type *selfType = scope->getSelfType();
  if (!selfType) {
    err::selfParamNotAllowed().at(param->location).report(reporter);
    return false;
  }

  if (idx != 0) {
    err::selfWrongPosition().at(param->location).report(reporter);
    return false;
  }

  if (!param->getType())
    return false;

  auto *refType = param->getType()->getAs<res::RefType>();
  if (!refType || !ctx.unify(refType->getReferencedType(), selfType).empty()) {
    err::selfWrongType().at(param->location).report(reporter);
    return false;
  }

  return true;
}

bool Sema::isSelfContainingTrait(res::TraitDecl *trait) {
  std::set<res::TraitDecl *> visited;
  std::stack<res::TraitDecl *> stack;

  stack.emplace(trait);
  while (!stack.empty()) {
    res::TraitDecl *decl = stack.top();
    stack.pop();

    if (!visited.emplace(decl).second)
      continue;

    auto *conformance = decl->conformance;
    if (!conformance)
      continue;

    for (auto &&requirement : conformance->traits)
      if (stack.emplace(requirement->getDecl()) == trait) {
        err::selfRequiringTrait()
            .at(trait->location)
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

  for (auto &&sd : ctx.getTU()->structs) {
    std::vector<std::pair<res::StructType *, int>> seen;
    worklist.emplace(sd->getType()->getAs<res::StructType>(), 0);

    while (!worklist.empty()) {
      auto [ty, level] = worklist.top();
      worklist.pop();

      res::StructDecl *decl = ty->getDecl();
      res::Substitution sub = ty->getSub();

      for (auto &&[seenTy, seenLevel] : seen)
        if (seenLevel < level && ctx.unify(seenTy, ty).empty())
          selfContaining.emplace(decl);

      if (selfContaining.count(decl))
        continue;

      seen.emplace_back(ty, level);

      for (auto &&field : decl->fields)
        if (auto *structTy = ctx.instantiate(field->getType(), sub)
                                 ->getAs<res::StructType>())
          worklist.emplace(structTy, level + 1);
    }
  }

  for (auto &&sd : selfContaining)
    err::selfContainingStruct()
        .at(sd->location)
        .with(sd->identifier)
        .report(reporter);

  return !selfContaining.empty();
}

bool Sema::runDeferredTypeChecks(res::Context &ctx) {
  shouldDeferTypeChecking = false;

  bool error = false;
  for (auto &&[ast, res] : deferredUserDefinedTypeChecks)
    error |= !validatedUserDefinedType(ctx, ast, res);

  for (auto &&[ast, res] : deferredAnyTypeChecks)
    error |= !validatedAnyTraitType(ctx, ast, res);

  return !error;
}

res::Type *Sema::validatedUserDefinedType(res::Context &ctx,
                                          const ast::UserDefinedType *astDecl,
                                          res::Type *type) {
  if (shouldDeferTypeChecking) {
    deferredUserDefinedTypeChecks.emplace_back(astDecl, type);
    return type;
  }

  res::GenericDeclContext *gdc = nullptr;

  if (auto *st = type->getAs<res::StructType>())
    gdc = st->getDecl();
  else if (auto *t = type->getAs<res::TraitType>())
    gdc = t->getDecl();
  else if (auto *a = type->getAs<res::AnyTraitType>())
    gdc = a->getDecl();

  assert(gdc && "unexpected type param type");

  auto expectedSub = ctx.getUninferredInstantiation(gdc);
  auto actualSub = type->getSub();

  auto astIt = astDecl->typeArguments.begin();
  for (auto &&typeParam : gdc->typeParams) {
    // AnyTraitType doesn't have a Self mapping, so nothing to check here.
    if (typeParam->isImplicitSelf)
      continue;

    auto *typeParamType = typeParam->getType();
    auto *expectedType = ctx.instantiate(typeParamType, expectedSub);
    auto *actualType = ctx.instantiate(typeParamType, actualSub);

    if (auto errs = ctx.unify(expectedType, actualType); !errs.empty()) {
      for (auto &&error : errs)
        error.at((*astIt)->location).report(reporter);

      return nullptr;
    }

    ++astIt;
  }

  return type;
}

res::AnyTraitType *Sema::validatedAnyTraitType(res::Context &ctx,
                                               const ast::AnyType *astDecl,
                                               res::AnyTraitType *type) {
  if (shouldDeferTypeChecking) {
    deferredAnyTypeChecks.emplace_back(astDecl, type);
    return type;
  }

  SourceLocation loc = astDecl->type->location;
  std::set<std::string> visited;
  if (!checkVtableCompatibility(
          ctx, loc, type->withSelfType(&ctx, res::UninferredType::create(ctx)),
          visited))
    return err::traitNotTraitObjectCompatible()
        .at(loc)
        .with(type->getDecl()->identifier)
        .report(reporter);

  return type;
}

bool Sema::checkVtableCompatibility(res::Context &ctx,
                                    SourceLocation loc,
                                    res::TraitType *trait,
                                    std::set<std::string> &visited) {
  if (!visited.emplace(trait->getName()).second)
    return true;

  bool error = false;
  for (auto &&fn : trait->getDecl()->functions) {
    SourceLocation fnLoc = fn->location;

    if (fn->typeParams.size() > 0) {
      err::traitObjectTemplateMemberFn()
          .at(fnLoc)
          .with(trait->getName())
          .report(reporter);
      error = true;
      continue;
    }

    if (fn->params.empty() || fn->params[0]->identifier != selfParamId) {
      err::traitObjectStaticMemberFn()
          .at(fnLoc)
          .with(trait->getName())
          .report(reporter);
      error = true;
      continue;
    }

    auto *selfTPType =
        trait->getDecl()->typeParams[0]->getType()->getAs<res::TypeParamType>();
    res::Substitution testSub;
    testSub[selfTPType] = res::BuiltinUnitType::create(ctx);

    for (size_t i = 1; i < fn->params.size(); ++i) {
      const auto &param = fn->params[i];
      res::Type *paramType = param->getType();

      if (!ctx.unify(paramType, ctx.instantiate(paramType, testSub)).empty()) {
        err::traitObjectSelfParam()
            .at(param->location)
            .with(trait->getName())
            .report(reporter);
        error = true;
        break;
      }
    }

    res::Type *retType =
        fn->getType()->getAs<res::FunctionType>()->getReturnType();
    if (!ctx.unify(retType, ctx.instantiate(retType, testSub)).empty()) {
      err::traitObjectSelfReturn()
          .at(fnLoc)
          .with(trait->getName())
          .report(reporter);
      error = true;
    }
  }

  for (auto &&parentTrait : ctx.getDirectConformance(trait))
    if (!checkVtableCompatibility(ctx, loc, parentTrait, visited)) {
      err::superTraitNotTraitObjectCompatible()
          .at(loc)
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
  auto checkDre = [&](const res::DeclRefExpr *dre) -> const res::DeclRefExpr * {
    for (auto &&fragment : dre->getPath())
      for (auto &&[from, to] : fragment->sub)
        if (to->getAs<res::UninferredType>())
          return err::annotationsNeeded()
              .at(fragment->location)
              .with(fragment->decl->identifier)
              .report(reporter);

    for (auto &&[from, to] : dre->sub)
      if (to->getAs<res::UninferredType>())
        return err::annotationsNeeded()
            .at(dre->location)
            .with(dre->decl->identifier)
            .report(reporter);

    return dre;
  };

  bool error = false;

  for (auto &&dre : functionInfo->paths)
    error |= !checkDre(dre);

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
         ? err::expectedReturnValueOnEveryPath()
               .at(fn->location)
               .report(reporter)
         : err::expectedReturnValue().at(fn->location).report(reporter));
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

  std::vector<Lattice> lattices(cfg.basicBlocks.size());
  std::vector<diag::DiagBuilder> errors;

  bool changed = true;
  while (changed) {
    changed = false;
    errors.clear();

    for (int bb = cfg.entry; bb != cfg.exit; --bb) {
      const auto &[preds, succs, stmts] = cfg.basicBlocks[bb];

      Lattice state;
      for (auto &&[block, reachable] : preds)
        for (auto &&[decl, declState] : lattices[block])
          state[decl] = joinStates(state[decl], declState);

      for (auto it = stmts.rbegin(); it != stmts.rend(); ++it) {
        if (auto *declStmt = dynamic_cast<const res::DeclStmt *>(*it)) {
          const res::VarDecl *decl = declStmt->varDecl;
          state[decl] = decl->initializer ? State::Assigned : State::Unassigned;

          if (decl->getType()->getAs<res::UninferredType>())
            errors.emplace_back(
                err::unknownType().at(decl->location).with(decl->identifier));

          continue;
        }

        if (auto *assignment = dynamic_cast<const res::Assignment *>(*it)) {
          const auto *assignee = assignment->assignee;
          auto *dre = dynamic_cast<const res::DeclRefExpr *>(assignee);

          bool alreadyInit = !dre || !dre->decl->getAs<res::VarDecl>() ||
                             state[dre->decl] > State::Unassigned;
          if (!assignee->isMutable() && alreadyInit)
            errors.emplace_back(
                err::immutableAssignment().at(assignment->location));

          if (dre)
            state[dre->decl] = State::Assigned;

          continue;
        }

        if (const auto *dre = dynamic_cast<const res::DeclRefExpr *>(*it)) {
          auto *decl = dre->decl->getAs<res::VarDecl>();
          if (decl && state[decl] != State::Assigned)
            errors.emplace_back(
                err::notInitialized().at(dre->location).with(decl->identifier));

          continue;
        }
      }

      changed |= lattices[bb] != state;
      lattices[bb] = state;
    }
  }

  for (auto &&err : errors)
    err.report(reporter);

  return errors.empty();
}
} // namespace yl
