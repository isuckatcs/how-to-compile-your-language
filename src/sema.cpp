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
  if (ctx.getType(&fn)
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

          const auto *dre = dynamic_cast<const res::DeclRefExpr *>(base);
          if (!dre)
            continue;

          const auto *decl = dre->decl->getAs<res::ValueDecl>();
          if (!decl->isMutable && tmp[decl] != State::Unassigned) {
            std::string msg = '\'' + decl->identifier + "' cannot be mutated";
            pendingErrors.emplace_back(assignment->location, std::move(msg));
          }

          tmp[decl] = State::Assigned;
          continue;
        }

        if (const auto *dre = dynamic_cast<const res::DeclRefExpr *>(stmt)) {
          for (auto &&typeArg : dre->getTypeArgs()) {
            if (!typeArg->getRootType()->getAs<res::UninferredType>())
              continue;

            std::string msg =
                "explicit type annotations are needed to infer the type of '" +
                dre->decl->identifier + "'";
            pendingErrors.emplace_back(dre->location, std::move(msg));
          }

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
    if (s == State::Unassigned && ctx.getType(d)->getAs<res::UninferredType>())
      report(d->location, "the type of '" + d->identifier + "' is unknown");

  for (auto &&[loc, msg] : pendingErrors)
    report(loc, msg);

  return !pendingErrors.empty();
}

bool Sema::insertDeclToCurrentScope(res::Decl *decl) {
  if (!decl)
    return false;

  if (!lexicalScope->insertDecl(decl)) {
    report(decl->location, "redeclaration of '" + decl->identifier + '\'');
    return false;
  }

  return true;
}

res::FunctionDecl *Sema::createBuiltinPrintln(res::Context &ctx) {
  SourceLocation loc{nullptr, 0, 0};

  auto *numTy = ctx.getBuiltinNumberType();
  auto *param = ctx.bind(ctx.create<res::ParamDecl>(loc, "n", false), numTy);

  auto *fn = ctx.create<res::FunctionDecl>(
      loc, "println", std::vector<res::TypeParamDecl *>{}, std::vector{param});
  fn->setBody(ctx.create<res::Block>(loc, std::vector<res::Stmt *>()));

  return ctx.bind(fn, ctx.getFunctionType({numTy}, ctx.getBuiltinUnitType()));
};

res::Type *Sema::resolveType(res::Context &ctx, const ast::Type &parsedType) {
  if (const auto *builtin =
          dynamic_cast<const ast::BuiltinType *>(&parsedType)) {
    switch (builtin->kind) {
    case ast::BuiltinType::Kind::Unit:
      return ctx.getBuiltinUnitType();
    case ast::BuiltinType::Kind::Number:
      return ctx.getBuiltinNumberType();
    case ast::BuiltinType::Kind::Self:
      if (!selfType)
        return report(parsedType.location,
                      "'Self' is only allowed inside structs");
      return selfType;
    }
  }

  if (const auto *udt =
          dynamic_cast<const ast::UserDefinedType *>(&parsedType)) {
    res::Decl *decl = lexicalScope->lookupDecl<res::TypeDecl>(udt->identifier);
    if (!decl)
      return report(udt->location,
                    "failed to resolve type '" + udt->identifier + "'");

    if (auto *sd = decl->getAs<res::StructDecl>()) {
      varOrReturn(res, checkTypeParameterCount(udt->location,
                                               udt->typeArguments.size(),
                                               sd->typeParams.size()));

      std::vector<res::Type *> resolvedTypeArgs;
      for (auto &&astArg : udt->typeArguments) {
        varOrReturn(resolvedType, resolveType(ctx, *astArg));
        resolvedTypeArgs.emplace_back(resolvedType);
      }

      return ctx.getStructType(*sd, std::move(resolvedTypeArgs));
    }

    if (const auto *typeParamDecl = decl->getAs<res::TypeParamDecl>())
      return ctx.getTypeParamType(*typeParamDecl);

    llvm_unreachable("unexpected value type encountered");
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
    return ctx.getFunctionType(std::move(args), retTy);
  }

  if (const auto *out = dynamic_cast<const ast::OutParamType *>(&parsedType)) {
    if (!(resolutionContext & ParamList))
      return report(out->location, "only parameters can have '&' type");

    varOrReturn(paramType, resolveType(ctx, *out->paramType));
    assert(!paramType->getAs<res::OutParamType>() &&
           "grammar doesn't allow nested out param types");

    return ctx.getPointerType(paramType);
  }

  llvm_unreachable("unexpected ast type encountered");
}

res::UnaryOperator *
Sema::resolveUnaryOperator(res::Context &ctx, const ast::UnaryOperator &unary) {
  varOrReturn(rhs, resolveExpr(ctx, *unary.operand));

  auto *rhsTy = ctx.getType(rhs);
  if (rhsTy->getAs<res::UninferredType>())
    return report(rhs->location,
                  "type of operand to unary operator is unknown");

  const auto &loc = unary.location;
  if (unary.op == TokenKind::Amp) {
    if (!(resolutionContext & ArgList))
      return report(unary.location,
                    "'&' can only be used to pass arguments to '&' parameters");

    if (!rhs->isMutable())
      return report(unary.location,
                    "only mutable lvalues can be passed to '&' parameters");

    rhsTy = ctx.getPointerType(rhsTy);
  } else if (!rhsTy->getAs<res::BuiltinNumberType>()) {
    return report(loc, '\'' + rhsTy->getName() +
                           "' cannot be used as an operand to unary operator");
  }

  return ctx.bind(ctx.create<res::UnaryOperator>(loc, unary.op, rhs), rhsTy);
}

res::BinaryOperator *
Sema::resolveBinaryOperator(res::Context &ctx,
                            const ast::BinaryOperator &binop) {
  varOrReturn(lhs, resolveExpr(ctx, *binop.lhs));
  varOrReturn(rhs, resolveExpr(ctx, *binop.rhs));

  auto *lhsTy = ctx.getType(lhs);
  auto *rhsTy = ctx.getType(rhs);

  if (auto *uninferredLHS = lhsTy->getAs<res::UninferredType>();
      uninferredLHS || rhsTy->getAs<res::UninferredType>())
    return report((uninferredLHS ? lhs : rhs)->location,
                  "type of " + std::string(uninferredLHS ? "LHS" : "RHS") +
                      " to binary operator is unknown");

  const auto &loc = binop.location;
  if (!ctx.unify(lhsTy, rhsTy) || !lhsTy->getAs<res::BuiltinNumberType>())
    return report(loc, "incompatible operands to binary operator ('" +
                           lhsTy->getName() + "' and '" + rhsTy->getName() +
                           "')");

  return ctx.bind(ctx.create<res::BinaryOperator>(loc, binop.op, lhs, rhs),
                  lhsTy);
}

res::GroupingExpr *
Sema::resolveGroupingExpr(res::Context &ctx,
                          const ast::GroupingExpr &grouping) {
  varOrReturn(expr, resolveExpr(ctx, *grouping.expr));
  return ctx.bind(ctx.create<res::GroupingExpr>(grouping.location, expr),
                  ctx.getType(expr));
}

res::DeclRefExpr *
Sema::resolveDeclRefExpr(res::Context &ctx,
                         const ast::DeclRefExpr &declRefExpr) {

  res::Decl *decl = nullptr;
  if (const auto &parent = declRefExpr.parent) {
    varOrReturn(parentDelc, resolveDeclRefExpr(ctx, *parent));

    auto *parentStruct = parentDelc->decl->getAs<res::StructDecl>();
    if (!parentStruct)
      return report(
          declRefExpr.location,
          "member functions can only be looked up in struct declarations");

    for (auto &&memberFunction : parentStruct->memberFunctions) {
      if (memberFunction->identifier == declRefExpr.identifier) {
        decl = memberFunction;
        break;
      }
    }

    if (!decl)
      return report(declRefExpr.location,
                    "failed to find member function named '" +
                        declRefExpr.identifier + "' in '" + parent->identifier +
                        "'");
  } else if (declRefExpr.identifier == "Self") {
    auto *structType = dynamic_cast<res::StructType *>(selfType);
    if (!structType)
      return report(declRefExpr.location,
                    "'Self' is only allowed inside structs");
    decl = structType->getDecl();
  } else {
    decl = lexicalScope->lookupDecl<res::Decl>(declRefExpr.identifier);
    if (!decl)
      return report(declRefExpr.location,
                    "symbol '" + declRefExpr.identifier + "' not found");
  }

  if (decl->getAs<res::TypeParamDecl>())
    return report(declRefExpr.location, "expected value, found type parameter");

  res::Expr::Kind kind;
  if (decl->getAs<res::FunctionDecl>() || decl->getAs<res::StructDecl>())
    kind = res::Expr::Kind::Rvalue;
  else
    kind = decl->getAs<res::ValueDecl>()->isMutable ? res::Expr::Kind::MutLvalue
                                                    : res::Expr::Kind::Lvalue;

  auto *declTy = ctx.getType(decl);
  res::Context::SubstitutionTy substitution = ctx.createSubstitution(decl);

  const auto &typeArgList = declRefExpr.typeArgumentList.get();
  if (typeArgList) {
    if (!decl->isGeneric())
      return report(typeArgList->location,
                    "'" + decl->identifier + "' is not a generic");

    varOrReturn(res, checkTypeParameterCount(typeArgList->location,
                                             typeArgList->args.size(),
                                             substitution.size()));

    size_t i = 0;
    for (auto &&typeArg : typeArgList->args) {
      varOrReturn(resolvedTypeArg, resolveType(ctx, *typeArg));
      ctx.unify(substitution[i++], resolvedTypeArg);
    }
  }

  if (decl->isGeneric())
    declTy = ctx.instantiate(ctx.getType(decl), substitution);

  return ctx.bind(ctx.create<res::DeclRefExpr>(declRefExpr.location, *decl,
                                               kind, std::move(substitution)),
                  declTy);
}

res::CallExpr *Sema::resolveCallExpr(res::Context &ctx,
                                     const ast::CallExpr &call) {
  varOrReturn(callee, resolveExpr(ctx, *call.callee));

  auto *calleeTy = ctx.getType(callee);
  auto *fnType = calleeTy->getAs<res::FunctionType>();

  if (!fnType)
    return report(call.location,
                  "calling expression of type '" + calleeTy->getName() + '\'');

  std::vector<res::Type *> argTypes = fnType->getArgs();

  if (call.arguments.size() != argTypes.size())
    return report(call.location,
                  "wrong number of arguments in function call, expected " +
                      std::to_string(argTypes.size()) + ", but received " +
                      std::to_string(call.arguments.size()));

  std::vector<res::Expr *> resolvedArgs;
  size_t idx = 0;
  for (auto &&argument : call.arguments) {
    ResolutionContextRAII argCtx(this, ArgList);
    varOrReturn(arg, resolveExpr(ctx, *argument));

    res::Type *expectedTy = argTypes[idx];
    res::Type *actualTy = ctx.getType(arg);

    if (!ctx.unify(expectedTy, actualTy))
      return report(arg->location, "expected '" + expectedTy->getName() +
                                       "' argument, but received '" +
                                       actualTy->getName() + "'");

    arg->setConstantValue(cee->evaluate(*arg, false));
    resolvedArgs.emplace_back(arg);
    ++idx;
  }

  return ctx.bind(
      ctx.create<res::CallExpr>(call.location, callee, std::move(resolvedArgs)),
      fnType->getReturnType());
}

res::StructInstantiationExpr *Sema::resolveStructInstantiation(
    res::Context &ctx,
    const ast::StructInstantiationExpr &structInstantiation) {

  varOrReturn(dre, resolveDeclRefExpr(ctx, *structInstantiation.structRef));

  if (!dre->decl->getAs<res::StructDecl>())
    return report(dre->location, "expected struct declaration to instantiate");

  auto *structTy = ctx.getType(dre)->getAs<res::StructType>();
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

    res::Type *initTy = ctx.getType(resolvedInitExpr);
    res::Type *fieldTy =
        ctx.instantiate(ctx.getType(fieldDecl), structTy->getTypeArgs());

    if (!ctx.unify(fieldTy, initTy)) {
      report(resolvedInitExpr->location,
             "a field of type '" + fieldTy->getName() +
                 "' cannot be initialized with an expression of type '" +
                 initTy->getName() + "'");
      error = true;
      continue;
    }

    inits[id] = resolvedFieldInits.emplace_back(
        ctx.create<res::FieldInitStmt>(loc, fieldDecl, resolvedInitExpr));
  }

  for (auto &&fieldDecl : sd->fields) {
    if (!inits.count(fieldDecl->identifier)) {
      report(structInstantiation.location,
             "field '" + fieldDecl->identifier + "' is not initialized");
      error = true;
      continue;
    }

    auto &initStmt = inits[fieldDecl->identifier];
    initStmt->initializer->setConstantValue(
        cee->evaluate(*initStmt->initializer, false));
  }

  if (error)
    return nullptr;

  return ctx.bind(
      ctx.create<res::StructInstantiationExpr>(
          structInstantiation.location, dre, std::move(resolvedFieldInits)),
      structTy);
}

res::MemberExpr *Sema::resolveMemberExpr(res::Context &ctx,
                                         const ast::MemberExpr &memberExpr) {
  varOrReturn(base, resolveExpr(ctx, *memberExpr.base));

  auto *baseTy = ctx.getType(base);
  auto *structTy = baseTy->getAs<res::StructType>();
  if (!structTy)
    return report(memberExpr.base->location,
                  "cannot access field of '" + baseTy->getName() + '\'');

  const auto *sd = structTy->getDecl();
  res::FieldDecl *fieldDecl = nullptr;
  for (auto &&field : sd->fields)
    if (field->identifier == memberExpr.member->identifier)
      fieldDecl = field;

  if (!fieldDecl)
    return report(memberExpr.location,
                  '\'' + sd->identifier + "' has no field called '" +
                      memberExpr.member->identifier + '\'');

  return ctx.bind(
      ctx.create<res::MemberExpr>(memberExpr.location, base, fieldDecl),
      ctx.instantiate(ctx.getType(fieldDecl), structTy->getTypeArgs()));
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
  if (!ctx.unify(ctx.getType(cond), ctx.getBuiltinNumberType()))
    return report(cond->location, "expected number in condition");

  varOrReturn(trueBlock, resolveBlock(ctx, *ifStmt.trueBlock));

  res::Block *falseBlock = nullptr;
  if (ifStmt.falseBlock) {
    falseBlock = resolveBlock(ctx, *ifStmt.falseBlock);
    if (!falseBlock)
      return nullptr;
  }

  cond->setConstantValue(cee->evaluate(*cond, false));

  return ctx.create<res::IfStmt>(ifStmt.location, cond, trueBlock, falseBlock);
}

res::WhileStmt *Sema::resolveWhileStmt(res::Context &ctx,
                                       const ast::WhileStmt &whileStmt) {
  varOrReturn(cond, resolveExpr(ctx, *whileStmt.condition));
  if (!ctx.unify(ctx.getType(cond), ctx.getBuiltinNumberType()))
    return report(cond->location, "expected number in condition");

  varOrReturn(body, resolveBlock(ctx, *whileStmt.body));

  cond->setConstantValue(cee->evaluate(*cond, false));

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
  varOrReturn(rhs, resolveExpr(ctx, *assignment.expr));
  varOrReturn(lhs, resolveExpr(ctx, *assignment.assignee));

  if (!lhs->isLvalue())
    return report(lhs->location, "cannot assign to rvalue");

  auto *lhsTy = ctx.getType(lhs);
  auto *rhsTy = ctx.getType(rhs);

  if (!ctx.unify(lhsTy, rhsTy))
    return report(rhs->location, "expected to assign '" + lhsTy->getName() +
                                     "' but received '" + rhsTy->getName() +
                                     "' instead");
  rhs->setConstantValue(cee->evaluate(*rhs, false));

  return ctx.create<res::Assignment>(assignment.location, lhs, rhs);
}

res::ReturnStmt *Sema::resolveReturnStmt(res::Context &ctx,
                                         const ast::ReturnStmt &returnStmt) {
  assert(currentFunction && "return stmt outside a function");

  auto *fnTy = ctx.getType(currentFunction)->getAs<res::FunctionType>();

  auto *retTy = fnTy->getReturnType();
  if (!retTy->getAs<res::BuiltinUnitType>() && !returnStmt.expr)
    return report(returnStmt.location, "expected a return value");

  res::Expr *expr = nullptr;
  if (returnStmt.expr) {
    expr = resolveExpr(ctx, *returnStmt.expr);
    if (!expr)
      return nullptr;

    res::Type *exprTy = ctx.getType(expr);
    if (!ctx.unify(retTy, exprTy)) {
      exprTy = ctx.getType(expr);
      retTy = fnTy->getReturnType();

      return report(expr->location, "cannot return '" + exprTy->getName() +
                                        "' from a function returning '" +
                                        retTy->getName() + "'");
    }

    expr->setConstantValue(cee->evaluate(*expr, false));
  }

  return ctx.create<res::ReturnStmt>(returnStmt.location, expr);
}

res::Expr *Sema::resolveExpr(res::Context &ctx, const ast::Expr &expr) {
  if (const auto *number = dynamic_cast<const ast::NumberLiteral *>(&expr))
    return ctx.bind(ctx.create<res::NumberLiteral>(number->location,
                                                   std::stod(number->value)),
                    ctx.getBuiltinNumberType());

  if (const auto *unit = dynamic_cast<const ast::UnitLiteral *>(&expr))
    return ctx.bind(ctx.create<res::UnitLiteral>(unit->location),
                    ctx.getBuiltinUnitType());

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

  if (const auto *declRefExpr = dynamic_cast<const ast::DeclRefExpr *>(&expr)) {
    varOrReturn(dre, resolveDeclRefExpr(ctx, *declRefExpr));

    if (dre->decl->getAs<res::StructDecl>())
      return report(declRefExpr->location, "expected an instance of '" +
                                               declRefExpr->identifier + '\'');

    auto *outType = ctx.getType(dre)->getAs<res::OutParamType>();
    if (outType)
      return ctx.bind(ctx.create<res::ImplicitDerefExpr>(dre->location, dre),
                      outType->getParamType());

    return dre;
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

res::VarDecl *Sema::resolveVarDecl(res::Context &ctx,
                                   const ast::VarDecl &varDecl) {
  res::Expr *initializer = nullptr;
  if (varDecl.initializer) {
    initializer = resolveExpr(ctx, *varDecl.initializer);
    if (!initializer)
      return nullptr;
  }

  auto *decl =
      ctx.bind(ctx.create<res::VarDecl>(varDecl.location, varDecl.identifier,
                                        varDecl.isMutable, initializer),
               ctx.getNewUninferredType());

  if (varDecl.type) {
    varOrReturn(type, resolveType(ctx, *varDecl.type));
    ctx.unify(ctx.getType(decl), type);
  }

  if (initializer) {
    auto *declTy = ctx.getType(decl);
    auto *initTy = ctx.getType(initializer);

    if (!ctx.unify(declTy, initTy)) {
      return report(decl->initializer->location,
                    "an expression of type '" + initTy->getName() +
                        "' cannot be used to initialize a variable of type '" +
                        declTy->getName() + "'");
    }

    initializer->setConstantValue(cee->evaluate(*initializer, false));
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

std::vector<res::TypeParamDecl *> Sema::resolveTypeParameters(
    res::Context &ctx,
    const std::vector<std::unique_ptr<ast::TypeParamDecl>> &typeParamDecls) {
  std::vector<res::TypeParamDecl *> resolvedTypeParams;
  for (auto &&typeParam : typeParamDecls) {
    auto *resolvedTypeParam = ctx.create<res::TypeParamDecl>(
        typeParam->location, typeParam->identifier, resolvedTypeParams.size());

    resolvedTypeParams.emplace_back(
        ctx.bind(resolvedTypeParam, ctx.getTypeParamType(*resolvedTypeParam)));
  }

  return resolvedTypeParams;
}

res::FunctionDecl *Sema::resolveFunctionDecl(res::Context &ctx,
                                             const ast::FunctionDecl &function,
                                             res::StructDecl *parent) {
  std::vector<res::TypeParamDecl *> resolvedTypeParams =
      resolveTypeParameters(ctx, function.typeParameters);

  bool error = false;
  ScopeRAII typeParamScope(this);
  for (auto &&typeParamDecl : resolvedTypeParams)
    if (!insertDeclToCurrentScope(typeParamDecl))
      error = true;

  res::Type *retTy = function.type ? resolveType(ctx, *function.type)
                                   : ctx.getBuiltinUnitType();
  error |= !retTy;

  std::vector<res::Type *> paramTypes;
  std::vector<res::ParamDecl *> resolvedParams;

  ScopeRAII paramScope(this);
  for (auto &&param : function.params) {
    ResolutionContextRAII paramCtx(this, ParamList);

    auto *type = paramTypes.emplace_back(resolveType(ctx, *param->type));
    error |= !type;

    bool isOutputType = type && type->getAs<res::OutParamType>();
    if (isOutputType && param->isMutable) {
      report(param->location,
             "unexpected 'mut' specifier, a '&' parameter is always mutable");
      error |= true;
    }

    auto *resolvedParam = resolvedParams.emplace_back(
        ctx.create<res::ParamDecl>(param->location, param->identifier,
                                   param->isMutable || isOutputType));
    error |= !insertDeclToCurrentScope(resolvedParam);

    if (error)
      continue;

    ctx.bind(resolvedParam, type);
  }

  if (error)
    return nullptr;

  auto *fnDecl = ctx.create<res::FunctionDecl>(
      function.location, function.identifier, std::move(resolvedTypeParams),
      std::move(resolvedParams), parent);
  auto *fnTy = ctx.getFunctionType(std::move(paramTypes), retTy);
  return ctx.bind(fnDecl, fnTy);
};

res::FunctionDecl *
Sema::resolveFunctionBody(res::Context &ctx,
                          const ast::FunctionDecl &functionDecl,
                          res::FunctionDecl *function) {
  currentFunction = function;

  ScopeRAII typeParamScope(this);
  for (auto &&typeParam : currentFunction->typeParams)
    insertDeclToCurrentScope(typeParam);

  ScopeRAII paramScope(this);
  for (auto &&param : currentFunction->params)
    insertDeclToCurrentScope(param);

  varOrReturn(body, resolveBlock(ctx, *functionDecl.body));

  currentFunction->setBody(body);
  if (runFlowSensitiveChecks(ctx, *currentFunction))
    return nullptr;

  return function;
}

res::StructDecl *Sema::resolveStructDecl(res::Context &ctx,
                                         const ast::StructDecl &structDecl) {
  std::vector<res::TypeParamDecl *> resolvedTypeParams =
      resolveTypeParameters(ctx, structDecl.typeParameters);
  auto *resolvedStruct =
      ctx.create<res::StructDecl>(structDecl.location, structDecl.identifier,
                                  std::move(resolvedTypeParams));

  std::vector<res::Type *> typeParamTypes;
  for (auto &&typeParamDecl : resolvedStruct->typeParams)
    typeParamTypes.emplace_back(ctx.getType(typeParamDecl));

  return ctx.bind(resolvedStruct, ctx.getStructType(*resolvedStruct,
                                                    std::move(typeParamTypes)));
}

bool Sema::resolveMemberDecls(res::Context &ctx,
                              res::StructDecl &structDecl,
                              const ast::StructDecl &astDecl) {
  selfType = ctx.getType(&structDecl);
  bool error = false;
  ScopeRAII typeParamScope(this);
  for (auto &&typeParamDecl : structDecl.typeParams)
    error |= !insertDeclToCurrentScope(typeParamDecl);

  ScopeRAII memberScope(this);
  std::vector<res::FieldDecl *> resolvedFields;
  std::vector<res::FunctionDecl *> resolvedMemberFunctions;
  for (auto &&decl : astDecl.decls) {
    if (auto *field = dynamic_cast<ast::FieldDecl *>(decl.get())) {
      res::Type *fieldTy = resolveType(ctx, *field->type);
      if (!fieldTy) {
        error = true;
        continue;
      }

      auto *fieldDecl =
          ctx.create<res::FieldDecl>(field->location, field->identifier);
      error |= !insertDeclToCurrentScope(fieldDecl);
      resolvedFields.emplace_back(ctx.bind(fieldDecl, fieldTy));
      continue;
    }

    if (auto *memberFunction = dynamic_cast<ast::FunctionDecl *>(decl.get())) {
      auto *memberFn = resolveFunctionDecl(ctx, *memberFunction, &structDecl);
      error |= !insertDeclToCurrentScope(memberFn);
      if (memberFn)
        resolvedMemberFunctions.emplace_back(memberFn);
    }
  }

  structDecl.setMembers(resolvedFields, resolvedMemberFunctions);
  selfType = nullptr;
  return !error;
}

bool Sema::resolveMemberFunctionBodies(res::Context &ctx,
                                       res::StructDecl &decl,
                                       const ast::StructDecl &astDecl) {
  selfType = ctx.getType(&decl);
  ScopeRAII typeParamScope(this);
  for (auto &&typeParamDecl : decl.typeParams)
    insertDeclToCurrentScope(typeParamDecl);

  ScopeRAII memberScope(this);
  for (auto &&field : decl.fields)
    insertDeclToCurrentScope(field);

  bool error = false;
  for (size_t i = 0; i < decl.memberFunctions.size(); ++i)
    error |= !resolveFunctionBody(ctx, *astDecl.memberFunctions[i],
                                  decl.memberFunctions[i]);

  selfType = nullptr;
  return !error;
}

res::Context *Sema::resolveAST() {
  ScopeRAII globalScope(this);
  bool error = false;

  std::vector<res::StructDecl *> resolvedStructs;
  for (auto &&st : ast->structs) {
    resolvedStructs.emplace_back(resolveStructDecl(ctx, *st));
    error |= !insertDeclToCurrentScope(resolvedStructs.back());
  }

  for (size_t i = 0; i < resolvedStructs.size(); ++i)
    error |= !resolveMemberDecls(ctx, *resolvedStructs[i], *ast->structs[i]);
  error |= hasSelfContainingStructs(ctx);

  insertDeclToCurrentScope(createBuiltinPrintln(ctx));

  for (auto &&fn : ast->functions) {
    res::FunctionDecl *fnDecl = resolveFunctionDecl(ctx, *fn);
    error |= !insertDeclToCurrentScope(fnDecl);
    if (fnDecl)
      error |= !checkBuiltinFunctionCollisions(fnDecl);
  }

  if (error)
    return nullptr;

  for (size_t i = 0; i < resolvedStructs.size(); ++i)
    error |= !resolveMemberFunctionBodies(ctx, *resolvedStructs[i],
                                          *ast->structs[i]);

  for (auto &&fn : ast->functions)
    error |= !resolveFunctionBody(
        ctx, *fn, lexicalScope->lookupDecl<res::FunctionDecl>(fn->identifier));

  if (error)
    return nullptr;

  return &ctx;
}

bool Sema::checkBuiltinFunctionCollisions(const res::FunctionDecl *fnDecl) {
  if (fnDecl->identifier == "main") {
    if (!ctx.getType(fnDecl)
             ->getAs<res::FunctionType>()
             ->getReturnType()
             ->getAs<res::BuiltinUnitType>()) {
      report(fnDecl->location, "'main' function is expected to return 'unit'");
      return false;
    }

    if (!fnDecl->params.empty()) {
      report(fnDecl->location,
             "'main' function is expected to take no arguments");
      return false;
    }
  }

  if (fnDecl->identifier == "printf") {
    report(fnDecl->location,
           "'printf' is a reserved function name and cannot be used for "
           "user-defined functions");
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

      for (auto &&field : decl->fields)
        if (const auto *structTy = ctx.getType(field)->getAs<res::StructType>())
          worklist.emplace(structTy->getDecl());
    }
  }

  for (auto &&sd : selfContaining)
    report(sd->location, "struct '" + sd->identifier + "' contains itself");

  return !selfContaining.empty();
}
} // namespace yl
