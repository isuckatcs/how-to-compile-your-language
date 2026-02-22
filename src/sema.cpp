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
  if (decl == nullptr)
    return false;

  const auto &[foundDecl, scopeIdx] = lookupDecl<res::Decl>(decl->identifier);

  if (foundDecl && scopeIdx == 0) {
    report(decl->location, "redeclaration of '" + decl->identifier + '\'');
    return false;
  }

  scopes.back().emplace_back(decl);
  return true;
}

template <typename T>
std::pair<T *, int> Sema::lookupDecl(const std::string id) {
  int scopeIdx = 0;
  for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
    for (auto &&decl : *it) {
      auto *correctDecl = dynamic_cast<T *>(decl);

      if (!correctDecl)
        continue;

      if (decl->identifier != id)
        continue;

      return {correctDecl, scopeIdx};
    }

    ++scopeIdx;
  }

  return {nullptr, -1};
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
    }
  }

  if (const auto *udt =
          dynamic_cast<const ast::UserDefinedType *>(&parsedType)) {
    res::Decl *decl = lookupDecl<res::TypeDecl>(udt->identifier).first;
    if (!decl)
      return report(udt->location,
                    "failed to resolve type '" + udt->identifier + "'");

    if (const auto *sd = decl->getAs<res::StructDecl>()) {
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

  if (const auto *ptr = dynamic_cast<const ast::PointerType *>(&parsedType)) {
    if (!(resolutionContext & ParamList))
      return report(ptr->location, "only parameters can have '*' type");

    varOrReturn(pointeeType, resolveType(ctx, *ptr->pointeeType));
    if (pointeeType->getAs<res::PointerType>())
      return report(ptr->location, "a type can have only one '*' specifier");

    return ctx.getPointerType(pointeeType);
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
                    "'&' can only be used to pass arguments to '*' parameters");

    if (!rhs->isMutable())
      return report(unary.location,
                    "only mutable lvalues can be passed to '*' parameters");

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
  res::Decl *decl = lookupDecl<res::Decl>(declRefExpr.identifier).first;
  if (!decl)
    return report(declRefExpr.location,
                  "symbol '" + declRefExpr.identifier + "' not found");

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

    auto *ptrType = ctx.getType(dre)->getAs<res::PointerType>();
    if (ptrType)
      return ctx.bind(ctx.create<res::ImplicitDerefExpr>(dre->location, dre),
                      ptrType->getPointeeType());

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

res::FunctionDecl *
Sema::resolveFunctionDecl(res::Context &ctx,
                          const ast::FunctionDecl &function) {
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

  if (function.identifier == "main") {
    if (!retTy || !retTy->getAs<res::BuiltinUnitType>())
      return report(function.location,
                    "'main' function is expected to return 'unit'");

    if (!function.params.empty())
      return report(function.location,
                    "'main' function is expected to take no arguments");
  } else if (function.identifier == "printf") {
    return report(function.location,
                  "'printf' is a reserved function name and cannot be used for "
                  "user-defined functions");
  }

  std::vector<res::Type *> paramTypes;
  std::vector<res::ParamDecl *> resolvedParams;

  ScopeRAII paramScope(this);
  for (auto &&param : function.params) {
    ResolutionContextRAII paramCtx(this, ParamList);

    auto *type = paramTypes.emplace_back(resolveType(ctx, *param->type));
    error |= !type;

    bool isOutputType = type && type->getAs<res::PointerType>();
    if (isOutputType && param->isMutable) {
      report(param->location,
             "unexpected 'mut' specifier, a '*' parameter is always mutable");
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
      std::move(resolvedParams));
  auto *fnTy = ctx.getFunctionType(std::move(paramTypes), retTy);
  return ctx.bind(fnDecl, fnTy);
};

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

bool Sema::resolveStructFields(res::Context &ctx,
                               res::StructDecl &decl,
                               const ast::StructDecl &astDecl) {
  assert(!decl.isComplete);

  bool error = false;
  ScopeRAII typeParamScope(this);
  for (auto &&typeParamDecl : decl.typeParams)
    error |= !insertDeclToCurrentScope(typeParamDecl);

  std::set<std::string_view> identifiers;
  std::vector<res::FieldDecl *> resolvedFields;

  for (auto &&field : astDecl.fields) {
    res::Type *fieldTy = resolveType(ctx, *field->type);
    if (!fieldTy) {
      error = true;
      continue;
    }

    auto loc = field->location;
    if (!identifiers.emplace(field->identifier).second) {
      report(loc, "field '" + field->identifier + "' is already declared");
      error = true;
    }

    auto *fieldDecl = ctx.create<res::FieldDecl>(loc, field->identifier);
    resolvedFields.emplace_back(ctx.bind(fieldDecl, fieldTy));
  }

  decl.setFields(std::move(resolvedFields));
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
    error |= !resolveStructFields(ctx, *resolvedStructs[i], *ast->structs[i]);
  error |= hasSelfContainingStructs(ctx);

  insertDeclToCurrentScope(createBuiltinPrintln(ctx));

  for (auto &&fn : ast->functions)
    error |= !insertDeclToCurrentScope(resolveFunctionDecl(ctx, *fn));
  if (error)
    return nullptr;

  for (auto &&fn : ast->functions) {
    currentFunction = lookupDecl<res::FunctionDecl>(fn->identifier).first;

    ScopeRAII typeParamScope(this);
    for (auto &&typeParam : currentFunction->typeParams)
      insertDeclToCurrentScope(typeParam);

    ScopeRAII paramScope(this);
    for (auto &&param : currentFunction->params)
      insertDeclToCurrentScope(param);

    auto *body = resolveBlock(ctx, *fn->body);
    if (!body) {
      error = true;
      continue;
    }

    currentFunction->setBody(body);
    error |= runFlowSensitiveChecks(ctx, *currentFunction);
  }

  if (error)
    return nullptr;

  return &ctx;
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
