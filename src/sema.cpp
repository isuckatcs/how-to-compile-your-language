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
  const auto *type = ctx.getType(&fn);
  assert(type && type->isFunctionType());
  if (static_cast<const res::FunctionType *>(type)
          ->getReturnType()
          ->isBuiltinVoid())
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
           returnCount > 0
               ? "non-void function doesn't return a value on every path"
               : "non-void function doesn't return a value");
  }

  return exitReached || returnCount == 0;
}

// FIXME: this function is actually doing liveness analysis and checking
// multiple things
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

        if (auto *decl = dynamic_cast<const res::DeclStmt *>(stmt)) {
          tmp[decl->varDecl] =
              decl->varDecl->initializer ? State::Assigned : State::Unassigned;
          continue;
        }

        if (auto *assignment = dynamic_cast<const res::Assignment *>(stmt)) {
          const res::Expr *base = assignment->assignee;
          while (const auto *member =
                     dynamic_cast<const res::MemberExpr *>(base))
            base = member->base;

          const auto *dre = dynamic_cast<const res::DeclRefExpr *>(base);

          // The base of the expression is not a variable, but a temporary,
          // which can be mutated.
          if (!dre)
            continue;

          // FIXME: what if this is a type?
          const auto *decl = dynamic_cast<const res::ValueDecl *>(dre->decl);

          if (!decl->isMutable && tmp[decl] != State::Unassigned) {
            std::string msg = '\'' + decl->identifier + "' cannot be mutated";
            pendingErrors.emplace_back(assignment->location, std::move(msg));
          }

          tmp[decl] = State::Assigned;
          continue;
        }

        if (const auto *dre = dynamic_cast<const res::DeclRefExpr *>(stmt)) {
          for (auto &&typeArg : dre->typeArgList) {
            if (typeArg->isUninferredType())
              pendingErrors.emplace_back(
                  dre->location,
                  "explicit type annotations needed to infer the type of '" +
                      dre->decl->identifier + "'");
          }

          const auto *var = dynamic_cast<const res::VarDecl *>(dre->decl);

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

  for (auto &&[d, s] : curLattices[cfg.exit + 1]) {
    if (s == State::Unassigned && ctx.getType(d)->isUninferredType())
      pendingErrors.emplace_back(d->location, "the type of '" + d->identifier +
                                                  "' is unknown");
  }

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

  auto *numTy = ctx.getBuiltinType(res::BuiltinType::Kind::Number);
  auto *fnTy = ctx.getUninferredFunctionType(1);

  ctx.unify(fnTy->getArgType(0), numTy);
  ctx.unify(fnTy->getReturnType(),
            ctx.getBuiltinType(res::BuiltinType::Kind::Void));

  auto *param = ctx.bind(ctx.create<res::ParamDecl>(loc, "n", false), numTy);
  auto *fn = ctx.create<res::FunctionDecl>(
      loc, "println", std::vector<res::TypeArgumentDecl *>{},
      std::vector{param});
  fn->setBody(ctx.create<res::Block>(loc, std::vector<res::Stmt *>()));

  return ctx.bind(fn, fnTy);
};

res::Type *Sema::resolveType(res::Context &ctx, const ast::Type &parsedType) {
  if (const auto *builtin =
          dynamic_cast<const ast::BuiltinType *>(&parsedType)) {
    switch (builtin->kind) {
    case ast::BuiltinType::Kind::Void:
      return ctx.getBuiltinType(res::BuiltinType::Kind::Void);
    case ast::BuiltinType::Kind::Number:
      return ctx.getBuiltinType(res::BuiltinType::Kind::Number);
    }
  }

  if (const auto *udt =
          dynamic_cast<const ast::UserDefinedType *>(&parsedType)) {
    res::Decl *decl = lookupDecl<res::TypeDecl>(udt->identifier).first;
    if (!decl)
      return report(udt->location,
                    "failed to resolve type '" + udt->identifier + "'");

    if (const auto *sd = dynamic_cast<const res::StructDecl *>(decl)) {
      varOrReturn(res, checkTypeParameterCount(udt->location,
                                               udt->typeArguments.size(),
                                               sd->typeArguments.size()));

      res::StructType *structTy = ctx.getUninferredStructType(*sd);
      for (size_t i = 0; i < udt->typeArguments.size(); ++i) {
        varOrReturn(resolvedType, resolveType(ctx, *udt->typeArguments[i]));
        ctx.unify(structTy->getTypeArg(i), resolvedType);
      }

      return structTy;
    }

    if (const auto *tad = dynamic_cast<const res::TypeArgumentDecl *>(decl))
      return ctx.getTypeArgumentType(*tad);

    llvm_unreachable("unexpected value type encountered");
  }

  if (const auto *function =
          dynamic_cast<const ast::FunctionType *>(&parsedType)) {
    auto *fnTy = ctx.getUninferredFunctionType(function->args.size());

    for (size_t i = 0; i < function->args.size(); ++i) {
      auto *argTy = resolveType(ctx, *function->args[i]);
      if (argTy->isBuiltinVoid())
        return report(function->args[i]->location,
                      "function type with 'void' argument is not allowed");
      ctx.unify(fnTy->getArgType(i), argTy);
    }

    ctx.unify(fnTy->getReturnType(), resolveType(ctx, *function->ret));
    return fnTy;
  }

  llvm_unreachable("ast type encountered");
}

res::UnaryOperator *
Sema::resolveUnaryOperator(res::Context &ctx, const ast::UnaryOperator &unary) {
  varOrReturn(rhs, resolveExpr(ctx, *unary.operand));

  auto *rhsTy = ctx.getType(rhs);
  if (rhsTy->isUninferredType())
    return report(rhs->location,
                  "type of operand to unary operator is unknown");

  const auto &loc = unary.location;
  if (!rhsTy->isBuiltinNumber())
    return report(loc, '\'' + rhsTy->getName() +
                           "' cannot be used as an operand to unary operator");

  return ctx.bind(ctx.create<res::UnaryOperator>(loc, unary.op, rhs), rhsTy);
}

res::BinaryOperator *
Sema::resolveBinaryOperator(res::Context &ctx,
                            const ast::BinaryOperator &binop) {
  varOrReturn(lhs, resolveExpr(ctx, *binop.lhs));
  varOrReturn(rhs, resolveExpr(ctx, *binop.rhs));

  auto *lhsTy = ctx.getType(lhs);
  auto *rhsTy = ctx.getType(rhs);

  if (lhsTy->isUninferredType() || rhsTy->isUninferredType())
    return report((lhsTy->isUninferredType() ? lhs : rhs)->location,
                  "type of " +
                      std::string(lhsTy->isUninferredType() ? "LHS" : "RHS") +
                      " to binary operator is unknown");

  const auto &loc = binop.location;
  if (lhsTy != rhsTy || !lhsTy->isBuiltinNumber())
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

  res::Expr::Kind kind = decl->isFunctionDecl() || decl->isStructDecl()
                             ? res::Expr::Kind::Rvalue
                             : res::Expr::Kind::Lvalue;

  auto *declTy = ctx.getType(decl);
  std::vector<res::Type *> instantiation = ctx.createInstantiation(decl);

  const auto &typeArgList = declRefExpr.typeArgumentList.get();
  if (typeArgList) {
    if (!decl->isGeneric())
      return report(typeArgList->location,
                    "'" + decl->identifier + "' is not a generic");

    varOrReturn(res, checkTypeParameterCount(typeArgList->location,
                                             typeArgList->args.size(),
                                             instantiation.size()));

    size_t i = 0;
    for (auto &&typeArg : typeArgList->args) {
      varOrReturn(resolvedTypeArg, resolveType(ctx, *typeArg));
      ctx.unify(instantiation[i++], resolvedTypeArg);
    }
  }

  if (decl->isGeneric())
    declTy = ctx.instantiate(ctx.getType(decl), instantiation);

  return ctx.bind(ctx.create<res::DeclRefExpr>(declRefExpr.location, *decl,
                                               kind, std::move(instantiation)),
                  declTy);
}

res::CallExpr *Sema::resolveCallExpr(res::Context &ctx,
                                     const ast::CallExpr &call) {
  varOrReturn(callee, resolveExpr(ctx, *call.callee));

  auto *calleeTy = ctx.getType(callee);
  if (!calleeTy->isFunctionType())
    return report(call.location,
                  "calling expression of type '" + calleeTy->getName() + '\'');

  auto *fnType = static_cast<res::FunctionType *>(calleeTy);
  if (call.arguments.size() != fnType->getArgCount())
    return report(call.location, "argument count mismatch in function call");

  std::vector<res::Expr *> args;
  int idx = 0;
  for (auto &&argument : call.arguments) {
    varOrReturn(arg, resolveExpr(ctx, *argument));

    if (!ctx.unify(fnType->getArgType(idx), ctx.getType(arg)))
      return report(arg->location, "expected '" +
                                       fnType->getArgType(idx)->getName() +
                                       "' argument, but received '" +
                                       ctx.getType(arg)->getName() + "'");

    arg->setConstantValue(cee.evaluate(*arg, false));
    args.emplace_back(arg);
    ++idx;
  }

  return ctx.bind(
      ctx.create<res::CallExpr>(call.location, callee, std::move(args)),
      fnType->getReturnType());
}

res::StructInstantiationExpr *Sema::resolveStructInstantiation(
    res::Context &ctx,
    const ast::StructInstantiationExpr &structInstantiation) {

  varOrReturn(dre, resolveDeclRefExpr(ctx, *structInstantiation.structRef));

  if (!dre->decl->isStructDecl())
    return report(dre->location, "expected struct declaration to instantiate");

  res::Type *ty = ctx.getType(dre);
  assert(ty->isStructType() && "struct decl doesn't have struct type");
  res::StructType *structTy = static_cast<res::StructType *>(ty);
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
    res::Type *fieldTy = ctx.getFieldType(structTy, fieldDecl);

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
        cee.evaluate(*initStmt->initializer, false));
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
  if (!baseTy->isStructType())
    return report(memberExpr.base->location,
                  "cannot access field of '" + baseTy->getName() + '\'');

  auto *structTy = static_cast<res::StructType *>(baseTy);
  const auto *sd = structTy->getDecl();

  res::FieldDecl *fieldDecl = nullptr;
  for (auto &&field : sd->fields)
    if (field->identifier == memberExpr.member->identifier)
      fieldDecl = field;

  if (!fieldDecl)
    return report(memberExpr.location,
                  '\'' + sd->identifier + "' has no field called '" +
                      memberExpr.member->identifier + '\'');

  res::Type *fieldTy = ctx.getFieldType(structTy, fieldDecl);

  return ctx.bind(
      ctx.create<res::MemberExpr>(memberExpr.location, base, fieldDecl),
      fieldTy);
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
  if (!ctx.unify(ctx.getType(cond),
                 ctx.getBuiltinType(res::BuiltinType::Kind::Number)))
    return report(cond->location, "expected number in condition");

  varOrReturn(trueBlock, resolveBlock(ctx, *ifStmt.trueBlock));

  res::Block *falseBlock = nullptr;
  if (ifStmt.falseBlock) {
    falseBlock = resolveBlock(ctx, *ifStmt.falseBlock);
    if (!falseBlock)
      return nullptr;
  }

  cond->setConstantValue(cee.evaluate(*cond, false));

  return ctx.create<res::IfStmt>(ifStmt.location, cond, trueBlock, falseBlock);
}

res::WhileStmt *Sema::resolveWhileStmt(res::Context &ctx,
                                       const ast::WhileStmt &whileStmt) {
  varOrReturn(cond, resolveExpr(ctx, *whileStmt.condition));
  if (!ctx.unify(ctx.getType(cond),
                 ctx.getBuiltinType(res::BuiltinType::Kind::Number)))
    return report(cond->location, "expected number in condition");

  varOrReturn(body, resolveBlock(ctx, *whileStmt.body));

  cond->setConstantValue(cee.evaluate(*cond, false));

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
    return report(lhs->location, "expression is not assignable");

  auto *lhsTy = ctx.getType(lhs);
  auto *rhsTy = ctx.getType(rhs);

  if (rhsTy->isBuiltinVoid())
    return report(rhs->location,
                  "'void' expression is not allowed inside assignment");

  if (!ctx.unify(lhsTy, rhsTy))
    return report(rhs->location, "expected to assign '" + lhsTy->getName() +
                                     "' but received '" + rhsTy->getName() +
                                     "' instead");
  rhs->setConstantValue(cee.evaluate(*rhs, false));

  return ctx.create<res::Assignment>(assignment.location, lhs, rhs);
}

res::ReturnStmt *Sema::resolveReturnStmt(res::Context &ctx,
                                         const ast::ReturnStmt &returnStmt) {
  assert(currentFunction && "return stmt outside a function");

  auto *ty = ctx.getType(currentFunction);
  assert(ty->isFunctionType() && "function decl is not of function type");
  auto *fnTy = static_cast<res::FunctionType *>(ty);

  auto *retTy = fnTy->getReturnType();
  if (retTy->isBuiltinVoid() && returnStmt.expr)
    return report(returnStmt.location,
                  "unexpected return value in 'void' function");

  if (!retTy->isUninferredType() && !retTy->isBuiltinVoid() && !returnStmt.expr)
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

    expr->setConstantValue(cee.evaluate(*expr, false));
  }

  return ctx.create<res::ReturnStmt>(returnStmt.location, expr);
}

res::Expr *Sema::resolveExpr(res::Context &ctx, const ast::Expr &expr) {
  if (const auto *number = dynamic_cast<const ast::NumberLiteral *>(&expr)) {
    return ctx.bind(ctx.create<res::NumberLiteral>(number->location,
                                                   std::stod(number->value)),
                    ctx.getBuiltinType(res::BuiltinType::Kind::Number));
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

  if (const auto *declRefExpr = dynamic_cast<const ast::DeclRefExpr *>(&expr)) {
    varOrReturn(dre, resolveDeclRefExpr(ctx, *declRefExpr));

    if (dre->decl->isStructDecl())
      return report(declRefExpr->location, "expected an instance of '" +
                                               declRefExpr->identifier + '\'');

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

res::ParamDecl *Sema::resolveParamDecl(res::Context &ctx,
                                       const ast::ParamDecl &param) {
  auto *type = resolveType(ctx, *param.type);

  if (!type || type->isBuiltinVoid())
    return report(param.location,
                  "parameter '" + param.identifier + "' has invalid type");

  return ctx.bind(ctx.create<res::ParamDecl>(param.location, param.identifier,
                                             param.isMutable),
                  type);
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
      declTy = ctx.getType(decl);
      initTy = ctx.getType(initializer);

      return report(decl->initializer->location,
                    "an expression of type '" + initTy->getName() +
                        "' cannot be used to initialize a variable of type '" +
                        declTy->getName() + "'");
    }

    initializer->setConstantValue(cee.evaluate(*initializer, false));
  }

  const auto *declTy = ctx.getType(decl);
  if (declTy->isBuiltinVoid())
    return report(decl->location, "a variable of '" + declTy->getName() +
                                      "' type is not allowed");
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

std::vector<res::TypeArgumentDecl *> Sema::resolveTypeParameters(
    res::Context &ctx,
    const std::vector<std::unique_ptr<ast::TypeParamDecl>> &typeParamDecls) {
  std::vector<res::TypeArgumentDecl *> resolvedTypeArguments;
  for (auto &&typeArg : typeParamDecls) {
    auto *resolvedTypeArg = ctx.create<res::TypeArgumentDecl>(
        typeArg->location, typeArg->identifier, resolvedTypeArguments.size());

    resolvedTypeArguments.emplace_back(
        ctx.bind(resolvedTypeArg, ctx.getTypeArgumentType(*resolvedTypeArg)));
  }

  return resolvedTypeArguments;
}

res::FunctionDecl *
Sema::resolveFunctionDecl(res::Context &ctx,
                          const ast::FunctionDecl &function) {
  std::vector<res::TypeArgumentDecl *> resolvedTypeArguments =
      resolveTypeParameters(ctx, function.typeParameters);

  ScopeRAII typeArgScope(this);
  for (auto &&typeArgDecl : resolvedTypeArguments)
    if (!insertDeclToCurrentScope(typeArgDecl))
      return nullptr;

  varOrReturn(retTy, resolveType(ctx, *function.type));

  if (function.identifier == "main") {
    if (!retTy->isBuiltinVoid())
      return report(function.location,
                    "'main' function is expected to return 'void'");

    if (!function.params.empty())
      return report(function.location,
                    "'main' function is expected to take no arguments");
  } else if (function.identifier == "printf") {
    return report(function.location,
                  "'printf' is a reserved function name and cannot be used for "
                  "user-defined functions");
  }

  std::vector<res::ParamDecl *> resolvedParams;
  auto *fnTy = ctx.getUninferredFunctionType(function.params.size());

  // FIXME: how to avoid doing this twice?
  ScopeRAII paramScope(this);
  for (size_t i = 0; i < function.params.size(); ++i) {
    auto *resolvedParam = resolveParamDecl(ctx, *function.params[i]);

    if (!resolvedParam || !insertDeclToCurrentScope(resolvedParam))
      return nullptr;

    ctx.unify(fnTy->getArgType(i), ctx.getType(resolvedParam));
    resolvedParams.emplace_back(std::move(resolvedParam));
  }

  ctx.unify(fnTy->getReturnType(), retTy);
  return ctx.bind(
      ctx.create<res::FunctionDecl>(function.location, function.identifier,
                                    std::move(resolvedTypeArguments),
                                    std::move(resolvedParams)),
      fnTy);
};

res::StructDecl *Sema::resolveStructDecl(res::Context &ctx,
                                         const ast::StructDecl &structDecl) {
  std::vector<res::TypeArgumentDecl *> resolvedTypeArguments =
      resolveTypeParameters(ctx, structDecl.typeParameters);
  auto *resolvedStruct =
      ctx.create<res::StructDecl>(structDecl.location, structDecl.identifier,
                                  std::move(resolvedTypeArguments));

  res::StructType *structTy = ctx.getUninferredStructType(*resolvedStruct);
  ScopeRAII typeArgScope(this);
  for (size_t i = 0; i < structTy->getTypeArgCount(); ++i) {
    res::Decl *typeArgDecl = resolvedStruct->typeArguments[i];
    if (!insertDeclToCurrentScope(typeArgDecl))
      return nullptr;

    ctx.unify(structTy->getTypeArg(i), ctx.getType(typeArgDecl));
  }

  return ctx.bind(resolvedStruct, structTy);
}

res::StructDecl *Sema::resolveStructFields(res::Context &ctx,
                                           const ast::StructDecl &astDecl) {
  // FIXME: should empty structs be allowed?
  res::StructDecl *decl = lookupDecl<res::StructDecl>(astDecl.identifier).first;
  assert(decl && !decl->isComplete);

  ScopeRAII typeArgScope(this);
  for (auto &&typeArgDecl : decl->typeArguments)
    insertDeclToCurrentScope(typeArgDecl);

  bool error = false;
  std::set<std::string_view> identifiers;
  std::vector<res::FieldDecl *> resolvedFields;
  for (auto &&field : astDecl.fields) {
    varOrReturn(fieldTy, resolveType(ctx, *field->type));

    auto loc = field->location;
    if (fieldTy->isBuiltinVoid()) {
      report(loc, "struct field cannot be 'void'");
      error = true;
    }

    if (!identifiers.emplace(field->identifier).second) {
      report(field->location,
             "field '" + field->identifier + "' is already declared");
      error = true;
    }

    auto *fieldDecl = ctx.create<res::FieldDecl>(loc, field->identifier,
                                                 resolvedFields.size());
    resolvedFields.emplace_back(ctx.bind(fieldDecl, fieldTy));
  }

  if (error)
    return nullptr;

  decl->setFields(std::move(resolvedFields));
  return decl;
}

std::optional<res::Context> Sema::resolveAST() {
  ScopeRAII globalScope(this);
  res::Context ctx = res::Context::createEmptyContext();
  bool error = false;

  // Resolve every struct first so that functions have access to them in their
  // signatures.
  for (auto &&st : ast->structs)
    error |= !insertDeclToCurrentScope(resolveStructDecl(ctx, *st));
  if (error)
    return std::nullopt;

  // Insert println first to be able to detect a possible redeclaration.
  insertDeclToCurrentScope(createBuiltinPrintln(ctx));

  for (auto &&fn : ast->functions)
    error |= !insertDeclToCurrentScope(resolveFunctionDecl(ctx, *fn));
  if (error)
    return std::nullopt;

  for (auto &&st : ast->structs)
    error |= !resolveStructFields(ctx, *st);
  if (error)
    return std::nullopt;

  if (!checkSelfContainingStructs(ctx))
    return std::nullopt;

  for (auto &&fn : ast->functions) {
    currentFunction = lookupDecl<res::FunctionDecl>(fn->identifier).first;

    ScopeRAII typeArgScope(this);
    for (auto &&typeArg : currentFunction->typeArguments)
      insertDeclToCurrentScope(typeArg);

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
    return std::nullopt;

  return ctx;
}

bool Sema::checkSelfContainingStructs(const res::Context &ctx) {
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

      for (auto &&field : decl->fields) {
        const auto *type = ctx.getType(field);
        if (!type->isStructType())
          continue;

        worklist.emplace(static_cast<const res::StructType *>(type)->getDecl());
      }
    }
  }

  for (auto &&sd : selfContaining)
    report(sd->location, "struct '" + sd->identifier + "' contains itself");

  return selfContaining.empty();
}
} // namespace yl
