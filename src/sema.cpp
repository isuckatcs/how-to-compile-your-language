#include <cassert>
#include <map>
#include <set>
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
  error |= checkVariableInitialization(cfg);

  return error;
};

bool Sema::checkReturnOnAllPaths(res::Context &ctx,
                                 const res::FunctionDecl &fn,
                                 const CFG &cfg) {
  const auto *type = ctx.getType(&fn);
  assert(type && type->isFunctionType());
  if (static_cast<const res::FunctionType *>(type)->ret->isBuiltinVoid())
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

  const auto *numTy = ctx.getBuiltinType(res::BuiltinType::Kind::Number);
  const auto *voidTy = ctx.getBuiltinType(res::BuiltinType::Kind::Void);
  const auto *fnTy = ctx.getFunctionType({numTy}, voidTy);

  auto *param = ctx.bind(ctx.create<res::ParamDecl>(loc, "n", false), numTy);
  auto *fn = ctx.create<res::FunctionDecl>(loc, "println", std::vector{param});
  fn->setBody(ctx.create<res::Block>(loc, std::vector<res::Stmt *>()));

  return ctx.bind(fn, fnTy);
};

const res::Type *Sema::resolveType(res::Context &ctx,
                                   const ast::Type &parsedType) {
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

    if (const auto *sd = dynamic_cast<const res::StructDecl *>(decl))
      return ctx.getStructType(*sd);

    llvm_unreachable("unexpected value type encountered");
  }

  if (const auto *function =
          dynamic_cast<const ast::FunctionType *>(&parsedType)) {

    std::vector<const res::Type *> args;
    for (auto &&arg : function->args) {
      varOrReturn(type, resolveType(ctx, *arg));
      args.emplace_back(type);
    }

    varOrReturn(ret, resolveType(ctx, *function->ret));
    return ctx.getFunctionType(std::move(args), ret);
  }

  // FIXME: we should error out here
  return nullptr;
}

res::UnaryOperator *
Sema::resolveUnaryOperator(res::Context &ctx, const ast::UnaryOperator &unary) {
  varOrReturn(rhs, resolveExpr(ctx, *unary.operand));

  auto rhsTy = ctx.getType(rhs);
  if (!rhsTy->isKnown())
    return report(rhs->location,
                  "type of operand to unary operator is unknown");

  const auto &loc = unary.location;
  if (!rhsTy->isBuiltinNumber())
    return report(loc, '\'' + rhsTy->asString() +
                           "' cannot be used as an operand to unary operator");

  return ctx.bind(ctx.create<res::UnaryOperator>(loc, unary.op, rhs), rhsTy);
}

res::BinaryOperator *
Sema::resolveBinaryOperator(res::Context &ctx,
                            const ast::BinaryOperator &binop) {
  varOrReturn(lhs, resolveExpr(ctx, *binop.lhs));
  varOrReturn(rhs, resolveExpr(ctx, *binop.rhs));

  auto lhsTy = ctx.getType(lhs);
  auto rhsTy = ctx.getType(rhs);

  // FIXME: catch this on read?
  if (!lhsTy->isKnown() || !rhsTy->isKnown())
    return report((!lhsTy->isKnown() ? lhs : rhs)->location,
                  "type of " + std::string(!lhsTy->isKnown() ? "LHS" : "RHS") +
                      " to binary operator is unknown");

  const auto &loc = binop.location;
  if (lhsTy != rhsTy || !lhsTy->isBuiltinNumber())
    return report(loc, "incompatible operands to binary operator ('" +
                           lhsTy->asString() + "' and '" + rhsTy->asString() +
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

  // FIXME: handle templates
  return ctx.bind(
      ctx.create<res::DeclRefExpr>(declRefExpr.location, *decl, kind),
      ctx.getType(decl));
}

res::CallExpr *Sema::resolveCallExpr(res::Context &ctx,
                                     const ast::CallExpr &call) {
  varOrReturn(callee, resolveExpr(ctx, *call.callee));

  auto calleeTy = ctx.getType(callee);
  if (!calleeTy->isFunctionType())
    return report(call.location,
                  "calling expression of type '" + calleeTy->asString() + '\'');

  const auto *fnType = static_cast<const res::FunctionType *>(calleeTy);
  if (call.arguments.size() != fnType->args.size())
    return report(call.location, "argument count mismatch in function call");

  std::vector<res::Expr *> args;
  int idx = 0;
  for (auto &&argument : call.arguments) {
    varOrReturn(arg, resolveExpr(ctx, *argument));

    if (!ctx.unify(fnType->args[idx], ctx.getType(arg)))
      return report(arg->location, "expected '" +
                                       fnType->args[idx]->asString() +
                                       "' argument, but received '" +
                                       ctx.getType(arg)->asString() + "'");

    arg->setConstantValue(cee.evaluate(*arg, false));
    args.emplace_back(arg);
    ++idx;
  }

  return ctx.bind(
      ctx.create<res::CallExpr>(call.location, callee, std::move(args)),
      fnType->ret);
}

res::StructInstantiationExpr *Sema::resolveStructInstantiation(
    res::Context &ctx,
    const ast::StructInstantiationExpr &structInstantiation) {

  varOrReturn(dre, resolveDeclRefExpr(ctx, *structInstantiation.structRef));

  if (!dre->decl->isStructDecl())
    return report(dre->location, "expected struct declaration to instantiate");

  auto *sd = static_cast<res::StructDecl *>(dre->decl);

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

    if (!ctx.unify(ctx.getType(fieldDecl), ctx.getType(resolvedInitExpr))) {
      report(resolvedInitExpr->location,
             "an expression of type '" +
                 ctx.getType(resolvedInitExpr)->asString() +
                 "' cannot be assigned to a variable of type '" +
                 ctx.getType(fieldDecl)->asString() + "'");
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
      ctx.create<res::StructInstantiationExpr>(structInstantiation.location, sd,
                                               std::move(resolvedFieldInits)),
      ctx.getType(sd));
}

res::MemberExpr *Sema::resolveMemberExpr(res::Context &ctx,
                                         const ast::MemberExpr &memberExpr) {
  // FIXME: revisit
  varOrReturn(base, resolveExpr(ctx, *memberExpr.base));

  auto baseTy = ctx.getType(base);
  if (!baseTy->isStructType())
    return report(memberExpr.base->location,
                  "cannot access field of '" + baseTy->asString() + '\'');

  const res::StructDecl *sd =
      static_cast<const res::StructType *>(baseTy)->decl;
  assert(sd && "struct type without decl");

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
      ctx.getType(fieldDecl));
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

  const auto *lhsTy = ctx.getType(lhs);
  const auto *rhsTy = ctx.getType(rhs);

  if (rhsTy->isBuiltinVoid())
    return report(rhs->location,
                  "'void' expression is not allowed inside assignment");

  if (!ctx.unify(lhsTy, rhsTy))
    return report(rhs->location, "expected to assign '" + lhsTy->asString() +
                                     "' but received '" + rhsTy->asString() +
                                     "' instead");
  rhs->setConstantValue(cee.evaluate(*rhs, false));

  return ctx.create<res::Assignment>(assignment.location, lhs, rhs);
}

res::ReturnStmt *Sema::resolveReturnStmt(res::Context &ctx,
                                         const ast::ReturnStmt &returnStmt) {
  assert(currentFunction && "return stmt outside a function");

  // FIXME: static_cast?!
  const res::Type *fnRetType =
      static_cast<const res::FunctionType *>(ctx.getType(currentFunction))->ret;
  if (fnRetType->isBuiltinVoid() && returnStmt.expr)
    return report(returnStmt.location,
                  "unexpected return value in 'void' function");

  if (fnRetType->isKnown() && !fnRetType->isBuiltinVoid() && !returnStmt.expr)
    return report(returnStmt.location, "expected a return value");

  res::Expr *expr = nullptr;
  if (returnStmt.expr) {
    expr = resolveExpr(ctx, *returnStmt.expr);
    if (!expr)
      return nullptr;

    const res::Type *exprTy = ctx.getType(expr);
    if (!ctx.unify(fnRetType, exprTy))
      // FIXME: unification changes types
      return report(expr->location, "cannot return '" + exprTy->asString() +
                                        "' from a function returning '" +
                                        fnRetType->asString() + "'");

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
    auto resolvedStmt = resolveStmt(ctx, *stmt);

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
  auto type = resolveType(ctx, *param.type);

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

  auto decl =
      ctx.bind(ctx.create<res::VarDecl>(varDecl.location, varDecl.identifier,
                                        varDecl.isMutable, initializer),
               ctx.getNewUninferredType());

  if (varDecl.type) {
    varOrReturn(type, resolveType(ctx, *varDecl.type));
    ctx.unify(ctx.getType(decl), type);
  }

  if (initializer) {
    auto declTy = ctx.getType(decl);
    auto initTy = ctx.getType(initializer);

    if (!ctx.unify(declTy, initTy)) {
      // FIXME: unification changes types
      return report(decl->initializer->location,
                    "an expression of type '" + initTy->asString() +
                        "' cannot be used to initialize a variable of type '" +
                        declTy->asString() + "'");
    }

    initializer->setConstantValue(cee.evaluate(*initializer, false));
  }

  auto declTy = ctx.getType(decl);
  if (declTy == ctx.getBuiltinType(res::BuiltinType::Kind::Void))
    return report(decl->location, "a variable of '" + declTy->asString() +
                                      "' type is not allowed");
  return decl;
}

res::FunctionDecl *
Sema::resolveFunctionDecl(res::Context &ctx,
                          const ast::FunctionDecl &function) {
  const auto *retTy = resolveType(ctx, *function.type);

  if (!retTy)
    return report(function.type->location, "function '" + function.identifier +
                                               "' has invalid return type");

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
  std::vector<const res::Type *> paramTypes;

  // FIXME: how to avoid doing this twice?
  ScopeRAII paramScope(this);
  for (auto &&param : function.params) {
    auto *resolvedParam = resolveParamDecl(ctx, *param);

    if (!resolvedParam || !insertDeclToCurrentScope(resolvedParam))
      return nullptr;

    paramTypes.emplace_back(ctx.getType(resolvedParam));
    resolvedParams.emplace_back(std::move(resolvedParam));
  }

  return ctx.bind(ctx.create<res::FunctionDecl>(function.location,
                                                function.identifier,
                                                std::move(resolvedParams)),
                  ctx.getFunctionType(std::move(paramTypes), retTy));
};

res::StructDecl *Sema::resolveStructDecl(res::Context &ctx,
                                         const ast::StructDecl &structDecl) {
  auto *resolvedStruct =
      ctx.create<res::StructDecl>(structDecl.location, structDecl.identifier);
  return ctx.bind(resolvedStruct, ctx.getStructType(*resolvedStruct));
}

res::StructDecl *Sema::resolveStructFields(res::Context &ctx,
                                           const ast::StructDecl &astDecl) {
  // FIXME: should empty structs be allowed?
  res::StructDecl *decl = lookupDecl<res::StructDecl>(astDecl.identifier).first;
  assert(decl && !decl->isComplete);

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
  // signature.
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
    ScopeRAII paramScope(this);

    currentFunction = lookupDecl<res::FunctionDecl>(fn->identifier).first;
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

        worklist.emplace(static_cast<const res::StructType *>(type)->decl);
      }
    }
  }

  for (auto &&sd : selfContaining)
    report(sd->location, "struct '" + sd->identifier + "' contains itself");

  return selfContaining.empty();
}
} // namespace yl
