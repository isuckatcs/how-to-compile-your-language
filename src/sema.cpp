#include <cassert>
#include <map>
#include <set>
#include <stack>

#include "cfg.h"
#include "sema.h"
#include "utils.h"

namespace yl {
bool Sema::runFlowSensitiveChecks(const ResolvedFunctionDecl &fn) {
  CFG cfg = CFGBuilder().build(fn);

  bool error = false;
  error |= checkReturnOnAllPaths(fn, cfg);
  error |= checkVariableInitialization(cfg);

  return error;
};

bool Sema::checkReturnOnAllPaths(const ResolvedFunctionDecl &fn,
                                 const CFG &cfg) {
  if (fn.type.kind == Type::Kind::Void)
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

    if (!stmts.empty() && dynamic_cast<const ResolvedReturnStmt *>(stmts[0])) {
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

  using Lattice = std::map<const ResolvedDecl *, State>;

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
        const ResolvedStmt *stmt = *it;

        if (auto *decl = dynamic_cast<const ResolvedDeclStmt *>(stmt)) {
          tmp[decl->varDecl.get()] =
              decl->varDecl->initializer ? State::Assigned : State::Unassigned;
          continue;
        }

        if (auto *assignment = dynamic_cast<const ResolvedAssignment *>(stmt)) {
          const ResolvedExpr *base = assignment->assignee.get();
          while (const auto *member =
                     dynamic_cast<const ResolvedMemberExpr *>(base))
            base = member->base.get();

          const auto *dre = dynamic_cast<const ResolvedDeclRefExpr *>(base);

          // The base of the expression is not a variable, but a temporary,
          // which can be mutated.
          if (!dre)
            continue;

          const auto *decl = dynamic_cast<const ResolvedDecl *>(dre->decl);

          if (!decl->isMutable && tmp[decl] != State::Unassigned) {
            std::string msg = '\'' + decl->identifier + "' cannot be mutated";
            pendingErrors.emplace_back(assignment->location, std::move(msg));
          }

          tmp[decl] = State::Assigned;
          continue;
        }

        if (const auto *dre = dynamic_cast<const ResolvedDeclRefExpr *>(stmt)) {
          const auto *var = dynamic_cast<const ResolvedVarDecl *>(dre->decl);

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

bool Sema::insertDeclToCurrentScope(ResolvedDecl &decl) {
  const auto &[foundDecl, scopeIdx] = lookupDecl<ResolvedDecl>(decl.identifier);

  if (foundDecl && scopeIdx == 0) {
    report(decl.location, "redeclaration of '" + decl.identifier + '\'');
    return false;
  }

  scopes.back().emplace_back(&decl);
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

std::unique_ptr<ResolvedFunctionDecl> Sema::createBuiltinPrintln() {
  SourceLocation loc{"<builtin>", 0, 0};

  auto param = std::make_unique<ResolvedParamDecl>(
      loc, "n", Type::builtinNumber(), false);

  std::vector<std::unique_ptr<ResolvedParamDecl>> params;
  params.emplace_back(std::move(param));

  auto block = std::make_unique<ResolvedBlock>(
      loc, std::vector<std::unique_ptr<ResolvedStmt>>());

  return std::make_unique<ResolvedFunctionDecl>(
      loc, "println", Type::builtinVoid(), std::move(params), std::move(block));
};

std::optional<Type> Sema::resolveType(Type parsedType) {
  if (parsedType.kind == Type::Kind::Custom) {
    if (auto *decl = lookupDecl<ResolvedStructDecl>(parsedType.name).first)
      return Type::structType(decl->identifier);

    return std::nullopt;
  }

  return parsedType;
}

std::unique_ptr<ResolvedUnaryOperator>
Sema::resolveUnaryOperator(const UnaryOperator &unary) {
  varOrReturn(resolvedRHS, resolveExpr(*unary.operand));

  if (resolvedRHS->type.kind != Type::Kind::Number)
    return report(resolvedRHS->location,
                  '\'' + resolvedRHS->type.name +
                      "' cannot be used as an operand to unary operator");

  return std::make_unique<ResolvedUnaryOperator>(unary.location, unary.op,
                                                 std::move(resolvedRHS));
}

std::unique_ptr<ResolvedBinaryOperator>
Sema::resolveBinaryOperator(const BinaryOperator &binop) {
  varOrReturn(resolvedLHS, resolveExpr(*binop.lhs));
  varOrReturn(resolvedRHS, resolveExpr(*binop.rhs));

  if (resolvedLHS->type.kind != Type::Kind::Number)
    return report(resolvedLHS->location,
                  '\'' + resolvedLHS->type.name +
                      "' cannot be used as LHS operand to binary operator");

  if (resolvedRHS->type.kind != Type::Kind::Number)
    return report(resolvedRHS->location,
                  '\'' + resolvedRHS->type.name +
                      "' cannot be used as RHS operand to binary operator");

  assert(resolvedLHS->type.kind == resolvedRHS->type.kind &&
         resolvedLHS->type.kind == Type::Kind::Number &&
         "unexpected type in binop");

  return std::make_unique<ResolvedBinaryOperator>(
      binop.location, binop.op, std::move(resolvedLHS), std::move(resolvedRHS));
}

std::unique_ptr<ResolvedGroupingExpr>
Sema::resolveGroupingExpr(const GroupingExpr &grouping) {
  varOrReturn(resolvedExpr, resolveExpr(*grouping.expr));
  return std::make_unique<ResolvedGroupingExpr>(grouping.location,
                                                std::move(resolvedExpr));
}

std::unique_ptr<ResolvedDeclRefExpr>
Sema::resolveDeclRefExpr(const DeclRefExpr &declRefExpr, bool isCallee) {
  ResolvedDecl *decl = lookupDecl<ResolvedDecl>(declRefExpr.identifier).first;
  if (!decl)
    return report(declRefExpr.location,
                  "symbol '" + declRefExpr.identifier + "' not found");

  if (!isCallee && dynamic_cast<ResolvedFunctionDecl *>(decl))
    return report(declRefExpr.location,
                  "expected to call function '" + declRefExpr.identifier + "'");

  if (dynamic_cast<ResolvedStructDecl *>(decl))
    return report(declRefExpr.location,
                  "expected an instance of '" + decl->type.name + '\'');

  return std::make_unique<ResolvedDeclRefExpr>(declRefExpr.location, *decl);
}

std::unique_ptr<ResolvedCallExpr> Sema::resolveCallExpr(const CallExpr &call) {
  const auto *dre = dynamic_cast<const DeclRefExpr *>(call.callee.get());
  if (!dre)
    return report(call.location, "expression cannot be called as a function");

  varOrReturn(resolvedCallee, resolveDeclRefExpr(*dre, true));

  const auto *resolvedFunctionDecl =
      dynamic_cast<const ResolvedFunctionDecl *>(resolvedCallee->decl);

  if (!resolvedFunctionDecl)
    return report(call.location, "calling non-function symbol");

  if (call.arguments.size() != resolvedFunctionDecl->params.size())
    return report(call.location, "argument count mismatch in function call");

  std::vector<std::unique_ptr<ResolvedExpr>> resolvedArguments;
  int idx = 0;
  for (auto &&arg : call.arguments) {
    varOrReturn(resolvedArg, resolveExpr(*arg));

    if (resolvedArg->type.name != resolvedFunctionDecl->params[idx]->type.name)
      return report(resolvedArg->location, "unexpected type of argument");

    resolvedArg->setConstantValue(cee.evaluate(*resolvedArg, false));

    ++idx;
    resolvedArguments.emplace_back(std::move(resolvedArg));
  }

  return std::make_unique<ResolvedCallExpr>(
      call.location, *resolvedFunctionDecl, std::move(resolvedArguments));
}

std::unique_ptr<ResolvedStructInstantiationExpr>
Sema::resolveStructInstantiation(
    const StructInstantiationExpr &structInstantiation) {
  const auto *st =
      lookupDecl<ResolvedStructDecl>(structInstantiation.identifier).first;

  if (!st)
    return report(structInstantiation.location,
                  "'" + structInstantiation.identifier +
                      "' is not a struct type");

  std::vector<std::unique_ptr<ResolvedMemberInitStmt>> resolvedMemberInits;
  std::map<std::string_view, const ResolvedMemberInitStmt *> inits;

  std::map<std::string_view, const ResolvedMemberDecl *> members;
  for (auto &&memberDecl : st->members)
    members[memberDecl->identifier] = memberDecl.get();

  bool error = false;
  for (auto &&initStmt : structInstantiation.memberInitializers) {
    std::string_view id = initStmt->identifier;
    const SourceLocation &loc = initStmt->location;

    if (inits.count(id)) {
      report(loc, "field '" + std::string{id} + "' is already initialized");
      error = true;
      continue;
    }

    const ResolvedMemberDecl *memberDecl = members[id];
    if (!memberDecl) {
      report(loc, "'" + st->identifier + "' has no field named '" +
                      std::string{id} + "'");
      error = true;
      continue;
    }

    auto resolvedInitExpr = resolveExpr(*initStmt->initializer);
    if (!resolvedInitExpr) {
      error = true;
      continue;
    }

    if (resolvedInitExpr->type.name != memberDecl->type.name) {
      report(resolvedInitExpr->location,
             "'" + resolvedInitExpr->type.name +
                 "' cannot be used to initialize a member of type '" +
                 memberDecl->type.name + "'");
      error = true;
      continue;
    }

    auto init = std::make_unique<ResolvedMemberInitStmt>(
        loc, *memberDecl, std::move(resolvedInitExpr));
    inits[id] = resolvedMemberInits.emplace_back(std::move(init)).get();
  }

  for (auto &&memberDecl : st->members) {
    if (!inits.count(memberDecl->identifier)) {
      report(structInstantiation.location,
             "member '" + memberDecl->identifier + "' is not initialized");
      error = true;
      continue;
    }

    auto &initStmt = inits[memberDecl->identifier];
    initStmt->initializer->setConstantValue(
        cee.evaluate(*initStmt->initializer, false));
  }

  if (error)
    return nullptr;

  return std::make_unique<ResolvedStructInstantiationExpr>(
      structInstantiation.location, *st, std::move(resolvedMemberInits));
}

std::unique_ptr<ResolvedMemberExpr>
Sema::resolveMemberExpr(const MemberExpr &memberExpr) {
  auto resolvedBase = resolveExpr(*memberExpr.base);
  if (!resolvedBase)
    return nullptr;

  if (resolvedBase->type.kind != Type::Kind::Struct)
    return report(memberExpr.base->location,
                  "cannot access member of '" + resolvedBase->type.name + '\'');

  const auto *st =
      lookupDecl<ResolvedStructDecl>(resolvedBase->type.name).first;

  assert(st && "failed to lookup struct");

  const ResolvedMemberDecl *memberDecl = nullptr;
  for (auto &&member : st->members) {
    if (member->identifier == memberExpr.member)
      memberDecl = member.get();
  }

  if (!memberDecl)
    return report(memberExpr.location, '\'' + resolvedBase->type.name +
                                           "' has no member called '" +
                                           memberExpr.member + '\'');

  return std::make_unique<ResolvedMemberExpr>(
      memberExpr.location, std::move(resolvedBase), *memberDecl);
}

std::unique_ptr<ResolvedStmt> Sema::resolveStmt(const Stmt &stmt) {
  if (auto *expr = dynamic_cast<const Expr *>(&stmt))
    return resolveExpr(*expr);

  if (auto *ifStmt = dynamic_cast<const IfStmt *>(&stmt))
    return resolveIfStmt(*ifStmt);

  if (auto *assignment = dynamic_cast<const Assignment *>(&stmt))
    return resolveAssignment(*assignment);

  if (auto *declStmt = dynamic_cast<const DeclStmt *>(&stmt))
    return resolveDeclStmt(*declStmt);

  if (auto *whileStmt = dynamic_cast<const WhileStmt *>(&stmt))
    return resolveWhileStmt(*whileStmt);

  if (auto *returnStmt = dynamic_cast<const ReturnStmt *>(&stmt))
    return resolveReturnStmt(*returnStmt);

  llvm_unreachable("unexpected statement");
}

std::unique_ptr<ResolvedIfStmt> Sema::resolveIfStmt(const IfStmt &ifStmt) {
  varOrReturn(condition, resolveExpr(*ifStmt.condition));

  if (condition->type.kind != Type::Kind::Number)
    return report(condition->location, "expected number in condition");

  varOrReturn(resolvedTrueBlock, resolveBlock(*ifStmt.trueBlock));

  std::unique_ptr<ResolvedBlock> resolvedFalseBlock;
  if (ifStmt.falseBlock) {
    resolvedFalseBlock = resolveBlock(*ifStmt.falseBlock);
    if (!resolvedFalseBlock)
      return nullptr;
  }

  condition->setConstantValue(cee.evaluate(*condition, false));

  return std::make_unique<ResolvedIfStmt>(ifStmt.location, std::move(condition),
                                          std::move(resolvedTrueBlock),
                                          std::move(resolvedFalseBlock));
}

std::unique_ptr<ResolvedWhileStmt>
Sema::resolveWhileStmt(const WhileStmt &whileStmt) {
  varOrReturn(condition, resolveExpr(*whileStmt.condition));

  if (condition->type.kind != Type::Kind::Number)
    return report(condition->location, "expected number in condition");

  varOrReturn(body, resolveBlock(*whileStmt.body));

  condition->setConstantValue(cee.evaluate(*condition, false));

  return std::make_unique<ResolvedWhileStmt>(
      whileStmt.location, std::move(condition), std::move(body));
}

std::unique_ptr<ResolvedDeclStmt>
Sema::resolveDeclStmt(const DeclStmt &declStmt) {
  varOrReturn(resolvedVarDecl, resolveVarDecl(*declStmt.varDecl));

  if (!insertDeclToCurrentScope(*resolvedVarDecl))
    return nullptr;

  return std::make_unique<ResolvedDeclStmt>(declStmt.location,
                                            std::move(resolvedVarDecl));
}

std::unique_ptr<ResolvedAssignment>
Sema::resolveAssignment(const Assignment &assignment) {
  varOrReturn(resolvedRHS, resolveExpr(*assignment.expr));
  varOrReturn(resolvedLHS, resolveAssignableExpr(*assignment.assignee));

  assert(resolvedLHS->type.kind != Type::Kind::Void &&
         "reference to void declaration in assignment LHS");

  if (resolvedRHS->type.name != resolvedLHS->type.name)
    return report(resolvedRHS->location,
                  "assigned value type doesn't match variable type");

  resolvedRHS->setConstantValue(cee.evaluate(*resolvedRHS, false));

  return std::make_unique<ResolvedAssignment>(
      assignment.location, std::move(resolvedLHS), std::move(resolvedRHS));
}

std::unique_ptr<ResolvedReturnStmt>
Sema::resolveReturnStmt(const ReturnStmt &returnStmt) {
  assert(currentFunction && "return stmt outside a function");

  if (currentFunction->type.kind == Type::Kind::Void && returnStmt.expr)
    return report(returnStmt.location,
                  "unexpected return value in void function");

  if (currentFunction->type.kind != Type::Kind::Void && !returnStmt.expr)
    return report(returnStmt.location, "expected a return value");

  std::unique_ptr<ResolvedExpr> resolvedExpr;
  if (returnStmt.expr) {
    resolvedExpr = resolveExpr(*returnStmt.expr);
    if (!resolvedExpr)
      return nullptr;

    if (currentFunction->type.name != resolvedExpr->type.name)
      return report(resolvedExpr->location, "unexpected return type");

    resolvedExpr->setConstantValue(cee.evaluate(*resolvedExpr, false));
  }

  return std::make_unique<ResolvedReturnStmt>(returnStmt.location,
                                              std::move(resolvedExpr));
}

std::unique_ptr<ResolvedAssignableExpr>
Sema::resolveAssignableExpr(const AssignableExpr &assignableExpr) {
  if (const auto *declRefExpr =
          dynamic_cast<const DeclRefExpr *>(&assignableExpr))
    return resolveDeclRefExpr(*declRefExpr);

  if (const auto *memberExpr =
          dynamic_cast<const MemberExpr *>(&assignableExpr))
    return resolveMemberExpr(*memberExpr);

  llvm_unreachable("unexpected assignable expression");
}

std::unique_ptr<ResolvedExpr> Sema::resolveExpr(const Expr &expr) {

  if (const auto *number = dynamic_cast<const NumberLiteral *>(&expr))
    return std::make_unique<ResolvedNumberLiteral>(number->location,
                                                   std::stod(number->value));

  if (const auto *callExpr = dynamic_cast<const CallExpr *>(&expr))
    return resolveCallExpr(*callExpr);

  if (const auto *groupingExpr = dynamic_cast<const GroupingExpr *>(&expr))
    return resolveGroupingExpr(*groupingExpr);

  if (const auto *binaryOperator = dynamic_cast<const BinaryOperator *>(&expr))
    return resolveBinaryOperator(*binaryOperator);

  if (const auto *unaryOperator = dynamic_cast<const UnaryOperator *>(&expr))
    return resolveUnaryOperator(*unaryOperator);

  if (const auto *structInstantiation =
          dynamic_cast<const StructInstantiationExpr *>(&expr))
    return resolveStructInstantiation(*structInstantiation);

  if (const auto *assignableExpr = dynamic_cast<const AssignableExpr *>(&expr))
    return resolveAssignableExpr(*assignableExpr);

  llvm_unreachable("unexpected expression");
}

std::unique_ptr<ResolvedBlock> Sema::resolveBlock(const Block &block) {
  std::vector<std::unique_ptr<ResolvedStmt>> resolvedStatements;

  bool error = false;
  int reportUnreachableCount = 0;

  ScopeRAII blockScope(this);
  for (auto &&stmt : block.statements) {
    auto resolvedStmt = resolveStmt(*stmt);

    error |= !resolvedStatements.emplace_back(std::move(resolvedStmt));
    if (error)
      continue;

    if (reportUnreachableCount == 1) {
      report(stmt->location, "unreachable statement", true);
      ++reportUnreachableCount;
    }

    if (dynamic_cast<ReturnStmt *>(stmt.get()))
      ++reportUnreachableCount;
  }

  if (error)
    return nullptr;

  return std::make_unique<ResolvedBlock>(block.location,
                                         std::move(resolvedStatements));
}

std::unique_ptr<ResolvedParamDecl>
Sema::resolveParamDecl(const ParamDecl &param) {
  std::optional<Type> type = resolveType(param.type);

  if (!type || type->kind == Type::Kind::Void)
    return report(param.location, "parameter '" + param.identifier +
                                      "' has invalid '" + param.type.name +
                                      "' type");

  return std::make_unique<ResolvedParamDecl>(param.location, param.identifier,
                                             *type, param.isMutable);
}

std::unique_ptr<ResolvedVarDecl> Sema::resolveVarDecl(const VarDecl &varDecl) {
  if (!varDecl.type && !varDecl.initializer)
    return report(
        varDecl.location,
        "an uninitialized variable is expected to have a type specifier");

  std::unique_ptr<ResolvedExpr> resolvedInitializer = nullptr;
  if (varDecl.initializer) {
    resolvedInitializer = resolveExpr(*varDecl.initializer);
    if (!resolvedInitializer)
      return nullptr;
  }

  Type resolvableType = varDecl.type.value_or(resolvedInitializer->type);
  std::optional<Type> type = resolveType(resolvableType);

  if (!type || type->kind == Type::Kind::Void)
    return report(varDecl.location, "variable '" + varDecl.identifier +
                                        "' has invalid '" +
                                        resolvableType.name + "' type");

  if (resolvedInitializer) {
    if (resolvedInitializer->type.name != type->name)
      return report(resolvedInitializer->location, "initializer type mismatch");

    resolvedInitializer->setConstantValue(
        cee.evaluate(*resolvedInitializer, false));
  }

  return std::make_unique<ResolvedVarDecl>(varDecl.location, varDecl.identifier,
                                           *type, varDecl.isMutable,
                                           std::move(resolvedInitializer));
}

std::unique_ptr<ResolvedFunctionDecl>
Sema::resolveFunctionDecl(const FunctionDecl &function) {
  std::optional<Type> type = resolveType(function.type);

  if (!type)
    return report(function.location, "function '" + function.identifier +
                                         "' has invalid '" +
                                         function.type.name + "' type");

  if (function.identifier == "main") {
    if (type->kind != Type::Kind::Void)
      return report(function.location,
                    "'main' function is expected to have 'void' type");

    if (!function.params.empty())
      return report(function.location,
                    "'main' function is expected to take no arguments");
  }

  std::vector<std::unique_ptr<ResolvedParamDecl>> resolvedParams;

  ScopeRAII paramScope(this);
  for (auto &&param : function.params) {
    auto resolvedParam = resolveParamDecl(*param);

    if (!resolvedParam || !insertDeclToCurrentScope(*resolvedParam))
      return nullptr;

    resolvedParams.emplace_back(std::move(resolvedParam));
  }

  return std::make_unique<ResolvedFunctionDecl>(
      function.location, function.identifier, *type, std::move(resolvedParams),
      nullptr);
};

std::unique_ptr<ResolvedStructDecl>
Sema::resolveStructDecl(const StructDecl &structDecl) {
  std::set<std::string_view> identifiers;
  std::vector<std::unique_ptr<ResolvedMemberDecl>> resolvedMembers;

  unsigned idx = 0;
  for (auto &&member : structDecl.members) {
    if (!identifiers.emplace(member->identifier).second)
      return report(member->location,
                    "field '" + member->identifier + "' is already declared");

    resolvedMembers.emplace_back(std::make_unique<ResolvedMemberDecl>(
        member->location, member->identifier, member->type, idx++));
  }

  return std::make_unique<ResolvedStructDecl>(
      structDecl.location, structDecl.identifier,
      Type::structType(structDecl.identifier), std::move(resolvedMembers));
}

bool Sema::resolveStructMembers(ResolvedStructDecl &resolvedStructDecl) {
  std::stack<
      std::pair<ResolvedStructDecl *, std::set<const ResolvedStructDecl *>>>
      worklist;
  worklist.push({&resolvedStructDecl, {}});

  while (!worklist.empty()) {
    auto [currentDecl, visited] = worklist.top();
    worklist.pop();

    if (!visited.emplace(currentDecl).second) {
      report(currentDecl->location,
             "struct '" + currentDecl->identifier + "' contains itself");
      return false;
    }

    for (auto &&member : currentDecl->members) {
      auto type = resolveType(member->type);
      if (!type) {
        report(member->location, "unable to resolve '" + member->type.name +
                                     "' type of struct member");
        return false;
      }

      if (type->kind == Type::Kind::Void) {
        report(member->location, "struct member cannot be void");
        return false;
      }

      if (type->kind == Type::Kind::Struct) {
        auto *nestedStruct = lookupDecl<ResolvedStructDecl>(type->name).first;
        assert(nestedStruct && "unexpected type");

        worklist.push({nestedStruct, visited});
      }

      member->type = *type;
    }
  }

  return true;
}

std::vector<std::unique_ptr<ResolvedDecl>> Sema::resolveAST() {
  ScopeRAII globalScope(this);
  std::vector<std::unique_ptr<ResolvedDecl>> resolvedTree;

  bool error = false;
  std::vector<const FunctionDecl *> functionsToResolve;

  // Resolve every struct first so that functions have access to them in their
  // signature.
  for (auto &&decl : ast) {
    if (const auto *st = dynamic_cast<const StructDecl *>(decl.get())) {
      std::unique_ptr<ResolvedDecl> resolvedDecl = resolveStructDecl(*st);

      if (!resolvedDecl || !insertDeclToCurrentScope(*resolvedDecl)) {
        error = true;
        continue;
      }

      resolvedTree.emplace_back(std::move(resolvedDecl));
      continue;
    }

    if (const auto *fn = dynamic_cast<const FunctionDecl *>(decl.get())) {
      functionsToResolve.emplace_back(fn);
      continue;
    }

    llvm_unreachable("unexpected declaration");
  }

  if (error)
    return {};

  // Insert println first to be able to detect a possible redeclaration.
  auto *printlnDecl = resolvedTree.emplace_back(createBuiltinPrintln()).get();
  insertDeclToCurrentScope(*printlnDecl);

  for (auto &&fn : functionsToResolve) {
    if (auto resolvedDecl = resolveFunctionDecl(*fn);
        resolvedDecl && insertDeclToCurrentScope(*resolvedDecl)) {
      resolvedTree.emplace_back(std::move(resolvedDecl));
      continue;
    }

    error = true;
  }

  if (error)
    return {};

  auto nextFunctionDecl = functionsToResolve.begin();
  for (auto &&currentDecl : resolvedTree) {
    if (auto *st = dynamic_cast<ResolvedStructDecl *>(currentDecl.get())) {
      if (!resolveStructMembers(*st))
        error = true;

      continue;
    }

    if (auto *fn = dynamic_cast<ResolvedFunctionDecl *>(currentDecl.get())) {
      if (fn == printlnDecl)
        continue;

      ScopeRAII paramScope(this);
      for (auto &&param : fn->params)
        insertDeclToCurrentScope(*param);

      currentFunction = fn;
      if (auto resolvedBody = resolveBlock(*(*nextFunctionDecl++)->body)) {
        fn->body = std::move(resolvedBody);
        error |= runFlowSensitiveChecks(*fn);
        continue;
      }

      error = true;
    }
  }

  if (error)
    return {};

  return resolvedTree;
}
} // namespace yl
