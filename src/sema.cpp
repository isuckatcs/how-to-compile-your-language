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

  using Lattice = std::map<const ResolvedVarDecl *, State>;

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

          const auto *var = dynamic_cast<const ResolvedVarDecl *>(
              dynamic_cast<const ResolvedDeclRefExpr *>(base)->decl);

          assert(var &&
                 "assignment to non-variables should have been caught by sema");

          if (!var->isMutable && tmp[var] != State::Unassigned) {
            std::string msg = '\'' + var->identifier + "' cannot be mutated";
            pendingErrors.emplace_back(assignment->location, std::move(msg));
          }

          tmp[var] = State::Assigned;
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

      if (!correctDecl || decl->identifier != id)
        continue;

      return {correctDecl, scopeIdx};
    }

    ++scopeIdx;
  }

  return {nullptr, -1};
}

std::unique_ptr<ResolvedFunctionDecl> Sema::createBuiltinPrintln() {
  SourceLocation loc{"<builtin>", 0, 0};

  auto param =
      std::make_unique<ResolvedParamDecl>(loc, "n", Type::builtinNumber());

  std::vector<std::unique_ptr<ResolvedParamDecl>> params;
  params.emplace_back(std::move(param));

  auto block = std::make_unique<ResolvedBlock>(
      loc, std::vector<std::unique_ptr<ResolvedStmt>>());

  return std::make_unique<ResolvedFunctionDecl>(
      loc, "println", Type::builtinVoid(), std::move(params), std::move(block));
};

std::optional<Type> Sema::resolveType(Type parsedType) {
  if (parsedType.kind == Type::Kind::Custom) {
    auto *decl = lookupDecl<ResolvedDecl>(parsedType.name).first;
    if (dynamic_cast<ResolvedStructDecl *>(decl))
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

// FIXME: refactor
std::unique_ptr<ResolvedStructInstantiationExpr>
Sema::resolveStructInstantiation(
    const StructInstantiationExpr &structInstantiation) {
  const auto *st =
      lookupDecl<ResolvedStructDecl>(structInstantiation.identifier).first;

  if (!st)
    return report(structInstantiation.location,
                  "'" + structInstantiation.identifier +
                      "' is not a struct type");

  std::vector<std::unique_ptr<ResolvedMemberInitStmt>>
      resolvedMemberInitializers;

  bool error = false;
  for (auto &&memberDecl : st->members) {
    bool found = false;
    for (auto &&memberInitializer : structInstantiation.memberInitializers) {
      if (memberInitializer->identifier != memberDecl->identifier)
        continue;

      bool alreadyInit = false;
      for (auto &&alreadyResolved : resolvedMemberInitializers) {
        if (alreadyResolved->member->identifier ==
            memberInitializer->identifier) {
          alreadyInit = true;
          error = true;
          report(memberInitializer->location,
                 "field '" + memberInitializer->identifier +
                     "' is already initialized");
        }
      }

      if (alreadyInit)
        continue;

      found = true;
      auto resolvedInitExpr = resolveExpr(*memberInitializer->initializer);

      if (!resolvedInitExpr) {
        error = true;
        continue;
      }

      if (resolvedInitExpr->type.name != memberDecl->type.name) {
        error = true;
        report(resolvedInitExpr->location,
               "'" + resolvedInitExpr->type.name +
                   "' cannot be used to initialize a member of type '" +
                   memberDecl->type.name + "'");
      }

      resolvedMemberInitializers.emplace_back(
          std::make_unique<ResolvedMemberInitStmt>(
              memberInitializer->location, *memberDecl,
              std::move(resolvedInitExpr)));
    }

    if (!found) {
      report(structInstantiation.location,
             "member '" + memberDecl->identifier + "' is not initialized");
      error = true;
    }
  }

  for (auto &&memberInitializer : structInstantiation.memberInitializers) {
    bool found = false;
    for (auto &&memberDecl : st->members)
      found |= memberDecl->identifier == memberInitializer->identifier;

    if (!found) {
      error = true;
      report(memberInitializer->location,
             '\'' + st->identifier + "' has no field named '" +
                 memberInitializer->identifier + '\'');
    }
  }

  if (error)
    return nullptr;

  return std::make_unique<ResolvedStructInstantiationExpr>(
      structInstantiation.location, *st, std::move(resolvedMemberInitializers));
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
  varOrReturn(resolvedLHS, resolveAssignableExpr(*assignment.assignee));
  varOrReturn(resolvedRHS, resolveExpr(*assignment.expr));

  assert(resolvedLHS->type.kind != Type::Kind::Void &&
         "reference to void declaration in assignment LHS");

  if (auto *dre = dynamic_cast<const ResolvedDeclRefExpr *>(resolvedLHS.get());
      dre && dynamic_cast<const ResolvedParamDecl *>(dre->decl))
    return report(resolvedLHS->location,
                  "parameters are immutable and cannot be assigned");

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
                                             *type);
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
Sema::resolveFunctionDeclaration(const FunctionDecl &function) {
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

std::unique_ptr<ResolvedMemberDecl>
Sema::resolveKnownMemberDecl(const MemberDecl &member) {
  Type currentType = member.type;
  auto resolvedType = currentType.kind == Type::Kind::Custom
                          ? currentType
                          : resolveType(member.type);

  assert(resolvedType && "member type unknown");

  if (resolvedType->kind == Type::Kind::Void)
    return report(member.location, "struct member cannot be void");

  return std::make_unique<ResolvedMemberDecl>(
      member.location, member.identifier, resolvedType.value_or(member.type));
}

std::unique_ptr<ResolvedStructDecl>
Sema::resolveStructDecl(const StructDecl &structDecl) {
  std::vector<std::unique_ptr<ResolvedMemberDecl>> resolvedMembers;

  for (auto &&member : structDecl.members) {
    for (auto &&alreadyResolved : resolvedMembers)
      if (alreadyResolved->identifier == member->identifier)
        return report(member->location,
                      "field '" + member->identifier + "' is already declared");

    auto resolvedMember = resolveKnownMemberDecl(*member);

    if (!resolvedMember)
      return nullptr;

    resolvedMembers.emplace_back(std::move(resolvedMember));
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
      if (member->type.kind != Type::Kind::Custom)
        continue;

      auto type = resolveType(member->type);
      if (!type) {
        report(member->location, "unable to resolve '" + member->type.name +
                                     "' type of struct member");
        return false;
      }

      auto *nestedStruct = lookupDecl<ResolvedStructDecl>(type->name).first;
      assert(nestedStruct && "unexpected type");

      member->type = *type;
      worklist.push({nestedStruct, visited});
    }
  }

  return true;
}

std::vector<std::unique_ptr<ResolvedDecl>> Sema::resolveAST() {
  std::vector<std::unique_ptr<ResolvedDecl>> resolvedTree;
  auto println = createBuiltinPrintln();

  // Insert println first to be able to detect a possible redeclaration.
  ScopeRAII globalScope(this);
  insertDeclToCurrentScope(*resolvedTree.emplace_back(std::move(println)));

  bool error = false;
  for (auto &&decl : ast) {
    std::unique_ptr<ResolvedDecl> resolvedDecl;

    if (const auto *fn = dynamic_cast<const FunctionDecl *>(decl.get())) {
      resolvedDecl = resolveFunctionDeclaration(*fn);
    } else if (const auto *st = dynamic_cast<const StructDecl *>(decl.get())) {
      resolvedDecl = resolveStructDecl(*st);
    } else {
      llvm_unreachable("unexpected declaration");
    }

    if (!resolvedDecl || !insertDeclToCurrentScope(*resolvedDecl)) {
      error = true;
      continue;
    }

    resolvedTree.emplace_back(std::move(resolvedDecl));
  }

  if (error)
    return {};

  // FIXME: think about a better solution
  for (size_t i = 1; i < resolvedTree.size(); ++i) {
    ResolvedDecl *currentDecl = resolvedTree[i].get();

    if (auto *st = dynamic_cast<ResolvedStructDecl *>(currentDecl)) {
      if (!resolveStructMembers(*st))
        return {};
    }
  }

  for (size_t i = 1; i < resolvedTree.size(); ++i) {
    ResolvedDecl *currentDecl = resolvedTree[i].get();

    if (!dynamic_cast<ResolvedFunctionDecl *>(currentDecl))
      continue;

    currentFunction =
        static_cast<ResolvedFunctionDecl *>(resolvedTree[i].get());

    ScopeRAII paramScope(this);
    for (auto &&param : currentFunction->params)
      insertDeclToCurrentScope(*param);

    auto resolvedBody =
        resolveBlock(*static_cast<FunctionDecl *>(ast[i - 1].get())->body);
    if (!resolvedBody) {
      error = true;
      continue;
    }

    currentFunction->body = std::move(resolvedBody);
    error |= runFlowSensitiveChecks(*currentFunction);
  }

  if (error)
    return {};

  return resolvedTree;
}
} // namespace yl
