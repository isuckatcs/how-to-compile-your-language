#include <cassert>
#include <map>
#include <set>

#include "cfg.h"
#include "sema.h"
#include "utils.h"

namespace yl {
bool Sema::runFlowSensitiveChecks(const ResolvedFunctionDecl &fn) {
  CFG cfg = CFGBuilder().build(fn);

  bool error = false;
  error |= checkReturnOnAllPaths(fn, cfg);
  error |= checkVariableInitialization(fn, cfg);

  return error;
};

bool Sema::checkReturnOnAllPaths(const ResolvedFunctionDecl &fn,
                                 const CFG &cfg) {
  if (fn.type == Type::Void)
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

bool Sema::checkVariableInitialization(const ResolvedFunctionDecl &fn,
                                       const CFG &cfg) {
  enum class State { Bottom, Unassigned, Assigned, Top };

  auto joinStates = [](State s1, State s2) {
    if (s1 == s2)
      return s1;

    if (s1 == State::Bottom)
      return s2;

    if (s2 == State::Bottom)
      return s1;

    return State::Top;
  };

  using Lattice = std::map<const ResolvedVarDecl *, State>;

  std::vector<Lattice> curLattices(cfg.basicBlocks.size());
  std::vector<std::pair<SourceLocation, std::string>> pendingErrors;

  bool changed = true;
  while (changed) {
    changed = false;

    for (int bb = cfg.entry; bb != cfg.exit; --bb) {
      const auto &[preds, succs, stmts] = cfg.basicBlocks[bb];

      Lattice tmp;
      for (auto &&pred : preds)
        for (auto &&[decl, state] : curLattices[pred.first])
          tmp[decl] = joinStates(tmp[decl], state);

      for (auto it = stmts.rbegin(); it != stmts.rend(); ++it) {
        const ResolvedStmt *stmt = *it;

        if (const auto *declStmt =
                dynamic_cast<const ResolvedDeclStmt *>(stmt)) {
          tmp[declStmt->varDecl.get()] = declStmt->varDecl->initializer
                                             ? State::Assigned
                                             : State::Unassigned;
        } else if (const auto *assignment =
                       dynamic_cast<const ResolvedAssignment *>(stmt)) {
          const auto *varDecl =
              dynamic_cast<const ResolvedVarDecl *>(assignment->variable->decl);

          assert(varDecl &&
                 "assignment to non-variables should have been caught by sema");

          if (!varDecl->isMutable && tmp[varDecl] != State::Unassigned)
            pendingErrors.emplace_back(assignment->location,
                                       '\'' + varDecl->identifier +
                                           "' cannot be mutated");

          tmp[varDecl] = State::Assigned;
        } else if (const auto *declRefExpr =
                       dynamic_cast<const ResolvedDeclRefExpr *>(stmt)) {
          const auto *varDecl =
              dynamic_cast<const ResolvedVarDecl *>(declRefExpr->decl);

          if (varDecl && tmp[varDecl] != State::Assigned)
            pendingErrors.emplace_back(declRefExpr->location,
                                       '\'' + varDecl->identifier +
                                           "' is not initialized");
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
  const auto &[foundDecl, scopeIdx] = lookupDecl(decl.identifier);

  if (foundDecl && scopeIdx == 0) {
    report(decl.location, "redeclaration of '" + decl.identifier + '\'');
    return false;
  }

  scopes.back().emplace_back(&decl);
  return true;
}

std::pair<ResolvedDecl *, int> Sema::lookupDecl(const std::string id) {
  int scopeIdx = 0;
  for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
    for (auto &&decl : *it) {
      if (decl->identifier != id)
        continue;

      return {decl, scopeIdx};
    }

    ++scopeIdx;
  }

  return {nullptr, -1};
}

std::unique_ptr<ResolvedFunctionDecl> Sema::createBuiltinPrintln() {
  SourceLocation loc = SourceLocation{"<builtin>", 0, 0};

  auto param = std::make_unique<ResolvedParamDecl>(loc, "n", Type::Number);

  std::vector<std::unique_ptr<ResolvedParamDecl>> params;
  params.emplace_back(std::move(param));

  auto block = std::make_unique<ResolvedBlock>(
      loc, std::vector<std::unique_ptr<ResolvedStmt>>());

  return std::make_unique<ResolvedFunctionDecl>(
      loc, "println", Type::Void, std::move(params), std::move(block));
};

std::optional<Type> Sema::resolveType(const std::string &typeSpecifier) {
  if (typeSpecifier == "void")
    return Type::Void;

  if (typeSpecifier == "number")
    return Type::Number;

  return std::nullopt;
}

std::unique_ptr<ResolvedUnaryOperator>
Sema::resolveUnaryOperator(const UnaryOperator &unary) {
  varOrReturn(resolvedRHS, resolveExpr(*unary.rhs));

  if (resolvedRHS->type == Type::Void)
    return report(
        resolvedRHS->location,
        "void expression cannot be used as operand to unary operator");

  return std::make_unique<ResolvedUnaryOperator>(
      unary.location, std::move(resolvedRHS), unary.op);
}

std::unique_ptr<ResolvedBinaryOperator>
Sema::resolveBinaryOperator(const BinaryOperator &binop) {
  varOrReturn(resolvedLHS, resolveExpr(*binop.lhs));
  varOrReturn(resolvedRHS, resolveExpr(*binop.rhs));

  if (resolvedLHS->type == Type::Void)
    return report(
        resolvedLHS->location,
        "void expression cannot be used as LHS operand to binary operator");

  if (resolvedRHS->type == Type::Void)
    return report(
        resolvedRHS->location,
        "void expression cannot be used as RHS operand to binary operator");

  assert(resolvedLHS->type == resolvedRHS->type &&
         resolvedLHS->type == Type::Number && "unexpexted type in binop");

  return std::make_unique<ResolvedBinaryOperator>(
      binop.location, std::move(resolvedLHS), std::move(resolvedRHS), binop.op);
}

std::unique_ptr<ResolvedGroupingExpr>
Sema::resolveGroupingExpr(const GroupingExpr &grouping) {
  varOrReturn(resolvedExpr, resolveExpr(*grouping.expr));
  return std::make_unique<ResolvedGroupingExpr>(grouping.location,
                                                std::move(resolvedExpr));
}

std::unique_ptr<ResolvedDeclRefExpr>
Sema::resolveDeclRefExpr(const DeclRefExpr &declRefExpr, bool inCall) {
  ResolvedDecl *decl = lookupDecl(declRefExpr.identifier).first;
  if (!decl)
    return report(declRefExpr.location,
                  "symbol '" + declRefExpr.identifier + "' not found");

  if (!inCall && dynamic_cast<ResolvedFunctionDecl *>(decl))
    return report(declRefExpr.location,
                  "expected to call function '" + declRefExpr.identifier + "'");

  return std::make_unique<ResolvedDeclRefExpr>(declRefExpr.location, *decl);
}

std::unique_ptr<ResolvedCallExpr> Sema::resolveCallExpr(const CallExpr &call) {
  varOrReturn(resolvedCallee, resolveDeclRefExpr(*call.identifier, true));

  const auto *resolvedFunctionDecl =
      dynamic_cast<const ResolvedFunctionDecl *>(resolvedCallee->decl);

  if (!resolvedFunctionDecl)
    return report(call.location, "calling non-function symbol");

  if (call.arguments.size() != resolvedFunctionDecl->params.size())
    return report(call.location, "argument count missmatch in function call");

  std::vector<std::unique_ptr<ResolvedExpr>> resolvedArguments;
  int idx = 0;
  for (auto &&arg : call.arguments) {
    varOrReturn(resolvedArg, resolveExpr(*arg));

    if (resolvedArg->type != resolvedFunctionDecl->params[idx]->type)
      return report(resolvedArg->location, "unexpected type of argument");

    if (std::optional<double> val = cee.evaluate(*resolvedArg))
      resolvedArg->setConstantValue(val);

    ++idx;
    resolvedArguments.emplace_back(std::move(resolvedArg));
  }

  return std::make_unique<ResolvedCallExpr>(
      call.location, *resolvedFunctionDecl, std::move(resolvedArguments));
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

  auto *returnStmt = dynamic_cast<const ReturnStmt *>(&stmt);
  assert(returnStmt && "unknown statement");

  return resolveReturnStmt(*returnStmt);
}

std::unique_ptr<ResolvedIfStmt> Sema::resolveIfStmt(const IfStmt &ifStmt) {
  varOrReturn(condition, resolveExpr(*ifStmt.condition));

  if (condition->type != Type::Number)
    return report(condition->location, "expected number in condition");

  varOrReturn(trueBlock, resolveBlock(*ifStmt.trueBlock));

  std::unique_ptr<ResolvedBlock> resolvedFalseBlock;
  if (ifStmt.falseBlock) {
    resolvedFalseBlock = resolveBlock(*ifStmt.falseBlock);
    if (!resolvedFalseBlock)
      return nullptr;
  }

  return std::make_unique<ResolvedIfStmt>(ifStmt.location, std::move(condition),
                                          std::move(trueBlock),
                                          std::move(resolvedFalseBlock));
}

std::unique_ptr<ResolvedWhileStmt>
Sema::resolveWhileStmt(const WhileStmt &whileStmt) {
  varOrReturn(condition, resolveExpr(*whileStmt.condition));

  if (condition->type != Type::Number)
    return report(condition->location, "expected number in condition");

  varOrReturn(body, resolveBlock(*whileStmt.body));

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
  varOrReturn(resolvedLHS, resolveDeclRefExpr(*assignment.variable));
  varOrReturn(resolvedRHS, resolveExpr(*assignment.expr));

  assert(resolvedLHS->type != Type::Void &&
         "reference to void declaration in assignment LHS");

  if (dynamic_cast<const ResolvedParamDecl *>(resolvedLHS->decl))
    return report(resolvedLHS->location,
                  "parameters are immutable and cannot be assigned");

  auto *var = dynamic_cast<const ResolvedVarDecl *>(resolvedLHS->decl);
  assert(var && "assignment LHS is not a variable");

  if (resolvedRHS->type != resolvedLHS->type)
    return report(resolvedRHS->location,
                  "assigned value type doesn't match variable type");

  return std::make_unique<ResolvedAssignment>(
      assignment.location, std::move(resolvedLHS), std::move(resolvedRHS));
}

std::unique_ptr<ResolvedReturnStmt>
Sema::resolveReturnStmt(const ReturnStmt &returnStmt) {
  assert(currentFunction && "return stmt outside a function");

  if (currentFunction->type == Type::Void && returnStmt.expr)
    return report(returnStmt.location,
                  "unexpected return value in void function");

  if (currentFunction->type != Type::Void && !returnStmt.expr)
    return report(returnStmt.location, "expected a return value");

  std::unique_ptr<ResolvedExpr> resolvedExpr;
  if (returnStmt.expr) {
    resolvedExpr = resolveExpr(*returnStmt.expr);
    if (!resolvedExpr)
      return nullptr;

    if (currentFunction->type != resolvedExpr->type)
      return report(resolvedExpr->location, "unexpected return type");
  }

  return std::make_unique<ResolvedReturnStmt>(returnStmt.location,
                                              std::move(resolvedExpr));
}

std::unique_ptr<ResolvedExpr> Sema::resolveExpr(const Expr &expr) {

  std::unique_ptr<ResolvedExpr> resolvedExpr = nullptr;

  if (const auto *numberLiteral = dynamic_cast<const NumberLiteral *>(&expr))
    resolvedExpr = std::make_unique<ResolvedNumberLiteral>(
        numberLiteral->location, std::stod(numberLiteral->value));
  else if (const auto *declRefExpr = dynamic_cast<const DeclRefExpr *>(&expr))
    resolvedExpr = resolveDeclRefExpr(*declRefExpr);
  else if (const auto *callExpr = dynamic_cast<const CallExpr *>(&expr))
    resolvedExpr = resolveCallExpr(*callExpr);
  else if (const auto *groupingExpr = dynamic_cast<const GroupingExpr *>(&expr))
    resolvedExpr = resolveGroupingExpr(*groupingExpr);
  else if (const auto *binaryOperator =
               dynamic_cast<const BinaryOperator *>(&expr))
    resolvedExpr = resolveBinaryOperator(*binaryOperator);
  else {
    const auto *unaryOperator = dynamic_cast<const UnaryOperator *>(&expr);
    assert(unaryOperator && "unexpected expression");
    resolvedExpr = resolveUnaryOperator(*unaryOperator);
  }

  if (!resolvedExpr)
    return nullptr;

  if (std::optional<double> val = cee.evaluate(*resolvedExpr))
    resolvedExpr->setConstantValue(val);

  return resolvedExpr;
}

std::unique_ptr<ResolvedBlock> Sema::resolveBlock(const Block &block) {
  ScopeRAII blockScope{this};
  std::vector<std::unique_ptr<ResolvedStmt>> resolvedStatements;

  bool error = false;
  int reportUnreachableCount = 0;

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

  if (!type || type == Type::Void)
    return report(param.location, "parameter '" + param.identifier +
                                      "' has invalid '" + param.type +
                                      "' type");

  return std::make_unique<ResolvedParamDecl>(param.location, param.identifier,
                                             *type);
}

std::unique_ptr<ResolvedVarDecl> Sema::resolveVarDecl(const VarDecl &varDecl) {
  if (!varDecl.type && !varDecl.initialzer)
    return report(
        varDecl.location,
        "an uninitialized variable is expected to have a type specifier");

  std::unique_ptr<ResolvedExpr> resolvedInitializer = nullptr;
  if (varDecl.initialzer) {
    resolvedInitializer = resolveExpr(*varDecl.initialzer);
    if (!resolvedInitializer)
      return nullptr;
  }

  std::optional<Type> type =
      varDecl.type ? resolveType(*varDecl.type) : resolvedInitializer->type;

  if (!type || type == Type::Void)
    return report(varDecl.location,
                  "variable '" + varDecl.identifier + "' has invalid '" +
                      varDecl.type.value_or("void") + "' type");

  if (resolvedInitializer && resolvedInitializer->type != type)
    return report(resolvedInitializer->location, "initializer type mismatch");

  return std::make_unique<ResolvedVarDecl>(varDecl.location, varDecl.identifier,
                                           *type, varDecl.isMutable,
                                           std::move(resolvedInitializer));
}

std::unique_ptr<ResolvedFunctionDecl>
Sema::resolveFunctionDeclaration(const FunctionDecl &function) {
  ScopeRAII paramScope{this};
  std::optional<Type> type = resolveType(function.type);

  if (!type)
    return report(function.location, "function '" + function.identifier +
                                         "' has invalid '" + function.type +
                                         "' type");

  if (function.identifier == "main") {
    if (type != Type::Void)
      return report(function.location,
                    "'main' function is expected to have 'void' type");

    if (!function.params.empty())
      return report(function.location,
                    "'main' function is expected to take no arguments");
  }

  std::vector<std::unique_ptr<ResolvedParamDecl>> resolvedParams;
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

std::vector<std::unique_ptr<ResolvedFunctionDecl>> Sema::resolveAST() {
  ScopeRAII globalScope{this};
  std::vector<std::unique_ptr<ResolvedFunctionDecl>> resolvedTree;

  // Insert print first to be able to detect possible redeclarations.
  auto println = createBuiltinPrintln();
  insertDeclToCurrentScope(*resolvedTree.emplace_back(std::move(println)));

  bool error = false;
  for (auto &&fn : ast) {
    auto resolvedFunctionDecl = resolveFunctionDeclaration(*fn);

    if (!resolvedFunctionDecl ||
        !insertDeclToCurrentScope(*resolvedFunctionDecl)) {
      error = true;
      continue;
    }

    resolvedTree.emplace_back(std::move(resolvedFunctionDecl));
  }

  if (error)
    return {};

  for (size_t i = 1; i < resolvedTree.size(); ++i) {
    ScopeRAII scope{this};
    currentFunction = resolvedTree[i].get();

    for (auto &&param : currentFunction->params)
      insertDeclToCurrentScope(*param);

    auto resolvedBody = resolveBlock(*ast[i - 1]->body);
    if (!resolvedBody) {
      error = true;
      continue;
    }

    currentFunction->body = std::move(resolvedBody);
    error |= runFlowSensitiveChecks(*currentFunction);
  }

  if (error)
    return {};

  return std::move(resolvedTree);
}
} // namespace yl
