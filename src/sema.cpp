#include <cassert>
#include <map>
#include <set>
#include <stack>

#include "cfg.h"
#include "sema.h"
#include "utils.h"

namespace yl {
bool Sema::runFlowSensitiveChecks(const res::FunctionDecl &fn) {
  CFG cfg = CFGBuilder().build(fn);

  bool error = false;
  error |= checkReturnOnAllPaths(fn, cfg);
  error |= checkVariableInitialization(cfg);

  return error;
};

bool Sema::checkReturnOnAllPaths(const res::FunctionDecl &fn, const CFG &cfg) {
  if (fn.type.kind == res::Type::Kind::Void)
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
          tmp[decl->varDecl.get()] =
              decl->varDecl->initializer ? State::Assigned : State::Unassigned;
          continue;
        }

        if (auto *assignment = dynamic_cast<const res::Assignment *>(stmt)) {
          const res::Expr *base = assignment->assignee.get();
          while (const auto *member =
                     dynamic_cast<const res::MemberExpr *>(base))
            base = member->base.get();

          const auto *dre = dynamic_cast<const res::DeclRefExpr *>(base);

          // The base of the expression is not a variable, but a temporary,
          // which can be mutated.
          if (!dre)
            continue;

          const auto *decl = dynamic_cast<const res::Decl *>(dre->decl);

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

bool Sema::insertDeclToCurrentScope(res::Decl &decl) {
  const auto &[foundDecl, scopeIdx] = lookupDecl<res::Decl>(decl.identifier);

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

std::unique_ptr<res::FunctionDecl> Sema::createBuiltinPrintln() {
  SourceLocation loc{nullptr, 0, 0};

  auto param = std::make_unique<res::ParamDecl>(
      loc, "n", res::Type::builtinNumber(), false);

  std::vector<std::unique_ptr<res::ParamDecl>> params;
  params.emplace_back(std::move(param));

  auto block = std::make_unique<res::Block>(
      loc, std::vector<std::unique_ptr<res::Stmt>>());

  return std::make_unique<res::FunctionDecl>(
      loc, "println", res::Type::builtinVoid(), std::move(params),
      std::move(block));
};

std::optional<res::Type> Sema::resolveType(ast::Type parsedType) {
  // FIXME: refactor type system
  const std::string_view typeName = parsedType.name;

  if (typeName == "number")
    return res::Type::builtinNumber();

  if (typeName == "void")
    return res::Type::builtinVoid();

  if (auto *decl = lookupDecl<res::StructDecl>(parsedType.name).first)
    return res::Type::structType(decl->identifier);

  return std::nullopt;
}

std::unique_ptr<res::UnaryOperator>
Sema::resolveUnaryOperator(const ast::UnaryOperator &unary) {
  varOrReturn(resolvedRHS, resolveExpr(*unary.operand));

  if (resolvedRHS->type.kind != res::Type::Kind::Number)
    return report(unary.location,
                  '\'' + resolvedRHS->type.name +
                      "' cannot be used as an operand to unary operator");

  return std::make_unique<res::UnaryOperator>(unary.location, unary.op,
                                              std::move(resolvedRHS));
}

std::unique_ptr<res::BinaryOperator>
Sema::resolveBinaryOperator(const ast::BinaryOperator &binop) {
  varOrReturn(resolvedLHS, resolveExpr(*binop.lhs));
  varOrReturn(resolvedRHS, resolveExpr(*binop.rhs));

  if (resolvedLHS->type.kind != res::Type::Kind::Number)
    return report(binop.location,
                  '\'' + resolvedLHS->type.name +
                      "' cannot be used as LHS operand to binary operator");

  if (resolvedRHS->type.kind != res::Type::Kind::Number)
    return report(binop.location,
                  '\'' + resolvedRHS->type.name +
                      "' cannot be used as RHS operand to binary operator");

  assert(resolvedLHS->type.kind == resolvedRHS->type.kind &&
         resolvedLHS->type.kind == res::Type::Kind::Number &&
         "unexpected type in binop");

  return std::make_unique<res::BinaryOperator>(
      binop.location, binop.op, std::move(resolvedLHS), std::move(resolvedRHS));
}

std::unique_ptr<res::GroupingExpr>
Sema::resolveGroupingExpr(const ast::GroupingExpr &grouping) {
  varOrReturn(resolvedExpr, resolveExpr(*grouping.expr));
  return std::make_unique<res::GroupingExpr>(grouping.location,
                                             std::move(resolvedExpr));
}

std::unique_ptr<res::DeclRefExpr>
Sema::resolveDeclRefExpr(const ast::DeclRefExpr &declRefExpr, bool isCallee) {
  res::Decl *decl = lookupDecl<res::Decl>(declRefExpr.identifier).first;
  if (!decl)
    return report(declRefExpr.location,
                  "symbol '" + declRefExpr.identifier + "' not found");

  if (!isCallee && dynamic_cast<res::FunctionDecl *>(decl))
    return report(declRefExpr.location,
                  "expected to call function '" + declRefExpr.identifier + "'");

  if (dynamic_cast<res::StructDecl *>(decl))
    return report(declRefExpr.location,
                  "expected an instance of '" + decl->type.name + '\'');

  return std::make_unique<res::DeclRefExpr>(declRefExpr.location, *decl);
}

std::unique_ptr<res::CallExpr>
Sema::resolveCallExpr(const ast::CallExpr &call) {
  const auto *dre = dynamic_cast<const ast::DeclRefExpr *>(call.callee.get());
  if (!dre)
    return report(call.location, "expression cannot be called as a function");

  varOrReturn(resolvedCallee, resolveDeclRefExpr(*dre, true));

  const auto *resolvedFunctionDecl =
      dynamic_cast<const res::FunctionDecl *>(resolvedCallee->decl);

  if (!resolvedFunctionDecl)
    return report(call.location, "calling non-function symbol");

  if (call.arguments.size() != resolvedFunctionDecl->params.size())
    return report(call.location, "argument count mismatch in function call");

  std::vector<std::unique_ptr<res::Expr>> resolvedArguments;
  int idx = 0;
  for (auto &&arg : call.arguments) {
    varOrReturn(resolvedArg, resolveExpr(*arg));

    if (resolvedArg->type.name != resolvedFunctionDecl->params[idx]->type.name)
      return report(resolvedArg->location, "unexpected type of argument");

    resolvedArg->setConstantValue(cee.evaluate(*resolvedArg, false));

    ++idx;
    resolvedArguments.emplace_back(std::move(resolvedArg));
  }

  return std::make_unique<res::CallExpr>(call.location, *resolvedFunctionDecl,
                                         std::move(resolvedArguments));
}

std::unique_ptr<res::StructInstantiationExpr> Sema::resolveStructInstantiation(
    const ast::StructInstantiationExpr &structInstantiation) {
  const auto *st =
      lookupDecl<res::StructDecl>(structInstantiation.identifier).first;

  if (!st)
    return report(structInstantiation.location,
                  "'" + structInstantiation.identifier +
                      "' is not a struct type");

  std::vector<std::unique_ptr<res::FieldInitStmt>> resolvedFieldInits;
  std::map<std::string_view, const res::FieldInitStmt *> inits;

  std::map<std::string_view, const res::FieldDecl *> fields;
  for (auto &&fieldDecl : st->fields)
    fields[fieldDecl->identifier] = fieldDecl.get();

  bool error = false;
  for (auto &&initStmt : structInstantiation.fieldInitializers) {
    std::string_view id = initStmt->identifier;
    const SourceLocation &loc = initStmt->location;

    if (inits.count(id)) {
      report(loc, "field '" + std::string{id} + "' is already initialized");
      error = true;
      continue;
    }

    const res::FieldDecl *fieldDecl = fields[id];
    if (!fieldDecl) {
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

    if (resolvedInitExpr->type.name != fieldDecl->type.name) {
      report(resolvedInitExpr->location,
             "'" + resolvedInitExpr->type.name +
                 "' cannot be used to initialize a field of type '" +
                 fieldDecl->type.name + "'");
      error = true;
      continue;
    }

    auto init = std::make_unique<res::FieldInitStmt>(
        loc, *fieldDecl, std::move(resolvedInitExpr));
    inits[id] = resolvedFieldInits.emplace_back(std::move(init)).get();
  }

  for (auto &&fieldDecl : st->fields) {
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

  return std::make_unique<res::StructInstantiationExpr>(
      structInstantiation.location, *st, std::move(resolvedFieldInits));
}

std::unique_ptr<res::MemberExpr>
Sema::resolveMemberExpr(const ast::MemberExpr &memberExpr) {
  auto resolvedBase = resolveExpr(*memberExpr.base);
  if (!resolvedBase)
    return nullptr;

  if (resolvedBase->type.kind != res::Type::Kind::Struct)
    return report(memberExpr.base->location,
                  "cannot access field of '" + resolvedBase->type.name + '\'');

  const auto *st = lookupDecl<res::StructDecl>(resolvedBase->type.name).first;

  assert(st && "failed to lookup struct");

  const res::FieldDecl *fieldDecl = nullptr;
  for (auto &&field : st->fields) {
    if (field->identifier == memberExpr.field)
      fieldDecl = field.get();
  }

  if (!fieldDecl)
    return report(memberExpr.location, '\'' + resolvedBase->type.name +
                                           "' has no field called '" +
                                           memberExpr.field + '\'');

  return std::make_unique<res::MemberExpr>(memberExpr.location,
                                           std::move(resolvedBase), *fieldDecl);
}

std::unique_ptr<res::Stmt> Sema::resolveStmt(const ast::Stmt &stmt) {
  if (auto *expr = dynamic_cast<const ast::Expr *>(&stmt))
    return resolveExpr(*expr);

  if (auto *ifStmt = dynamic_cast<const ast::IfStmt *>(&stmt))
    return resolveIfStmt(*ifStmt);

  if (auto *assignment = dynamic_cast<const ast::Assignment *>(&stmt))
    return resolveAssignment(*assignment);

  if (auto *declStmt = dynamic_cast<const ast::DeclStmt *>(&stmt))
    return resolveDeclStmt(*declStmt);

  if (auto *whileStmt = dynamic_cast<const ast::WhileStmt *>(&stmt))
    return resolveWhileStmt(*whileStmt);

  if (auto *returnStmt = dynamic_cast<const ast::ReturnStmt *>(&stmt))
    return resolveReturnStmt(*returnStmt);

  llvm_unreachable("unexpected statement");
}

std::unique_ptr<res::IfStmt> Sema::resolveIfStmt(const ast::IfStmt &ifStmt) {
  varOrReturn(condition, resolveExpr(*ifStmt.condition));

  if (condition->type.kind != res::Type::Kind::Number)
    return report(condition->location, "expected number in condition");

  varOrReturn(resolvedTrueBlock, resolveBlock(*ifStmt.trueBlock));

  std::unique_ptr<res::Block> resolvedFalseBlock;
  if (ifStmt.falseBlock) {
    resolvedFalseBlock = resolveBlock(*ifStmt.falseBlock);
    if (!resolvedFalseBlock)
      return nullptr;
  }

  condition->setConstantValue(cee.evaluate(*condition, false));

  return std::make_unique<res::IfStmt>(ifStmt.location, std::move(condition),
                                       std::move(resolvedTrueBlock),
                                       std::move(resolvedFalseBlock));
}

std::unique_ptr<res::WhileStmt>
Sema::resolveWhileStmt(const ast::WhileStmt &whileStmt) {
  varOrReturn(condition, resolveExpr(*whileStmt.condition));

  if (condition->type.kind != res::Type::Kind::Number)
    return report(condition->location, "expected number in condition");

  varOrReturn(body, resolveBlock(*whileStmt.body));

  condition->setConstantValue(cee.evaluate(*condition, false));

  return std::make_unique<res::WhileStmt>(
      whileStmt.location, std::move(condition), std::move(body));
}

std::unique_ptr<res::DeclStmt>
Sema::resolveDeclStmt(const ast::DeclStmt &declStmt) {
  varOrReturn(resolvedVarDecl, resolveVarDecl(*declStmt.varDecl));

  if (!insertDeclToCurrentScope(*resolvedVarDecl))
    return nullptr;

  return std::make_unique<res::DeclStmt>(declStmt.location,
                                         std::move(resolvedVarDecl));
}

std::unique_ptr<res::Assignment>
Sema::resolveAssignment(const ast::Assignment &assignment) {
  varOrReturn(resolvedRHS, resolveExpr(*assignment.expr));
  varOrReturn(resolvedLHS, resolveAssignableExpr(*assignment.assignee));

  assert(resolvedLHS->type.kind != res::Type::Kind::Void &&
         "reference to void declaration in assignment LHS");

  if (resolvedRHS->type.name != resolvedLHS->type.name)
    return report(resolvedRHS->location,
                  "assigned value type doesn't match variable type");

  resolvedRHS->setConstantValue(cee.evaluate(*resolvedRHS, false));

  return std::make_unique<res::Assignment>(
      assignment.location, std::move(resolvedLHS), std::move(resolvedRHS));
}

std::unique_ptr<res::ReturnStmt>
Sema::resolveReturnStmt(const ast::ReturnStmt &returnStmt) {
  assert(currentFunction && "return stmt outside a function");

  if (currentFunction->type.kind == res::Type::Kind::Void && returnStmt.expr)
    return report(returnStmt.location,
                  "unexpected return value in void function");

  if (currentFunction->type.kind != res::Type::Kind::Void && !returnStmt.expr)
    return report(returnStmt.location, "expected a return value");

  std::unique_ptr<res::Expr> resolvedExpr;
  if (returnStmt.expr) {
    resolvedExpr = resolveExpr(*returnStmt.expr);
    if (!resolvedExpr)
      return nullptr;

    if (currentFunction->type.name != resolvedExpr->type.name)
      return report(resolvedExpr->location, "unexpected return type");

    resolvedExpr->setConstantValue(cee.evaluate(*resolvedExpr, false));
  }

  return std::make_unique<res::ReturnStmt>(returnStmt.location,
                                           std::move(resolvedExpr));
}

std::unique_ptr<res::AssignableExpr>
Sema::resolveAssignableExpr(const ast::AssignableExpr &assignableExpr) {
  if (const auto *declRefExpr =
          dynamic_cast<const ast::DeclRefExpr *>(&assignableExpr))
    return resolveDeclRefExpr(*declRefExpr);

  if (const auto *memberExpr =
          dynamic_cast<const ast::MemberExpr *>(&assignableExpr))
    return resolveMemberExpr(*memberExpr);

  llvm_unreachable("unexpected assignable expression");
}

std::unique_ptr<res::Expr> Sema::resolveExpr(const ast::Expr &expr) {

  if (const auto *number = dynamic_cast<const ast::NumberLiteral *>(&expr))
    return std::make_unique<res::NumberLiteral>(number->location,
                                                std::stod(number->value));

  if (const auto *callExpr = dynamic_cast<const ast::CallExpr *>(&expr))
    return resolveCallExpr(*callExpr);

  if (const auto *groupingExpr = dynamic_cast<const ast::GroupingExpr *>(&expr))
    return resolveGroupingExpr(*groupingExpr);

  if (const auto *binaryOperator =
          dynamic_cast<const ast::BinaryOperator *>(&expr))
    return resolveBinaryOperator(*binaryOperator);

  if (const auto *unaryOperator =
          dynamic_cast<const ast::UnaryOperator *>(&expr))
    return resolveUnaryOperator(*unaryOperator);

  if (const auto *structInstantiation =
          dynamic_cast<const ast::StructInstantiationExpr *>(&expr))
    return resolveStructInstantiation(*structInstantiation);

  if (const auto *assignableExpr =
          dynamic_cast<const ast::AssignableExpr *>(&expr))
    return resolveAssignableExpr(*assignableExpr);

  llvm_unreachable("unexpected expression");
}

std::unique_ptr<res::Block> Sema::resolveBlock(const ast::Block &block) {
  std::vector<std::unique_ptr<res::Stmt>> resolvedStatements;

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

    if (dynamic_cast<ast::ReturnStmt *>(stmt.get()))
      ++reportUnreachableCount;
  }

  if (error)
    return nullptr;

  return std::make_unique<res::Block>(block.location,
                                      std::move(resolvedStatements));
}

std::unique_ptr<res::ParamDecl>
Sema::resolveParamDecl(const ast::ParamDecl &param) {
  std::optional<res::Type> type = resolveType(param.type);

  if (!type || type->kind == res::Type::Kind::Void)
    return report(param.location, "parameter '" + param.identifier +
                                      "' has invalid '" + param.type.name +
                                      "' type");

  return std::make_unique<res::ParamDecl>(param.location, param.identifier,
                                          *type, param.isMutable);
}

std::unique_ptr<res::VarDecl>
Sema::resolveVarDecl(const ast::VarDecl &varDecl) {
  if (!varDecl.type && !varDecl.initializer)
    return report(
        varDecl.location,
        "an uninitialized variable is expected to have a type specifier");

  std::unique_ptr<res::Expr> resolvedInitializer = nullptr;
  if (varDecl.initializer) {
    resolvedInitializer = resolveExpr(*varDecl.initializer);
    if (!resolvedInitializer)
      return nullptr;
  }

  std::optional<res::Type> type =
      varDecl.type ? resolveType(*varDecl.type) : resolvedInitializer->type;
  if (!type || type->kind == res::Type::Kind::Void)
    return report(varDecl.location,
                  "variable '" + varDecl.identifier + "' has invalid '" +
                      (varDecl.type ? varDecl.type->name
                                    : resolvedInitializer->type.name) +
                      "' type");

  if (resolvedInitializer) {
    if (resolvedInitializer->type.name != type->name)
      return report(resolvedInitializer->location, "initializer type mismatch");

    resolvedInitializer->setConstantValue(
        cee.evaluate(*resolvedInitializer, false));
  }

  return std::make_unique<res::VarDecl>(varDecl.location, varDecl.identifier,
                                        *type, varDecl.isMutable,
                                        std::move(resolvedInitializer));
}

std::unique_ptr<res::FunctionDecl>
Sema::resolveFunctionDecl(const ast::FunctionDecl &function) {
  std::optional<res::Type> type = resolveType(function.type);

  if (!type)
    return report(function.location, "function '" + function.identifier +
                                         "' has invalid '" +
                                         function.type.name + "' type");

  if (function.identifier == "main") {
    if (type->kind != res::Type::Kind::Void)
      return report(function.location,
                    "'main' function is expected to have 'void' type");

    if (!function.params.empty())
      return report(function.location,
                    "'main' function is expected to take no arguments");
  } else if (function.identifier == "printf") {
    return report(function.location,
                  "'printf' is a reserved function name and cannot be used for "
                  "user-defined functions");
  }

  std::vector<std::unique_ptr<res::ParamDecl>> resolvedParams;

  ScopeRAII paramScope(this);
  for (auto &&param : function.params) {
    auto resolvedParam = resolveParamDecl(*param);

    if (!resolvedParam || !insertDeclToCurrentScope(*resolvedParam))
      return nullptr;

    resolvedParams.emplace_back(std::move(resolvedParam));
  }

  return std::make_unique<res::FunctionDecl>(
      function.location, function.identifier, *type, std::move(resolvedParams),
      nullptr);
};

std::unique_ptr<res::StructDecl>
Sema::resolveStructDecl(const ast::StructDecl &structDecl) {
  std::set<std::string_view> identifiers;
  std::vector<std::unique_ptr<res::FieldDecl>> resolvedFields;

  unsigned idx = 0;
  for (auto &&field : structDecl.fields) {
    if (!identifiers.emplace(field->identifier).second)
      return report(field->location,
                    "field '" + field->identifier + "' is already declared");

    // FIXME: this resolution logic will be reworked soon
    resolvedFields.emplace_back(std::make_unique<res::FieldDecl>(
        field->location, field->identifier, res::Type::custom(field->type.name),
        idx++));
  }

  return std::make_unique<res::StructDecl>(
      structDecl.location, structDecl.identifier,
      res::Type::structType(structDecl.identifier), std::move(resolvedFields));
}

bool Sema::resolveStructFields(res::StructDecl &resolvedStructDecl) {
  std::stack<std::pair<res::StructDecl *, std::set<const res::StructDecl *>>>
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

    for (auto &&field : currentDecl->fields) {
      // FIXME: this resolution logic will be reworked soon
      auto type = resolveType(ast::Type(field->location, field->type.name));
      if (!type) {
        report(field->location, "unable to resolve '" + field->type.name +
                                    "' type of struct field");
        return false;
      }

      if (type->kind == res::Type::Kind::Void) {
        report(field->location, "struct field cannot be void");
        return false;
      }

      if (type->kind == res::Type::Kind::Struct) {
        auto *nestedStruct = lookupDecl<res::StructDecl>(type->name).first;
        assert(nestedStruct && "unexpected type");

        worklist.push({nestedStruct, visited});
      }

      field->type = *type;
    }
  }

  return true;
}

std::vector<std::unique_ptr<res::Decl>> Sema::resolveAST() {
  ScopeRAII globalScope(this);
  std::vector<std::unique_ptr<res::Decl>> resolvedTree;

  bool error = false;
  std::vector<const ast::FunctionDecl *> functionsToResolve;

  // Resolve every struct first so that functions have access to them in their
  // signature.
  for (auto &&decl : ast) {
    if (const auto *st = dynamic_cast<const ast::StructDecl *>(decl.get())) {
      std::unique_ptr<res::Decl> resolvedDecl = resolveStructDecl(*st);

      if (!resolvedDecl || !insertDeclToCurrentScope(*resolvedDecl)) {
        error = true;
        continue;
      }

      resolvedTree.emplace_back(std::move(resolvedDecl));
      continue;
    }

    if (const auto *fn = dynamic_cast<const ast::FunctionDecl *>(decl.get())) {
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
    if (auto *st = dynamic_cast<res::StructDecl *>(currentDecl.get())) {
      if (!resolveStructFields(*st))
        error = true;

      continue;
    }

    if (auto *fn = dynamic_cast<res::FunctionDecl *>(currentDecl.get())) {
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
