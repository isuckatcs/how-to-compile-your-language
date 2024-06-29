#include <cassert>

#include "cfg.h"
#include "sema.h"
#include "utils.h"

bool Sema::insertDeclToCurrentScope(ResolvedDecl &decl) {
  const auto &[foundDecl, scopeIdx] = lookupDecl(decl.identifier);

  if (foundDecl && scopeIdx == 0) {
    error(decl.location, "redeclaration of '" + decl.identifier + '\'');
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

std::unique_ptr<ResolvedFunctionDecl> Sema::createBuiltinPrint() {
  SourceLocation builtinLocation = SourceLocation{"<builtin>", 0, 0};

  auto param =
      std::make_unique<ResolvedParamDecl>(builtinLocation, "n", Type::Number);

  std::vector<std::unique_ptr<ResolvedParamDecl>> params;
  params.emplace_back(std::move(param));

  auto block = std::make_unique<ResolvedBlock>(
      builtinLocation, std::vector<std::unique_ptr<ResolvedStmt>>{});

  return std::make_unique<ResolvedFunctionDecl>(SourceLocation{}, "print",
                                                Type::Void, std::move(params),
                                                std::move(block));
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

  return std::make_unique<ResolvedUnaryOperator>(
      unary.location, std::move(resolvedRHS), unary.op);
}

std::unique_ptr<ResolvedBinaryOperator>
Sema::resolveBinaryOperator(const BinaryOperator &binop) {
  varOrReturn(resolvedLHS, resolveExpr(*binop.lhs));
  varOrReturn(resolvedRHS, resolveExpr(*binop.rhs));

  if (resolvedLHS->type == Type::Void)
    return error(
        resolvedLHS->location,
        "void expression cannot be used as LHS operand to binary operator");

  if (resolvedRHS->type == Type::Void)
    return error(
        resolvedRHS->location,
        "void expression cannot be used as RHS operand to binary operator");

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
Sema::resolveDeclRefExpr(const DeclRefExpr &declRefExpr) {
  if (ResolvedDecl *decl = lookupDecl(declRefExpr.identifier).first)
    return std::make_unique<ResolvedDeclRefExpr>(declRefExpr.location, *decl);

  return error(declRefExpr.location,
               "symbol '" + declRefExpr.identifier + "' not found");
}

std::unique_ptr<ResolvedCallExpr> Sema::resolveCallExpr(const CallExpr &call) {
  varOrReturn(resolvedCallee, resolveDeclRefExpr(*call.identifier));

  const auto *resolvedFunctionDecl =
      dynamic_cast<const ResolvedFunctionDecl *>(resolvedCallee->decl);

  if (!resolvedFunctionDecl)
    return error(call.location, "calling non-function symbol");

  if (call.arguments.size() != resolvedFunctionDecl->params.size())
    return error(call.location, "argument count missmatch in function call");

  std::vector<std::unique_ptr<ResolvedExpr>> resolvedArguments;
  int idx = 0;
  for (auto &&arg : call.arguments) {
    varOrReturn(resolvedArg, resolveExpr(*arg));

    if (resolvedArg->type != resolvedFunctionDecl->params[idx]->type)
      return error(resolvedArg->location, "unexpected type of argument");

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

  if (auto *returnStmt = dynamic_cast<const ReturnStmt *>(&stmt))
    return resolveReturnStmt(*returnStmt);

  assert(false && "unknown statement");
}

std::unique_ptr<ResolvedIfStmt> Sema::resolveIfStmt(const IfStmt &ifStmt) {
  varOrReturn(condition, resolveExpr(*ifStmt.condition));

  if (condition->type != Type::Number)
    return error(condition->location, "expected number in condition");

  varOrReturn(trueBlock, resolveBlock(*ifStmt.trueBlock));

  if (ifStmt.falseBlock) {
    varOrReturn(falseBlock, resolveBlock(*ifStmt.falseBlock));
    return std::make_unique<ResolvedIfStmt>(
        ifStmt.location, std::move(condition), std::move(trueBlock),
        std::move(falseBlock));
  }

  if (ifStmt.falseBranch) {
    varOrReturn(falseBranch, resolveIfStmt(*ifStmt.falseBranch));
    return std::make_unique<ResolvedIfStmt>(
        ifStmt.location, std::move(condition), std::move(trueBlock),
        std::move(falseBranch));
  }

  return std::make_unique<ResolvedIfStmt>(ifStmt.location, std::move(condition),
                                          std::move(trueBlock));
}

std::unique_ptr<ResolvedWhileStmt>
Sema::resolveWhileStmt(const WhileStmt &whileStmt) {
  varOrReturn(condition, resolveExpr(*whileStmt.condition));

  if (condition->type != Type::Number)
    return error(condition->location, "expected number in condition");

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

  if (resolvedLHS->type == Type::Void)
    return error(
        resolvedLHS->location,
        "void expression cannot be used as LHS operand to binary operator");

  if (resolvedRHS->type == Type::Void)
    return error(
        resolvedRHS->location,
        "void expression cannot be used as RHS operand to binary operator");

  return std::make_unique<ResolvedAssignment>(
      assignment.location, std::move(resolvedLHS), std::move(resolvedRHS));
}

std::unique_ptr<ResolvedReturnStmt>
Sema::resolveReturnStmt(const ReturnStmt &returnStmt) {
  assert(currentFunction && "return stmt outside a function");

  if (currentFunction->type == Type::Void && returnStmt.expr)
    return error(returnStmt.location,
                 "unexpected return value in void function");

  if (currentFunction->type != Type::Void && !returnStmt.expr)
    return error(returnStmt.location, "expected a return value");

  std::unique_ptr<ResolvedExpr> resolvedExpr;
  if (returnStmt.expr) {
    resolvedExpr = resolveExpr(*returnStmt.expr);
    if (!resolvedExpr)
      return nullptr;

    if (currentFunction->type != resolvedExpr->type)
      return error(resolvedExpr->location, "unexpected return type");
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
  else if (const auto *unaryOperator =
               dynamic_cast<const UnaryOperator *>(&expr))
    resolvedExpr = resolveUnaryOperator(*unaryOperator);
  else
    assert(false && "unexpected expression");

  if (!resolvedExpr)
    return nullptr;

  if (std::optional<double> val = cee.evaluate(*resolvedExpr))
    resolvedExpr->setConstantValue(val);

  return resolvedExpr;
}

std::unique_ptr<ResolvedBlock> Sema::resolveBlock(const Block &block) {
  ScopeRAII scope{this};
  std::vector<std::unique_ptr<ResolvedStmt>> resolvedStatements;

  for (auto &&stmt : block.statements)
    if (!resolvedStatements.emplace_back(resolveStmt(*stmt)))
      return nullptr;

  return std::make_unique<ResolvedBlock>(block.location,
                                         std::move(resolvedStatements));
}

std::unique_ptr<ResolvedParamDecl>
Sema::resolveParamDecl(const ParamDecl &param) {
  std::optional<Type> type = resolveType(param.type);

  if (!type || type == Type::Void)
    return error(param.location, "parameter '" + param.identifier +
                                     "' has invalid '" + param.type + "' type");

  return std::make_unique<ResolvedParamDecl>(param.location, param.identifier,
                                             *type);
}

std::unique_ptr<ResolvedVarDecl> Sema::resolveVarDecl(const VarDecl &varDecl) {
  std::optional<Type> type = resolveType(varDecl.type);

  if (!type || type == Type::Void)
    return error(varDecl.location, "variable '" + varDecl.identifier +
                                       "' has invalid '" + varDecl.type +
                                       "' type");

  std::unique_ptr<ResolvedExpr> resolvedInitializer = nullptr;
  if (varDecl.initialzer) {
    resolvedInitializer = resolveExpr(*varDecl.initialzer);
    if (!resolvedInitializer)
      return nullptr;

    if (resolvedInitializer->type != type)
      return error(resolvedInitializer->location, "initializer type mismatch");
  }

  return std::make_unique<ResolvedVarDecl>(varDecl.location, varDecl.identifier,
                                           *type, varDecl.isMutable,
                                           std::move(resolvedInitializer));
}

std::unique_ptr<ResolvedFunctionDecl>
Sema::resolveFunctionWithoutBody(const FunctionDecl &function) {
  ScopeRAII scope{this};
  std::optional<Type> type = resolveType(function.type);

  if (!type)
    return error(function.location, "function '" + function.identifier +
                                        "' has invalid '" + function.type +
                                        "' type");

  if (function.identifier == "main" && type != Type::Void)
    return error(function.location,
                 "'main' function is expected to have 'void' type");

  std::vector<std::unique_ptr<ResolvedParamDecl>> resolvedParams;
  for (auto &&param : function.params) {
    auto resolvedParamDecl = resolveParamDecl(*param);

    if (!resolvedParamDecl || !insertDeclToCurrentScope(*resolvedParamDecl))
      return nullptr;

    resolvedParams.emplace_back(std::move(resolvedParamDecl));
  }

  return std::make_unique<ResolvedFunctionDecl>(
      function.location, function.identifier, *type, std::move(resolvedParams),
      nullptr);
};

std::vector<std::unique_ptr<ResolvedFunctionDecl>> Sema::resolveSourceFile() {
  ScopeRAII scope{this};
  std::vector<std::unique_ptr<ResolvedFunctionDecl>> resolvedSourceFile;

  std::unique_ptr<ResolvedFunctionDecl> builtinPrint = createBuiltinPrint();
  insertDeclToCurrentScope(
      *resolvedSourceFile.emplace_back(std::move(builtinPrint)));

  bool error = false;
  for (auto &&function : sourceFile) {
    auto resolvedFunctionDecl = resolveFunctionWithoutBody(*function);

    if (!resolvedFunctionDecl ||
        !insertDeclToCurrentScope(*resolvedFunctionDecl)) {
      error = true;
      continue;
    }

    resolvedSourceFile.emplace_back(std::move(resolvedFunctionDecl));
  }

  if (error)
    return {};

  for (size_t i = 1; i < resolvedSourceFile.size(); ++i) {
    ScopeRAII scope{this};

    for (auto &&param : resolvedSourceFile[i]->params)
      insertDeclToCurrentScope(*param);

    currentFunction = resolvedSourceFile[i].get();
    auto resolvedBody = resolveBlock(*sourceFile[i - 1]->body);
    if (!resolvedBody)
      return {};
    resolvedSourceFile[i]->body = std::move(resolvedBody);
    currentFunction = nullptr;

    CFGBuilder b;
    b.build(*resolvedSourceFile[i]);
  }

  return std::move(resolvedSourceFile);
}
