#include <cassert>

#include "sema.h"
#include "utils.h"

namespace yl {
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
  if (parsedType.kind == Type::Kind::Custom)
    return std::nullopt;

  return parsedType;
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

    if (resolvedArg->type.kind != resolvedFunctionDecl->params[idx]->type.kind)
      return report(resolvedArg->location, "unexpected type of argument");

    ++idx;
    resolvedArguments.emplace_back(std::move(resolvedArg));
  }

  return std::make_unique<ResolvedCallExpr>(
      call.location, *resolvedFunctionDecl, std::move(resolvedArguments));
}

std::unique_ptr<ResolvedStmt> Sema::resolveStmt(const Stmt &stmt) {
  if (auto *expr = dynamic_cast<const Expr *>(&stmt))
    return resolveExpr(*expr);

  auto *returnStmt = dynamic_cast<const ReturnStmt *>(&stmt);
  assert(returnStmt && "unknown statement");

  return resolveReturnStmt(*returnStmt);
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

    if (currentFunction->type.kind != resolvedExpr->type.kind)
      return report(resolvedExpr->location, "unexpected return type");
  }

  return std::make_unique<ResolvedReturnStmt>(returnStmt.location,
                                              std::move(resolvedExpr));
}

std::unique_ptr<ResolvedExpr> Sema::resolveExpr(const Expr &expr) {

  if (const auto *number = dynamic_cast<const NumberLiteral *>(&expr))
    return std::make_unique<ResolvedNumberLiteral>(number->location,
                                                   std::stod(number->value));

  if (const auto *declRefExpr = dynamic_cast<const DeclRefExpr *>(&expr))
    return resolveDeclRefExpr(*declRefExpr);

  if (const auto *callExpr = dynamic_cast<const CallExpr *>(&expr))
    return resolveCallExpr(*callExpr);

  assert(false && "unexpected expression");
  return nullptr;
}

std::unique_ptr<ResolvedBlock> Sema::resolveBlock(const Block &block) {
  std::vector<std::unique_ptr<ResolvedStmt>> resolvedStatements;

  bool error = false;
  int reportUnreachableCount = 0;

  ScopeRAII blockScope{this};
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

  ScopeRAII paramScope{this};
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
  }

  if (error)
    return {};

  return std::move(resolvedTree);
}
} // namespace yl