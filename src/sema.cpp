#include "sema.h"
#include "utils.h"

bool Sema::insertDeclToCurrentScope(ResolvedDecl &decl) {
  const auto &[foundDecl, scopeIdx] = lookupDecl(decl.identifier);

  if (foundDecl && scopeIdx == 0) {
    error(decl.location, "redeclaration of '" + decl.identifier + '\'');
    return false;
  }

  Scopes.back().emplace_back(&decl);
  return true;
}

std::pair<ResolvedDecl *, int> Sema::lookupDecl(const std::string id) {
  int scopeIdx = 0;
  for (auto it = Scopes.rbegin(); it != Scopes.rend(); ++it) {
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
      std::make_unique<ResolvedParamDecl>(builtinLocation, "n", Type::NUMBER);

  std::vector<std::unique_ptr<ResolvedParamDecl>> params;
  params.emplace_back(std::move(param));

  auto block = std::make_unique<ResolvedBlock>(
      builtinLocation, std::vector<std::unique_ptr<ResolvedExpr>>{});

  return std::make_unique<ResolvedFunctionDecl>(SourceLocation{}, "print",
                                                Type::VOID, std::move(params),
                                                std::move(block));
};

std::optional<Type> Sema::resolveType(const std::string &typeSpecifier) {
  if (typeSpecifier == "void")
    return Type::VOID;
  if (typeSpecifier == "number")
    return Type::NUMBER;

  return std::nullopt;
}

std::unique_ptr<ResolvedGroupingExpr>
Sema::resolveGroupingExpr(const GroupingExpr &grouping) {
  auto resolvedExpr = resolveExpr(*grouping.expr);
  if (!resolvedExpr)
    return nullptr;

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
  auto resolvedCallee = resolveDeclRefExpr(*call.identifier);
  if (!resolvedCallee)
    return nullptr;

  auto resolvedFunctionDecl =
      dynamic_cast<const ResolvedFunctionDecl *>(resolvedCallee->decl);

  if (!resolvedFunctionDecl)
    return error(call.location, "calling non-function symbol");

  if (call.arguments.size() != resolvedFunctionDecl->params.size())
    return error(call.location, "argument count missmatch in function call");

  std::vector<std::unique_ptr<ResolvedExpr>> resolvedArguments;
  int idx = 0;
  for (auto &&arg : call.arguments) {
    auto resolvedArg = resolveExpr(*arg);
    if (!resolvedArg)
      return nullptr;

    if (resolvedArg->type != resolvedFunctionDecl->params[idx]->type)
      return error(resolvedArg->location, "unexpected type of argument");

    ++idx;
    resolvedArguments.emplace_back(std::move(resolvedArg));
  }

  return std::make_unique<ResolvedCallExpr>(
      call.location, *resolvedFunctionDecl, std::move(resolvedArguments));
}

std::unique_ptr<ResolvedExpr> Sema::resolveExpr(const Expr &expr) {
  if (auto numberLiteral = dynamic_cast<const NumberLiteral *>(&expr))
    return std::make_unique<ResolvedNumberLiteral>(
        numberLiteral->location, std::stod(numberLiteral->value));

  if (auto declRefExpr = dynamic_cast<const DeclRefExpr *>(&expr))
    return resolveDeclRefExpr(*declRefExpr);

  if (auto callExpr = dynamic_cast<const CallExpr *>(&expr))
    return resolveCallExpr(*callExpr);

  if (auto groupingExpr = dynamic_cast<const GroupingExpr *>(&expr))
    return resolveGroupingExpr(*groupingExpr);

  return nullptr;
}

std::unique_ptr<ResolvedBlock> Sema::resolveBlock(const Block &block) {
  std::vector<std::unique_ptr<ResolvedExpr>> resolvedExpressions;

  for (auto &&expr : block.expressions)
    if (!resolvedExpressions.emplace_back(resolveExpr(*expr)))
      return nullptr;

  return std::make_unique<ResolvedBlock>(block.location,
                                         std::move(resolvedExpressions));
}

std::unique_ptr<ResolvedParamDecl>
Sema::resolveParamDecl(const ParamDecl &param) {
  std::optional<Type> type = resolveType(param.type);

  if (!type || type == Type::VOID)
    return error(param.location, "parameter '" + param.identifier +
                                     "' has invalid '" + param.type + "' type");

  return std::make_unique<ResolvedParamDecl>(param.location, param.identifier,
                                             *type);
}

std::unique_ptr<ResolvedFunctionDecl>
Sema::resolveFunctionWithoutBody(const FunctionDecl &function) {
  ScopeRAII scope{this};
  std::optional<Type> type = resolveType(function.type);

  if (!type)
    return error(function.location, "function '" + function.identifier +
                                        "' has invalid '" + function.type +
                                        "' type");

  if (type != Type::VOID)
    return error(function.location, "only void functions are supported");

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

  for (auto &&function : sourceFile) {
    auto resolvedFunctionDecl = resolveFunctionWithoutBody(*function);

    if (!resolvedFunctionDecl ||
        !insertDeclToCurrentScope(*resolvedFunctionDecl))
      continue;

    resolvedSourceFile.emplace_back(std::move(resolvedFunctionDecl));
  }

  if (resolvedSourceFile.size() != sourceFile.size())
    return {};

  std::unique_ptr<ResolvedFunctionDecl> builtinPrint = createBuiltinPrint();
  insertDeclToCurrentScope(
      *resolvedSourceFile.emplace_back(std::move(builtinPrint)));

  for (size_t i = 0; i < sourceFile.size(); ++i) {
    ScopeRAII scope{this};

    for (auto &&param : resolvedSourceFile[i]->params)
      insertDeclToCurrentScope(*param);

    auto resolvedBody = resolveBlock(*sourceFile[i]->body);
    if (!resolvedBody)
      return {};
    resolvedSourceFile[i]->body = std::move(resolvedBody);
  }

  return std::move(resolvedSourceFile);
}
