#include "sema.h"
#include "utils.h"
#include <cassert>

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
      builtinLocation, std::vector<std::unique_ptr<ResolvedStmt>>{});

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

std::unique_ptr<ResolvedUnaryOperator>
Sema::resolveUnaryOperator(const UnaryOperator &unary) {
  auto resolvedRHS = resolveExpr(*unary.RHS);

  if (!resolvedRHS)
    return nullptr;

  return std::make_unique<ResolvedUnaryOperator>(
      unary.location, std::move(resolvedRHS), unary.op);
}

std::unique_ptr<ResolvedBinaryOperator>
Sema::resolveBinaryOperator(const BinaryOperator &binop) {
  auto resolvedLHS = resolveExpr(*binop.LHS);
  auto resolvedRHS = resolveExpr(*binop.RHS);

  if (!resolvedLHS || !resolvedRHS)
    return nullptr;

  return std::make_unique<ResolvedBinaryOperator>(
      binop.location, std::move(resolvedLHS), std::move(resolvedRHS), binop.op);
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

    if (std::optional<double> val = CEE.evaluate(*resolvedArg))
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

  if (auto *assignment = dynamic_cast<const BinaryOperator *>(&stmt))
    return resolveBinaryOperator(*assignment);

  if (auto *declStmt = dynamic_cast<const DeclStmt *>(&stmt))
    return resolveDeclStmt(*declStmt);

  assert(false && "unknown statement");
}

std::unique_ptr<ResolvedIfStmt> Sema::resolveIfStmt(const IfStmt &ifStmt) {
  std::unique_ptr<ResolvedExpr> condition = resolveExpr(*ifStmt.condition);
  if (!condition)
    return nullptr;

  if (condition->type != Type::NUMBER)
    return error(condition->location, "unexpected type of expression");

  auto trueBlock = resolveBlock(*ifStmt.trueBlock);
  if (!trueBlock)
    return nullptr;

  if (ifStmt.falseBlock) {
    auto falseBlock = resolveBlock(*ifStmt.falseBlock);
    if (!falseBlock)
      return nullptr;

    return std::make_unique<ResolvedIfStmt>(
        ifStmt.location, std::move(condition), std::move(trueBlock),
        std::move(falseBlock));
  }

  if (ifStmt.falseBranch) {
    auto falseBranch = resolveIfStmt(*ifStmt.falseBranch);
    if (!falseBranch)
      return nullptr;

    return std::make_unique<ResolvedIfStmt>(
        ifStmt.location, std::move(condition), std::move(trueBlock),
        std::move(falseBranch));
  }

  return std::make_unique<ResolvedIfStmt>(ifStmt.location, std::move(condition),
                                          std::move(trueBlock));
}

std::unique_ptr<ResolvedDeclStmt>
Sema::resolveDeclStmt(const DeclStmt &declStmt) {
  auto resolvedVarDecl = resolveVarDecl(*declStmt.varDecl);
  if (!resolvedVarDecl)
    return nullptr;

  if (!insertDeclToCurrentScope(*resolvedVarDecl))
    return nullptr;

  return std::make_unique<ResolvedDeclStmt>(declStmt.location,
                                            std::move(resolvedVarDecl));
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

  if (auto binaryOperator = dynamic_cast<const BinaryOperator *>(&expr))
    return resolveBinaryOperator(*binaryOperator);

  if (auto unaryOperator = dynamic_cast<const UnaryOperator *>(&expr))
    return resolveUnaryOperator(*unaryOperator);

  return nullptr;
}

std::unique_ptr<ResolvedBlock> Sema::resolveBlock(const Block &block) {
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

  if (!type || type == Type::VOID)
    return error(param.location, "parameter '" + param.identifier +
                                     "' has invalid '" + param.type + "' type");

  return std::make_unique<ResolvedParamDecl>(param.location, param.identifier,
                                             *type);
}

std::unique_ptr<ResolvedVarDecl> Sema::resolveVarDecl(const VarDecl &varDecl) {
  std::optional<Type> type = resolveType(varDecl.type);

  if (!type || type == Type::VOID)
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
