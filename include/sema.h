#ifndef A_COMPILER_SEMA_H
#define A_COMPILER_SEMA_H

#include <memory>
#include <optional>

#include "ast.h"
#include "utils.h"

class Sema {
  std::vector<std::unique_ptr<FunctionDecl>> TopLevel;
  std::vector<std::vector<ResolvedDecl *>> Scopes;

  ResolvedDecl *lookupDecl(const std::string id) {
    for (auto it = Scopes.rbegin(); it != Scopes.rend(); ++it) {
      for (auto &&decl : *it)
        if (decl->identifier == id)
          return decl;
    }

    return nullptr;
  }

  std::optional<Type> resolveType(const std::string &typeSpecifier) {
    if (typeSpecifier == "void")
      return Type::VOID;
    if (typeSpecifier == "string")
      return Type::STRING;
    if (typeSpecifier == "number")
      return Type::NUMBER;

    return std::nullopt;
  }

  std::unique_ptr<ResolvedDeclRefExpr>
  resolveDeclRefExpr(const DeclRefExpr &DRE) {
    ResolvedDecl *decl = lookupDecl(DRE.identifier);

    if (!decl)
      return error(DRE.location, "symbol '" + DRE.identifier + "' not found");

    return std::make_unique<ResolvedDeclRefExpr>(DRE.location, *decl);
  }

  std::unique_ptr<ResolvedCallExpr> resolveCallExpr(const CallExpr &call) {
    auto resolvedCallee = resolveDeclRefExpr(*call.identifier);
    if (!resolvedCallee)
      return nullptr;

    auto resolvedFunctionDecl =
        dynamic_cast<const ResolvedFunctionDecl *>(resolvedCallee->decl);
    if (!resolvedFunctionDecl)
      return error(call.location, "calling non-function element");

    std::vector<std::unique_ptr<ResolvedExpr>> resolvedArguments;

    int idx = 0;
    for (auto &&arg : call.arguments) {
      if (auto resolvedArg = resolveExpr(*arg)) {
        if (resolvedArg->type != resolvedFunctionDecl->params[idx]->type)
          return error(resolvedArg->location, "unexpected type of argument");
        resolvedArguments.emplace_back(std::move(resolvedArg));
      } else
        return nullptr;
    }

    if (resolvedArguments.size() != resolvedFunctionDecl->params.size())
      return error(call.location, "missing arguments from function call");

    return std::make_unique<ResolvedCallExpr>(
        call.location, *resolvedFunctionDecl, std::move(resolvedArguments));
  }

  std::unique_ptr<ResolvedExpr> resolveExpr(const Expr &expr) {
    if (auto numberLiteral = dynamic_cast<const NumberLiteral *>(&expr))
      return std::make_unique<ResolvedNumberLiteral>(
          numberLiteral->location, std::stod(numberLiteral->value));

    if (auto DRE = dynamic_cast<const DeclRefExpr *>(&expr))
      return resolveDeclRefExpr(*DRE);

    if (auto call = dynamic_cast<const CallExpr *>(&expr))
      return resolveCallExpr(*call);

    return nullptr;
  }

  std::unique_ptr<ResolvedStmt> resolveStmt(const Stmt &stmt) {
    return resolveExpr(dynamic_cast<const Expr &>(stmt));
  };

  std::unique_ptr<ResolvedBlock> resolveBlock(const Block &block) {
    std::vector<std::unique_ptr<ResolvedStmt>> resolvedStatements;

    for (auto &&stmt : block.expressions) {
      if (auto resolvedStmt = resolveStmt(*stmt))
        resolvedStatements.emplace_back(std::move(resolvedStmt));
      else
        return nullptr;
    }

    return std::make_unique<ResolvedBlock>(block.location,
                                           std::move(resolvedStatements));
  }

  std::unique_ptr<ResolvedParamDecl> resolveParamDecl(const ParamDecl &param) {
    std::optional<Type> type = resolveType(param.type);

    if (!type) {
      return error(param.location, std::string{"parameter '"} +
                                       param.identifier + "' has invalid '" +
                                       param.type + "' type");
    }

    if (lookupDecl(param.identifier))
      return error(param.location, std::string{"parameter '"} +
                                       param.identifier + "' redeclared");

    return std::make_unique<ResolvedParamDecl>(param.location, param.identifier,
                                               *type);
  }

  std::unique_ptr<ResolvedFunctionDecl>
  resolveFunctionWithoutBody(const FunctionDecl &function) {
    std::vector<std::unique_ptr<ResolvedParamDecl>> resolvedParams;

    for (auto &&param : function.params) {
      if (auto resolvedParam = resolveParamDecl(*param)) {
        resolvedParams.emplace_back(std::move(resolvedParam));
      } else
        return nullptr;
    }

    std::optional<Type> type = resolveType(function.type);

    if (!type)
      return error(function.location,
                   std::string{"function '"} + function.identifier +
                       "' has invalid '" + function.type + "' type");

    if (type != Type::VOID)
      return error(function.location, "only void functions are supported");

    return std::make_unique<ResolvedFunctionDecl>(
        function.location, function.identifier, *type,
        std::move(resolvedParams), nullptr);
  };

public:
  explicit Sema(std::vector<std::unique_ptr<FunctionDecl>> TopLevel)
      : TopLevel(std::move(TopLevel)) {}
  std::vector<std::unique_ptr<ResolvedFunctionDecl>> resolve() {
    // FIXME: Use RAII
    Scopes.emplace_back();

    std::vector<std::unique_ptr<ResolvedFunctionDecl>> resolvedFunctions;

    // Hoist functions first.
    for (auto &&function : TopLevel)
      if (auto resolvedFunction = resolveFunctionWithoutBody(*function)) {
        Scopes.back().emplace_back(resolvedFunction.get());
        resolvedFunctions.emplace_back(std::move(resolvedFunction));
      }

    if (resolvedFunctions.size() != TopLevel.size())
      return {};

    // Create builtin functions.
    {
      auto block = std::make_unique<ResolvedBlock>(
          SourceLocation{}, std::vector<std::unique_ptr<ResolvedStmt>>{});
      auto param = std::make_unique<ResolvedParamDecl>(SourceLocation{}, "num",
                                                       Type::NUMBER);
      std::vector<std::unique_ptr<ResolvedParamDecl>> params;
      params.emplace_back(std::move(param));

      auto builtinPrint = std::make_unique<ResolvedFunctionDecl>(
          SourceLocation{}, "print", Type::VOID, std::move(params),
          std::move(block));

      Scopes.back().emplace_back(builtinPrint.get());
      resolvedFunctions.emplace_back(std::move(builtinPrint));
    }

    for (size_t i = 0; i < TopLevel.size(); ++i) {
      Scopes.emplace_back();

      for (auto &&param : resolvedFunctions[i]->params)
        Scopes.back().emplace_back(param.get());

      if (auto resolvedBody = resolveBlock(*TopLevel[i]->body))
        resolvedFunctions[i]->body = std::move(resolvedBody);
      else
        return {};

      Scopes.pop_back();
    }

    return std::move(resolvedFunctions);
  }
};

#endif // A_COMPILER_SEMA_H
