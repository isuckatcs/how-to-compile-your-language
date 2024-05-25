#ifndef A_COMPILER_AST_H
#define A_COMPILER_AST_H

#include <iostream>
#include <memory>
#include <vector>

#include "utils.h"

struct Decl : public Dumpable {
  SourceLocation location;
  std::string identifier;

  Decl(SourceLocation location, std::string identifier)
      : location(location), identifier(std::move(identifier)) {}
  virtual ~Decl() = default;
};

struct Expr : public Dumpable {
  SourceLocation location;
  Expr(SourceLocation location) : location(std::move(location)) {}

  virtual ~Expr() = default;
};

struct Block : public Dumpable {
  SourceLocation location;
  std::vector<std::unique_ptr<Expr>> expressions;

  Block(SourceLocation location, std::vector<std::unique_ptr<Expr>> expressions)
      : location(location), expressions(std::move(expressions)) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "Block\n";

    for (auto &&stmt : expressions)
      stmt->dump(level + 1);
  }
};

struct NumberLiteral : public Expr {
  std::string value;

  NumberLiteral(SourceLocation location, std::string value)
      : Expr(std::move(location)), value(value) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "NumberLiteral: '" + value + "'\n";
  }
};

struct DeclRefExpr : public Expr {
  std::string identifier;

  DeclRefExpr(SourceLocation location, std::string identifier)
      : Expr(std::move(location)), identifier(identifier) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "DeclRefExpr: " + identifier + "\n";
  }
};

struct CallExpr : public Expr {
  std::unique_ptr<DeclRefExpr> identifier;
  std::vector<std::unique_ptr<Expr>> arguments;

  CallExpr(SourceLocation location, std::unique_ptr<DeclRefExpr> identifier,
           std::vector<std::unique_ptr<Expr>> arguments)
      : Expr(std::move(location)), identifier(std::move(identifier)),
        arguments(std::move(arguments)) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "CallExpr:\n";

    identifier->dump(level + 1);

    for (auto &&arg : arguments)
      arg->dump(level + 1);
  }
};

struct ParamDecl : public Decl {
  std::string type;
  ParamDecl(SourceLocation location, std::string identifier, std::string type)
      : Decl{std::move(location), std::move(identifier)},
        type(std::move(type)) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "ParamDecl: " + identifier + ":" + type + "\n";
  }
};

struct FunctionDecl : public Decl {
  std::string type;
  std::vector<std::unique_ptr<ParamDecl>> params;
  std::unique_ptr<Block> body;

  FunctionDecl(SourceLocation location, std::string identifier,
               std::string type, std::vector<std::unique_ptr<ParamDecl>> params,
               std::unique_ptr<Block> body)
      : Decl{std::move(location), std::move(identifier)}, type(std::move(type)),
        params(std::move(params)), body(std::move(body)) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "FunctionDecl: " + identifier + ":" + type + "\n";

    for (auto &&param : params)
      param->dump(level + 1);

    body->dump(level + 1);
  }
};

enum class Type { NUMBER, STRING, VOID };

struct ResolvedStmt : public Dumpable {
  SourceLocation location;

  ResolvedStmt(SourceLocation location) : location(location) {}
  virtual ~ResolvedStmt() = default;
};

struct ResolvedDecl : public Dumpable {
  SourceLocation location;
  std::string identifier;
  Type type;

  ResolvedDecl(SourceLocation location, std::string identifier, Type type)
      : location(location), identifier(std::move(identifier)), type(type) {}
  virtual ~ResolvedDecl() = default;
};

struct ResolvedBlock : public Dumpable {
  SourceLocation location;
  std::vector<std::unique_ptr<ResolvedStmt>> statements;

  ResolvedBlock(SourceLocation location,
                std::vector<std::unique_ptr<ResolvedStmt>> statements)
      : location(location), statements(std::move(statements)) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "ResolvedBlock\n";

    for (auto &&stmt : statements)
      stmt->dump(level + 1);
  }
};

struct ResolvedParamDecl : public ResolvedDecl {
  ResolvedParamDecl(SourceLocation location, std::string identifier, Type type)
      : ResolvedDecl{std::move(location), std::move(identifier), type} {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "ResolvedParamDecl: @(" << this << ") " << identifier << ":"
              << "\n";
  }
};

struct ResolvedFunctionDecl : public ResolvedDecl {
  std::vector<std::unique_ptr<ResolvedParamDecl>> params;
  std::unique_ptr<ResolvedBlock> body;

  ResolvedFunctionDecl(SourceLocation location, std::string identifier,
                       Type type,
                       std::vector<std::unique_ptr<ResolvedParamDecl>> params,
                       std::unique_ptr<ResolvedBlock> body)
      : ResolvedDecl{std::move(location), std::move(identifier), type},
        params(std::move(params)), body(std::move(body)) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "ResolvedFunctionDecl: @(" << this << ") "
              << identifier + ":" + "\n";

    for (auto &&param : params)
      param->dump(level + 1);

    body->dump(level + 1);
  }
};

struct ResolvedExpr : public ResolvedStmt {
  Type type;
  ResolvedExpr(SourceLocation location, Type type)
      : ResolvedStmt(std::move(location)), type(type) {}
};

struct ResolvedNumberLiteral : public ResolvedExpr {
  double value;

  ResolvedNumberLiteral(SourceLocation location, double value)
      : ResolvedExpr(std::move(location), Type::NUMBER), value(value) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "NumberLiteral: '" << value << "'\n";
  }
};

struct ResolvedDeclRefExpr : public ResolvedExpr {
  ResolvedDecl *decl;

  ResolvedDeclRefExpr(SourceLocation location, ResolvedDecl &decl)
      : ResolvedExpr(std::move(location), decl.type), decl(&decl) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "ResolvedDeclRefExpr: @(" << decl << ") " << decl->identifier
              << "\n";
  }
};

struct ResolvedCallExpr : public ResolvedExpr {
  const ResolvedFunctionDecl *callee;
  std::vector<std::unique_ptr<ResolvedExpr>> arguments;

  ResolvedCallExpr(SourceLocation location, const ResolvedFunctionDecl &callee,
                   std::vector<std::unique_ptr<ResolvedExpr>> arguments)
      : ResolvedExpr(std::move(location), callee.type), callee(&callee),
        arguments(std::move(arguments)) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "ResolvedCallExpr: @(" << callee << ") " << callee->identifier
              << "\n";

    for (auto &&arg : arguments)
      arg->dump(level + 1);
  }
};

#endif // A_COMPILER_AST_H
