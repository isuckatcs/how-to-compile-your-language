#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_AST_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_AST_H

#include <iostream>
#include <memory>
#include <vector>

#include "utils.h"

namespace yl {
struct Type {
  enum class Kind { Void, Number, Custom };

  Kind kind;
  std::string name;

  static Type builtinVoid() { return {Kind::Void, "void"}; }
  static Type builtinNumber() { return {Kind::Number, "number"}; }
  static Type custom(const std::string &name) { return {Kind::Custom, name}; }

private:
  Type(Kind kind, std::string name) : kind(kind), name(std::move(name)){};
};

struct Decl : public Dumpable {
  SourceLocation location;
  std::string identifier;

  Decl(SourceLocation location, std::string identifier)
      : location(location), identifier(std::move(identifier)) {}
  virtual ~Decl() = default;
};

struct Stmt : public Dumpable {
  SourceLocation location;
  Stmt(SourceLocation location) : location(location) {}

  virtual ~Stmt() = default;
};

struct Expr : public Stmt {
  Expr(SourceLocation location) : Stmt(location) {}
};

struct Block : public Dumpable {
  SourceLocation location;
  std::vector<std::unique_ptr<Stmt>> statements;

  Block(SourceLocation location, std::vector<std::unique_ptr<Stmt>> statements)
      : location(location), statements(std::move(statements)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "Block\n";

    for (auto &&stmt : statements)
      stmt->dump(level + 1);
  }
};

struct ReturnStmt : public Stmt {
  std::unique_ptr<Expr> expr;

  ReturnStmt(SourceLocation location, std::unique_ptr<Expr> expr = nullptr)
      : Stmt(location), expr(std::move(expr)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "ReturnStmt\n";

    if (expr)
      expr->dump(level + 1);
  }
};

struct NumberLiteral : public Expr {
  std::string value;

  NumberLiteral(SourceLocation location, std::string value)
      : Expr(location), value(value) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "NumberLiteral: '" << value << "'\n";
  }
};

struct DeclRefExpr : public Expr {
  std::string identifier;

  DeclRefExpr(SourceLocation location, std::string identifier)
      : Expr(location), identifier(identifier) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "DeclRefExpr: " << identifier << '\n';
  }
};

struct CallExpr : public Expr {
  std::unique_ptr<DeclRefExpr> identifier;
  std::vector<std::unique_ptr<Expr>> arguments;

  CallExpr(SourceLocation location, std::unique_ptr<DeclRefExpr> identifier,
           std::vector<std::unique_ptr<Expr>> arguments)
      : Expr(location), identifier(std::move(identifier)),
        arguments(std::move(arguments)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "CallExpr:\n";

    identifier->dump(level + 1);

    for (auto &&arg : arguments)
      arg->dump(level + 1);
  }
};

struct ParamDecl : public Decl {
  Type type;
  ParamDecl(SourceLocation location, std::string identifier, Type type)
      : Decl{location, std::move(identifier)}, type(std::move(type)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "ParamDecl: " << identifier << ':'
              << type.name << '\n';
  }
};

struct FunctionDecl : public Decl {
  Type type;
  std::vector<std::unique_ptr<ParamDecl>> params;
  std::unique_ptr<Block> body;

  FunctionDecl(SourceLocation location, std::string identifier, Type type,
               std::vector<std::unique_ptr<ParamDecl>> params,
               std::unique_ptr<Block> body)
      : Decl{location, std::move(identifier)}, type(std::move(type)),
        params(std::move(params)), body(std::move(body)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level)
              << "FunctionDecl: " + identifier + ":" + type.name + "\n";

    for (auto &&param : params)
      param->dump(level + 1);

    body->dump(level + 1);
  }
};

struct ResolvedStmt : public Dumpable {
  SourceLocation location;

  ResolvedStmt(SourceLocation location) : location(location) {}

  virtual ~ResolvedStmt() = default;
};

struct ResolvedExpr : public ResolvedStmt {
  Type type;

  ResolvedExpr(SourceLocation location, Type type)
      : ResolvedStmt(location), type(type) {}

  virtual ~ResolvedExpr() = default;
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

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "ResolvedBlock\n";

    for (auto &&stmt : statements)
      stmt->dump(level + 1);
  }
};

struct ResolvedParamDecl : public ResolvedDecl {
  ResolvedParamDecl(SourceLocation location, std::string identifier, Type type)
      : ResolvedDecl{location, std::move(identifier), type} {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "ResolvedParamDecl: @(" << this << ") "
              << identifier << ":"
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
      : ResolvedDecl{location, std::move(identifier), type},
        params(std::move(params)), body(std::move(body)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "ResolvedFunctionDecl: @(" << this << ") "
              << identifier + ":" + "\n";

    for (auto &&param : params)
      param->dump(level + 1);

    body->dump(level + 1);
  }
};

struct ResolvedNumberLiteral : public ResolvedExpr {
  double value;

  ResolvedNumberLiteral(SourceLocation location, double value)
      : ResolvedExpr(location, Type::builtinNumber()), value(value) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "ResolvedNumberLiteral: '" << value << "'\n";
  }
};

struct ResolvedDeclRefExpr : public ResolvedExpr {
  const ResolvedDecl *decl;

  ResolvedDeclRefExpr(SourceLocation location, ResolvedDecl &decl)
      : ResolvedExpr(location, decl.type), decl(&decl) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "ResolvedDeclRefExpr: @(" << decl << ") "
              << decl->identifier << "\n";
  }
};

struct ResolvedCallExpr : public ResolvedExpr {
  const ResolvedFunctionDecl *callee;
  std::vector<std::unique_ptr<ResolvedExpr>> arguments;

  ResolvedCallExpr(SourceLocation location, const ResolvedFunctionDecl &callee,
                   std::vector<std::unique_ptr<ResolvedExpr>> arguments)
      : ResolvedExpr(location, callee.type), callee(&callee),
        arguments(std::move(arguments)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "ResolvedCallExpr: @(" << callee << ") "
              << callee->identifier << "\n";

    for (auto &&arg : arguments)
      arg->dump(level + 1);
  }
};

struct ResolvedReturnStmt : public ResolvedStmt {
  std::unique_ptr<ResolvedExpr> expr;

  ResolvedReturnStmt(SourceLocation location,
                     std::unique_ptr<ResolvedExpr> expr = nullptr)
      : ResolvedStmt(location), expr(std::move(expr)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "ResolvedReturnStmt\n";

    if (expr)
      expr->dump(level + 1);
  }
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_AST_H
