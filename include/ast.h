#ifndef A_COMPILER_AST_H
#define A_COMPILER_AST_H

#include <iostream>
#include <memory>
#include <vector>

#include "lexer.h"
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
  Expr(SourceLocation location) : location(location) {}

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
      : Expr(location), value(value) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "NumberLiteral: '" + value + "'\n";
  }
};

struct DeclRefExpr : public Expr {
  std::string identifier;

  DeclRefExpr(SourceLocation location, std::string identifier)
      : Expr(location), identifier(identifier) {}

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
      : Expr(location), identifier(std::move(identifier)),
        arguments(std::move(arguments)) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "CallExpr:\n";

    identifier->dump(level + 1);

    for (auto &&arg : arguments)
      arg->dump(level + 1);
  }
};

struct GroupingExpr : public Expr {
  std::unique_ptr<Expr> expr;

  GroupingExpr(SourceLocation location, std::unique_ptr<Expr> expr)
      : Expr(location), expr(std::move(expr)) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "GroupingExpr:\n";

    expr->dump(level + 1);
  }
};

struct BinaryOperator : public Expr {
  std::unique_ptr<Expr> LHS;
  std::unique_ptr<Expr> RHS;
  TokenKind op;

  BinaryOperator(SourceLocation location, std::unique_ptr<Expr> lhs,
                 std::unique_ptr<Expr> rhs, TokenKind op)
      : Expr(location), LHS(std::move(lhs)), RHS(std::move(rhs)), op(op) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "BinaryOperator: '";
    if (op == TokenKind::plus)
      std::cerr << '+';
    if (op == TokenKind::minus)
      std::cerr << '-';
    if (op == TokenKind::asterisk)
      std::cerr << '*';
    if (op == TokenKind::slash)
      std::cerr << '/';
    std::cerr << '\'' << '\n';

    LHS->dump(level + 1);
    RHS->dump(level + 1);
  }
};

struct ParamDecl : public Decl {
  std::string type;
  ParamDecl(SourceLocation location, std::string identifier, std::string type)
      : Decl{location, std::move(identifier)}, type(std::move(type)) {}

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
      : Decl{location, std::move(identifier)}, type(std::move(type)),
        params(std::move(params)), body(std::move(body)) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "FunctionDecl: " + identifier + ":" + type + "\n";

    for (auto &&param : params)
      param->dump(level + 1);

    body->dump(level + 1);
  }
};

enum class Type { NUMBER, VOID };

struct ResolvedExpr : public Dumpable {
  SourceLocation location;
  Type type;
  ResolvedExpr(SourceLocation location, Type type)
      : location(location), type(type) {}

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
  std::vector<std::unique_ptr<ResolvedExpr>> expressions;

  ResolvedBlock(SourceLocation location,
                std::vector<std::unique_ptr<ResolvedExpr>> statements)
      : location(location), expressions(std::move(statements)) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "ResolvedBlock\n";

    for (auto &&stmt : expressions)
      stmt->dump(level + 1);
  }
};

struct ResolvedParamDecl : public ResolvedDecl {
  ResolvedParamDecl(SourceLocation location, std::string identifier, Type type)
      : ResolvedDecl{location, std::move(identifier), type} {}

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
      : ResolvedDecl{location, std::move(identifier), type},
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

struct ResolvedNumberLiteral : public ResolvedExpr {
  double value;

  ResolvedNumberLiteral(SourceLocation location, double value)
      : ResolvedExpr(location, Type::NUMBER), value(value) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "NumberLiteral: '" << value << "'\n";
  }
};

struct ResolvedDeclRefExpr : public ResolvedExpr {
  const ResolvedDecl *decl;

  ResolvedDeclRefExpr(SourceLocation location, ResolvedDecl &decl)
      : ResolvedExpr(location, decl.type), decl(&decl) {}

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
      : ResolvedExpr(location, callee.type), callee(&callee),
        arguments(std::move(arguments)) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "ResolvedCallExpr: @(" << callee << ") " << callee->identifier
              << "\n";

    for (auto &&arg : arguments)
      arg->dump(level + 1);
  }
};

struct ResolvedGroupingExpr : public ResolvedExpr {
  std::unique_ptr<ResolvedExpr> expr;

  ResolvedGroupingExpr(SourceLocation location,
                       std::unique_ptr<ResolvedExpr> expr)
      : ResolvedExpr(location, expr->type), expr(std::move(expr)) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "ResolvedGroupingExpr:\n";

    expr->dump(level + 1);
  }
};

struct ResolvedBinaryOperator : public ResolvedExpr {
  std::unique_ptr<ResolvedExpr> LHS;
  std::unique_ptr<ResolvedExpr> RHS;
  TokenKind op;

  ResolvedBinaryOperator(SourceLocation location,
                         std::unique_ptr<ResolvedExpr> lhs,
                         std::unique_ptr<ResolvedExpr> rhs, TokenKind op)
      : ResolvedExpr(location, lhs->type), LHS(std::move(lhs)),
        RHS(std::move(rhs)), op(op) {}

  void dump(size_t level = 0) override {
    indent(level);
    std::cerr << "ResolvedBinaryOperator: '";
    if (op == TokenKind::plus)
      std::cerr << '+';
    if (op == TokenKind::minus)
      std::cerr << '-';
    if (op == TokenKind::asterisk)
      std::cerr << '*';
    if (op == TokenKind::slash)
      std::cerr << '/';
    std::cerr << '\'' << '\n';

    LHS->dump(level + 1);
    RHS->dump(level + 1);
  }
};

#endif // A_COMPILER_AST_H
