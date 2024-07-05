#ifndef A_COMPILER_AST_H
#define A_COMPILER_AST_H

#include <cassert>
#include <iostream>
#include <memory>
#include <vector>

#include "lexer.h"
#include "utils.h"

namespace {
std::string_view dumpOp(TokenKind op) {
  if (op == TokenKind::Plus)
    return "+";
  if (op == TokenKind::Minus)
    return "-";
  if (op == TokenKind::Asterisk)
    return "*";
  if (op == TokenKind::Slash)
    return "/";
  if (op == TokenKind::EqualEqual)
    return "==";
  if (op == TokenKind::AmpAmp)
    return "&&";
  if (op == TokenKind::PipePipe)
    return "||";
  if (op == TokenKind::Lt)
    return "<";
  if (op == TokenKind::Gt)
    return ">";
  if (op == TokenKind::Excl)
    return "!";

  assert(false && "unexpected operator");
}
} // namespace

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

struct IfStmt : public Stmt {
  std::unique_ptr<Expr> condition;
  std::unique_ptr<Block> trueBlock;
  std::unique_ptr<Block> falseBlock;

  IfStmt(SourceLocation location, std::unique_ptr<Expr> condition,
         std::unique_ptr<Block> trueBlock,
         std::unique_ptr<Block> falseBlock = nullptr)
      : Stmt(location), condition(std::move(condition)),
        trueBlock(std::move(trueBlock)), falseBlock(std::move(falseBlock)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "IfStmt\n";

    condition->dump(level + 1);
    trueBlock->dump(level + 1);
    if (falseBlock)
      falseBlock->dump(level + 1);
  }
};

struct WhileStmt : public Stmt {
  std::unique_ptr<Expr> condition;
  std::unique_ptr<Block> body;

  WhileStmt(SourceLocation location, std::unique_ptr<Expr> condition,
            std::unique_ptr<Block> body)
      : Stmt(location), condition(std::move(condition)), body(std::move(body)) {
  }

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "WhileStmt\n";

    condition->dump(level + 1);
    body->dump(level + 1);
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
    std::cerr << indent(level) << "NumberLiteral: '" + value + "'\n";
  }
};

struct DeclRefExpr : public Expr {
  std::string identifier;

  DeclRefExpr(SourceLocation location, std::string identifier)
      : Expr(location), identifier(identifier) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "DeclRefExpr: " + identifier + "\n";
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

struct GroupingExpr : public Expr {
  std::unique_ptr<Expr> expr;

  GroupingExpr(SourceLocation location, std::unique_ptr<Expr> expr)
      : Expr(location), expr(std::move(expr)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "GroupingExpr:\n";

    expr->dump(level + 1);
  }
};

struct BinaryOperator : public Expr {
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
  TokenKind op;

  BinaryOperator(SourceLocation location, std::unique_ptr<Expr> lhs,
                 std::unique_ptr<Expr> rhs, TokenKind op)
      : Expr(location), lhs(std::move(lhs)), rhs(std::move(rhs)), op(op) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "BinaryOperator: '" << dumpOp(op) << '\''
              << '\n';

    lhs->dump(level + 1);
    rhs->dump(level + 1);
  }
};

struct UnaryOperator : public Expr {
  std::unique_ptr<Expr> rhs;
  TokenKind op;

  UnaryOperator(SourceLocation location, std::unique_ptr<Expr> rhs,
                TokenKind op)
      : Expr(location), rhs(std::move(rhs)), op(op) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "UnaryOperator: '" << dumpOp(op) << '\''
              << '\n';

    rhs->dump(level + 1);
  }
};

struct ParamDecl : public Decl {
  std::string type;
  ParamDecl(SourceLocation location, std::string identifier, std::string type)
      : Decl{location, std::move(identifier)}, type(std::move(type)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level)
              << "ParamDecl: " + identifier + ":" + type + "\n";
  }
};

struct VarDecl : public Decl {
  std::string type;
  std::unique_ptr<Expr> initialzer;
  bool isMutable;

  VarDecl(SourceLocation location, std::string identifier, std::string type,
          bool isMutable, std::unique_ptr<Expr> initializer = nullptr)
      : Decl{location, std::move(identifier)}, type(std::move(type)),
        isMutable(isMutable), initialzer(std::move(initializer)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "VarDecl: " + identifier + ":" + type + "\n";
    if (initialzer)
      initialzer->dump(level + 1);
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

  void dump(size_t level = 0) const override {
    std::cerr << indent(level)
              << "FunctionDecl: " + identifier + ":" + type + "\n";

    for (auto &&param : params)
      param->dump(level + 1);

    body->dump(level + 1);
  }
};

struct DeclStmt : public Stmt {
  std::unique_ptr<VarDecl> varDecl;

  DeclStmt(SourceLocation location, std::unique_ptr<VarDecl> varDecl)
      : Stmt{location}, varDecl(std::move(varDecl)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "DeclStmt:\n";
    varDecl->dump(level + 1);
  }
};

struct Assignment : public Stmt {
  std::unique_ptr<DeclRefExpr> variable;
  std::unique_ptr<Expr> expr;

  Assignment(SourceLocation location, std::unique_ptr<DeclRefExpr> variable,
             std::unique_ptr<Expr> expr)
      : Stmt(location), variable(std::move(variable)), expr(std::move(expr)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "Assignment:\n";
    variable->dump(level + 1);
    expr->dump(level + 1);
  }
};

struct ResolvedStmt : public Dumpable {
  SourceLocation location;

  ResolvedStmt(SourceLocation location) : location(location) {}

  virtual ~ResolvedStmt() = default;
};

enum class Type { Number, Void };

struct ResolvedExpr : public ConstantValueContainer<ResolvedExpr, double>,
                      public ResolvedStmt {
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

struct ResolvedIfStmt : public ResolvedStmt {
  std::unique_ptr<ResolvedExpr> condition;
  std::unique_ptr<ResolvedBlock> trueBlock;
  std::unique_ptr<ResolvedBlock> falseBlock;

  ResolvedIfStmt(SourceLocation location,
                 std::unique_ptr<ResolvedExpr> condition,
                 std::unique_ptr<ResolvedBlock> trueBlock,
                 std::unique_ptr<ResolvedBlock> falseBlock = nullptr)
      : ResolvedStmt(location), condition(std::move(condition)),
        trueBlock(std::move(trueBlock)), falseBlock(std::move(falseBlock)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "ResolvedIfStmt\n";

    condition->dump(level + 1);
    trueBlock->dump(level + 1);
    if (falseBlock)
      falseBlock->dump(level + 1);
  }
};

struct ResolvedWhileStmt : public ResolvedStmt {
  std::unique_ptr<ResolvedExpr> condition;
  std::unique_ptr<ResolvedBlock> body;

  ResolvedWhileStmt(SourceLocation location,
                    std::unique_ptr<ResolvedExpr> condition,
                    std::unique_ptr<ResolvedBlock> body)
      : ResolvedStmt(location), condition(std::move(condition)),
        body(std::move(body)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "ResolvedWhileStmt\n";

    condition->dump(level + 1);
    body->dump(level + 1);
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

struct ResolvedVarDecl : public ResolvedDecl {
  std::unique_ptr<ResolvedExpr> initializer;
  bool isMutable;

  ResolvedVarDecl(SourceLocation location, std::string identifier, Type type,
                  bool isMutable,
                  std::unique_ptr<ResolvedExpr> initializer = nullptr)
      : ResolvedDecl{location, std::move(identifier), type},
        isMutable(isMutable), initializer(std::move(initializer)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "ResolvedVarDecl: @(" << this << ") "
              << identifier << ":"
              << "\n";
    if (initializer)
      initializer->dump(level + 1);
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
      : ResolvedExpr(location, Type::Number), value(value) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "NumberLiteral: '" << value << "'\n";
    if (auto val = getConstantValue())
      std::cerr << indent(level) << "| value: " << *val << '\n';
  }
};

struct ResolvedDeclRefExpr : public ResolvedExpr {
  const ResolvedDecl *decl;

  ResolvedDeclRefExpr(SourceLocation location, ResolvedDecl &decl)
      : ResolvedExpr(location, decl.type), decl(&decl) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "ResolvedDeclRefExpr: @(" << decl << ") "
              << decl->identifier << "\n";
    if (auto val = getConstantValue())
      std::cerr << indent(level) << "| value: " << *val << '\n';
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
    if (auto val = getConstantValue())
      std::cerr << indent(level) << "| value: " << *val << '\n';

    for (auto &&arg : arguments)
      arg->dump(level + 1);
  }
};

struct ResolvedGroupingExpr : public ResolvedExpr {
  std::unique_ptr<ResolvedExpr> expr;

  ResolvedGroupingExpr(SourceLocation location,
                       std::unique_ptr<ResolvedExpr> expr)
      : ResolvedExpr(location, expr->type), expr(std::move(expr)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "ResolvedGroupingExpr:\n";
    if (auto val = getConstantValue())
      std::cerr << indent(level) << "| value: " << *val << '\n';

    expr->dump(level + 1);
  }
};

struct ResolvedBinaryOperator : public ResolvedExpr {
  std::unique_ptr<ResolvedExpr> lhs;
  std::unique_ptr<ResolvedExpr> rhs;
  TokenKind op;

  ResolvedBinaryOperator(SourceLocation location,
                         std::unique_ptr<ResolvedExpr> lhs,
                         std::unique_ptr<ResolvedExpr> rhs, TokenKind op)
      : ResolvedExpr(location, lhs->type), lhs(std::move(lhs)),
        rhs(std::move(rhs)), op(op) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "ResolvedBinaryOperator: '" << dumpOp(op)
              << '\'' << '\n';

    if (auto val = getConstantValue())
      std::cerr << indent(level) << "| value: " << *val << '\n';

    lhs->dump(level + 1);
    rhs->dump(level + 1);
  }
};

struct ResolvedUnaryOperator : public ResolvedExpr {
  std::unique_ptr<ResolvedExpr> rhs;
  TokenKind op;

  ResolvedUnaryOperator(SourceLocation location,
                        std::unique_ptr<ResolvedExpr> rhs, TokenKind op)
      : ResolvedExpr(location, rhs->type), rhs(std::move(rhs)), op(op) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "ResolvedUnaryOperator: '" << dumpOp(op)
              << '\'' << '\n';

    if (auto val = getConstantValue())
      std::cerr << indent(level) << "| value: " << *val << '\n';

    rhs->dump(level + 1);
  }
};

struct ResolvedDeclStmt : public ResolvedStmt {
  std::unique_ptr<ResolvedVarDecl> varDecl;

  ResolvedDeclStmt(SourceLocation location,
                   std::unique_ptr<ResolvedVarDecl> varDecl)
      : ResolvedStmt{location}, varDecl(std::move(varDecl)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "ResolvedDeclStmt:\n";
    varDecl->dump(level + 1);
  }
};

struct ResolvedAssignment : public ResolvedStmt {
  std::unique_ptr<ResolvedDeclRefExpr> variable;
  std::unique_ptr<ResolvedExpr> expr;

  ResolvedAssignment(SourceLocation location,
                     std::unique_ptr<ResolvedDeclRefExpr> variable,
                     std::unique_ptr<ResolvedExpr> expr)
      : ResolvedStmt(location), variable(std::move(variable)),
        expr(std::move(expr)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "ResolvedAssignment:\n";
    variable->dump(level + 1);
    expr->dump(level + 1);
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

#endif // A_COMPILER_AST_H
