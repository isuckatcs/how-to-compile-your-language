#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_RES_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_RES_H

#include <memory>
#include <vector>

#include "lexer.h"
#include "utils.h"

namespace yl {
namespace res {

// FIXME: introduce proper type system
struct Type {
  enum class Kind { Void, Number, Custom, Struct };

  Kind kind;
  std::string name;

  static Type builtinVoid() { return {Kind::Void, "void"}; }
  static Type builtinNumber() { return {Kind::Number, "number"}; }
  static Type custom(const std::string &name) { return {Kind::Custom, name}; }
  static Type structType(const std::string &id) { return {Kind::Struct, id}; }

private:
  Type(Kind kind, std::string name)
      : kind(kind),
        name(std::move(name)){};
};

struct Stmt {
  SourceLocation location;

  Stmt(SourceLocation location)
      : location(location) {}

  virtual ~Stmt() = default;

  virtual void dump(size_t level = 0) const = 0;
};

struct Expr : public ConstantValueContainer<double>, public Stmt {
  Type type;

  Expr(SourceLocation location, Type type)
      : Stmt(location),
        type(type) {}

  virtual ~Expr() = default;
};

struct Decl {
  SourceLocation location;
  std::string identifier;
  Type type;
  bool isMutable;

  Decl(SourceLocation location,
       std::string identifier,
       Type type,
       bool isMutable)
      : location(location),
        identifier(std::move(identifier)),
        type(type),
        isMutable(isMutable) {}
  virtual ~Decl() = default;

  virtual void dump(size_t level = 0) const = 0;
};

struct Block {
  SourceLocation location;
  std::vector<std::unique_ptr<Stmt>> statements;

  Block(SourceLocation location, std::vector<std::unique_ptr<Stmt>> statements)
      : location(location),
        statements(std::move(statements)) {}

  void dump(size_t level = 0) const;
};

struct IfStmt : public Stmt {
  std::unique_ptr<Expr> condition;
  std::unique_ptr<Block> trueBlock;
  std::unique_ptr<Block> falseBlock;

  IfStmt(SourceLocation location,
         std::unique_ptr<Expr> condition,
         std::unique_ptr<Block> trueBlock,
         std::unique_ptr<Block> falseBlock = nullptr)
      : Stmt(location),
        condition(std::move(condition)),
        trueBlock(std::move(trueBlock)),
        falseBlock(std::move(falseBlock)) {}

  void dump(size_t level = 0) const override;
};

struct WhileStmt : public Stmt {
  std::unique_ptr<Expr> condition;
  std::unique_ptr<Block> body;

  WhileStmt(SourceLocation location,
            std::unique_ptr<Expr> condition,
            std::unique_ptr<Block> body)
      : Stmt(location),
        condition(std::move(condition)),
        body(std::move(body)) {}

  void dump(size_t level = 0) const override;
};

struct ParamDecl : public Decl {
  ParamDecl(SourceLocation location,
            std::string identifier,
            Type type,
            bool isMutable)
      : Decl(location, std::move(identifier), type, isMutable) {}

  void dump(size_t level = 0) const override;
};

struct FieldDecl : public Decl {
  unsigned index;

  FieldDecl(SourceLocation location,
            std::string identifier,
            Type type,
            unsigned index)
      : Decl(location, std::move(identifier), type, false),
        index(index) {}

  void dump(size_t level = 0) const override;
};

struct VarDecl : public Decl {
  std::unique_ptr<Expr> initializer;

  VarDecl(SourceLocation location,
          std::string identifier,
          Type type,
          bool isMutable,
          std::unique_ptr<Expr> initializer = nullptr)
      : Decl(location, std::move(identifier), type, isMutable),
        initializer(std::move(initializer)) {}

  void dump(size_t level = 0) const override;
};

struct FunctionDecl : public Decl {
  std::vector<std::unique_ptr<ParamDecl>> params;
  std::unique_ptr<Block> body;

  FunctionDecl(SourceLocation location,
               std::string identifier,
               Type type,
               std::vector<std::unique_ptr<ParamDecl>> params,
               std::unique_ptr<Block> body)
      : Decl(location, std::move(identifier), type, false),
        params(std::move(params)),
        body(std::move(body)) {}

  void dump(size_t level = 0) const override;
};

struct StructDecl : public Decl {
  std::vector<std::unique_ptr<FieldDecl>> fields;

  StructDecl(SourceLocation location,
             std::string identifier,
             Type type,
             std::vector<std::unique_ptr<FieldDecl>> fields)
      : Decl(location, std::move(identifier), type, false),
        fields(std::move(fields)) {}

  void dump(size_t level = 0) const override;
};

struct NumberLiteral : public Expr {
  double value;

  NumberLiteral(SourceLocation location, double value)
      : Expr(location, Type::builtinNumber()),
        value(value) {}

  void dump(size_t level = 0) const override;
};

struct CallExpr : public Expr {
  const FunctionDecl *callee;
  std::vector<std::unique_ptr<Expr>> arguments;

  CallExpr(SourceLocation location,
           const FunctionDecl &callee,
           std::vector<std::unique_ptr<Expr>> arguments)
      : Expr(location, callee.type),
        callee(&callee),
        arguments(std::move(arguments)) {}

  void dump(size_t level = 0) const override;
};

struct AssignableExpr : public Expr {
  AssignableExpr(SourceLocation location, Type type)
      : Expr(location, type) {}
};

struct DeclRefExpr : public AssignableExpr {
  const Decl *decl;

  DeclRefExpr(SourceLocation location, Decl &decl)
      : AssignableExpr(location, decl.type),
        decl(&decl) {}

  void dump(size_t level = 0) const override;
};

struct MemberExpr : public AssignableExpr {
  std::unique_ptr<Expr> base;
  const FieldDecl *field;

  MemberExpr(SourceLocation location,
             std::unique_ptr<Expr> base,
             const FieldDecl &field)
      : AssignableExpr(location, field.type),
        base(std::move(base)),
        field(&field) {}

  void dump(size_t level = 0) const override;
};

struct GroupingExpr : public Expr {
  std::unique_ptr<Expr> expr;

  GroupingExpr(SourceLocation location, std::unique_ptr<Expr> expr)
      : Expr(location, expr->type),
        expr(std::move(expr)) {}

  void dump(size_t level = 0) const override;
};

struct BinaryOperator : public Expr {
  TokenKind op;
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;

  BinaryOperator(SourceLocation location,
                 TokenKind op,
                 std::unique_ptr<Expr> lhs,
                 std::unique_ptr<Expr> rhs)
      : Expr(location, lhs->type),
        op(op),
        lhs(std::move(lhs)),
        rhs(std::move(rhs)) {}

  void dump(size_t level = 0) const override;
};

struct UnaryOperator : public Expr {
  TokenKind op;
  std::unique_ptr<Expr> operand;

  UnaryOperator(SourceLocation location,
                TokenKind op,
                std::unique_ptr<Expr> operand)
      : Expr(location, operand->type),
        op(op),
        operand(std::move(operand)) {}

  void dump(size_t level = 0) const override;
};

struct DeclStmt : public Stmt {
  std::unique_ptr<VarDecl> varDecl;

  DeclStmt(SourceLocation location, std::unique_ptr<VarDecl> varDecl)
      : Stmt(location),
        varDecl(std::move(varDecl)) {}

  void dump(size_t level = 0) const override;
};

struct Assignment : public Stmt {
  std::unique_ptr<AssignableExpr> assignee;
  std::unique_ptr<Expr> expr;

  Assignment(SourceLocation location,
             std::unique_ptr<AssignableExpr> assignee,
             std::unique_ptr<Expr> expr)
      : Stmt(location),
        assignee(std::move(assignee)),
        expr(std::move(expr)) {}

  void dump(size_t level = 0) const override;
};

struct ReturnStmt : public Stmt {
  std::unique_ptr<Expr> expr;

  ReturnStmt(SourceLocation location, std::unique_ptr<Expr> expr = nullptr)
      : Stmt(location),
        expr(std::move(expr)) {}

  void dump(size_t level = 0) const override;
};

struct FieldInitStmt : public Stmt {
  const FieldDecl *field;
  std::unique_ptr<Expr> initializer;

  FieldInitStmt(SourceLocation location,
                const FieldDecl &field,
                std::unique_ptr<Expr> initializer)
      : Stmt(location),
        field(&field),
        initializer(std::move(initializer)) {}

  void dump(size_t level = 0) const override;
};

struct StructInstantiationExpr : public Expr {
  const StructDecl *structDecl;
  std::vector<std::unique_ptr<FieldInitStmt>> fieldInitializers;

  StructInstantiationExpr(
      SourceLocation location,
      const StructDecl &structDecl,
      std::vector<std::unique_ptr<FieldInitStmt>> fieldInitializers)
      : Expr(location, structDecl.type),
        structDecl(&structDecl),
        fieldInitializers(std::move(fieldInitializers)) {}

  void dump(size_t level = 0) const override;
};
} // namespace res
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_RES_H
