#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_AST_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_AST_H

#include <llvm/Support/ErrorHandling.h>

#include <memory>
#include <vector>

#include "lexer.h"
#include "utils.h"

namespace yl {

// FIXME: rework for better struct support
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

struct Decl {
  SourceLocation location;
  std::string identifier;

  Decl(SourceLocation location, std::string identifier)
      : location(location),
        identifier(std::move(identifier)) {}
  virtual ~Decl() = default;

  virtual void dump(size_t level = 0) const = 0;
};

struct Stmt {
  SourceLocation location;
  Stmt(SourceLocation location)
      : location(location) {}

  virtual ~Stmt() = default;

  virtual void dump(size_t level = 0) const = 0;
};

struct Expr : public Stmt {
  Expr(SourceLocation location)
      : Stmt(location) {}
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

struct ReturnStmt : public Stmt {
  std::unique_ptr<Expr> expr;

  ReturnStmt(SourceLocation location, std::unique_ptr<Expr> expr = nullptr)
      : Stmt(location),
        expr(std::move(expr)) {}

  void dump(size_t level = 0) const override;
};

struct MemberInitStmt : public Stmt {
  std::string identifier;
  std::unique_ptr<Expr> initializer;

  MemberInitStmt(SourceLocation location,
                 std::string identifier,
                 std::unique_ptr<Expr> initializer)
      : Stmt(location),
        identifier(identifier),
        initializer(std::move(initializer)) {}

  void dump(size_t level = 0) const override;
};

struct StructInstantiationExpr : public Expr {
  std::string identifier;
  std::vector<std::unique_ptr<MemberInitStmt>> memberInitializers;

  StructInstantiationExpr(
      SourceLocation location,
      std::string identifier,
      std::vector<std::unique_ptr<MemberInitStmt>> memberInitializers)
      : Expr(location),
        identifier(identifier),
        memberInitializers(std::move(memberInitializers)) {}

  void dump(size_t level = 0) const override;
};

struct NumberLiteral : public Expr {
  std::string value;

  NumberLiteral(SourceLocation location, std::string value)
      : Expr(location),
        value(value) {}

  void dump(size_t level = 0) const override;
};

struct CallExpr : public Expr {
  std::unique_ptr<Expr> callee;
  std::vector<std::unique_ptr<Expr>> arguments;

  CallExpr(SourceLocation location,
           std::unique_ptr<Expr> callee,
           std::vector<std::unique_ptr<Expr>> arguments)
      : Expr(location),
        callee(std::move(callee)),
        arguments(std::move(arguments)) {}

  void dump(size_t level = 0) const override;
};

struct AssignableExpr : public Expr {
  AssignableExpr(SourceLocation location)
      : Expr(location) {}
};

struct DeclRefExpr : public AssignableExpr {
  std::string identifier;

  DeclRefExpr(SourceLocation location, std::string identifier)
      : AssignableExpr(location),
        identifier(identifier) {}

  void dump(size_t level = 0) const override;
};

struct MemberExpr : public AssignableExpr {
  std::unique_ptr<Expr> base;
  std::string member;

  MemberExpr(SourceLocation location,
             std::unique_ptr<Expr> base,
             std::string member)
      : AssignableExpr(location),
        base(std::move(base)),
        member(std::move(member)) {}

  void dump(size_t level = 0) const override;
};

struct GroupingExpr : public Expr {
  std::unique_ptr<Expr> expr;

  GroupingExpr(SourceLocation location, std::unique_ptr<Expr> expr)
      : Expr(location),
        expr(std::move(expr)) {}

  void dump(size_t level = 0) const override;
};

struct BinaryOperator : public Expr {
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
  TokenKind op;

  BinaryOperator(SourceLocation location,
                 std::unique_ptr<Expr> lhs,
                 std::unique_ptr<Expr> rhs,
                 TokenKind op)
      : Expr(location),
        lhs(std::move(lhs)),
        rhs(std::move(rhs)),
        op(op) {}

  void dump(size_t level = 0) const override;
};

struct UnaryOperator : public Expr {
  std::unique_ptr<Expr> operand;
  TokenKind op;

  UnaryOperator(SourceLocation location,
                std::unique_ptr<Expr> operand,
                TokenKind op)
      : Expr(location),
        operand(std::move(operand)),
        op(op) {}

  void dump(size_t level = 0) const override;
};

struct MemberDecl : public Decl {
  Type type;

  MemberDecl(SourceLocation location, std::string identifier, Type type)
      : Decl(location, std::move(identifier)),
        type(std::move(type)) {}

  void dump(size_t level = 0) const override;
};

struct StructDecl : public Decl {
  std::vector<std::unique_ptr<MemberDecl>> members;

  StructDecl(SourceLocation location,
             std::string identifier,
             std::vector<std::unique_ptr<MemberDecl>> members)
      : Decl(location, std::move(identifier)),
        members(std::move(members)) {}

  void dump(size_t level = 0) const override;
};

struct ParamDecl : public Decl {
  Type type;
  ParamDecl(SourceLocation location, std::string identifier, Type type)
      : Decl(location, std::move(identifier)),
        type(std::move(type)) {}

  void dump(size_t level = 0) const override;
};

struct VarDecl : public Decl {
  std::optional<Type> type;
  std::unique_ptr<Expr> initializer;
  bool isMutable;

  VarDecl(SourceLocation location,
          std::string identifier,
          std::optional<Type> type,
          bool isMutable,
          std::unique_ptr<Expr> initializer = nullptr)
      : Decl(location, std::move(identifier)),
        type(std::move(type)),
        initializer(std::move(initializer)),
        isMutable(isMutable) {}

  void dump(size_t level = 0) const override;
};

struct FunctionDecl : public Decl {
  Type type;
  std::vector<std::unique_ptr<ParamDecl>> params;
  std::unique_ptr<Block> body;

  FunctionDecl(SourceLocation location,
               std::string identifier,
               Type type,
               std::vector<std::unique_ptr<ParamDecl>> params,
               std::unique_ptr<Block> body)
      : Decl(location, std::move(identifier)),
        type(std::move(type)),
        params(std::move(params)),
        body(std::move(body)) {}

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

struct ResolvedStmt {
  SourceLocation location;

  ResolvedStmt(SourceLocation location)
      : location(location) {}

  virtual ~ResolvedStmt() = default;

  virtual void dump(size_t level = 0) const = 0;
};

struct ResolvedExpr : public ConstantValueContainer<double>,
                      public ResolvedStmt {
  Type type;

  ResolvedExpr(SourceLocation location, Type type)
      : ResolvedStmt(location),
        type(type) {}

  virtual ~ResolvedExpr() = default;
};

struct ResolvedDecl {
  SourceLocation location;
  std::string identifier;
  Type type;
  bool isMutable;

  ResolvedDecl(SourceLocation location,
               std::string identifier,
               Type type,
               bool isMutable)
      : location(location),
        identifier(std::move(identifier)),
        type(type),
        isMutable(isMutable) {}
  virtual ~ResolvedDecl() = default;

  virtual void dump(size_t level = 0) const = 0;
};

struct ResolvedBlock {
  SourceLocation location;
  std::vector<std::unique_ptr<ResolvedStmt>> statements;

  ResolvedBlock(SourceLocation location,
                std::vector<std::unique_ptr<ResolvedStmt>> statements)
      : location(location),
        statements(std::move(statements)) {}

  void dump(size_t level = 0) const;
};

struct ResolvedIfStmt : public ResolvedStmt {
  std::unique_ptr<ResolvedExpr> condition;
  std::unique_ptr<ResolvedBlock> trueBlock;
  std::unique_ptr<ResolvedBlock> falseBlock;

  ResolvedIfStmt(SourceLocation location,
                 std::unique_ptr<ResolvedExpr> condition,
                 std::unique_ptr<ResolvedBlock> trueBlock,
                 std::unique_ptr<ResolvedBlock> falseBlock = nullptr)
      : ResolvedStmt(location),
        condition(std::move(condition)),
        trueBlock(std::move(trueBlock)),
        falseBlock(std::move(falseBlock)) {}

  void dump(size_t level = 0) const override;
};

struct ResolvedWhileStmt : public ResolvedStmt {
  std::unique_ptr<ResolvedExpr> condition;
  std::unique_ptr<ResolvedBlock> body;

  ResolvedWhileStmt(SourceLocation location,
                    std::unique_ptr<ResolvedExpr> condition,
                    std::unique_ptr<ResolvedBlock> body)
      : ResolvedStmt(location),
        condition(std::move(condition)),
        body(std::move(body)) {}

  void dump(size_t level = 0) const override;
};

struct ResolvedParamDecl : public ResolvedDecl {
  ResolvedParamDecl(SourceLocation location, std::string identifier, Type type)
      : ResolvedDecl(location, std::move(identifier), type, true) {}

  void dump(size_t level = 0) const override;
};

struct ResolvedMemberDecl : public ResolvedDecl {
  unsigned index;

  ResolvedMemberDecl(SourceLocation location,
                     std::string identifier,
                     Type type,
                     unsigned index)
      : ResolvedDecl(location, std::move(identifier), type, false),
        index(index) {}

  void dump(size_t level = 0) const override;
};

struct ResolvedVarDecl : public ResolvedDecl {
  std::unique_ptr<ResolvedExpr> initializer;

  ResolvedVarDecl(SourceLocation location,
                  std::string identifier,
                  Type type,
                  bool isMutable,
                  std::unique_ptr<ResolvedExpr> initializer = nullptr)
      : ResolvedDecl(location, std::move(identifier), type, isMutable),
        initializer(std::move(initializer)) {}

  void dump(size_t level = 0) const override;
};

struct ResolvedFunctionDecl : public ResolvedDecl {
  std::vector<std::unique_ptr<ResolvedParamDecl>> params;
  std::unique_ptr<ResolvedBlock> body;

  ResolvedFunctionDecl(SourceLocation location,
                       std::string identifier,
                       Type type,
                       std::vector<std::unique_ptr<ResolvedParamDecl>> params,
                       std::unique_ptr<ResolvedBlock> body)
      : ResolvedDecl(location, std::move(identifier), type, false),
        params(std::move(params)),
        body(std::move(body)) {}

  void dump(size_t level = 0) const override;
};

struct ResolvedStructDecl : public ResolvedDecl {
  std::vector<std::unique_ptr<ResolvedMemberDecl>> members;

  ResolvedStructDecl(SourceLocation location,
                     std::string identifier,
                     Type type,
                     std::vector<std::unique_ptr<ResolvedMemberDecl>> members)
      : ResolvedDecl(location, std::move(identifier), type, false),
        members(std::move(members)) {}

  void dump(size_t level = 0) const override;
};

struct ResolvedNumberLiteral : public ResolvedExpr {
  double value;

  ResolvedNumberLiteral(SourceLocation location, double value)
      : ResolvedExpr(location, Type::builtinNumber()),
        value(value) {}

  void dump(size_t level = 0) const override;
};

struct ResolvedCallExpr : public ResolvedExpr {
  const ResolvedFunctionDecl *callee;
  std::vector<std::unique_ptr<ResolvedExpr>> arguments;

  ResolvedCallExpr(SourceLocation location,
                   const ResolvedFunctionDecl &callee,
                   std::vector<std::unique_ptr<ResolvedExpr>> arguments)
      : ResolvedExpr(location, callee.type),
        callee(&callee),
        arguments(std::move(arguments)) {}

  void dump(size_t level = 0) const override;
};

struct ResolvedAssignableExpr : public ResolvedExpr {
  ResolvedAssignableExpr(SourceLocation location, Type type)
      : ResolvedExpr(location, type) {}
};

struct ResolvedDeclRefExpr : public ResolvedAssignableExpr {
  const ResolvedDecl *decl;

  ResolvedDeclRefExpr(SourceLocation location, ResolvedDecl &decl)
      : ResolvedAssignableExpr(location, decl.type),
        decl(&decl) {}

  void dump(size_t level = 0) const override;
};

struct ResolvedMemberExpr : public ResolvedAssignableExpr {
  std::unique_ptr<ResolvedExpr> base;
  const ResolvedMemberDecl *member;

  ResolvedMemberExpr(SourceLocation location,
                     std::unique_ptr<ResolvedExpr> base,
                     const ResolvedMemberDecl &member)
      : ResolvedAssignableExpr(location, member.type),
        base(std::move(base)),
        member(&member) {}

  void dump(size_t level = 0) const override;
};

struct ResolvedGroupingExpr : public ResolvedExpr {
  std::unique_ptr<ResolvedExpr> expr;

  ResolvedGroupingExpr(SourceLocation location,
                       std::unique_ptr<ResolvedExpr> expr)
      : ResolvedExpr(location, expr->type),
        expr(std::move(expr)) {}

  void dump(size_t level = 0) const override;
};

struct ResolvedBinaryOperator : public ResolvedExpr {
  TokenKind op;
  std::unique_ptr<ResolvedExpr> lhs;
  std::unique_ptr<ResolvedExpr> rhs;

  ResolvedBinaryOperator(SourceLocation location,
                         TokenKind op,
                         std::unique_ptr<ResolvedExpr> lhs,
                         std::unique_ptr<ResolvedExpr> rhs)
      : ResolvedExpr(location, lhs->type),
        op(op),
        lhs(std::move(lhs)),
        rhs(std::move(rhs)) {}

  void dump(size_t level = 0) const override;
};

struct ResolvedUnaryOperator : public ResolvedExpr {
  TokenKind op;
  std::unique_ptr<ResolvedExpr> operand;

  ResolvedUnaryOperator(SourceLocation location,
                        TokenKind op,
                        std::unique_ptr<ResolvedExpr> operand)
      : ResolvedExpr(location, operand->type),
        op(op),
        operand(std::move(operand)) {}

  void dump(size_t level = 0) const override;
};

struct ResolvedDeclStmt : public ResolvedStmt {
  std::unique_ptr<ResolvedVarDecl> varDecl;

  ResolvedDeclStmt(SourceLocation location,
                   std::unique_ptr<ResolvedVarDecl> varDecl)
      : ResolvedStmt(location),
        varDecl(std::move(varDecl)) {}

  void dump(size_t level = 0) const override;
};

struct ResolvedAssignment : public ResolvedStmt {
  std::unique_ptr<ResolvedAssignableExpr> assignee;
  std::unique_ptr<ResolvedExpr> expr;

  ResolvedAssignment(SourceLocation location,
                     std::unique_ptr<ResolvedAssignableExpr> assignee,
                     std::unique_ptr<ResolvedExpr> expr)
      : ResolvedStmt(location),
        assignee(std::move(assignee)),
        expr(std::move(expr)) {}

  void dump(size_t level = 0) const override;
};

struct ResolvedReturnStmt : public ResolvedStmt {
  std::unique_ptr<ResolvedExpr> expr;

  ResolvedReturnStmt(SourceLocation location,
                     std::unique_ptr<ResolvedExpr> expr = nullptr)
      : ResolvedStmt(location),
        expr(std::move(expr)) {}

  void dump(size_t level = 0) const override;
};

struct ResolvedMemberInitStmt : public ResolvedStmt {
  const ResolvedMemberDecl *member;
  std::unique_ptr<ResolvedExpr> initializer;

  ResolvedMemberInitStmt(SourceLocation location,
                         const ResolvedMemberDecl &member,
                         std::unique_ptr<ResolvedExpr> initializer)
      : ResolvedStmt(location),
        member(&member),
        initializer(std::move(initializer)) {}

  void dump(size_t level = 0) const override;
};

struct ResolvedStructInstantiationExpr : public ResolvedExpr {
  const ResolvedStructDecl *structDecl;
  std::vector<std::unique_ptr<ResolvedMemberInitStmt>> memberInitializers;

  ResolvedStructInstantiationExpr(
      SourceLocation location,
      const ResolvedStructDecl &structDecl,
      std::vector<std::unique_ptr<ResolvedMemberInitStmt>> memberInitializers)
      : ResolvedExpr(location, structDecl.type),
        structDecl(&structDecl),
        memberInitializers(std::move(memberInitializers)) {}

  void dump(size_t level = 0) const override;
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_AST_H
