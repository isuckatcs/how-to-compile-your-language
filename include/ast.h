#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_AST_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_AST_H

#include <memory>
#include <vector>

#include "lexer.h"
#include "utils.h"

namespace yl {
namespace ast {
struct Type {
  SourceLocation location;

  Type(SourceLocation location)
      : location(location) {}
  virtual ~Type() = default;

  virtual void dump(size_t level = 0) const = 0;
};

struct BuiltinType : public Type {
  enum class Kind { Void, Number };

  Kind kind;

  BuiltinType(SourceLocation location, Kind kind)
      : Type(location),
        kind(kind) {}

  void dump(size_t level = 0) const override;
};

struct UserDefinedType : public Type {
  std::string identifier;
  std::vector<std::unique_ptr<Type>> typeArguments;

  UserDefinedType(SourceLocation location,
                  std::string identifier,
                  std::vector<std::unique_ptr<Type>> typeArguments)
      : Type(location),
        identifier(std::move(identifier)),
        typeArguments(std::move(typeArguments)) {}

  void dump(size_t level = 0) const override;
};

struct FunctionType : public Type {
  std::vector<std::unique_ptr<Type>> args;
  std::unique_ptr<Type> ret;

  FunctionType(SourceLocation location,
               std::vector<std::unique_ptr<Type>> args,
               std::unique_ptr<Type> ret)
      : Type(location),
        args(std::move(args)),
        ret(std::move(ret)) {}

  void dump(size_t level = 0) const override;
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

struct FieldInitStmt : public Stmt {
  std::string identifier;
  std::unique_ptr<Expr> initializer;

  FieldInitStmt(SourceLocation location,
                std::string identifier,
                std::unique_ptr<Expr> initializer)
      : Stmt(location),
        identifier(identifier),
        initializer(std::move(initializer)) {}

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

struct TypeArgumentList : public Expr {
  std::vector<std::unique_ptr<Type>> args;

  TypeArgumentList(SourceLocation location,
                   std::vector<std::unique_ptr<Type>> args)
      : Expr(location),
        args(std::move(args)) {}

  void dump(size_t level = 0) const override;
};

struct DeclRefExpr : public Expr {
  std::string identifier;
  std::unique_ptr<TypeArgumentList> typeArgumentList;

  DeclRefExpr(SourceLocation location,
              std::string identifier,
              std::unique_ptr<TypeArgumentList> typeArgumentList = nullptr)
      : Expr(location),
        identifier(identifier),
        typeArgumentList(std::move(typeArgumentList)) {}

  void dump(size_t level = 0) const override;
};

struct StructInstantiationExpr : public Expr {
  std::unique_ptr<DeclRefExpr> structRef;
  std::vector<std::unique_ptr<FieldInitStmt>> fieldInitializers;

  StructInstantiationExpr(
      SourceLocation location,
      std::unique_ptr<DeclRefExpr> structRef,
      std::vector<std::unique_ptr<FieldInitStmt>> fieldInitializers)
      : Expr(location),
        structRef(std::move(structRef)),
        fieldInitializers(std::move(fieldInitializers)) {}

  void dump(size_t level = 0) const override;
};

struct MemberExpr : public Expr {
  std::unique_ptr<Expr> base;
  std::unique_ptr<DeclRefExpr> member;

  MemberExpr(SourceLocation location,
             std::unique_ptr<Expr> base,
             std::unique_ptr<DeclRefExpr> member)
      : Expr(location),
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

struct TypeParamDecl : public Decl {
  TypeParamDecl(SourceLocation location, std::string identifier)
      : Decl(location, std::move(identifier)) {}

  void dump(size_t level = 0) const override;
};

struct FieldDecl : public Decl {
  std::unique_ptr<Type> type;

  FieldDecl(SourceLocation location,
            std::string identifier,
            std::unique_ptr<Type> type)
      : Decl(location, std::move(identifier)),
        type(std::move(type)) {}

  void dump(size_t level = 0) const override;
};

struct StructDecl : public Decl {
  std::vector<std::unique_ptr<TypeParamDecl>> typeParameters;
  std::vector<std::unique_ptr<FieldDecl>> fields;

  StructDecl(SourceLocation location,
             std::string identifier,
             std::vector<std::unique_ptr<TypeParamDecl>> typeParameters,
             std::vector<std::unique_ptr<FieldDecl>> fields)
      : Decl(location, std::move(identifier)),
        typeParameters(std::move(typeParameters)),
        fields(std::move(fields)) {}

  void dump(size_t level = 0) const override;
};

struct ParamDecl : public Decl {
  std::unique_ptr<Type> type;
  bool isMutable;

  ParamDecl(SourceLocation location,
            std::string identifier,
            std::unique_ptr<Type> type,
            bool isMutable)
      : Decl(location, std::move(identifier)),
        type(std::move(type)),
        isMutable(isMutable) {}

  void dump(size_t level = 0) const override;
};

struct VarDecl : public Decl {
  std::unique_ptr<Type> type;
  std::unique_ptr<Expr> initializer;
  bool isMutable;

  VarDecl(SourceLocation location,
          std::string identifier,
          std::unique_ptr<Type> type,
          bool isMutable,
          std::unique_ptr<Expr> initializer = nullptr)
      : Decl(location, std::move(identifier)),
        type(std::move(type)),
        initializer(std::move(initializer)),
        isMutable(isMutable) {}

  void dump(size_t level = 0) const override;
};

struct FunctionDecl : public Decl {
  std::unique_ptr<Type> type;
  std::vector<std::unique_ptr<TypeParamDecl>> typeParameters;
  std::vector<std::unique_ptr<ParamDecl>> params;
  std::unique_ptr<Block> body;

  FunctionDecl(SourceLocation location,
               std::string identifier,
               std::unique_ptr<Type> type,
               std::vector<std::unique_ptr<TypeParamDecl>> typeParameters,
               std::vector<std::unique_ptr<ParamDecl>> params,
               std::unique_ptr<Block> body)
      : Decl(location, std::move(identifier)),
        type(std::move(type)),
        typeParameters(std::move(typeParameters)),
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
  std::unique_ptr<Expr> assignee;
  std::unique_ptr<Expr> expr;

  Assignment(SourceLocation location,
             std::unique_ptr<Expr> assignee,
             std::unique_ptr<Expr> expr)
      : Stmt(location),
        assignee(std::move(assignee)),
        expr(std::move(expr)) {}

  void dump(size_t level = 0) const override;
};

// FIXME: support incremental parsing
struct Context {
  std::vector<std::unique_ptr<ast::Decl>> decls;

  std::vector<const StructDecl *> structs;
  std::vector<const FunctionDecl *> functions;

  void addStructDecl(std::unique_ptr<StructDecl> sd) {
    structs.emplace_back(sd.get());
    decls.emplace_back(std::move(sd));
  }

  void addFunctionDecl(std::unique_ptr<FunctionDecl> function) {
    functions.emplace_back(function.get());
    decls.emplace_back(std::move(function));
  }
};
} // namespace ast
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_AST_H
