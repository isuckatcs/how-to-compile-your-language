#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_AST_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_AST_H

#include <memory>
#include <vector>

#include "lexer.h"
#include "utils.h"

namespace yl {
namespace ast {
struct Node {
  const SourceLocation location;

  explicit Node(SourceLocation location)
      : location(location) {}

  virtual ~Node() = default;
  virtual void dump(size_t level = 0) const = 0;
};

struct SourceFile final : public Node {
  const std::vector<std::unique_ptr<ast::Node>> topLevel;
  const bool isComplete;

  SourceFile(SourceLocation eofLocation,
             std::vector<std::unique_ptr<ast::Node>> nodes,
             bool isComplete)
      : Node(eofLocation),
        topLevel(std::move(nodes)),
        isComplete(isComplete) {}

  void dump(size_t level = 0) const override;
};

struct Type : public Node {
protected:
  explicit Type(SourceLocation location)
      : Node(location) {}
};

struct BuiltinType final : public Type {
  enum class Kind { Unit, Number, Bool, Self };

  const Kind kind;

  BuiltinType(SourceLocation location, Kind kind)
      : Type(location),
        kind(kind) {}

  void dump(size_t level = 0) const override;
};

struct UserDefinedType final : public Type {
  const std::string identifier;
  const std::vector<std::unique_ptr<Type>> typeArguments;

  UserDefinedType(SourceLocation location,
                  std::string identifier,
                  std::vector<std::unique_ptr<Type>> typeArguments)
      : Type(location),
        identifier(std::move(identifier)),
        typeArguments(std::move(typeArguments)) {}

  void dump(size_t level = 0) const override;
};

struct RefModifier final : public Node {
  const bool isMut;

  RefModifier(SourceLocation location, bool isMut)
      : Node(location),
        isMut(isMut) {}

  void dump(size_t level = 0) const override;
};

struct ArgumentType final : public Type {
  const std::unique_ptr<RefModifier> refModifier;
  const std::unique_ptr<Type> type;

  ArgumentType(SourceLocation location,
               std::unique_ptr<RefModifier> refModifier,
               std::unique_ptr<Type> type)
      : Type(location),
        refModifier(std::move(refModifier)),
        type(std::move(type)) {}

  void dump(size_t level = 0) const override;
};

struct FunctionType final : public Type {
  const std::vector<std::unique_ptr<ArgumentType>> args;
  const std::unique_ptr<Type> ret;

  FunctionType(SourceLocation location,
               std::vector<std::unique_ptr<ArgumentType>> args,
               std::unique_ptr<Type> ret)
      : Type(location),
        args(std::move(args)),
        ret(std::move(ret)) {}

  void dump(size_t level = 0) const override;
};

struct PointerType final : public Type {
  const std::unique_ptr<Type> pointeeType;
  const bool isMut;

  PointerType(SourceLocation location,
              std::unique_ptr<Type> pointeeType,
              bool isMut)
      : Type(location),
        pointeeType(std::move(pointeeType)),
        isMut(isMut) {}

  void dump(size_t level = 0) const override;
};

struct AnyType final : public Type {
  const std::unique_ptr<UserDefinedType> type;

  AnyType(SourceLocation location, std::unique_ptr<UserDefinedType> type)
      : Type(location),
        type(std::move(type)) {}

  void dump(size_t level = 0) const override;
};

struct Decl : public Node {
  const std::string identifier;

  Decl(SourceLocation location, std::string identifier)
      : Node(location),
        identifier(std::move(identifier)) {}
};

struct Stmt : public Node {
protected:
  explicit Stmt(SourceLocation location)
      : Node(location) {}
};

struct Expr : public Stmt {
protected:
  explicit Expr(SourceLocation location)
      : Stmt(location) {}
};

struct Block final : public Node {
  const std::vector<std::unique_ptr<Stmt>> statements;

  Block(SourceLocation location, std::vector<std::unique_ptr<Stmt>> statements)
      : Node(location),
        statements(std::move(statements)) {}

  void dump(size_t level = 0) const override;
};

struct TraitConformance : public Node {
  const std::vector<std::unique_ptr<UserDefinedType>> traits;

  TraitConformance(SourceLocation location,
                   std::vector<std::unique_ptr<UserDefinedType>> traits)
      : Node(std::move(location)),
        traits(std::move(traits)) {}

  void dump(size_t level = 0) const override;
};

struct IfStmt final : public Stmt {
  const std::unique_ptr<Expr> condition;
  const std::unique_ptr<Block> trueBlock;
  const std::unique_ptr<Block> falseBlock;

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

struct WhileStmt final : public Stmt {
  const std::unique_ptr<Expr> condition;
  const std::unique_ptr<Block> body;

  WhileStmt(SourceLocation location,
            std::unique_ptr<Expr> condition,
            std::unique_ptr<Block> body)
      : Stmt(location),
        condition(std::move(condition)),
        body(std::move(body)) {}

  void dump(size_t level = 0) const override;
};

struct ReturnStmt final : public Stmt {
  const std::unique_ptr<Expr> expr;

  ReturnStmt(SourceLocation location, std::unique_ptr<Expr> expr = nullptr)
      : Stmt(location),
        expr(std::move(expr)) {}

  void dump(size_t level = 0) const override;
};

struct FieldInitStmt final : public Stmt {
  const std::string identifier;
  const std::unique_ptr<Expr> initializer;

  FieldInitStmt(SourceLocation location,
                std::string identifier,
                std::unique_ptr<Expr> initializer)
      : Stmt(location),
        identifier(identifier),
        initializer(std::move(initializer)) {}

  void dump(size_t level = 0) const override;
};

struct NumberLiteral final : public Expr {
  const std::string value;

  NumberLiteral(SourceLocation location, std::string value)
      : Expr(location),
        value(value) {}

  void dump(size_t level = 0) const override;
};

struct BoolLiteral final : public Expr {
  const std::string value;

  BoolLiteral(SourceLocation location, std::string value)
      : Expr(location),
        value(value) {}

  void dump(size_t level = 0) const override;
};

struct UnitLiteral final : public Expr {
  explicit UnitLiteral(SourceLocation location)
      : Expr(location) {}

  void dump(size_t level = 0) const override;
};

struct CallExpr final : public Expr {
  const std::unique_ptr<Expr> callee;
  const std::vector<std::unique_ptr<Expr>> arguments;

  CallExpr(SourceLocation location,
           std::unique_ptr<Expr> callee,
           std::vector<std::unique_ptr<Expr>> arguments)
      : Expr(location),
        callee(std::move(callee)),
        arguments(std::move(arguments)) {}

  void dump(size_t level = 0) const override;
};

struct TypeArgumentList final : public Node {
  const std::vector<std::unique_ptr<Type>> args;

  TypeArgumentList(SourceLocation location,
                   std::vector<std::unique_ptr<Type>> args)
      : Node(location),
        args(std::move(args)) {}

  void dump(size_t level = 0) const override;
};

struct DeclRefExpr final : public Expr {
  const std::string identifier;
  const std::unique_ptr<TypeArgumentList> typeArgumentList;

  DeclRefExpr(SourceLocation location,
              std::string identifier,
              std::unique_ptr<TypeArgumentList> typeArgumentList = nullptr)
      : Expr(location),
        identifier(identifier),
        typeArgumentList(std::move(typeArgumentList)) {}

  void dump(size_t level = 0) const override;
};

struct TraitSpecifier final : public Node {
  const std::unique_ptr<Type> type;
  const std::unique_ptr<UserDefinedType> trait;

  TraitSpecifier(SourceLocation location,
                 std::unique_ptr<Type> type,
                 std::unique_ptr<UserDefinedType> trait)
      : Node(location),
        type(std::move(type)),
        trait(std::move(trait)) {}

  void dump(size_t level = 0) const override;
};

struct PathExpr final : public Expr {
  const std::unique_ptr<TraitSpecifier> traitSpecifier;
  const std::vector<std::unique_ptr<DeclRefExpr>> fragments;

  PathExpr(std::unique_ptr<TraitSpecifier> traitSpecifier,
           std::vector<std::unique_ptr<DeclRefExpr>> fragments)
      : Expr(fragments.back()->location),
        traitSpecifier(std::move(traitSpecifier)),
        fragments(std::move(fragments)) {}

  void dump(size_t level = 0) const override;
};

struct StructInstantiationExpr final : public Expr {
  const std::unique_ptr<PathExpr> structRef;
  const std::vector<std::unique_ptr<FieldInitStmt>> fieldInitializers;

  StructInstantiationExpr(
      SourceLocation location,
      std::unique_ptr<PathExpr> structRef,
      std::vector<std::unique_ptr<FieldInitStmt>> fieldInitializers)
      : Expr(location),
        structRef(std::move(structRef)),
        fieldInitializers(std::move(fieldInitializers)) {}

  void dump(size_t level = 0) const override;
};

struct MemberExpr final : public Expr {
  const std::unique_ptr<Expr> base;
  const std::unique_ptr<DeclRefExpr> member;

  MemberExpr(SourceLocation location,
             std::unique_ptr<Expr> base,
             std::unique_ptr<DeclRefExpr> member)
      : Expr(location),
        base(std::move(base)),
        member(std::move(member)) {}

  void dump(size_t level = 0) const override;
};

struct GroupingExpr final : public Expr {
  const std::unique_ptr<Expr> expr;

  GroupingExpr(SourceLocation location, std::unique_ptr<Expr> expr)
      : Expr(location),
        expr(std::move(expr)) {}

  void dump(size_t level = 0) const override;
};

struct BinaryOperator final : public Expr {
  const std::unique_ptr<Expr> lhs;
  const std::unique_ptr<Expr> rhs;
  const TokenKind op;

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

struct UnaryOperator final : public Expr {
  const std::unique_ptr<Expr> operand;
  const TokenKind op;

  UnaryOperator(SourceLocation location,
                std::unique_ptr<Expr> operand,
                TokenKind op)
      : Expr(location),
        operand(std::move(operand)),
        op(op) {}

  void dump(size_t level = 0) const override;
};

struct TypeParamDecl final : public Decl {
  const std::unique_ptr<TraitConformance> traitConformance;

  TypeParamDecl(SourceLocation location,
                std::string identifier,
                std::unique_ptr<TraitConformance> traitConformance)
      : Decl(location, std::move(identifier)),
        traitConformance(std::move(traitConformance)) {}

  void dump(size_t level = 0) const override;
};

struct ParamDecl final : public Decl {
  const std::unique_ptr<ArgumentType> type;
  const bool isMutable;

  ParamDecl(SourceLocation location,
            std::string identifier,
            std::unique_ptr<ArgumentType> type,
            bool isMutable)
      : Decl(location, std::move(identifier)),
        type(std::move(type)),
        isMutable(isMutable) {}

  void dump(size_t level = 0) const override;
};

struct VarDecl final : public Decl {
  const std::unique_ptr<Type> type;
  const std::unique_ptr<Expr> initializer;
  const bool isMutable;

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

struct FunctionDecl final : public Decl {
  const std::unique_ptr<Type> type;
  const std::vector<std::unique_ptr<TypeParamDecl>> typeParameters;
  const std::vector<std::unique_ptr<ParamDecl>> params;
  const std::unique_ptr<Block> body;

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

struct FieldDecl final : public Decl {
  const std::unique_ptr<Type> type;

  FieldDecl(SourceLocation location,
            std::string identifier,
            std::unique_ptr<Type> type)
      : Decl(location, std::move(identifier)),
        type(std::move(type)) {}

  void dump(size_t level = 0) const override;
};

struct StructDecl final : public Decl {
  const std::vector<std::unique_ptr<TypeParamDecl>> typeParameters;
  const std::vector<std::unique_ptr<FieldDecl>> fields;

  StructDecl(SourceLocation location,
             std::string identifier,
             std::vector<std::unique_ptr<TypeParamDecl>> typeParameters,
             std::vector<std::unique_ptr<FieldDecl>> fields)
      : Decl(std::move(location), std::move(identifier)),
        typeParameters(std::move(typeParameters)),
        fields(std::move(fields)) {}

  void dump(size_t level = 0) const override;
};

struct TraitDecl final : public Decl {
  const std::unique_ptr<TraitConformance> traitConformance;
  const std::vector<std::unique_ptr<TypeParamDecl>> typeParameters;
  const std::vector<std::unique_ptr<FunctionDecl>> traitFunctions;

  TraitDecl(SourceLocation location,
            std::string identifier,
            std::unique_ptr<TraitConformance> traitConformance,
            std::vector<std::unique_ptr<TypeParamDecl>> typeParameters,
            std::vector<std::unique_ptr<FunctionDecl>> traitFunctions)
      : Decl(std::move(location), std::move(identifier)),
        traitConformance(std::move(traitConformance)),
        typeParameters(std::move(typeParameters)),
        traitFunctions(std::move(traitFunctions)) {}

  void dump(size_t level = 0) const override;
};

struct DeclStmt final : public Stmt {
  const std::unique_ptr<VarDecl> varDecl;

  DeclStmt(SourceLocation location, std::unique_ptr<VarDecl> varDecl)
      : Stmt(location),
        varDecl(std::move(varDecl)) {}

  void dump(size_t level = 0) const override;
};

struct Assignment final : public Stmt {
  const std::unique_ptr<Expr> assignee;
  const std::unique_ptr<Expr> expr;

  Assignment(SourceLocation location,
             std::unique_ptr<Expr> assignee,
             std::unique_ptr<Expr> expr)
      : Stmt(location),
        assignee(std::move(assignee)),
        expr(std::move(expr)) {}

  void dump(size_t level = 0) const override;
};

struct GCExpr final : public Expr {
  const std::unique_ptr<Expr> expr;
  const bool isMut;

  GCExpr(SourceLocation location, std::unique_ptr<Expr> expr, bool isMut)
      : Expr(location),
        expr(std::move(expr)),
        isMut(isMut) {}

  void dump(size_t level = 0) const override;
};

struct LambdaExpr final : public Expr {
  const std::vector<std::unique_ptr<ParamDecl>> params;
  const std::unique_ptr<Type> returnType;
  const std::unique_ptr<Block> body;

  LambdaExpr(SourceLocation location,
             std::vector<std::unique_ptr<ParamDecl>> params,
             std::unique_ptr<Type> returnType,
             std::unique_ptr<Block> body)
      : Expr(location),
        params(std::move(params)),
        returnType(std::move(returnType)),
        body(std::move(body)) {}

  void dump(size_t level = 0) const override;
};

struct TypeExtension final : public Node {
  const std::vector<std::unique_ptr<TypeParamDecl>> typeParams;
  const std::unique_ptr<Type> type;
  const std::unique_ptr<UserDefinedType> trait;
  const std::vector<std::unique_ptr<ast::FunctionDecl>> functions;

  TypeExtension(SourceLocation location,
                std::vector<std::unique_ptr<TypeParamDecl>> typeParams,
                std::unique_ptr<Type> type,
                std::unique_ptr<UserDefinedType> trait,
                std::vector<std::unique_ptr<ast::FunctionDecl>> functions)
      : Node(location),
        typeParams(std::move(typeParams)),
        type(std::move(type)),
        trait(std::move(trait)),
        functions(std::move(functions)) {}

  void dump(size_t level = 0) const override;
};
} // namespace ast
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_AST_H
