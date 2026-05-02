#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_RES_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_RES_H

#include <memory>
#include <utility>
#include <variant>
#include <vector>

#include "lexer.h"
#include "type.h"
#include "utils.h"

namespace yl {
namespace res {
struct ConstVal : public std::variant<std::monostate, bool, double> {
  using std::variant<std::monostate, bool, double>::variant;

  bool isKnown() const { return index() != 0; }
  std::string asString() const;
};

class TypedNode {
  Type *type;

public:
  explicit TypedNode(Type *type)
      : type(type) {}

  Type *getType() { return type; }
  const Type *getType() const { return type; }
};

struct Stmt {
  SourceLocation location;

  Stmt(SourceLocation location)
      : location(location) {}

  virtual ~Stmt() = default;

  virtual void dump(size_t level = 0) const = 0;
};

struct Expr : public TypedNode, public Stmt {
  enum class Kind { Rvalue, MutLvalue, Lvalue };

  Kind kind;
  ConstVal constVal;

  Expr(SourceLocation location, Type *type, Kind kind)
      : TypedNode(type),
        Stmt(location),
        kind(kind) {}

  bool isLvalue() const { return kind != Kind::Rvalue; }
  bool isMutable() const { return kind == Kind::MutLvalue; }

  bool hasConstantValue() const { return constVal.isKnown(); }
  ConstVal getConstantValue() const { return constVal; }
  void setConstantValue(ConstVal val) { constVal = val; }

  virtual ~Expr() = default;
};

struct TypeParamDecl;

struct Decl : public TypedNode {
  SourceLocation location;

  std::string identifier;
  std::vector<TypeParamDecl *> typeParams;

  Decl(SourceLocation location,
       Type *type,
       std::string identifier,
       std::vector<TypeParamDecl *> typeParams = {})
      : TypedNode(type),
        location(location),
        identifier(std::move(identifier)),
        typeParams(std::move(typeParams)) {}
  virtual ~Decl() = default;

  template <typename T> T *getAs() {
    return const_cast<T *>(const_cast<const Decl *>(this)->getAs<T>());
  }

  template <typename T> const T *getAs() const {
    static_assert(std::is_base_of_v<Decl, T>, "expected decl");
    return dynamic_cast<const T *>(this);
  }

  bool isGeneric() const { return !typeParams.empty(); }
  virtual void dump(size_t level = 0) const = 0;
};

struct DeclContext {
  DeclContext *parent;
  std::vector<res::Decl *> decls;

  DeclContext(DeclContext *parent)
      : parent(parent) {}
  virtual ~DeclContext() = default;

  bool insertDecl(res::Decl *decl);

  template <typename T> T *lookupDecl(const std::string id) const {
    for (auto &&decl : decls) {
      auto *correctDecl = dynamic_cast<T *>(decl);

      if (!correctDecl)
        continue;

      if (decl->identifier != id)
        continue;

      return correctDecl;
    }

    if (!parent)
      return nullptr;

    return parent->lookupDecl<T>(id);
  }

  template <typename T> std::vector<T *> getAll() const {
    std::vector<T *> out;
    for (auto &&decl : decls)
      if (auto *d = dynamic_cast<T *>(decl))
        out.emplace_back(d);
    return out;
  }
};

struct TypeDecl : public Decl {
  TypeDecl(SourceLocation location,
           Type *type,
           std::string identifier,
           std::vector<TypeParamDecl *> typeParams = {})
      : Decl(location, type, std::move(identifier), std::move(typeParams)) {}
};

struct ValueDecl : public Decl {
  bool isMutable;

  ValueDecl(SourceLocation location,
            Type *type,
            std::string identifier,
            bool isMutable,
            std::vector<TypeParamDecl *> typeParams = {})
      : Decl(location, type, std::move(identifier), std::move(typeParams)),
        isMutable(isMutable) {}
};

struct Block {
  SourceLocation location;
  std::vector<Stmt *> statements;

  Block(SourceLocation location, std::vector<Stmt *> statements)
      : location(location),
        statements(std::move(statements)) {}

  void dump(size_t level = 0) const;
};

struct IfStmt : public Stmt {
  Expr *condition;
  Block *trueBlock;
  Block *falseBlock;

  IfStmt(SourceLocation location,
         Expr *condition,
         Block *trueBlock,
         Block *falseBlock = nullptr)
      : Stmt(location),
        condition(condition),
        trueBlock(trueBlock),
        falseBlock(falseBlock) {}

  void dump(size_t level = 0) const override;
};

struct WhileStmt : public Stmt {
  Expr *condition;
  Block *body;

  WhileStmt(SourceLocation location, Expr *condition, Block *body)
      : Stmt(location),
        condition(condition),
        body(body) {}

  void dump(size_t level = 0) const override;
};

struct ParamDecl : public ValueDecl {
  ParamDecl(SourceLocation location,
            Type *type,
            std::string identifier,
            bool isMutable)
      : ValueDecl(location, type, std::move(identifier), isMutable) {}

  void dump(size_t level = 0) const override;
};

struct TraitInstance;
struct TraitDecl : public Decl, public DeclContext {
  std::vector<TraitInstance *> traits;

  TraitDecl(SourceLocation location,
            Type *type,
            std::string identifier,
            std::vector<TypeParamDecl *> typeParams)
      : Decl(location, type, std::move(identifier), std::move(typeParams)),
        DeclContext(nullptr) {}

  void dump(size_t level = 0) const override;
};

struct TraitInstance : public TypedNode {
  SourceLocation location;
  TraitDecl *decl;

  std::vector<res::Type *> typeArgs;
  std::vector<yl::SourceLocation> typeLocations;

  TraitInstance(yl::SourceLocation location,
                Type *type,
                TraitDecl *decl,
                std::vector<res::Type *> typeArgs,
                std::vector<yl::SourceLocation> typeLocations)
      : TypedNode(type),
        location(location),
        decl(decl),
        typeArgs(std::move(typeArgs)),
        typeLocations(std::move(typeLocations)) {}

  void dump(size_t level = 0) const;
};

struct TypeParamDecl : public TypeDecl {
  std::vector<TraitInstance *> traits;

  TypeParamDecl(SourceLocation location, Type *type, std::string identifier)
      : TypeDecl(location, type, std::move(identifier)) {}

  void dump(size_t level = 0) const override;
};

struct FieldDecl : public ValueDecl {
  FieldDecl(SourceLocation location, Type *type, std::string identifier)
      : ValueDecl(location, type, std::move(identifier), false) {}

  void dump(size_t level = 0) const override;
};

struct VarDecl : public ValueDecl {
  Expr *initializer;

  VarDecl(SourceLocation location,
          Type *type,
          std::string identifier,
          bool isMutable,
          Expr *initializer = nullptr)
      : ValueDecl(location, type, std::move(identifier), isMutable),
        initializer(initializer) {}

  void dump(size_t level = 0) const override;
};

struct ImplDecl : public Decl, public DeclContext {
  TraitInstance *traitInstance;

  ImplDecl(SourceLocation location,
           std::string identifier,
           TraitInstance *traitInstance)
      : Decl(location, traitInstance->getType(), identifier),
        DeclContext(nullptr),
        traitInstance(traitInstance) {}

  void dump(size_t level = 0) const;
};

struct StructDecl : public TypeDecl, public DeclContext {
  StructDecl(SourceLocation location,
             Type *type,
             std::string identifier,
             std::vector<TypeParamDecl *> typeParams)
      : TypeDecl(location, type, std::move(identifier), std::move(typeParams)),
        DeclContext(nullptr) {}

  void dump(size_t level = 0) const override;
};

struct FunctionDecl : public ValueDecl {
  std::vector<ParamDecl *> params;
  Decl *parent = nullptr;
  FunctionDecl *implements = nullptr;
  Block *body = nullptr;
  bool isComplete = false;

  FunctionDecl(SourceLocation location,
               Type *type,
               std::string identifier,
               std::vector<TypeParamDecl *> typeParams,
               std::vector<ParamDecl *> params,
               Decl *parent = nullptr,
               FunctionDecl *implements = nullptr)
      : ValueDecl(location,
                  type,
                  std::move(identifier),
                  false,
                  std::move(typeParams)),
        params(std::move(params)),
        parent(parent),
        implements(implements) {}

  void setBody(Block *body);

  void dump(size_t level = 0) const override;
};

struct NumberLiteral : public Expr {
  double value;

  NumberLiteral(SourceLocation location, Type *type, double value)
      : Expr(location, type, Expr::Kind::Rvalue),
        value(value) {}

  void dump(size_t level = 0) const override;
};

struct BoolLiteral : public Expr {
  bool value;

  BoolLiteral(SourceLocation location, Type *type, bool value)
      : Expr(location, type, Expr::Kind::Rvalue),
        value(value) {}

  void dump(size_t level = 0) const override;
};

struct UnitLiteral : public Expr {
  UnitLiteral(SourceLocation location, Type *type)
      : Expr(location, type, Expr::Kind::Rvalue) {}

  void dump(size_t level = 0) const override;
};

struct CallExpr : public Expr {
  Expr *callee;
  std::vector<Expr *> arguments;

  CallExpr(SourceLocation location,
           Type *type,
           Expr *callee,
           std::vector<Expr *> arguments)
      : Expr(location, type, Expr::Kind::Rvalue),
        callee(callee),
        arguments(std::move(arguments)) {}

  void dump(size_t level = 0) const override;
};

struct DeclRefExpr : public Expr {
  std::vector<Type *> typeArgs;
  const Decl *decl;

  Type *parentTy;
  TraitType *trait;

  DeclRefExpr(SourceLocation location,
              Type *type,
              Decl &decl,
              Expr::Kind kind,
              std::vector<Type *> typeArgs = {},
              Type *parentTy = nullptr,
              TraitType *trait = nullptr)
      : Expr(location, type, kind),
        typeArgs(std::move(typeArgs)),
        decl(&decl),
        parentTy(parentTy),
        trait(trait) {}

  void dump(size_t level = 0) const override;
};

struct PathExpr : public Expr {
  std::vector<DeclRefExpr *> fragments;

  explicit PathExpr(std::vector<DeclRefExpr *> fragments)
      : Expr(fragments.back()->location,
             fragments.back()->getType(),
             fragments.back()->kind),
        fragments(std::move(fragments)) {}

  void dump(size_t level = 0) const override;
};

struct MemberExpr : public Expr {
  Expr *base;
  DeclRefExpr *member;

  MemberExpr(SourceLocation location, Expr *base, DeclRefExpr *member)
      : Expr(location,
             member->getType(),
             !base->isLvalue() ? Expr::Kind::MutLvalue : base->kind),
        base(base),
        member(member) {}

  void dump(size_t level = 0) const override;
};

struct GroupingExpr : public Expr {
  Expr *expr;

  GroupingExpr(SourceLocation location, Expr *expr)
      : Expr(location, expr->getType(), expr->kind),
        expr(expr) {}

  void dump(size_t level = 0) const override;
};

struct BinaryOperator : public Expr {
  TokenKind op;
  Expr *lhs;
  Expr *rhs;

  BinaryOperator(
      SourceLocation location, Type *type, TokenKind op, Expr *lhs, Expr *rhs)
      : Expr(location, type, Expr::Kind::Rvalue),
        op(op),
        lhs(lhs),
        rhs(rhs) {}

  void dump(size_t level = 0) const override;
};

struct UnaryOperator : public Expr {
  TokenKind op;
  Expr *operand;

  UnaryOperator(SourceLocation location,
                Type *type,
                TokenKind op,
                Expr *operand)
      : Expr(location, type, Expr::Kind::Rvalue),
        op(op),
        operand(operand) {}

  void dump(size_t level = 0) const override;
};

struct DeclStmt : public Stmt {
  VarDecl *varDecl;

  DeclStmt(SourceLocation location, VarDecl *varDecl)
      : Stmt(location),
        varDecl(varDecl) {}

  void dump(size_t level = 0) const override;
};

struct Assignment : public Stmt {
  Expr *assignee;
  Expr *expr;

  Assignment(SourceLocation location, Expr *assignee, Expr *expr)
      : Stmt(location),
        assignee(assignee),
        expr(expr) {}

  void dump(size_t level = 0) const override;
};

struct ReturnStmt : public Stmt {
  Expr *expr;

  ReturnStmt(SourceLocation location, Expr *expr = nullptr)
      : Stmt(location),
        expr(expr) {}

  void dump(size_t level = 0) const override;
};

struct FieldInitStmt : public Stmt {
  FieldDecl *field;
  Expr *initializer;

  FieldInitStmt(SourceLocation location, FieldDecl *field, Expr *initializer)
      : Stmt(location),
        field(field),
        initializer(initializer) {}

  void dump(size_t level = 0) const override;
};

struct StructInstantiationExpr : public Expr {
  const PathExpr *structPath;
  std::vector<FieldInitStmt *> fieldInitializers;

  StructInstantiationExpr(SourceLocation location,
                          Type *type,
                          const PathExpr *structPath,
                          std::vector<FieldInitStmt *> fieldInitializers)
      : Expr(location, type, Expr::Kind::Rvalue),
        structPath(structPath),
        fieldInitializers(std::move(fieldInitializers)) {}

  void dump(size_t level = 0) const override;
};

struct ImplicitDerefExpr : public Expr {
  const PathExpr *outParamRef;

  ImplicitDerefExpr(SourceLocation location,
                    Type *type,
                    const PathExpr *outParamRef)
      : Expr(location, type, outParamRef->kind),
        outParamRef(outParamRef) {}

  void dump(size_t level = 0) const override;
};

class Context {
  std::vector<std::unique_ptr<Stmt>> statements;
  std::vector<std::unique_ptr<Decl>> decls;
  std::vector<std::unique_ptr<Block>> blocks;
  std::vector<std::unique_ptr<TraitInstance>> traitInstances;

  std::vector<TraitDecl *> traits;
  std::vector<StructDecl *> structs;
  std::vector<FunctionDecl *> functions;

public:
  template <typename T, typename... Args> T *create(Args &&...args) {
    auto ptr = std::make_unique<T>(std::forward<Args>(args)...);
    T *raw = static_cast<T *>(ptr.get());

    if constexpr (std::is_base_of_v<Stmt, T>)
      statements.emplace_back(std::move(ptr));
    else if constexpr (std::is_base_of_v<Decl, T>)
      decls.emplace_back(std::move(ptr));
    else if constexpr (std::is_base_of_v<Block, T>)
      blocks.emplace_back(std::move(ptr));
    else if constexpr (std::is_base_of_v<TraitInstance, T>)
      traitInstances.emplace_back(std::move(ptr));
    else
      llvm_unreachable(
          "can only create statements, declarations, blocks and traits");

    if constexpr (std::is_base_of_v<TraitDecl, T>)
      traits.emplace_back(raw);
    else if constexpr (std::is_base_of_v<StructDecl, T>)
      structs.emplace_back(raw);
    else if constexpr (std::is_base_of_v<FunctionDecl, T>)
      if (!static_cast<FunctionDecl *>(raw)->parent)
        functions.emplace_back(raw);

    return raw;
  }

  const std::vector<StructDecl *> &getStructs() const { return structs; }
  std::vector<StructDecl *> &getStructs() { return structs; }

  const std::vector<FunctionDecl *> &getFunctions() const { return functions; }
  std::vector<FunctionDecl *> &getFunctions() { return functions; }

  std::vector<TraitInstance *> getTraitInstances() {
    std::vector<TraitInstance *> out;

    for (auto &&traitInstance : traitInstances)
      out.emplace_back(traitInstance.get());

    return out;
  }

  void dump() const;
};
} // namespace res
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_RES_H
