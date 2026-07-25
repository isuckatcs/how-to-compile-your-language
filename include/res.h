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
  Type *type = nullptr;

public:
  void setType(Type *type) { this->type = type; }
  Type *getType() const { return type->getRootType(); }
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

  Expr(SourceLocation location, Kind kind)
      : Stmt(location),
        kind(kind) {}

  bool isLvalue() const { return kind != Kind::Rvalue; }
  bool isMutable() const { return kind == Kind::MutLvalue; }

  bool hasConstantValue() const { return constVal.isKnown(); }
  ConstVal getConstantValue() const { return constVal; }
  void setConstantValue(ConstVal val) { constVal = val; }

  virtual ~Expr() = default;
};

struct TypeParamDecl;

struct DeclContext;

struct Decl : public TypedNode {
  SourceLocation location;

  // FIXME: for local scopes this becomes dangling
  DeclContext *parent = nullptr;
  std::string identifier;
  std::vector<TypeParamDecl *> typeParams;
  bool needsStorage = false;

  Decl(SourceLocation location,
       std::string identifier,
       std::vector<TypeParamDecl *> typeParams = {})
      : location(location),
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

  void setStorageNeeded() { needsStorage = true; }
  bool isGeneric() const { return !typeParams.empty(); }
  void setParent(DeclContext *parent) { this->parent = parent; }
  virtual void dump(size_t level = 0) const = 0;
};

// FIXME: symbol tables should be separated from declaration
struct DeclContext {
  DeclContext *parent;
  std::vector<res::Decl *> decls;

  DeclContext(DeclContext *parent)
      : parent(parent) {}
  virtual ~DeclContext() = default;

  bool insertDecl(res::Decl *decl);
  std::vector<res::Decl *> lookupDecl(const std::string id) const;

  // FIXME: remove this
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
           std::string identifier,
           std::vector<TypeParamDecl *> typeParams = {})
      : Decl(location, std::move(identifier), std::move(typeParams)) {}
};

struct ValueDecl : public Decl {
  bool isMutable;

  ValueDecl(SourceLocation location,
            std::string identifier,
            bool isMutable,
            std::vector<TypeParamDecl *> typeParams = {})
      : Decl(location, std::move(identifier), std::move(typeParams)),
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
  ParamDecl(SourceLocation location, std::string identifier, bool isMutable)
      : ValueDecl(location, std::move(identifier), isMutable) {}

  void dump(size_t level = 0) const override;
};

struct TraitInstance;
struct TraitDecl : public Decl, public DeclContext {
  std::vector<TraitInstance *> traits;

  TraitDecl(SourceLocation location,
            std::string identifier,
            std::vector<TypeParamDecl *> typeParams)
      : Decl(location, std::move(identifier), std::move(typeParams)),
        DeclContext(nullptr) {}

  void dump(size_t level = 0) const override;
};

struct TraitInstance : public TypedNode {
  SourceLocation location;
  TraitDecl *decl;

  std::vector<res::Type *> typeArgs;
  std::vector<yl::SourceLocation> typeLocations;

  TraitInstance(yl::SourceLocation location,
                TraitDecl *decl,
                std::vector<res::Type *> typeArgs,
                std::vector<yl::SourceLocation> typeLocations)
      : location(location),
        decl(decl),
        typeArgs(std::move(typeArgs)),
        typeLocations(std::move(typeLocations)) {}

  void dump(size_t level = 0) const;
};

struct TypeExtension : public DeclContext {
  std::vector<TypeParamDecl *> typeParams;
  Type *type;
  TraitInstance *trait;

  TypeExtension(std::vector<TypeParamDecl *> typeParams,
                Type *type,
                TraitInstance *trait)
      : DeclContext(nullptr),
        typeParams(std::move(typeParams)),
        type(type),
        trait(trait) {}

  void dump(size_t level = 0) const;
};

struct TypeParamDecl : public TypeDecl {
  std::vector<TraitInstance *> traits;
  bool isImplicitSelf;

  TypeParamDecl(SourceLocation location,
                std::string identifier,
                bool isImplicitSelf = false)
      : TypeDecl(location, std::move(identifier)),
        isImplicitSelf(isImplicitSelf) {}

  void dump(size_t level = 0) const override;
};

struct FieldDecl : public ValueDecl {
  FieldDecl(SourceLocation location, std::string identifier)
      : ValueDecl(location, std::move(identifier), false) {}

  void dump(size_t level = 0) const override;
};

struct VarDecl : public ValueDecl {
  Expr *initializer;

  VarDecl(SourceLocation location,
          std::string identifier,
          bool isMutable,
          Expr *initializer = nullptr)
      : ValueDecl(location, std::move(identifier), isMutable),
        initializer(initializer) {}

  void dump(size_t level = 0) const override;
};

struct StructDecl : public TypeDecl, public DeclContext {
  bool isLambda;

  StructDecl(SourceLocation location,
             std::string identifier,
             std::vector<TypeParamDecl *> typeParams = {},
             bool isLambda = false)
      : TypeDecl(location, std::move(identifier), std::move(typeParams)),
        DeclContext(nullptr),
        isLambda(isLambda) {}

  void dump(size_t level = 0) const override;
};

struct FunctionDecl : public ValueDecl {
  std::vector<ParamDecl *> params;
  FunctionDecl *implements = nullptr;
  Block *body = nullptr;
  bool isComplete = false;

  FunctionDecl(SourceLocation location,
               std::string identifier,
               std::vector<TypeParamDecl *> typeParams = {},
               std::vector<ParamDecl *> params = {},
               FunctionDecl *implements = nullptr)
      : ValueDecl(
            location, std::move(identifier), false, std::move(typeParams)),
        params(std::move(params)),
        implements(implements) {}

  void setBody(Block *body);

  bool hasSelfParam();

  void dump(size_t level = 0) const override;
};

struct NumberLiteral : public Expr {
  double value;

  NumberLiteral(SourceLocation location, double value)
      : Expr(location, Expr::Kind::Rvalue),
        value(value) {}

  void dump(size_t level = 0) const override;
};

struct BoolLiteral : public Expr {
  bool value;

  BoolLiteral(SourceLocation location, bool value)
      : Expr(location, Expr::Kind::Rvalue),
        value(value) {}

  void dump(size_t level = 0) const override;
};

struct UnitLiteral : public Expr {
  explicit UnitLiteral(SourceLocation location)
      : Expr(location, Expr::Kind::Rvalue) {}

  void dump(size_t level = 0) const override;
};

struct CallExpr : public Expr {
  Expr *callee;
  std::vector<Expr *> arguments;

  CallExpr(SourceLocation location, Expr *callee, std::vector<Expr *> arguments)
      : Expr(location, Expr::Kind::Rvalue),
        callee(callee),
        arguments(std::move(arguments)){};

  bool isVirtual() const;
  void dump(size_t level = 0) const override;
};

struct DeclRefExpr : public Expr {
  Decl *decl;
  Substitution sub;
  // FIXME: remove this field
  std::vector<Type *> typeArgs;

  DeclRefExpr(SourceLocation location,
              Decl *decl,
              Expr::Kind kind,
              Substitution sub,
              std::vector<Type *> typeArgs = {})
      : Expr(location, kind),
        decl(decl),
        sub(sub),
        typeArgs(std::move(typeArgs)) {}

  Type *getReceiverType() const;
  std::string getFullPath() const;

  void dump(size_t level = 0) const override;
};

struct MemberExpr : public Expr {
  Expr *base;
  DeclRefExpr *member;

  MemberExpr(SourceLocation location, Expr *base, DeclRefExpr *member)
      : Expr(location, !base->isLvalue() ? Expr::Kind::MutLvalue : base->kind),
        base(base),
        member(member) {}

  void dump(size_t level = 0) const override;
};

struct GroupingExpr : public Expr {
  Expr *expr;

  GroupingExpr(SourceLocation location, Expr *expr)
      : Expr(location, expr->kind),
        expr(expr) {}

  void dump(size_t level = 0) const override;
};

struct BinaryOperator : public Expr {
  TokenKind op;
  Expr *lhs;
  Expr *rhs;

  BinaryOperator(SourceLocation location, TokenKind op, Expr *lhs, Expr *rhs)
      : Expr(location, Expr::Kind::Rvalue),
        op(op),
        lhs(lhs),
        rhs(rhs) {}

  void dump(size_t level = 0) const override;
};

struct UnaryOperator : public Expr {
  TokenKind op;
  Expr *operand;

  UnaryOperator(SourceLocation location,
                TokenKind op,
                Expr *operand,
                Expr::Kind kind)
      : Expr(location, kind),
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
  const DeclRefExpr *structPath;
  std::vector<FieldInitStmt *> fieldInitializers;

  StructInstantiationExpr(SourceLocation location,
                          const DeclRefExpr *structPath,
                          std::vector<FieldInitStmt *> fieldInitializers)
      : Expr(location, Expr::Kind::Rvalue),
        structPath(structPath),
        fieldInitializers(std::move(fieldInitializers)) {}

  void dump(size_t level = 0) const override;
};

struct ImplicitDerefExpr : public Expr {
  DeclRefExpr *dre;

  ImplicitDerefExpr(SourceLocation location, DeclRefExpr *dre)
      : Expr(location, dre->kind),
        dre(dre) {}

  void dump(size_t level = 0) const override;
};

struct GCExpr : public Expr {
  Expr *expr;

  GCExpr(SourceLocation location, Expr *expr)
      : Expr(location, Expr::Kind::Rvalue),
        expr(expr) {}

  void dump(size_t level = 0) const override;
};

struct LambdaExpr : public Expr {
  res::StructDecl *closure;
  std::vector<const res::Expr *> fieldInits;
  res::FunctionDecl *method;

  LambdaExpr(SourceLocation location,
             res::StructDecl *closure,
             res::FunctionDecl *method,
             std::vector<const res::Expr *> fieldInits = {})
      : Expr(location, Expr::Kind::Rvalue),
        closure(closure),
        method(method),
        fieldInits(std::move(fieldInits)) {}

  void dump(size_t level = 0) const override;
};

struct ImplicitPtrToBorrowDecay : public Expr {
  res::Expr *expr;

  ImplicitPtrToBorrowDecay(SourceLocation location, res::Expr *expr)
      : Expr(location, Expr::Kind::Rvalue),
        expr(expr) {}

  void dump(size_t level = 0) const override;
};

struct ImplicitBorrowExpr : public Expr {
  res::Expr *expr;

  ImplicitBorrowExpr(SourceLocation location, res::Expr *expr)
      : Expr(location, Expr::Kind::Rvalue),
        expr(expr) {}

  void dump(size_t level = 0) const override;
};

struct MaterializeTemporaryExpr : public Expr {
  res::Expr *expr;

  MaterializeTemporaryExpr(SourceLocation location, res::Expr *expr)
      : Expr(location, Expr::Kind::MutLvalue),
        expr(expr) {}

  void dump(size_t level = 0) const override;
};

struct TraitObjectPromoExpr : public Expr {
  res::Expr *expr;

  TraitObjectPromoExpr(SourceLocation location, res::Expr *expr)
      : Expr(location, Expr::Kind::Rvalue),
        expr(expr) {}

  void dump(size_t level = 0) const override;
};

class Context {
  std::vector<std::unique_ptr<Stmt>> statements;
  std::vector<std::unique_ptr<Decl>> decls;
  std::vector<std::unique_ptr<Block>> blocks;
  std::vector<std::unique_ptr<TypeExtension>> typeExtensions;
  std::vector<std::unique_ptr<TraitInstance>> traitInstances;

  std::vector<TraitDecl *> traits;
  std::vector<StructDecl *> structs;
  std::vector<FunctionDecl *> functions;
  std::vector<TypeExtension *> extensions;

public:
  // FIXME: rethink this whole method
  template <typename T, typename... Args> T *create(Args &&...args) {
    auto ptr = std::make_unique<T>(std::forward<Args>(args)...);
    T *raw = static_cast<T *>(ptr.get());

    if constexpr (std::is_base_of_v<Stmt, T>)
      statements.emplace_back(std::move(ptr));
    else if constexpr (std::is_base_of_v<Decl, T>)
      decls.emplace_back(std::move(ptr));
    else if constexpr (std::is_base_of_v<Block, T>)
      blocks.emplace_back(std::move(ptr));
    else if constexpr (std::is_base_of_v<TypeExtension, T>)
      typeExtensions.emplace_back(std::move(ptr));
    else if constexpr (std::is_base_of_v<TraitInstance, T>)
      traitInstances.emplace_back(std::move(ptr));
    else
      llvm_unreachable(
          "can only create statements, declarations, blocks and traits");

    if constexpr (std::is_base_of_v<TraitDecl, T>)
      traits.emplace_back(raw);
    else if constexpr (std::is_base_of_v<TypeExtension, T>)
      extensions.emplace_back(raw);
    else if constexpr (std::is_base_of_v<StructDecl, T>) {
      if (!raw->isLambda)
        structs.emplace_back(raw);
    } else if constexpr (std::is_base_of_v<FunctionDecl, T>)
      // FIXME: rething how these nodes are stored
      functions.emplace_back(raw);

    return raw;
  }

  const std::vector<StructDecl *> &getStructs() const { return structs; }
  std::vector<StructDecl *> &getStructs() { return structs; }

  std::vector<FunctionDecl *> getFunctions() const {
    std::vector<FunctionDecl *> out;
    for (auto &&function : functions)
      if (!function->parent && !function->implements)
        out.emplace_back(function);

    return out;
  }

  const std::vector<TypeExtension *> &getTypeExtensions() const {
    return extensions;
  }
  std::vector<TypeExtension *> &getTypeExtensions() { return extensions; }

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
