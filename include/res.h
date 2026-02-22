#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_RES_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_RES_H

#include <memory>
#include <variant>
#include <vector>

#include "lexer.h"
#include "utils.h"

namespace yl {
namespace res {
class Context;

struct Type {
  template <typename T> T *getAs() {
    return const_cast<T *>(const_cast<const Type *>(this)->getAs<T>());
  }

  template <typename T> const T *getAs() const {
    static_assert(std::is_base_of_v<Type, T>, "expected type");
    return dynamic_cast<const T *>(getRootType());
  }

  Type *getRootType() {
    return const_cast<Type *>(const_cast<const Type *>(this)->getRootType());
  }

  virtual const Type *getRootType() const { return this; }
  virtual std::string getName() const { return name; };
  virtual ~Type() = default;

protected:
  std::string name;
  std::vector<Type *> args;

  Type(std::string identifier, std::vector<Type *> args)
      : name(std::move(identifier)),
        args(std::move(args)){};

  friend class Context;
};

struct Stmt {
  SourceLocation location;

  Stmt(SourceLocation location)
      : location(location) {}

  virtual ~Stmt() = default;

  virtual void dump(const Context &ctx, size_t level = 0) const = 0;
};

struct Expr : public ConstantValueContainer<double>, public Stmt {
  enum class Kind { Rvalue, MutLvalue, Lvalue };

  Kind kind;

  Expr(SourceLocation location, Kind kind)
      : Stmt(location),
        kind(kind) {}

  bool isLvalue() const { return kind != Kind::Rvalue; }
  bool isMutable() const { return kind == Kind::MutLvalue; }

  virtual ~Expr() = default;
};

struct Decl {
  SourceLocation location;
  std::string identifier;

  Decl(SourceLocation location, std::string identifier)
      : location(location),
        identifier(std::move(identifier)) {}
  virtual ~Decl() = default;

  template <typename T> T *getAs() {
    return const_cast<T *>(const_cast<const Decl *>(this)->getAs<T>());
  }

  template <typename T> const T *getAs() const {
    static_assert(std::is_base_of_v<Decl, T>, "expected decl");
    return dynamic_cast<const T *>(this);
  }

  virtual bool isGeneric() const { return false; }

  virtual void dump(const Context &ctx, size_t level = 0) const = 0;
};

struct TypeDecl : public Decl {
  TypeDecl(SourceLocation location, std::string identifier)
      : Decl(location, std::move(identifier)) {}
};

struct ValueDecl : public Decl {
  bool isMutable;

  ValueDecl(SourceLocation location, std::string identifier, bool isMutable)
      : Decl(location, std::move(identifier)),
        isMutable(isMutable) {}
};

struct Block {
  SourceLocation location;
  std::vector<Stmt *> statements;

  Block(SourceLocation location, std::vector<Stmt *> statements)
      : location(location),
        statements(std::move(statements)) {}

  void dump(const Context &ctx, size_t level = 0) const;
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

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct WhileStmt : public Stmt {
  Expr *condition;
  Block *body;

  WhileStmt(SourceLocation location, Expr *condition, Block *body)
      : Stmt(location),
        condition(condition),
        body(body) {}

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct ParamDecl : public ValueDecl {
  ParamDecl(SourceLocation location, std::string identifier, bool isMutable)
      : ValueDecl(location, std::move(identifier), isMutable) {}

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct TypeParamDecl : public TypeDecl {
  unsigned index;

  TypeParamDecl(SourceLocation location, std::string identifier, unsigned index)
      : TypeDecl(location, std::move(identifier)),
        index(index) {}

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct FieldDecl : public ValueDecl {
  FieldDecl(SourceLocation location, std::string identifier)
      : ValueDecl(location, std::move(identifier), false) {}

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct VarDecl : public ValueDecl {
  Expr *initializer;

  VarDecl(SourceLocation location,
          std::string identifier,
          bool isMutable,
          Expr *initializer = nullptr)
      : ValueDecl(location, std::move(identifier), isMutable),
        initializer(initializer) {}

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct FunctionDecl : public ValueDecl {
  std::vector<TypeParamDecl *> typeParams;
  std::vector<ParamDecl *> params;
  Block *body = nullptr;
  bool isComplete = false;

  FunctionDecl(SourceLocation location,
               std::string identifier,
               std::vector<TypeParamDecl *> typeParams,
               std::vector<ParamDecl *> params)
      : ValueDecl(location, std::move(identifier), false),
        typeParams(std::move(typeParams)),
        params(std::move(params)) {}

  void setBody(Block *body);
  bool isGeneric() const override { return !typeParams.empty(); }

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct StructDecl : public TypeDecl {
  std::vector<TypeParamDecl *> typeParams;
  std::vector<FieldDecl *> fields;
  bool isComplete = false;

  StructDecl(SourceLocation location,
             std::string identifier,
             std::vector<TypeParamDecl *> typeParams)
      : TypeDecl(location, std::move(identifier)),
        typeParams(std::move(typeParams)) {}

  void setFields(std::vector<FieldDecl *> fields);
  bool isGeneric() const override { return !typeParams.empty(); }

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct NumberLiteral : public Expr {
  double value;

  NumberLiteral(SourceLocation location, double value)
      : Expr(location, Expr::Kind::Rvalue),
        value(value) {}

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct UnitLiteral : public Expr {

  UnitLiteral(SourceLocation location)
      : Expr(location, Expr::Kind::Rvalue) {}

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct CallExpr : public Expr {
  Expr *callee;
  std::vector<Expr *> arguments;

  CallExpr(SourceLocation location, Expr *callee, std::vector<Expr *> arguments)
      : Expr(location, Expr::Kind::Rvalue),
        callee(callee),
        arguments(std::move(arguments)) {}

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct DeclRefExpr : public Expr {
private:
  std::vector<Type *> typeArgs;

public:
  Decl *decl;

  DeclRefExpr(SourceLocation location,
              Decl &decl,
              Expr::Kind kind,
              std::vector<Type *> typeArgs)
      : Expr(location, kind),
        decl(&decl),
        typeArgs(std::move(typeArgs)) {}

  std::vector<Type *> getTypeArgs() { return typeArgs; }
  std::vector<const Type *> getTypeArgs() const {
    return std::vector<const Type *>(typeArgs.begin(), typeArgs.end());
  }

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct MemberExpr : public Expr {
  Expr *base;
  FieldDecl *field;

  MemberExpr(SourceLocation location, Expr *base, FieldDecl *field)
      : Expr(location, !base->isLvalue() ? Expr::Kind::MutLvalue : base->kind),
        base(base),
        field(field) {}

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct GroupingExpr : public Expr {
  Expr *expr;

  GroupingExpr(SourceLocation location, Expr *expr)
      : Expr(location, expr->kind),
        expr(expr) {}

  void dump(const Context &ctx, size_t level = 0) const override;
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

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct UnaryOperator : public Expr {
  TokenKind op;
  Expr *operand;

  UnaryOperator(SourceLocation location, TokenKind op, Expr *operand)
      : Expr(location, Expr::Kind::Rvalue),
        op(op),
        operand(operand) {}

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct DeclStmt : public Stmt {
  VarDecl *varDecl;

  DeclStmt(SourceLocation location, VarDecl *varDecl)
      : Stmt(location),
        varDecl(varDecl) {}

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct Assignment : public Stmt {
  Expr *assignee;
  Expr *expr;

  Assignment(SourceLocation location, Expr *assignee, Expr *expr)
      : Stmt(location),
        assignee(assignee),
        expr(expr) {}

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct ReturnStmt : public Stmt {
  Expr *expr;

  ReturnStmt(SourceLocation location, Expr *expr = nullptr)
      : Stmt(location),
        expr(expr) {}

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct FieldInitStmt : public Stmt {
  FieldDecl *field;
  Expr *initializer;

  FieldInitStmt(SourceLocation location, FieldDecl *field, Expr *initializer)
      : Stmt(location),
        field(field),
        initializer(initializer) {}

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct StructInstantiationExpr : public Expr {
  const DeclRefExpr *structDecl;
  std::vector<FieldInitStmt *> fieldInitializers;

  StructInstantiationExpr(SourceLocation location,
                          const DeclRefExpr *structDecl,
                          std::vector<FieldInitStmt *> fieldInitializers)
      : Expr(location, Expr::Kind::Rvalue),
        structDecl(structDecl),
        fieldInitializers(std::move(fieldInitializers)) {}

  void dump(const Context &ctx, size_t level = 0) const override;
};

struct ImplicitDerefExpr : public Expr {
  const DeclRefExpr *outParamRef;

  ImplicitDerefExpr(SourceLocation location, const DeclRefExpr *outParamRef)
      : Expr(location, outParamRef->kind),
        outParamRef(outParamRef) {}

  void dump(const Context &ctx, size_t level = 0) const override;
};

class UninferredType : public Type {
  Type *parent = nullptr;

  UninferredType(std::string name)
      : Type(name, {}){};

  void infer(Type *t) {
    assert(!parent && "already inferred");
    parent = t;
  }

public:
  const Type *getRootType() const override {
    if (parent)
      return parent->getRootType();
    return this;
  }

  std::string getName() const override {
    if (parent)
      return parent->getName();
    return "_";
  };

  friend class Context;
};

class BuiltinUnitType : public Type {
  BuiltinUnitType()
      : Type("unit", {}){};

  friend class Context;
};

class BuiltinNumberType : public Type {
  BuiltinNumberType()
      : Type("number", {}){};

  friend class Context;
};

class TypeParamType : public Type {
  TypeParamType(const TypeParamDecl &decl)
      : Type(decl.identifier, {}),
        decl(&decl){};

  friend class Context;

public:
  const TypeParamDecl *decl;
};

class FunctionType : public Type {
  FunctionType(std::vector<Type *> args)
      : Type("fn", std::move(args)){};

public:
  std::vector<Type *> getArgs() { return {args.begin(), --args.end()}; }
  std::vector<const Type *> getArgs() const {
    return {args.begin(), --args.end()};
  }

  Type *getReturnType() { return args.back()->getRootType(); }
  const Type *getReturnType() const { return args.back()->getRootType(); }

  std::string getName() const override;

  friend class Context;
};

class StructType : public Type {
  const StructDecl *decl;

  StructType(const StructDecl &decl, std::vector<Type *> typeArgs)
      : Type(decl.identifier, std::move(typeArgs)),
        decl(&decl){};

public:
  const StructDecl *getDecl() const { return decl; }

  std::vector<Type *> getTypeArgs() { return args; }
  std::vector<const Type *> getTypeArgs() const {
    return std::vector<const Type *>(args.begin(), args.end());
  }

  std::string getName() const override;

  friend class Context;
};

class PointerType : public Type {
  PointerType(Type *pointeeType)
      : Type("*", std::vector<res::Type *>{pointeeType}){};

  std::string getName() const override { return "*" + args[0]->getName(); }

public:
  Type *getPointeeType() { return args[0]->getRootType(); }
  const Type *getPointeeType() const { return args[0]->getRootType(); }

  friend class Context;
};

class Context {
  std::vector<std::unique_ptr<Stmt>> statements;
  std::vector<std::unique_ptr<Decl>> decls;
  std::vector<std::unique_ptr<Block>> blocks;

  std::vector<StructDecl *> structs;
  std::vector<FunctionDecl *> functions;

  BuiltinUnitType unitType = BuiltinUnitType();
  BuiltinNumberType numberType = BuiltinNumberType();
  std::unordered_map<const TypeParamDecl *, std::unique_ptr<TypeParamType>>
      typeParamTys;
  size_t typeVariableCount = 0;
  std::vector<std::unique_ptr<Type>> types;

  std::unordered_map<std::variant<const Expr *, const Decl *>, Type *> env;

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
    else
      llvm_unreachable("can only create statements, declarations and blocks");

    if constexpr (std::is_base_of_v<StructDecl, T>)
      structs.emplace_back(raw);
    else if constexpr (std::is_base_of_v<FunctionDecl, T>)
      functions.emplace_back(raw);

    return raw;
  }

  const std::vector<StructDecl *> &getStructs() const { return structs; }
  std::vector<StructDecl *> &getStructs() { return structs; }

  const std::vector<FunctionDecl *> &getFunctions() const { return functions; }
  std::vector<FunctionDecl *> &getFunctions() { return functions; }

  UninferredType *getNewUninferredType();
  BuiltinUnitType *getBuiltinUnitType() { return &unitType; };
  BuiltinNumberType *getBuiltinNumberType() { return &numberType; };
  FunctionType *getFunctionType(std::vector<Type *> args, Type *ret);
  StructType *getStructType(const StructDecl &decl,
                            std::vector<Type *> typeArgs);
  TypeParamType *getTypeParamType(const TypeParamDecl &decl);
  PointerType *getPointerType(Type *pointeeType);

  using SubstitutionTy = std::vector<res::Type *>;
  SubstitutionTy createSubstitution(const Decl *decl);
  Type *instantiate(Type *t, const SubstitutionTy &substitution);

  bool unify(Type *t1, Type *t2);

  template <typename T> T *bind(T *node, Type *type) {
    env[node] = type;
    return node;
  }

  template <typename T> const Type *getType(T *node) const {
    if (env.count(node) == 0)
      return nullptr;

    return env.find(node)->second->getRootType();
  }

  template <typename T> Type *getType(T *node) {
    return const_cast<Type *>(const_cast<const Context *>(this)->getType(node));
  }

  void dump();
};
} // namespace res
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_RES_H
