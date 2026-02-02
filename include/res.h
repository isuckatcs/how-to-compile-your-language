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

class Type {
public:
  virtual bool isUninferredType() const { return false; }
  virtual bool isBuiltinVoid() const { return false; }
  virtual bool isBuiltinNumber() const { return false; }
  virtual bool isFunctionType() const { return false; }
  virtual bool isStructType() const { return false; }
  virtual bool isTypeArgumentType() const { return false; }

  virtual Type *getRootType() { return this; }
  virtual const Type *getRootType() const { return this; }

  virtual std::string getName() const = 0;
  virtual ~Type() = default;

protected:
  Type() {}

  friend class Context;
};

struct Stmt {
  SourceLocation location;

  Stmt(SourceLocation location)
      : location(location) {}

  virtual ~Stmt() = default;

  virtual void dump(Context &ctx, size_t level = 0) const = 0;
};

struct Expr : public ConstantValueContainer<double>, public Stmt {
  enum class Kind { Lvalue, Rvalue };

  Kind kind;

  Expr(SourceLocation location, Kind kind)
      : Stmt(location),
        kind(kind) {}

  bool isLvalue() const { return kind == Kind::Lvalue; }

  virtual ~Expr() = default;
};

// FIXME: revisit layout
struct Decl {
  enum class Kind {
    StructDecl,
    TypeArgumentDecl,
    FieldDecl,
    FunctionDecl,
    ParamDecl,
    VarDecl
  };

  SourceLocation location;
  std::string identifier;
  Kind kind;

  Decl(SourceLocation location, std::string identifier, Kind kind)
      : location(location),
        identifier(std::move(identifier)),
        kind(kind) {}
  virtual ~Decl() = default;

  bool isStructDecl() const { return kind == Kind::StructDecl; }
  bool isTypeArgumentDecl() const { return kind == Kind::TypeArgumentDecl; }
  bool isFieldDecl() const { return kind == Kind::FieldDecl; }
  bool isFunctionDecl() const { return kind == Kind::FunctionDecl; }
  bool isParamDecl() const { return kind == Kind::ParamDecl; }
  bool isVarDecl() const { return kind == Kind::VarDecl; }
  virtual bool isGeneric() const { return false; }

  virtual void dump(Context &ctx, size_t level = 0) const = 0;
};

struct TypeDecl : public Decl {
  TypeDecl(SourceLocation location, std::string identifier, Kind kind)
      : Decl(location, std::move(identifier), kind) {}
};

struct ValueDecl : public Decl {
  bool isMutable;

  ValueDecl(SourceLocation location,
            std::string identifier,
            Kind kind,
            bool isMutable)
      : Decl(location, std::move(identifier), kind),
        isMutable(isMutable) {}
};

struct Block {
  SourceLocation location;
  std::vector<Stmt *> statements;

  Block(SourceLocation location, std::vector<Stmt *> statements)
      : location(location),
        statements(std::move(statements)) {}

  void dump(Context &ctx, size_t level = 0) const;
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

  void dump(Context &ctx, size_t level = 0) const override;
};

struct WhileStmt : public Stmt {
  Expr *condition;
  Block *body;

  WhileStmt(SourceLocation location, Expr *condition, Block *body)
      : Stmt(location),
        condition(condition),
        body(body) {}

  void dump(Context &ctx, size_t level = 0) const override;
};

struct ParamDecl : public ValueDecl {
  ParamDecl(SourceLocation location, std::string identifier, bool isMutable)
      : ValueDecl(
            location, std::move(identifier), Decl::Kind::ParamDecl, isMutable) {
  }

  void dump(Context &ctx, size_t level = 0) const override;
};

struct TypeArgumentDecl : public TypeDecl {
  unsigned index;

  TypeArgumentDecl(SourceLocation location,
                   std::string identifier,
                   unsigned index)
      : TypeDecl(location, std::move(identifier), Decl::Kind::TypeArgumentDecl),
        index(index) {}

  void dump(Context &ctx, size_t level = 0) const override;
};

struct FieldDecl : public ValueDecl {
  unsigned index;

  FieldDecl(SourceLocation location, std::string identifier, unsigned index)
      : ValueDecl(
            location, std::move(identifier), Decl::Kind::FieldDecl, false),
        index(index) {}

  void dump(Context &ctx, size_t level = 0) const override;
};

struct VarDecl : public ValueDecl {
  Expr *initializer;

  VarDecl(SourceLocation location,
          std::string identifier,
          bool isMutable,
          Expr *initializer = nullptr)
      : ValueDecl(
            location, std::move(identifier), Decl::Kind::VarDecl, isMutable),
        initializer(initializer) {}

  void dump(Context &ctx, size_t level = 0) const override;
};

struct FunctionDecl : public ValueDecl {
  std::vector<TypeArgumentDecl *> typeArguments;
  std::vector<ParamDecl *> params;
  Block *body = nullptr;
  bool isComplete = false;

  FunctionDecl(SourceLocation location,
               std::string identifier,
               std::vector<TypeArgumentDecl *> typeArguments,
               std::vector<ParamDecl *> params)
      : ValueDecl(
            location, std::move(identifier), Decl::Kind::FunctionDecl, false),
        typeArguments(std::move(typeArguments)),
        params(std::move(params)) {}

  void setBody(Block *body);
  bool isGeneric() const override { return !typeArguments.empty(); }

  void dump(Context &ctx, size_t level = 0) const override;
};

struct StructDecl : public TypeDecl {
  std::vector<TypeArgumentDecl *> typeArguments;
  std::vector<FieldDecl *> fields;
  bool isComplete = false;

  StructDecl(SourceLocation location,
             std::string identifier,
             std::vector<TypeArgumentDecl *> typeArguments)
      : TypeDecl(location, std::move(identifier), Decl::Kind::StructDecl),
        typeArguments(std::move(typeArguments)) {}

  void setFields(std::vector<FieldDecl *> fields);
  bool isGeneric() const override { return !typeArguments.empty(); }

  void dump(Context &ctx, size_t level = 0) const override;
};

struct NumberLiteral : public Expr {
  double value;

  NumberLiteral(SourceLocation location, double value)
      : Expr(location, Expr::Kind::Rvalue),
        value(value) {}

  void dump(Context &ctx, size_t level = 0) const override;
};

struct CallExpr : public Expr {
  Expr *callee;
  std::vector<Expr *> arguments;

  CallExpr(SourceLocation location, Expr *callee, std::vector<Expr *> arguments)
      : Expr(location, Expr::Kind::Rvalue),
        callee(callee),
        arguments(std::move(arguments)) {}

  void dump(Context &ctx, size_t level = 0) const override;
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

  void dump(Context &ctx, size_t level = 0) const override;
};

struct MemberExpr : public Expr {
  Expr *base;
  FieldDecl *field;

  MemberExpr(SourceLocation location, Expr *base, FieldDecl *field)
      : Expr(location, Expr::Kind::Lvalue),
        base(base),
        field(field) {}

  void dump(Context &ctx, size_t level = 0) const override;
};

struct GroupingExpr : public Expr {
  Expr *expr;

  GroupingExpr(SourceLocation location, Expr *expr)
      : Expr(location, expr->kind),
        expr(expr) {}

  void dump(Context &ctx, size_t level = 0) const override;
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

  void dump(Context &ctx, size_t level = 0) const override;
};

struct UnaryOperator : public Expr {
  TokenKind op;
  Expr *operand;

  UnaryOperator(SourceLocation location, TokenKind op, Expr *operand)
      : Expr(location, Expr::Kind::Rvalue),
        op(op),
        operand(operand) {}

  void dump(Context &ctx, size_t level = 0) const override;
};

struct DeclStmt : public Stmt {
  VarDecl *varDecl;

  DeclStmt(SourceLocation location, VarDecl *varDecl)
      : Stmt(location),
        varDecl(varDecl) {}

  void dump(Context &ctx, size_t level = 0) const override;
};

struct Assignment : public Stmt {
  Expr *assignee;
  Expr *expr;

  Assignment(SourceLocation location, Expr *assignee, Expr *expr)
      : Stmt(location),
        assignee(assignee),
        expr(expr) {}

  void dump(Context &ctx, size_t level = 0) const override;
};

struct ReturnStmt : public Stmt {
  Expr *expr;

  ReturnStmt(SourceLocation location, Expr *expr = nullptr)
      : Stmt(location),
        expr(expr) {}

  void dump(Context &ctx, size_t level = 0) const override;
};

struct FieldInitStmt : public Stmt {
  FieldDecl *field;
  Expr *initializer;

  FieldInitStmt(SourceLocation location, FieldDecl *field, Expr *initializer)
      : Stmt(location),
        field(field),
        initializer(initializer) {}

  void dump(Context &ctx, size_t level = 0) const override;
};

struct StructInstantiationExpr : public Expr {
  const DeclRefExpr *structDecl;
  std::vector<FieldInitStmt *> fieldInitializers;

  StructInstantiationExpr(SourceLocation location,
                          const DeclRefExpr *structDecl,
                          std::vector<FieldInitStmt *> fieldInitializers)
      : Expr(location, Expr::Kind::Lvalue),
        structDecl(structDecl),
        fieldInitializers(std::move(fieldInitializers)) {}

  void dump(Context &ctx, size_t level = 0) const override;
};

class UninferredType : public Type {
  Type *parent;
  size_t id;

  UninferredType(size_t id)
      : parent(nullptr),
        id(id){};

  void infer(Type *t) {
    assert(!parent && "already inferred");
    parent = t;
  }

public:
  Type *getRootType() override {
    return const_cast<Type *>(
        const_cast<const UninferredType *>(this)->getRootType());
  }

  const Type *getRootType() const override {
    if (parent)
      return parent->getRootType();
    return this;
  }

  bool isUninferredType() const override {
    if (parent)
      return parent->isUninferredType();
    return true;
  }

  std::string getName() const override {
    // FIXME: hide this in error messages
    // fn bar(): void {}

    // fn foo<T>(x: () -> T): T {
    //   return x();
    // }

    // fn main(): void {
    //   foo(1);
    //       ^ error: expected '() -> t7' argument, but received 'number'
    // }
    if (parent)
      return parent->getName();
    return "t" + std::to_string(id);
  };

  friend class Context;
};

class BuiltinType : public Type {
public:
  enum class Kind { Void, Number };

private:
  Kind kind;

  BuiltinType(Kind kind)
      : kind(kind){};

public:
  bool isBuiltinVoid() const override { return kind == Kind::Void; }
  bool isBuiltinNumber() const override { return kind == Kind::Number; }
  std::string getName() const override {
    return isBuiltinVoid() ? "void" : "number";
  };

  friend class Context;
};

class TypeArgumentType : public Type {
public:
  const TypeArgumentDecl *decl;

  bool isTypeArgumentType() const override { return true; }
  std::string getName() const override { return decl->identifier; };

private:
  TypeArgumentType(const TypeArgumentDecl &decl)
      : decl(&decl){};

  friend class Context;
};

class FunctionType : public Type {
  std::vector<Type *> args;
  Type *ret;

  FunctionType(std::vector<Type *> args, Type *ret)
      : args(std::move(args)),
        ret(ret){};

public:
  size_t getArgCount() const { return args.size(); }

  const Type *getArgType(size_t idx) const { return args[idx]->getRootType(); }
  Type *getArgType(size_t idx) { return args[idx]->getRootType(); }

  const Type *getReturnType() const { return ret->getRootType(); }
  Type *getReturnType() { return ret->getRootType(); }

  bool isFunctionType() const override { return true; }
  std::string getName() const override;

  friend class Context;
};

class StructType : public Type {
private:
  std::vector<Type *> typeArgs;
  const StructDecl *decl;

  StructType(const StructDecl &decl, std::vector<Type *> typeArgs)
      : decl(&decl),
        typeArgs(std::move(typeArgs)) {
    assert(decl.typeArguments.size() == this->typeArgs.size() &&
           "mismatching type argument size for struct");
  };

public:
  const StructDecl *getDecl() const { return decl; }

  std::vector<Type *> getTypeArgs() { return typeArgs; }
  std::vector<const Type *> getTypeArgs() const {
    return std::vector<const Type *>(typeArgs.begin(), typeArgs.end());
  }

  bool isStructType() const override { return true; }
  std::string getName() const override;

  friend class Context;
};

class Context {
  template <typename T>
  using is_env_key = std::bool_constant<std::is_base_of_v<Expr, T> ||
                                        std::is_base_of_v<Decl, T>>;
  template <typename T>
  using is_res_node = std::bool_constant<std::is_base_of_v<Stmt, T> ||
                                         std::is_base_of_v<Decl, T> ||
                                         std::is_base_of_v<Block, T>>;

  std::vector<std::unique_ptr<Stmt>> statements;
  std::vector<std::unique_ptr<Decl>> decls;
  std::vector<std::unique_ptr<Block>> blocks;

  std::vector<StructDecl *> structs;
  std::vector<FunctionDecl *> functions;

  std::unique_ptr<BuiltinType> numberTy =
      std::unique_ptr<BuiltinType>(new BuiltinType(BuiltinType::Kind::Number));
  std::unique_ptr<BuiltinType> voidTy =
      std::unique_ptr<BuiltinType>(new BuiltinType(BuiltinType::Kind::Void));
  std::unordered_map<const TypeArgumentDecl *,
                     std::unique_ptr<TypeArgumentType>>
      typeArgTys;
  std::vector<std::unique_ptr<UninferredType>> typeVariables;
  std::vector<std::unique_ptr<StructType>> structTys;
  std::vector<std::unique_ptr<FunctionType>> functionTys;

  using EnvKeyTy = std::variant<const Expr *, const Decl *>;
  std::unordered_map<EnvKeyTy, Type *> environment;

  Context() = default;
  Context(const Context &) = delete;
  Context &operator=(const Context &) = delete;

public:
  Context(Context &&) = default;
  Context &operator=(Context &&) = delete;

  static Context createEmptyContext() { return Context{}; }

  const std::vector<StructDecl *> &getStructs() const { return structs; }
  std::vector<StructDecl *> &getStructs() { return structs; }

  const std::vector<FunctionDecl *> &getFunctions() const { return functions; }
  std::vector<FunctionDecl *> &getFunctions() { return functions; }

  UninferredType *getNewUninferredType();
  BuiltinType *getBuiltinType(const BuiltinType::Kind kind);
  FunctionType *getUninferredFunctionType(size_t argCount);
  StructType *getUninferredStructType(const StructDecl &decl);
  TypeArgumentType *getTypeArgumentType(const TypeArgumentDecl &decl);

  std::vector<res::Type *> createInstantiation(const Decl *decl);
  Type *instantiate(Type *t, const std::vector<res::Type *> &instantiation);

  bool unify(Type *t1, Type *t2);

  void dump();

  template <typename T, typename... Args> T *create(Args &&...args) {
    static_assert(is_res_node<T>::value,
                  "can only create statements, declarations and blocks");

    auto ptr = std::make_unique<T>(std::forward<Args>(args)...);
    T *raw = static_cast<T *>(ptr.get());

    if constexpr (std::is_base_of_v<Stmt, T>)
      statements.emplace_back(std::move(ptr));
    else if constexpr (std::is_base_of_v<Decl, T>)
      decls.emplace_back(std::move(ptr));
    else if constexpr (std::is_base_of_v<Block, T>)
      blocks.emplace_back(std::move(ptr));
    else
      llvm_unreachable("created unexpected node");

    if constexpr (std::is_base_of_v<StructDecl, T>)
      structs.emplace_back(raw);
    else if constexpr (std::is_base_of_v<FunctionDecl, T>)
      functions.emplace_back(raw);

    return raw;
  }

  template <typename T> T *bind(T *node, Type *type) {
    static_assert(is_env_key<T>(), "");
    environment[node] = type;
    return node;
  }

  template <typename T> const Type *getType(T *node) const {
    static_assert(is_env_key<T>(), "");
    if (environment.count(node) == 0)
      return nullptr;

    return environment.find(node)->second->getRootType();
  }

  template <typename T> Type *getType(T *node) {
    return const_cast<Type *>(const_cast<const Context *>(this)->getType(node));
  }
};
} // namespace res
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_RES_H
