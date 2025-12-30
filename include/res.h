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

struct Decl {
  enum class Kind { StructDecl, FieldDecl, FunctionDecl, ParamDecl, VarDecl };

  SourceLocation location;
  std::string identifier;
  Kind kind;

  Decl(SourceLocation location, std::string identifier, Kind kind)
      : location(location),
        identifier(std::move(identifier)),
        kind(kind) {}
  virtual ~Decl() = default;

  bool isStructDecl() const { return kind == Kind::StructDecl; }
  bool isFieldDecl() const { return kind == Kind::FieldDecl; }
  bool isFunctionDecl() const { return kind == Kind::FunctionDecl; }
  bool isParamDecl() const { return kind == Kind::ParamDecl; }
  bool isVarDecl() const { return kind == Kind::VarDecl; }

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
  std::vector<ParamDecl *> params;
  Block *body = nullptr;
  bool isComplete = false;

  FunctionDecl(SourceLocation location,
               std::string identifier,
               std::vector<ParamDecl *> params)
      : ValueDecl(
            location, std::move(identifier), Decl::Kind::FunctionDecl, false),
        params(std::move(params)) {}

  void setBody(Block *body);

  void dump(Context &ctx, size_t level = 0) const override;
};

struct StructDecl : public TypeDecl {
  std::vector<FieldDecl *> fields;
  bool isComplete = false;

  StructDecl(SourceLocation location, std::string identifier)
      : TypeDecl(location, std::move(identifier), Decl::Kind::StructDecl) {}

  void setFields(std::vector<FieldDecl *> fields);

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
  Decl *decl;

  DeclRefExpr(SourceLocation location, Decl &decl, Expr::Kind kind)
      : Expr(location, kind),
        decl(&decl) {}

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
  StructDecl *structDecl;
  std::vector<FieldInitStmt *> fieldInitializers;

  StructInstantiationExpr(SourceLocation location,
                          StructDecl *structDecl,
                          std::vector<FieldInitStmt *> fieldInitializers)
      : Expr(location, Expr::Kind::Lvalue),
        structDecl(structDecl),
        fieldInitializers(std::move(fieldInitializers)) {}

  void dump(Context &ctx, size_t level = 0) const override;
};

struct Type {
  virtual bool isKnown() const { return true; }
  virtual bool isBuiltinVoid() const { return false; }
  virtual bool isBuiltinNumber() const { return false; }
  virtual bool isFunctionType() const { return false; }
  virtual bool isStructType() const { return false; }

  virtual bool operator==(const Type &b) const = 0;
  virtual std::string asString() const = 0;
  virtual void dump() const = 0;

  virtual ~Type() = default;
};

struct UninferredType : public Type {
  size_t id;

  UninferredType(size_t id)
      : id(id){};

  bool isKnown() const override { return false; }

  bool operator==(const Type &b) const override;
  std::string asString() const override;
  void dump() const override;
};

struct BuiltinType : public Type {
  enum class Kind { Void, Number };

  Kind kind;

  BuiltinType(Kind kind)
      : kind(kind){};

  bool isBuiltinVoid() const override { return kind == Kind::Void; }
  bool isBuiltinNumber() const override { return kind == Kind::Number; }

  bool operator==(const Type &b) const override;
  std::string asString() const override;
  void dump() const override;
};

struct StructType : public Type {
  const StructDecl *decl;

  StructType(const StructDecl &decl)
      : decl(&decl){};

  bool isStructType() const override { return true; }

  bool operator==(const Type &b) const override;
  std::string asString() const override;
  void dump() const override;
};

struct FunctionType : public Type {
  std::vector<const Type *> args;
  const Type *ret;

  FunctionType(std::vector<const Type *> args, const Type *ret)
      : args(std::move(args)),
        ret(ret){};

  bool isFunctionType() const override { return true; }

  bool operator==(const Type &b) const override;
  std::string asString() const override;
  void dump() const override;
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
  std::vector<std::unique_ptr<Type>> types;

  std::vector<StructDecl *> structs;
  std::vector<FunctionDecl *> functions;

  using EnvKeyTy = std::variant<const Expr *, const Decl *>;
  std::unordered_map<EnvKeyTy, const Type *> environment;
  size_t uninferredTypeIdx = 0;

  Context() = default;

  template <typename T, typename... Args> const Type *getType(Args &&...args) {
    auto typeToGet = std::make_unique<T>(std::forward<Args>(args)...);

    for (auto &&type : types)
      if (*type == *typeToGet)
        return type.get();

    return types.emplace_back(std::move(typeToGet)).get();
  }
  void replace(const Type *t1, const Type *t2);

public:
  static Context createEmptyContext() { return Context{}; }

  const std::vector<StructDecl *> &getStructs() const { return structs; }
  std::vector<StructDecl *> &getStructs() { return structs; }

  const std::vector<FunctionDecl *> &getFunctions() const { return functions; }
  std::vector<FunctionDecl *> &getFunctions() { return functions; }

  const Type *getNewUninferredType();
  const Type *getBuiltinType(const BuiltinType::Kind kind);
  const Type *getFunctionType(std::vector<const Type *> args, const Type *ret);
  const Type *getStructType(const StructDecl &decl);

  bool unify(const Type *t1, const Type *t2);

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

  template <typename T> T *bind(T *node, const Type *type) {
    static_assert(is_env_key<T>(), "");
    environment[node] = type;
    return node;
  }

  template <typename T> const Type *getType(T *node) const {
    static_assert(is_env_key<T>(), "");
    if (environment.count(node) == 0)
      return nullptr;

    return environment.at(node);
  }
};
} // namespace res
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_RES_H
