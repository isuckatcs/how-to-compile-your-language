#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_RES_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_RES_H

#include <memory>
#include <utility>
#include <variant>
#include <vector>

#include "diag.h"
#include "lexer.h"
#include "utils.h"

namespace yl {
namespace res {
struct Stmt;
struct Decl;
struct Type;
struct Block;
struct TraitConformance;
struct TypeExtension;
struct FunctionDecl;
struct TypeParamDecl;
struct TraitType;
struct UninferredType;
struct TraitDecl;
struct StructDecl;

struct Substitution : public std::unordered_map<res::Type *, res::Type *> {
  res::Type *getSelfType() const;

  void dump() const;
};

struct AssociatedDeclRef final {
  res::Type *typeHint = nullptr;
  res::Decl *decl = nullptr;
  res::Substitution sub = {};
};

struct GenericDeclContext {
  GenericDeclContext *parent;
  std::vector<res::TypeParamDecl *> typeParams;

  GenericDeclContext(GenericDeclContext *parent,
                     std::vector<res::TypeParamDecl *> typeParams);

  virtual ~GenericDeclContext() = default;
};

struct TranslationUnit final : public GenericDeclContext {
  std::vector<TraitDecl *> traits;
  std::vector<StructDecl *> structs;
  std::vector<FunctionDecl *> functions;
  std::vector<TypeExtension *> extensions;

  TranslationUnit()
      : GenericDeclContext(nullptr, {}) {}

  void dump(size_t level = 0) const;
};

class Context final {
public:
  enum class QueryState {
    Success,
    Ambiguous,
    Error,
  };

  template <typename T> struct QueryResult final {
    QueryState state = QueryState::Success;
    std::vector<T> items = {};
    std::vector<diag::DiagBuilder> diags = {};
  };

  struct ExtensionCache
      : private std::unordered_map<size_t, QueryResult<TypeExtension *>> {
    void insertIfMissing(Type *type,
                         TraitType *trait,
                         QueryResult<TypeExtension *> result);
    std::optional<QueryResult<TypeExtension *>> get(Type *type,
                                                    TraitType *trait) const;

  private:
    std::hash<std::string> hash = {};

    void buildKey(Type *type, TraitType *trait, std::stringstream &ss) const;
  };

private:
  std::vector<std::unique_ptr<Stmt>> statements;
  std::vector<std::unique_ptr<Decl>> decls;
  std::vector<std::unique_ptr<Type>> types;
  std::vector<std::unique_ptr<Block>> blocks;
  std::vector<std::unique_ptr<TraitConformance>> conformances;
  std::vector<std::unique_ptr<TypeExtension>> extensions;
  std::unique_ptr<TranslationUnit> translationUnit;

  struct UnifyResult final {
    bool success = true;
    bool hasAmbiguousObligations = false;
    std::vector<diag::DiagBuilder> diags = {};
    std::vector<UninferredType *> inferredTypes = {};
    std::unordered_map<UninferredType *, size_t> propagatedObligations = {};
  };

  bool occurs(UninferredType *type, Type *in);
  void doUnify(Type *t1, Type *t2, UnifyResult &result);
  void processObligations(UnifyResult &result, bool allowAmbiguity);
  void rollbackUnify(const UnifyResult &result);

  template <typename Fn> void probe(Fn &&fn);

  const int extensionDepthLimit = 10;
  int extensionDepth = 0;

  ExtensionCache extensionCache;

public:
  Context()
      : translationUnit(std::make_unique<TranslationUnit>()) {}

  void add(std::unique_ptr<Stmt> stmt);
  void add(std::unique_ptr<Decl> decl);
  void add(std::unique_ptr<Type> type);
  void add(std::unique_ptr<Block> block);
  void add(std::unique_ptr<TraitConformance> conformance);
  void add(std::unique_ptr<TypeExtension> extension);

  TranslationUnit *getTU() const { return translationUnit.get(); };

  QueryResult<TypeExtension *> queryExtensions(Type *type, TraitType *trait);
  QueryResult<TraitType *> querySatisfyingTraits(Type *type, TraitType *trait);
  QueryResult<AssociatedDeclRef>
  queryAssociatedDecls(std::string identifier, Type *type, TraitType *trait);

  bool eq(Type *t1, Type *t2) const;
  std::vector<diag::DiagBuilder> unify(Type *t1, Type *t2);

  Type *instantiate(Type *t, const Substitution &sub);
  Substitution instantiate(const Substitution &s, const Substitution &sub);

  Substitution getUninferredInstantiation(GenericDeclContext *declCtx);

  std::vector<TraitType *> getDirectConformance(Type *type);
  std::vector<TraitType *> getEveryConformance(Type *type);

  void dumpEveryFunctionCFG() const;
};

template <typename T> struct Creatable {
  template <typename... Args> static T *create(Context &c, Args &&...args) {
    T *t = new T(std::forward<Args>(args)...);
    c.add(std::unique_ptr<T>(t));
    return t;
  }

private:
  Creatable(){};
  friend T;
};

struct ConstVal : public std::variant<std::monostate, bool, double> {
  using std::variant<std::monostate, bool, double>::variant;

  bool isKnown() const { return index() != 0; }
  std::string asString() const;
};

struct Type {
  template <typename T> T *getAs() { return dynamic_cast<T *>(getRootType()); }

  virtual Type *getRootType() { return this; }
  virtual std::string getName() const;
  virtual Substitution getSub() const { return {}; }

  virtual ~Type() = default;

protected:
  std::string baseName;
  std::vector<Type *> args;
  std::variant<void *, size_t> metadata;

  Type(std::string baseName,
       std::variant<void *, size_t> metadata = nullptr,
       std::vector<Type *> args = {});

  bool isSameKind(Type *other) const;

  friend Context;
};

class TypedNode {
  Type *type = nullptr;

public:
  void setType(Type *type) { this->type = type; }

  Type *getType() const {
    if (type)
      return type->getRootType();
    return nullptr;
  }
};

struct Stmt {
  SourceLocation location;

  virtual ~Stmt() = default;
  virtual void dump(size_t level = 0) const = 0;

protected:
  Stmt(SourceLocation location)
      : location(location) {}
};

struct Expr : public TypedNode, public Stmt {
  enum class ValueCategory { Rvalue, MutLvalue, Lvalue };

  ValueCategory valueCategory;
  ConstVal constVal;

  bool isLvalue() const { return valueCategory != ValueCategory::Rvalue; }
  bool isMutable() const { return valueCategory == ValueCategory::MutLvalue; }

  bool hasConstantValue() const { return constVal.isKnown(); }
  ConstVal getConstantValue() const { return constVal; }
  void setConstantValue(ConstVal val) { constVal = val; }

protected:
  Expr(SourceLocation location, ValueCategory valueCategory)
      : Stmt(location),
        valueCategory(valueCategory) {}
};

struct Decl : public TypedNode {
  std::string identifier;
  SourceLocation location;
  GenericDeclContext *declContext;
  bool needsStorage = false;
  bool used = false;

  virtual ~Decl() = default;

  template <typename T> T *getAs() {
    return const_cast<T *>(const_cast<const Decl *>(this)->getAs<T>());
  }

  template <typename T> const T *getAs() const {
    return dynamic_cast<const T *>(this);
  }

  void setUsed(bool value) { used = value; }
  void setStorageNeeded() { needsStorage = true; }

  virtual void dump(size_t level = 0) const = 0;

protected:
  Decl(SourceLocation location,
       std::string identifier,
       GenericDeclContext *declContext)
      : identifier(std::move(identifier)),
        location(location),
        declContext(declContext) {}
};

struct TypeDecl : public Decl {
  TypeDecl(SourceLocation location,
           std::string identifier,
           GenericDeclContext *declContext)
      : Decl(location, std::move(identifier), declContext) {}
};

struct ValueDecl : public Decl {
  bool isMutable;

  ValueDecl(SourceLocation location,
            std::string identifier,
            GenericDeclContext *declContext,
            bool isMutable)
      : Decl(location, std::move(identifier), declContext),
        isMutable(isMutable) {}
};

struct ParamDecl final : public Creatable<ParamDecl>, public ValueDecl {
  void dump(size_t level = 0) const override;

private:
  ParamDecl(SourceLocation l, std::string i, GenericDeclContext *c, bool m);
  friend Creatable<ParamDecl>;
};

struct TraitConformance;

struct TraitDecl final : public Creatable<TraitDecl>,
                         public TypeDecl,
                         public GenericDeclContext {
  TraitConformance *conformance = nullptr;
  std::vector<res::FunctionDecl *> functions;

  res::FunctionDecl *lookupFunction(const std::string &id) const;

  void dump(size_t level = 0) const override;

private:
  TraitDecl(SourceLocation l,
            std::string i,
            GenericDeclContext *c,
            std::vector<TypeParamDecl *> p);
  friend Creatable<TraitDecl>;
};

struct TypeParamDecl final : public Creatable<TypeParamDecl>, public TypeDecl {
  TraitConformance *conformance = nullptr;
  bool isImplicitSelf;

  void setDeclContext(GenericDeclContext *declContext) {
    this->declContext = declContext;
  }

  void dump(size_t level = 0) const override;

private:
  TypeParamDecl(SourceLocation l, std::string i, bool s = false);
  friend Creatable<TypeParamDecl>;
};

struct FieldDecl final : public Creatable<FieldDecl>, public ValueDecl {
  void dump(size_t level = 0) const override;

private:
  FieldDecl(SourceLocation l, std::string i, GenericDeclContext *c);
  friend Creatable<FieldDecl>;
};

struct VarDecl final : public Creatable<VarDecl>, public ValueDecl {
  Expr *initializer;

  void dump(size_t level = 0) const override;

private:
  VarDecl(SourceLocation location,
          std::string identifier,
          GenericDeclContext *declContext,
          bool isMutable,
          Expr *initializer = nullptr);
  friend Creatable<VarDecl>;
};

struct StructDecl final : public Creatable<StructDecl>,
                          public TypeDecl,
                          public GenericDeclContext {
  std::vector<res::FieldDecl *> fields;
  bool isLambda;

  res::FieldDecl *lookupField(const std::string &id) const;

  void dump(size_t level = 0) const override;

private:
  StructDecl(SourceLocation location,
             std::string identifier,
             GenericDeclContext *declContext,
             std::vector<TypeParamDecl *> typeParams,
             bool isLambda = false);
  friend Creatable<StructDecl>;
};

struct FunctionDecl final : public Creatable<FunctionDecl>,
                            public ValueDecl,
                            public GenericDeclContext {
  std::vector<ParamDecl *> params;
  Block *body = nullptr;
  bool mustImplement = false;
  bool isBuiltin = false;

  void setBuiltin(bool b) { isBuiltin = b; }
  void setMustImplement(bool b) { mustImplement = b; }
  void setBody(Block *body) { this->body = body; }
  void setParams(std::vector<ParamDecl *> params) {
    this->params = std::move(params);
  };

  void dump(size_t level = 0) const override;

private:
  FunctionDecl(SourceLocation location,
               std::string identifier,
               GenericDeclContext *declContext,
               std::vector<TypeParamDecl *> typeParams = {});
  friend Creatable<FunctionDecl>;
};

class BuiltinUnitType final : public Creatable<BuiltinUnitType>, public Type {
  BuiltinUnitType();
  friend Creatable<BuiltinUnitType>;
};

class BuiltinNumberType final : public Creatable<BuiltinNumberType>,
                                public Type {
  BuiltinNumberType();
  friend Creatable<BuiltinNumberType>;
};

class BuiltinBoolType final : public Creatable<BuiltinBoolType>, public Type {
  BuiltinBoolType();
  friend Creatable<BuiltinBoolType>;
};

struct TypeParamType final : public Creatable<TypeParamType>, public Type {
  TypeParamDecl *getDecl() const {
    return static_cast<TypeParamDecl *>(std::get<void *>(metadata));
  }

private:
  explicit TypeParamType(TypeParamDecl *decl);

  friend Creatable<TypeParamType>;
};

struct FunctionType final : public Creatable<FunctionType>, public Type {
  std::string getName() const override;
  std::vector<Type *> getArgs() const { return {args.begin(), --args.end()}; }
  Type *getReturnType() const { return args.back()->getRootType(); }

private:
  explicit FunctionType(std::vector<Type *> args, Type *ret);

  friend Creatable<FunctionType>;
};

struct RefType final : public Creatable<RefType>, public Type {
  Type *getReferencedType() const { return args[0]->getRootType(); }
  bool isMutable() const { return std::get<size_t>(metadata); }
  std::string getName() const override { return baseName + args[0]->getName(); }

private:
  RefType(Type *referencedType, bool isMutable);

  friend Creatable<RefType>;
};

struct PointerType final : public Creatable<PointerType>, public Type {
  Type *getPointeeType() const { return args[0]->getRootType(); }
  bool isMutable() const { return std::get<size_t>(metadata); }
  std::string getName() const override { return baseName + args[0]->getName(); }

private:
  PointerType(Type *pointeeType, bool isMutable);

  friend Creatable<PointerType>;
};

class StructType final : public Creatable<StructType>, public Type {
  StructType(StructDecl *decl, std::vector<Type *> typeArgs = {});

public:
  StructDecl *getDecl() const {
    return static_cast<StructDecl *>(std::get<void *>(metadata));
  }

  std::vector<Type *> getTypeArgs() const { return args; }
  Substitution getSub() const override;

  friend Creatable<StructType>;
};

struct TraitType final : public Creatable<TraitType>, public Type {
  TraitDecl *getDecl() const {
    return static_cast<TraitDecl *>(std::get<void *>(metadata));
  }
  std::vector<Type *> getTypeArgs() const { return args; }
  std::string getName() const override;
  Substitution getSub() const override;
  std::vector<std::pair<TraitType *, FunctionDecl *>>
  getVtableLayout(Context *ctx);

private:
  TraitType(TraitDecl *decl, std::vector<Type *> args = {});

  friend Creatable<TraitType>;
};

struct AnyTraitType final : public Creatable<AnyTraitType>, public Type {
  TraitDecl *getDecl() const {
    return static_cast<TraitDecl *>(std::get<void *>(metadata));
  }
  std::vector<Type *> getTypeArgs() const { return args; }
  Substitution getSub() const override;

  TraitType *withSelfType(Context *ctx, Type *selfType) const;

private:
  AnyTraitType(TraitDecl *decl, std::vector<Type *> args);

  friend Creatable<AnyTraitType>;
};

struct UninferredType final : public Creatable<UninferredType>, public Type {
  Type *getRootType() override;
  std::string getName() const override;

  void addObligation(res::TraitType *trait);

private:
  inline static size_t nextId = 0;

  Type *parent = nullptr;
  std::vector<res::TraitType *> obligations;

  UninferredType();

  void setParent(Type *t) { parent = t; };

  friend Creatable<UninferredType>;
  friend Context;
};

struct Block final : public Creatable<Block> {
  SourceLocation location;
  std::vector<Stmt *> statements;

  void dump(size_t level = 0) const;

private:
  Block(SourceLocation l, std::vector<Stmt *> s);
  friend Creatable<Block>;
};

struct IfStmt final : public Creatable<IfStmt>, public Stmt {
  Expr *condition;
  Block *trueBlock;
  Block *falseBlock;

  void dump(size_t level = 0) const override;

private:
  IfStmt(SourceLocation l, Expr *c, Block *t, Block *f = nullptr);
  friend Creatable<IfStmt>;
};

struct WhileStmt final : public Creatable<WhileStmt>, public Stmt {
  Expr *condition;
  Block *body;

  void dump(size_t level = 0) const override;

private:
  WhileStmt(SourceLocation l, Expr *c, Block *b);
  friend Creatable<WhileStmt>;
};

struct TraitConformance final : public Creatable<TraitConformance> {
  SourceLocation location;
  res::Type *type;
  std::vector<res::TraitType *> traits;

  void dump(size_t level = 0) const;

private:
  TraitConformance(SourceLocation l,
                   res::Type *t,
                   std::vector<res::TraitType *> ts);
  friend Creatable<TraitConformance>;
};

struct TypeExtension final : public Creatable<TypeExtension>,
                             public GenericDeclContext {
  SourceLocation location;
  Type *type;
  TraitType *trait;
  std::vector<res::FunctionDecl *> functions;

  res::FunctionDecl *getFunction(const std::string &id) const;

  void dump(size_t level = 0) const;

private:
  TypeExtension(SourceLocation location,
                std::vector<TypeParamDecl *> typeParams,
                Type *type,
                TraitType *trait);
  friend Creatable<TypeExtension>;
};

struct NumberLiteral final : public Creatable<NumberLiteral>, public Expr {
  double value;

  void dump(size_t level = 0) const override;

private:
  NumberLiteral(SourceLocation location, double value);
  friend Creatable<NumberLiteral>;
};

struct BoolLiteral final : public Creatable<BoolLiteral>, public Expr {
  bool value;

  void dump(size_t level = 0) const override;

private:
  BoolLiteral(SourceLocation location, bool value);
  friend Creatable<BoolLiteral>;
};

struct UnitLiteral final : public Creatable<UnitLiteral>, public Expr {
  void dump(size_t level = 0) const override;

private:
  explicit UnitLiteral(SourceLocation location);
  friend Creatable<UnitLiteral>;
};

struct CallExpr final : public Creatable<CallExpr>, public Expr {
  Expr *callee;
  std::vector<Expr *> arguments;

  void addArg(res::Expr *arg) { arguments.emplace_back(arg); }

  void dump(size_t level = 0) const override;

private:
  CallExpr(SourceLocation location, Expr *callee);
  friend Creatable<CallExpr>;
};

struct DeclRefExpr final : public Creatable<DeclRefExpr>, public Expr {
  Decl *decl;
  Substitution sub;

  void setPath(std::vector<res::DeclRefExpr *> path) {
    this->path = std::move(path);
  }

  const std::vector<res::DeclRefExpr *> &getPath() const { return path; }

  void dump(size_t level = 0) const override;

private:
  std::vector<res::DeclRefExpr *> path;

  DeclRefExpr(SourceLocation loc,
              Decl *d,
              Expr::ValueCategory valueCategory,
              Substitution sub);
  friend Creatable<DeclRefExpr>;
};

struct MemberExpr final : public Creatable<MemberExpr>, public Expr {
  Expr *base;
  DeclRefExpr *member;

  void dump(size_t level = 0) const override;

private:
  MemberExpr(SourceLocation location, Expr *base, DeclRefExpr *member);
  friend Creatable<MemberExpr>;
};

struct BinaryOperator final : public Creatable<BinaryOperator>, public Expr {
  TokenKind op;
  Expr *lhs;
  Expr *rhs;

  void dump(size_t level = 0) const override;

private:
  BinaryOperator(SourceLocation location, TokenKind op, Expr *lhs, Expr *rhs);
  friend Creatable<BinaryOperator>;
};

struct UnaryOperator final : public Creatable<UnaryOperator>, public Expr {
  TokenKind op;
  Expr *operand;

  void dump(size_t level = 0) const override;

private:
  UnaryOperator(SourceLocation loc,
                TokenKind op,
                Expr *e,
                Expr::ValueCategory valueCategory);
  friend Creatable<UnaryOperator>;
};

struct DeclStmt final : public Creatable<DeclStmt>, public Stmt {
  VarDecl *varDecl;

  void dump(size_t level = 0) const override;

private:
  DeclStmt(SourceLocation location, VarDecl *varDecl);
  friend Creatable<DeclStmt>;
};

struct Assignment final : public Creatable<Assignment>, public Stmt {
  Expr *assignee;
  Expr *expr;

  void dump(size_t level = 0) const override;

private:
  Assignment(SourceLocation location, Expr *assignee, Expr *expr);
  friend Creatable<Assignment>;
};

struct ReturnStmt final : public Creatable<ReturnStmt>, public Stmt {
  Expr *expr;

  void dump(size_t level = 0) const override;

private:
  ReturnStmt(SourceLocation location, Expr *expr = nullptr);
  friend Creatable<ReturnStmt>;
};

struct FieldInitStmt final : public Creatable<FieldInitStmt>, public Stmt {
  FieldDecl *field;
  Expr *initializer;

  void dump(size_t level = 0) const override;

private:
  FieldInitStmt(SourceLocation loc, FieldDecl *field, Expr *init);
  friend Creatable<FieldInitStmt>;
};

struct StructInstantiationExpr final
    : public Creatable<StructInstantiationExpr>,
      public Expr {
  DeclRefExpr *structPath;
  std::vector<FieldInitStmt *> fieldInitializers;

  void dump(size_t level = 0) const override;

private:
  StructInstantiationExpr(SourceLocation loc,
                          DeclRefExpr *dre,
                          std::vector<FieldInitStmt *> inits);
  friend Creatable<StructInstantiationExpr>;
};

struct ImplicitDerefExpr final : public Creatable<ImplicitDerefExpr>,
                                 public Expr {
  DeclRefExpr *dre;

  void dump(size_t level = 0) const override;

private:
  ImplicitDerefExpr(SourceLocation location, DeclRefExpr *dre);
  friend Creatable<ImplicitDerefExpr>;
};

struct GCExpr final : public Creatable<GCExpr>, public Expr {
  Expr *expr;

  void dump(size_t level = 0) const override;

private:
  GCExpr(SourceLocation location, Expr *expr);
  friend Creatable<GCExpr>;
};

struct LambdaExpr final : public Creatable<LambdaExpr>, public Expr {
  std::vector<res::Expr *> fieldInits;
  res::StructDecl *closure;
  res::TypeExtension *ext;

  res::FunctionDecl *getFunction() const { return ext->functions[0]; }

  void dump(size_t level = 0) const override;

private:
  LambdaExpr(SourceLocation location,
             res::StructDecl *closure,
             res::TypeExtension *ext,
             std::vector<res::Expr *> fieldInits = {});
  friend Creatable<LambdaExpr>;
};

struct ImplicitPtrToRefDecay final : public Creatable<ImplicitPtrToRefDecay>,
                                     public Expr {
  res::Expr *expr;

  void dump(size_t level = 0) const override;

private:
  ImplicitPtrToRefDecay(SourceLocation location, res::Expr *expr);
  friend Creatable<ImplicitPtrToRefDecay>;
};

struct ImplicitAsRefExpr final : public Creatable<ImplicitAsRefExpr>,
                                 public Expr {
  res::Expr *expr;

  void dump(size_t level = 0) const override;

private:
  ImplicitAsRefExpr(SourceLocation location, res::Expr *expr);
  friend Creatable<ImplicitAsRefExpr>;
};

struct MaterializeTemporaryExpr final
    : public Creatable<MaterializeTemporaryExpr>,
      public Expr {
  res::Expr *expr;

  void dump(size_t level = 0) const override;

private:
  MaterializeTemporaryExpr(SourceLocation location, res::Expr *expr);
  friend Creatable<MaterializeTemporaryExpr>;
};

struct TraitObjectPromoExpr final : public Creatable<TraitObjectPromoExpr>,
                                    public Expr {
  res::Expr *expr;

  void dump(size_t level = 0) const override;

private:
  TraitObjectPromoExpr(SourceLocation location, res::Expr *expr);
  friend Creatable<TraitObjectPromoExpr>;
};
} // namespace res
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_RES_H
