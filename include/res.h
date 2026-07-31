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
class Stmt;
class Decl;
class Block;
class TraitConformance;
class TypeExtension;
struct TypeParamDecl;

struct GenericDeclContext {
  GenericDeclContext *parent;
  std::vector<res::Decl *> decls;
  std::vector<res::TypeParamDecl *> typeParams;

  GenericDeclContext(GenericDeclContext *parent,
                     std::vector<res::TypeParamDecl *> typeParams);

  virtual ~GenericDeclContext() = default;

  void insertDecl(res::Decl *decl) { decls.emplace_back(decl); }
  std::vector<res::Decl *> lookupDirect(const std::string id) const;

  template <typename T> std::vector<T *> getAll() const {
    std::vector<T *> out;
    for (auto &&decl : decls)
      if (auto *d = dynamic_cast<T *>(decl))
        out.emplace_back(d);
    return out;
  }
};

struct TranslationUnit final : public GenericDeclContext {
  std::vector<TypeExtension *> extensions;

  TranslationUnit()
      : GenericDeclContext(nullptr, {}) {}

  void dump(size_t level = 0) const;
};

class Context final {
  std::vector<std::unique_ptr<Stmt>> statements;
  std::vector<std::unique_ptr<Decl>> decls;
  std::vector<std::unique_ptr<Block>> blocks;
  std::vector<std::unique_ptr<TraitConformance>> conformances;
  std::vector<std::unique_ptr<TypeExtension>> extensions;

public:
  TranslationUnit translationUnit;

  void add(std::unique_ptr<Stmt> stmt);
  void add(std::unique_ptr<Decl> decl);
  void add(std::unique_ptr<Block> block);
  void add(std::unique_ptr<TraitConformance> conformance);
  void add(std::unique_ptr<TypeExtension> extension);
};

template <typename T> struct Creatable {
  template <typename... Args> static T *create(Context &c, Args... args) {
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

class TypedNode {
  Type *type = nullptr;

public:
  void setType(Type *type) { this->type = type; }
  Type *getType() const { return type->getRootType(); }
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
  enum class Kind { Rvalue, MutLvalue, Lvalue };

  Kind kind;
  ConstVal constVal;

  bool isLvalue() const { return kind != Kind::Rvalue; }
  bool isMutable() const { return kind == Kind::MutLvalue; }

  bool hasConstantValue() const { return constVal.isKnown(); }
  ConstVal getConstantValue() const { return constVal; }
  void setConstantValue(ConstVal val) { constVal = val; }

protected:
  Expr(SourceLocation location, Kind kind)
      : Stmt(location),
        kind(kind) {}
};

struct Decl : public TypedNode {
  std::string identifier;
  SourceLocation location;
  GenericDeclContext *declContext;
  bool needsStorage = false;

  virtual ~Decl() = default;

  template <typename T> T *getAs() {
    return const_cast<T *>(const_cast<const Decl *>(this)->getAs<T>());
  }

  template <typename T> const T *getAs() const {
    return dynamic_cast<const T *>(this);
  }

  void setStorageNeeded() { needsStorage = true; }
  void setDeclContext(GenericDeclContext *declContext) {
    this->declContext = declContext;
  }
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

  void dump(size_t level = 0) const override;

private:
  TraitDecl(SourceLocation l,
            std::string i,
            GenericDeclContext *c,
            std::vector<TypeParamDecl *> p);
  friend Creatable<TraitDecl>;
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

  void dump(size_t level = 0) const;

private:
  TypeExtension(SourceLocation location,
                std::vector<TypeParamDecl *> typeParams,
                Type *type,
                TraitType *trait);
  friend Creatable<TypeExtension>;
};

struct TypeParamDecl final : public Creatable<TypeParamDecl>, public TypeDecl {
  TraitConformance *conformance = nullptr;
  bool isImplicitSelf;

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
  bool isLambda;

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

  bool isVirtual() const;
  void dump(size_t level = 0) const override;

private:
  CallExpr(SourceLocation location, Expr *callee, std::vector<Expr *> args);
  friend Creatable<CallExpr>;
};

struct DeclRefExpr final : public Creatable<DeclRefExpr>, public Expr {
  Decl *decl;
  Substitution sub;

  Type *getReceiverType() const;

  void dump(size_t level = 0) const override;

private:
  DeclRefExpr(SourceLocation loc, Decl *d, Expr::Kind kind, Substitution sub);
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

struct GroupingExpr final : public Creatable<GroupingExpr>, public Expr {
  Expr *expr;

  void dump(size_t level = 0) const override;

private:
  GroupingExpr(SourceLocation location, Expr *expr);
  friend Creatable<GroupingExpr>;
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
  UnaryOperator(SourceLocation loc, TokenKind op, Expr *e, Expr::Kind kind);
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
  res::StructDecl *closure;
  std::vector<res::Expr *> fieldInits;
  res::FunctionDecl *method;

  void dump(size_t level = 0) const override;

private:
  LambdaExpr(SourceLocation location,
             res::StructDecl *closure,
             res::FunctionDecl *method,
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
