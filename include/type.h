#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_TYPE_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_TYPE_H

#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace yl {
namespace res {
class Decl;
class TypeParamDecl;
class StructDecl;
class TraitDecl;
class TypedNode;

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

  virtual std::string getName() const { return name; };

  virtual const Type *getRootType() const { return this; }
  virtual ~Type() = default;

protected:
  std::string name;
  std::vector<Type *> args;

  Type(std::string identifier, std::vector<Type *> args)
      : name(std::move(identifier)),
        args(std::move(args)){};

  friend class TypeManager;
};

class UninferredType : public Type {
  Type *parent = nullptr;

  UninferredType(std::string name);

  void infer(Type *t);

public:
  const Type *getRootType() const override;
  std::string getName() const override;

  friend class TypeManager;
};

class BuiltinUnitType : public Type {
  BuiltinUnitType();

  friend class TypeManager;
};

class BuiltinNumberType : public Type {
  BuiltinNumberType();

  friend class TypeManager;
};

class TypeParamType : public Type {
  TypeParamType(TypeParamDecl &decl);

public:
  TypeParamDecl *decl;

  friend class TypeManager;
};

class FunctionType : public Type {
  FunctionType(std::vector<Type *> args);

public:
  std::vector<Type *> getArgs() { return {args.begin(), --args.end()}; }
  std::vector<const Type *> getArgs() const {
    return {args.begin(), --args.end()};
  }

  Type *getReturnType() { return args.back()->getRootType(); }
  const Type *getReturnType() const { return args.back()->getRootType(); }

  std::string getName() const override;

  friend class TypeManager;
};

class StructType : public Type {
  StructDecl *decl;

  StructType(StructDecl &decl, std::vector<Type *> typeArgs);

public:
  StructDecl *getDecl() { return decl; }
  const StructDecl *getDecl() const { return decl; }

  std::vector<Type *> getTypeArgs() const { return args; }

  std::string getName() const override;

  friend class TypeManager;
};

class OutParamType : public Type {
  OutParamType(Type *paramType);

public:
  Type *getParamType() { return args[0]->getRootType(); }
  const Type *getParamType() const { return args[0]->getRootType(); }

  std::string getName() const override { return "&" + args[0]->getName(); }

  friend class TypeManager;
};

class TraitType : public Type {
  TraitDecl *decl;

  TraitType(TraitDecl &decl, std::vector<Type *> args);

public:
  TraitDecl *getDecl() { return decl; }
  const TraitDecl *getDecl() const { return decl; }

  std::vector<Type *> getTypeArgs() const { return args; }
  std::string getName() const override;

  friend class TypeManager;
};

class Substitution : public std::unordered_map<res::Type *, res::Type *> {
  void dump() const;
};

class TypeManager {
  std::vector<std::unique_ptr<Type>> types;
  std::unordered_map<const TypedNode *, Type *> env;

  std::vector<std::pair<Type *, TraitType *>> upperBounds;
  std::unordered_map<UninferredType *, std::vector<TraitType *>> obligations;

  bool unifyImpl(Type *t1, Type *t2, std::vector<std::string> &errors);

public:
  void bind(const TypedNode *node, Type *type) { env[node] = type; }
  Type *getType(const TypedNode *expr) { return env[expr]; }

  void addUpperBound(Decl *decl, TraitType *trait);
  UninferredType *withObligation(UninferredType *type, TraitType *obligation);
  std::vector<TraitType *> getUpperBounds(Type *type);

  Substitution extractSubstitutionFrom(Type *ty);

  UninferredType *getNewUninferredType();
  BuiltinUnitType *getBuiltinUnitType();
  BuiltinNumberType *getBuiltinNumberType();
  FunctionType *getFunctionType(std::vector<Type *> args, Type *ret);
  StructType *getStructType(StructDecl &decl, std::vector<Type *> typeArgs);
  TraitType *getTraitType(TraitDecl &decl, std::vector<Type *> args);
  TypeParamType *getTypeParamType(TypeParamDecl &decl);
  OutParamType *getOutParamType(Type *pointeeType);

  bool moreGeneral(Type *t1, Type *t2);

  // Check if t2 can be substituted with t1... t2 is the type whose traits are
  // checked...
  std::vector<std::string> unify(Type *t1, Type *t2);
  Type *instantiate(Type *t, const Substitution &substitution);
};
} // namespace res
} // namespace yl
#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_TYPE_H
