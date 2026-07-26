#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_TYPE_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_TYPE_H

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace yl {
namespace res {
class Decl;
class TypeParamDecl;
class FunctionDecl;
class StructDecl;
class TraitDecl;
class TypedNode;
class ExtensionDecl;
class TraitInstance;

struct Type {
  template <typename T> T *getAs() {
    return const_cast<T *>(const_cast<const Type *>(this)->getAs<T>());
  }

  template <typename T> const T *getAs() const {
    static_assert(std::is_base_of_v<Type, T>, "expected type");
    return dynamic_cast<const T *>(getRootType());
  }

  virtual Type *getRootType() {
    return const_cast<Type *>(const_cast<const Type *>(this)->getRootType());
  }

  virtual const Type *getRootType() const { return this; }

  virtual std::string getName() const { return name; };

  virtual bool isSameKind(const Type *other) const = 0;

  virtual ~Type() = default;

protected:
  std::string name;
  std::vector<Type *> args;

  Type(std::string identifier, std::vector<Type *> args = {})
      : name(std::move(identifier)),
        args(std::move(args)){};

  friend class TypeManager;
};

class UninferredType : public Type {
  Type *parent = nullptr;
  size_t id;

  UninferredType(size_t id);

  void infer(Type *t);
  void reset();

  bool isSameKind(const Type *other) const override;

public:
  Type *getRootType() override;
  const Type *getRootType() const override;
  std::string getName() const override;

  friend class TypeManager;
};

class BuiltinUnitType : public Type {
  BuiltinUnitType();

  bool isSameKind(const Type *other) const override;

  friend class TypeManager;
};

class BuiltinNumberType : public Type {
  BuiltinNumberType();

  bool isSameKind(const Type *other) const override;

  friend class TypeManager;
};

class BuiltinBoolType : public Type {
  BuiltinBoolType();

  bool isSameKind(const Type *other) const override;

  friend class TypeManager;
};

class TypeParamType : public Type {
  TypeParamType(TypeParamDecl &decl);

  bool isSameKind(const Type *other) const override;

public:
  TypeParamDecl *decl;

  friend class TypeManager;
};

class FunctionType : public Type {
  FunctionType(std::vector<Type *> args);

  bool isSameKind(const Type *other) const override;

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

  bool isSameKind(const Type *other) const override;

public:
  StructDecl *getDecl() { return decl; }
  const StructDecl *getDecl() const { return decl; }

  std::vector<Type *> getTypeArgs() const { return args; }

  std::string getName() const override;

  friend class TypeManager;
};

class BorrowedType : public Type {
  bool isMut;

  BorrowedType(Type *borrowedType, bool isMutable);

  bool isSameKind(const Type *other) const override;

public:
  Type *getBorrowedType() { return args[0]->getRootType(); }
  const Type *getBorrowedType() const { return args[0]->getRootType(); }

  bool isMutable() const { return isMut; }
  std::string getName() const override {
    return name + " " + args[0]->getName();
  }

  friend class TypeManager;
};

class PointerType : public Type {
  bool isMut;

  PointerType(Type *pointeeType, bool isMutable);

  bool isSameKind(const Type *other) const override;

public:
  Type *getPointeeType() { return args[0]->getRootType(); }
  const Type *getPointeeType() const { return args[0]->getRootType(); }

  bool isMutable() const { return isMut; }
  std::string getName() const override { return name + args[0]->getName(); }

  friend class TypeManager;
};

class TraitType : public Type {
  TraitDecl *decl;

  TraitType(TraitDecl &decl, std::vector<Type *> args);

  bool isSameKind(const Type *other) const override;

public:
  TraitDecl *getDecl() { return decl; }
  const TraitDecl *getDecl() const { return decl; }

  std::vector<Type *> getTypeArgs() const { return args; }
  std::string getName() const override;

  friend class TypeManager;
};

// FIXME: is this needed when there is any type?
class AnyTraitType : public Type {
  TraitDecl *decl;

  AnyTraitType(TraitDecl &decl, std::vector<Type *> args);

  bool isSameKind(const Type *other) const override;

public:
  TraitDecl *getDecl() { return decl; }
  const TraitDecl *getDecl() const { return decl; }

  std::vector<Type *> getTypeArgs() const { return args; }
  std::string getName() const override;

  friend class TypeManager;
};

class AnyType : public Type {
  AnyType(res::AnyTraitType *trait);

  bool isSameKind(const Type *other) const override;

public:
  res::AnyTraitType *getTrait() {
    return args[0]->getRootType()->getAs<res::AnyTraitType>();
  }
  const res::AnyTraitType *getTrait() const {
    return args[0]->getRootType()->getAs<res::AnyTraitType>();
  }

  std::string getName() const override;

  friend class TypeManager;
};

// struct ExtensionInfo {
//   std::vector<TypeParamType *> typeParams;
//   Type *type;
//   TraitType *trait;
//   TypeExtension *extensionNode;

//   void dump() const;
// };

class Substitution : public std::unordered_map<const res::Type *, res::Type *> {
  void dump() const;
};

class TypeManager {
  size_t uninferredTypeId = 0;
  std::vector<std::unique_ptr<Type>> types;
  // FIXME: this should only contain the trait prerequisites
  std::vector<std::pair<Type *, TraitType *>> constraints;
  std::vector<ExtensionDecl *> extensions;
  std::unordered_map<UninferredType *, std::vector<TraitType *>> obligations;

  using VtableEntryTy =
      std::pair<const res::TraitType *, const res::FunctionDecl *>;
  using VtableLayoutTy = std::vector<VtableEntryTy>;
  std::vector<std::pair<const TraitType *, VtableLayoutTy>> vtableLayouts;

  std::vector<std::string>
  unifyImpl(Type *t1, Type *t2, std::vector<UninferredType *> &inferredTypes);
  std::vector<std::string>
  checkObligations(std::vector<UninferredType *> &inferredTypes);

public:
  void addConstraint(Type *type, TraitType *trait);
  std::vector<TraitType *> getConstraints(const Type *type);
  bool hasConstraint(Type *type, TraitType *trait);

  void createObligation(UninferredType *type, TraitType *trait);

  void addExtension(ExtensionDecl *typeExtension);
  std::vector<std::pair<ExtensionDecl *, Substitution>>
  getExtensions(Type *type, TraitType *trait = nullptr, bool probeOnly = false);

  Substitution extractSubstitutionFrom(const Type *ty);

  UninferredType *getNewUninferredType();
  BuiltinUnitType *getBuiltinUnitType();
  BuiltinNumberType *getBuiltinNumberType();
  BuiltinBoolType *getBuiltinBoolType();
  FunctionType *getFunctionType(std::vector<Type *> args, Type *ret);
  StructType *getStructType(StructDecl &decl, std::vector<Type *> typeArgs);
  TraitType *getTraitType(TraitDecl &decl, std::vector<Type *> args);
  AnyTraitType *getAnyTraitType(TraitDecl &decl, std::vector<Type *> args);
  TypeParamType *getTypeParamType(TypeParamDecl &decl);
  BorrowedType *getBorrowedType(Type *borrowedType, bool isMutable);
  PointerType *getPointerType(Type *pointeeType, bool isMutable);
  AnyType *getAnyType(AnyTraitType *trait);

  bool eq(const Type *t1, const Type *t2) const;
  std::vector<std::string> unify(Type *t1, Type *t2, bool probeOnly = false);
  Type *instantiate(Type *t, const Substitution &substitution);

  // FIXME: these should live in the types
  TraitType *withSelfType(AnyTraitType *anyTraitType, Type *selfType);
  VtableLayoutTy getVtableLayout(const res::TraitType *trait);
};
} // namespace res
} // namespace yl
#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_TYPE_H
