#include <iostream>
#include <sstream>

#include "res.h"
#include "type.h"

namespace yl {
namespace res {
UninferredType::UninferredType(size_t id)
    : Type("t" + std::to_string(id)){};

bool UninferredType::isSameKind(const Type *other) const {
  const auto *u = other->getAs<UninferredType>();
  return u && u->id == id;
};

void UninferredType::infer(Type *t) {
  assert(!parent && "already inferred");
  parent = t;
}

void UninferredType::reset() { parent = nullptr; }

Type *UninferredType::getRootType() {
  if (parent)
    return parent->getRootType();
  return this;
}

const Type *UninferredType::getRootType() const {
  if (parent)
    return parent->getRootType();
  return this;
}

std::string UninferredType::getName() const {
  if (parent)
    return parent->getName();
  return "_";
};

BuiltinUnitType::BuiltinUnitType()
    : Type("unit"){};

bool BuiltinUnitType::isSameKind(const Type *other) const {
  return other->getAs<BuiltinUnitType>();
};

BuiltinNumberType::BuiltinNumberType()
    : Type("number"){};

bool BuiltinNumberType::isSameKind(const Type *other) const {
  return other->getAs<BuiltinNumberType>();
};

BuiltinBoolType::BuiltinBoolType()
    : Type("bool"){};

bool BuiltinBoolType::isSameKind(const Type *other) const {
  return other->getAs<BuiltinBoolType>();
};

TypeParamType::TypeParamType(TypeParamDecl &decl)
    : Type(decl.identifier),
      decl(&decl) {}

bool TypeParamType::isSameKind(const Type *other) const {
  const auto *p = other->getAs<TypeParamType>();
  return p && p->decl == decl;
};

FunctionType::FunctionType(std::vector<Type *> args)
    : Type("fn", std::move(args)) {}

bool FunctionType::isSameKind(const Type *other) const {
  const auto *f = other->getAs<FunctionType>();
  return f && f->args.size() == args.size();
};

std::string FunctionType::getName() const {
  std::stringstream ss;
  // FIXME: repeated pattern
  ss << '(';
  for (int i = 0; i < args.size() - 1; ++i) {
    ss << args[i]->getRootType()->getName();

    if (i < args.size() - 2)
      ss << ',' << ' ';
  }
  ss << ") -> " << getReturnType()->getName();

  return ss.str();
}

StructType::StructType(StructDecl &decl, std::vector<Type *> typeArgs)
    : Type(decl.identifier, std::move(typeArgs)),
      decl(&decl){};

bool StructType::isSameKind(const Type *other) const {
  const auto *s = other->getAs<StructType>();
  return s && s->decl == decl;
};

std::string StructType::getName() const {
  std::stringstream ss;
  ss << decl->identifier;

  if (!args.empty()) {
    ss << '<';
    for (int i = 0; i < args.size(); ++i) {
      ss << args[i]->getName();

      if (i < args.size() - 1)
        ss << ',' << ' ';
    }
    ss << '>';
  }

  return ss.str();
}

BorrowedType::BorrowedType(Type *borrowedType, bool isMutable)
    : Type(isMutable ? "borrowed mut" : "borrowed",
           std::vector<res::Type *>{borrowedType}),
      isMut(isMutable){};

bool BorrowedType::isSameKind(const Type *other) const {
  const auto *b = other->getAs<BorrowedType>();
  return b && b->isMut == isMut;
};

PointerType::PointerType(Type *pointeeType, bool isMutable)
    : Type(isMutable ? "*mut " : "*", std::vector<res::Type *>{pointeeType}),
      isMut(isMutable){};

bool PointerType::isSameKind(const Type *other) const {
  const auto *p = other->getAs<PointerType>();
  return p && p->isMut == isMut;
};

TraitType::TraitType(TraitDecl &decl, std::vector<Type *> args)
    : Type(decl.identifier, std::move(args)),
      decl(&decl) {}

bool TraitType::isSameKind(const Type *other) const {
  const auto *t = other->getAs<TraitType>();
  return t && t->decl == decl;
};

std::string TraitType::getName() const {
  std::stringstream ss;
  ss << decl->identifier;

  if (args.size() > 1) {
    ss << '<';
    // FIXME: print Self type as well
    for (int i = 1; i < args.size(); ++i) {
      ss << args[i]->getName();

      if (i < args.size() - 1)
        ss << ',' << ' ';
    }
    ss << '>';
  }

  return ss.str();
}

AnyTraitType::AnyTraitType(TraitDecl &decl, std::vector<Type *> args)
    : Type(decl.identifier, std::move(args)),
      decl(&decl) {}

bool AnyTraitType::isSameKind(const Type *other) const {
  const auto *t = other->getAs<AnyTraitType>();
  return t && t->decl == decl;
};

std::string AnyTraitType::getName() const {
  std::stringstream ss;
  ss << decl->identifier;

  if (!args.empty()) {
    ss << '<';
    for (int i = 0; i < args.size(); ++i) {
      ss << args[i]->getName();

      if (i < args.size() - 1)
        ss << ',' << ' ';
    }
    ss << '>';
  }

  return ss.str();
}

AnyType::AnyType(res::AnyTraitType *trait)
    : Type("any", {trait}) {}

bool AnyType::isSameKind(const Type *other) const {
  return other->getAs<AnyType>();
};

std::string AnyType::getName() const {
  return name + ' ' + getTrait()->getName();
}

void Substitution::dump() const {
  for (auto &&[from, to] : *this)
    std::cerr << from->getName() << " -> " << to->getName() << '\n';
}

// FIXME: what if the substitution is stored in the type?
Substitution TypeManager::extractSubstitutionFrom(const Type *ty) {
  if (!ty)
    return {};

  Substitution sub;

  std::vector<res::TypeParamDecl *> from;
  std::vector<res::Type *> to;

  if (auto *structTy = ty->getAs<res::StructType>()) {
    from = structTy->decl->typeParams;
    to = structTy->getTypeArgs();
  }

  if (auto *traitTy = ty->getAs<res::TraitType>()) {
    from = traitTy->decl->typeParams;
    to = traitTy->getTypeArgs();
  }

  if (auto *traitTy = ty->getAs<res::AnyTraitType>()) {
    auto declParams = traitTy->decl->typeParams;
    // FIXME: Skip Self
    from.insert(from.end(), declParams.begin() + 1, declParams.end());
    to = traitTy->getTypeArgs();
  }

  for (int i = 0; i < from.size(); ++i)
    sub[from[i]->getType()] = to[i];

  return sub;
}

UninferredType *TypeManager::getNewUninferredType() {
  auto *typeVariable = new UninferredType(uninferredTypeId++);
  types.emplace_back(std::unique_ptr<UninferredType>(typeVariable));
  return typeVariable;
}

BuiltinUnitType *TypeManager::getBuiltinUnitType() {
  static BuiltinUnitType unitType;
  return &unitType;
}

BuiltinNumberType *TypeManager::getBuiltinNumberType() {
  static BuiltinNumberType numberType;
  return &numberType;
}

BuiltinBoolType *TypeManager::getBuiltinBoolType() {
  static BuiltinBoolType boolType;
  return &boolType;
}

FunctionType *TypeManager::getFunctionType(std::vector<Type *> args,
                                           Type *ret) {
  args.emplace_back(ret);
  auto *fnTy = new FunctionType(std::move(args));
  types.emplace_back(std::unique_ptr<FunctionType>(fnTy));
  return fnTy;
}

StructType *TypeManager::getStructType(res::StructDecl &decl,
                                       std::vector<Type *> typeArgs) {
  auto *structTy = new StructType(decl, std::move(typeArgs));
  types.emplace_back(std::unique_ptr<StructType>(structTy));
  return structTy;
}

TraitType *TypeManager::getTraitType(TraitDecl &decl,
                                     std::vector<Type *> args) {
  auto *traitTy = new TraitType(decl, std::move(args));
  types.emplace_back(std::unique_ptr<TraitType>(traitTy));
  return traitTy;
}

AnyTraitType *TypeManager::getAnyTraitType(TraitDecl &decl,
                                           std::vector<Type *> args) {
  auto *traitTy = new AnyTraitType(decl, std::move(args));
  types.emplace_back(std::unique_ptr<AnyTraitType>(traitTy));
  return traitTy;
}

TypeParamType *TypeManager::getTypeParamType(TypeParamDecl &decl) {
  auto *typeParamTy = new TypeParamType(decl);
  types.emplace_back(std::unique_ptr<TypeParamType>(typeParamTy));
  return typeParamTy;
}

BorrowedType *TypeManager::getBorrowedType(Type *borrowedType, bool isMutable) {
  auto *ptrTy = new BorrowedType(borrowedType, isMutable);
  types.emplace_back(std::unique_ptr<BorrowedType>(ptrTy));
  return ptrTy;
}

PointerType *TypeManager::getPointerType(Type *pointeeType, bool isMutable) {
  auto *ptrTy = new PointerType(pointeeType, isMutable);
  types.emplace_back(std::unique_ptr<PointerType>(ptrTy));
  return ptrTy;
}

AnyType *TypeManager::getAnyType(AnyTraitType *trait) {
  auto *implTy = new AnyType(trait);
  types.emplace_back(std::unique_ptr<AnyType>(implTy));
  return implTy;
}

void TypeManager::addConstraint(Type *type, TraitType *trait) {
  constraints.emplace_back(type, trait);
}

void TypeManager::addExtension(ExtensionDecl *typeExtension) {
  extensions.emplace_back(typeExtension);
}

std::vector<std::pair<ExtensionDecl *, Substitution>>
TypeManager::getExtensions(Type *type, TraitType *trait, bool probeOnly) {
  std::vector<std::pair<ExtensionDecl *, Substitution>> foundExtensions;
  for (auto &&extension : extensions) {
    Substitution extSub;
    for (auto &&typeParam : extension->typeParams)
      extSub[typeParam->getType()] = getNewUninferredType();

    Type *probedType = instantiate(extension->type, extSub);

    // FIXME: propagate errors for more decriptive messages?
    if (!unify(type, probedType, probeOnly).empty())
      continue;

    if (!trait) {
      foundExtensions.emplace_back(extension, extSub);
      continue;
    }

    Type *probedTrait = instantiate(extension->trait->getType(), extSub);
    if (!unify(trait, probedTrait, probeOnly).empty())
      continue;

    foundExtensions.emplace_back(extension, extSub);
  }

  return foundExtensions;
}

std::vector<TraitType *> TypeManager::getConstraints(const res::Type *type) {
  type = type->getRootType();
  Substitution sub = extractSubstitutionFrom(type);

  std::vector<TraitType *> traits;

  // FIXME: this needs to traverse the extensions
  for (auto &&[constrainedType, trait] : constraints)
    if (eq(type, instantiate(constrainedType, sub))) {
      traits.emplace_back(instantiate(trait, sub)->getAs<res::TraitType>());

      for (auto &&traitConstraint : getConstraints(traits.back()))
        traits.emplace_back(traitConstraint);
    }

  // FIXME: clean this up
  std::vector<TraitType *> filtered;
  for (auto &&t : traits) {
    bool found = false;
    for (auto &&f : filtered)
      found |= eq(t, f);

    if (found)
      continue;

    filtered.emplace_back(t);
  }

  return filtered;
}

bool TypeManager::hasConstraint(Type *type, TraitType *trait) {
  for (auto &&constraint : getConstraints(type))
    if (unify(trait, constraint).empty())
      return true;

  return false;
}

void TypeManager::createObligation(UninferredType *type, TraitType *trait) {
  obligations[type].emplace_back(trait);
}

bool TypeManager::eq(const Type *t1, const Type *t2) const {
  t1 = t1->getRootType();
  t2 = t2->getRootType();

  if (!t1->isSameKind(t2))
    return false;

  for (size_t i = 0; i < t1->args.size(); ++i)
    if (!eq(t1->args[i], t2->args[i]))
      return false;

  return true;
}

std::vector<std::string> TypeManager::unifyImpl(
    Type *t1, Type *t2, std::vector<UninferredType *> &inferredTypes) {
  t1 = t1->getRootType();
  t2 = t2->getRootType();

  if (t1 == t2)
    return {};

  if (auto *u = t1->getAs<UninferredType>()) {
    u->infer(t2);
    inferredTypes.emplace_back(u);
    return {};
  }

  if (t2->getAs<UninferredType>())
    return unifyImpl(t2, t1, inferredTypes);

  if (!t1->isSameKind(t2))
    return {"cannot unify '" + t1->getName() + "' with '" + t2->getName() +
            "'"};

  for (size_t i = 0; i < t1->args.size(); ++i) {
    auto errs = unifyImpl(t1->args[i], t2->args[i], inferredTypes);
    if (!errs.empty()) {
      errs.emplace_back("cannot unify '" + t1->getName() + "' with '" +
                        t2->getName() + "'");
      return errs;
    }
  }

  return {};
}

std::vector<std::string>
TypeManager::checkObligations(std::vector<UninferredType *> &inferredTypes) {
  std::vector<std::string> errors;

  for (auto &&type : inferredTypes) {
    for (auto &&requiredTrait : obligations[type]) {
      if (hasConstraint(type, requiredTrait))
        continue;

      // FIXME: consider returning candidates
      auto extensions = getExtensions(type->getRootType(), requiredTrait, true);

      if (extensions.empty())
        errors.emplace_back("cannot satisfy requirement '" + type->getName() +
                            " : " + requiredTrait->getName() + "'");
      else if (extensions.size() == 1) {
        auto [extension, sub] = extensions[0];
        unify(type, instantiate(extension->type, sub));
        unify(requiredTrait, instantiate(extension->trait->getType(), sub));
      } else
        for (auto &&extension : extensions)
          errors.emplace_back(
              "'" + extension.first->trait->getType()->getName() +
              "' ambigously satisfies requirement '" + type->getName() + " : " +
              requiredTrait->getName() + "'");
    }
  }

  return errors;
}

std::vector<std::string>
TypeManager::unify(Type *t1, Type *t2, bool probeOnly) {
  std::vector<UninferredType *> inferredTypes;
  std::vector<std::string> errors = unifyImpl(t1, t2, inferredTypes);

  if (errors.empty())
    for (auto &&err : checkObligations(inferredTypes))
      errors.emplace_back(err);

  if (probeOnly)
    for (auto &&ty : inferredTypes)
      ty->reset();

  inferredTypes.clear();
  return errors;
}

Type *TypeManager::instantiate(Type *t, const Substitution &substitution) {
  for (auto &&[from, to] : substitution)
    if (eq(from->getRootType(), t->getRootType()))
      return to;

  if (auto *fnTy = t->getAs<FunctionType>())
    t = getFunctionType(fnTy->getArgs(), fnTy->getReturnType());
  else if (auto *s = t->getAs<StructType>())
    t = getStructType(*s->getDecl(), s->getTypeArgs());
  else if (auto *b = t->getAs<BorrowedType>())
    t = getBorrowedType(b->getBorrowedType(), b->isMutable());
  else if (auto *p = t->getAs<PointerType>())
    t = getPointerType(p->getPointeeType(), p->isMutable());
  else if (auto *trait = t->getAs<TraitType>())
    t = getTraitType(*trait->decl, trait->args);
  else if (auto *trait = t->getAs<AnyTraitType>())
    t = getAnyTraitType(*trait->decl, trait->args);
  else if (auto *i = t->getAs<res::AnyType>())
    t = getAnyType(i->getTrait());

  for (auto &arg : t->args)
    arg = instantiate(arg, substitution);

  return t;
}

TraitType *TypeManager::withSelfType(AnyTraitType *anyTraitType,
                                     Type *selfType) {
  std::vector<Type *> args = {selfType};
  auto currentArgs = anyTraitType->getTypeArgs();
  args.insert(args.end(), currentArgs.begin(), currentArgs.end());
  return getTraitType(*anyTraitType->decl, std::move(args));
}

TypeManager::VtableLayoutTy
TypeManager::getVtableLayout(const res::TraitType *trait) {
  VtableLayoutTy layout;

  Substitution sub = extractSubstitutionFrom(trait);

  for (auto &&superTrait : trait->getDecl()->traits) {
    auto *superType =
        instantiate(superTrait->getType(), sub)->getAs<res::TraitType>();

    // FIXME: find a more efficient filtering
    bool alreadyInserted = false;

    for (auto &&[trait, _] : layout)
      alreadyInserted |= eq(trait, superType);

    if (alreadyInserted)
      continue;

    const auto &superLayout = getVtableLayout(superType);
    layout.insert(layout.end(), superLayout.begin(), superLayout.end());
  }

  for (auto &&fn : trait->getDecl()->getAll<res::FunctionDecl>())
    layout.emplace_back(trait, fn);

  return layout;
}
} // namespace res
} // namespace yl
