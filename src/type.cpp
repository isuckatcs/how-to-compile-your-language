#include <iostream>
#include <sstream>

#include "res.h"
#include "type.h"

namespace yl {
namespace res {
UninferredType::UninferredType(size_t id)
    : Type("t" + std::to_string(id), reinterpret_cast<void *>(id)){};

void UninferredType::infer(Type *t) {
  assert(!parent && "already inferred");
  parent = t;
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

BuiltinNumberType::BuiltinNumberType()
    : Type("number"){};

BuiltinBoolType::BuiltinBoolType()
    : Type("bool"){};

TypeParamType::TypeParamType(TypeParamDecl &decl)
    : Type(decl.identifier, &decl),
      decl(&decl) {}

FunctionType::FunctionType(std::vector<Type *> args)
    : Type("fn", reinterpret_cast<void *>(args.size()), std::move(args)) {}

std::string FunctionType::getName() const {
  std::stringstream ss;
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
    : Type(decl.identifier, &decl, std::move(typeArgs)),
      decl(&decl){};

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
           reinterpret_cast<void *>(isMutable),
           std::vector<res::Type *>{borrowedType}),
      isMut(isMutable){};

PointerType::PointerType(Type *pointeeType, bool isMutable)
    : Type(isMutable ? "*mut " : "*",
           reinterpret_cast<void *>(isMutable),
           std::vector<res::Type *>{pointeeType}),
      isMut(isMutable){};

TraitType::TraitType(TraitDecl &decl, std::vector<Type *> args)
    : Type(decl.identifier, &decl, std::move(args)),
      decl(&decl) {}

std::string TraitType::getName() const {
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

ImplType::ImplType(res::TraitType *trait)
    : Type("impl", nullptr, {trait}),
      trait(trait) {}

std::string ImplType::getName() const { return "impl " + trait->getName(); }

void Substitution::dump() const {
  for (auto &&[from, to] : *this)
    std::cerr << from->getName() << " -> " << to->getName() << '\n';
}

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

ImplType *TypeManager::getImplType(TraitType *trait) {
  auto *implTy = new ImplType(trait);
  types.emplace_back(std::unique_ptr<ImplType>(implTy));
  return implTy;
}

void TypeManager::addConstraint(Type *type, TraitType *trait) {
  constraints.emplace_back(type, trait);
}

std::vector<TraitType *> TypeManager::getConstraints(const res::Type *type) {
  type = type->getRootType();
  Substitution sub = extractSubstitutionFrom(type);

  std::vector<TraitType *> traits;

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

std::vector<std::string> TypeManager::checkObligations(
    const std::vector<UninferredType *> &inferredTypes) {
  std::vector<std::string> errors;

  for (auto &&u : inferredTypes) {
    for (auto &&requiredTrait : obligations[u]) {
      std::vector<TraitType *> foundTraits;
      std::vector<std::pair<TraitType *, std::vector<std::string>>>
          pendingErrors;

      for (auto &&currentTrait : getConstraints(u->getRootType())) {
        Substitution sub = extractSubstitutionFrom(requiredTrait);
        for (auto &[key, val] : sub)
          if (auto *u = val->getAs<res::UninferredType>()) {
            auto *newVal = getNewUninferredType();
            for (auto &&o : obligations[u])
              createObligation(newVal, o);

            val = newVal;
          }

        auto *probedType = instantiate(requiredTrait->decl->getType(), sub)
                               ->getAs<res::TraitType>();
        const auto &errors = unify(probedType, currentTrait);
        if (errors.empty())
          foundTraits.emplace_back(currentTrait);
        else
          pendingErrors.emplace_back(probedType, std::move(errors));
      };

      // FIXME: clean this up
      if (foundTraits.empty()) {
        if (pendingErrors.empty()) {
          errors.emplace_back("cannot satisfy requirement '" + u->getName() +
                              " : " + requiredTrait->getName() + "'");
        } else {
          for (auto &&[trait, errs] : pendingErrors) {
            errors.insert(errors.end(), errs.begin(), errs.end());

            errors.emplace_back("cannot satisfy requirement '" + u->getName() +
                                " : " + trait->getName() + "'");
          }
        }

        continue;
      }

      if (foundTraits.size() == 1) {
        unify(requiredTrait, foundTraits[0]);
        continue;
      }

      for (auto &&trait : foundTraits)
        errors.emplace_back(
            "'" + trait->getName() + "' ambigously satisfies requirement '" +
            u->getName() + " : " + requiredTrait->getName() + "'");
    }
  }

  return errors;
}

std::vector<std::string> TypeManager::unify(Type *t1, Type *t2) {
  std::vector<UninferredType *> inferredTypes;
  std::vector<std::string> errors = unifyImpl(t1, t2, inferredTypes);

  if (errors.empty())
    for (auto &&err : checkObligations(inferredTypes))
      errors.emplace_back(err);

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

  for (auto &arg : t->args)
    arg = instantiate(arg, substitution);

  return t;
}

TypeManager::VtableLayoutTy
TypeManager::getVtableLayout(const res::TraitType *trait) const {
  for (auto &&[t, layout] : vtableLayouts)
    if (eq(t, trait))
      return layout;

  return {};
}

TypeManager::VtableLayoutTy
TypeManager::constructVtableLayoutImpl(res::TraitType *trait) {
  VtableLayoutTy layout;

  Substitution sub = extractSubstitutionFrom(trait);

  for (auto &&superTrait : trait->getDecl()->traits) {
    const auto &superLayout = constructVtableLayoutImpl(
        instantiate(superTrait->getType(), sub)->getAs<res::TraitType>());
    layout.insert(layout.end(), superLayout.begin(), superLayout.end());
  }

  for (auto &&fn : trait->getDecl()->getAll<res::FunctionDecl>())
    layout.emplace_back(trait, fn);

  return layout;
}

void TypeManager::constructVtableLayout(res::TraitType *trait) {
  if (getVtableLayout(trait).empty()) {
    auto layout = constructVtableLayoutImpl(trait);
    if (!layout.empty())
      vtableLayouts.emplace_back(trait, std::move(layout));
  }
}

} // namespace res
} // namespace yl
