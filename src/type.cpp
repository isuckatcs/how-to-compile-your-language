#include <iostream>
#include <sstream>

#include "res.h"
#include "type.h"

namespace yl {
namespace res {
UninferredType::UninferredType(std::string name)
    : Type(name, {}){};

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
    : Type("unit", {}){};

BuiltinNumberType::BuiltinNumberType()
    : Type("number", {}){};

BuiltinBoolType::BuiltinBoolType()
    : Type("bool", {}){};

TypeParamType::TypeParamType(TypeParamDecl &decl)
    : Type(decl.identifier, {}),
      decl(&decl) {}

FunctionType::FunctionType(std::vector<Type *> args)
    : Type("fn", std::move(args)) {}

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
    : Type(decl.identifier, std::move(typeArgs)),
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

OutParamType::OutParamType(Type *paramType)
    : Type("&", std::vector<res::Type *>{paramType}){};

TraitType::TraitType(TraitDecl &decl, std::vector<Type *> args)
    : Type(decl.identifier, std::move(args)),
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

void Substitution::dump() const {
  for (auto &&[from, to] : *this)
    std::cerr << from->getName() << " -> " << to->getName() << '\n';
}

Substitution TypeManager::extractSubstitutionFrom(Type *ty) {
  if (!ty)
    return {};

  Substitution sub;

  if (auto *structTy = ty->getAs<res::StructType>())
    for (int i = 0; i < structTy->getTypeArgs().size(); ++i)
      sub[structTy->decl->typeParams[i]->getType()] =
          structTy->getTypeArgs()[i];

  if (auto *traitTy = ty->getAs<res::TraitType>())
    for (int i = 0; i < traitTy->getTypeArgs().size(); ++i)
      sub[traitTy->decl->typeParams[i]->getType()] = traitTy->getTypeArgs()[i];

  return sub;
}

UninferredType *TypeManager::getNewUninferredType() {
  auto *typeVariable = new UninferredType("t" + std::to_string(types.size()));
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

OutParamType *TypeManager::getOutParamType(Type *pointeeType) {
  auto *ptrTy = new OutParamType(pointeeType);
  types.emplace_back(std::unique_ptr<OutParamType>(ptrTy));
  return ptrTy;
}

void TypeManager::addUpperBound(res::Decl *decl, TraitType *trait) {
  upperBounds.emplace_back(decl->getType(), trait);
}

std::vector<TraitType *> TypeManager::getUpperBounds(res::Type *type) {
  type = type->getRootType();
  if (auto *uninferredType = type->getAs<res::UninferredType>())
    return {};

  std::vector<TraitType *> traitInstances;
  Substitution sub = extractSubstitutionFrom(type);

  for (auto &&[declTy, traitTy] : upperBounds)
    if (unify(type, instantiate(declTy, sub)).empty()) {
      auto *traitInst = instantiate(traitTy, sub)->getAs<res::TraitType>();
      traitInstances.emplace_back(traitInst);

      for (auto &&instUpperBound : getUpperBounds(traitInst))
        traitInstances.emplace_back(instUpperBound);
    }

  return traitInstances;
}

UninferredType *TypeManager::withObligation(UninferredType *type,
                                            TraitType *obligation) {
  obligations[type].emplace_back(obligation);
  return type;
}

bool TypeManager::moreGeneral(Type *t1, Type *t2) {
  t1 = t1->getRootType();
  t2 = t2->getRootType();

  auto *typeParamTy1 = t1->getAs<res::TypeParamType>();
  auto *typeParamTy2 = t2->getAs<res::TypeParamType>();

  if (typeParamTy1 &&
      (!typeParamTy2 || typeParamTy1->decl != typeParamTy2->decl))
    return true;

  if (t1->name != t2->name || t1->args.size() != t2->args.size())
    return false;

  for (size_t i = 0; i < t1->args.size(); ++i)
    if (moreGeneral(t1->args[i], t2->args[i]))
      return true;

  return false;
}

bool TypeManager::unifyImpl(Type *t1,
                            Type *t2,
                            std::vector<std::string> &errors) {
  t1 = t1->getRootType();
  t2 = t2->getRootType();

  if (t1 == t2)
    return true;

  if (auto *u = t1->getAs<UninferredType>()) {
    if (auto *ptrTy = t2->getAs<OutParamType>()) {
      u->infer(ptrTy->getParamType());
      errors.emplace_back("cannot unify '" + t1->getName() + "' with '" +
                          t2->getName() + "'");
      return false;
    }

    std::vector<std::pair<res::Type *, res::TraitType *>> constraints;

    if (auto *ut1 = t1->getAs<UninferredType>()) {
      for (auto &&trait : obligations[ut1]) {
        constraints.emplace_back(ut1, trait);
      }
    }

    if (auto *ut2 = t2->getAs<UninferredType>()) {
      for (auto &&trait : obligations[ut2]) {
        constraints.emplace_back(ut2, trait);
      }
    }

    u->infer(t2);

    for (auto &&[type, trait] : constraints) {
      bool satisfied = false;

      for (auto &&typeTraitImpl : getUpperBounds(type)) {
        if (trait->decl == typeTraitImpl->decl &&
            unifyImpl(typeTraitImpl, trait, errors)) {
          satisfied = true;
          break;
        }
      }

      if (!satisfied) {
        errors.emplace_back("cannot satisfy constraint '" + type->getName() +
                            " : " + trait->getName() + "'");
        return false;
      }
    }

    return true;
  }

  if (t2->getAs<UninferredType>())
    return unifyImpl(t2, t1, errors);

  if (t1->name != t2->name || t1->args.size() != t2->args.size()) {
    errors.emplace_back("cannot unify '" + t1->getName() + "' with '" +
                        t2->getName() + "'");
    return false;
  }

  if (auto *tpt1 = t1->getAs<TypeParamType>()) {
    auto *tpt2 = t2->getAs<TypeParamType>();
    if (!tpt2 || tpt1->decl != tpt2->decl) {
      errors.emplace_back("cannot unify '" + t1->getName() + "' with '" +
                          t2->getName() + "'");
      return false;
    }
  }

  for (size_t i = 0; i < t1->args.size(); ++i)
    if (!unifyImpl(t1->args[i], t2->args[i], errors)) {
      errors.emplace_back("cannot unify '" + t1->getName() + "' with '" +
                          t2->getName() + "'");
      return false;
    }

  return true;
}

std::vector<std::string> TypeManager::unify(Type *t1, Type *t2) {
  std::vector<std::string> errors;
  if (unifyImpl(t1, t2, errors))
    errors.clear();
  return errors;
}

Type *TypeManager::instantiate(Type *t, const Substitution &substitution) {
  for (auto &&[from, to] : substitution) {
    if (from->getAs<UninferredType>() || t->getAs<UninferredType>()) {
      if (from->getRootType() == t->getRootType())
        return to;

      continue;
    }

    if (unify(from, t).empty())
      return to;
  }

  if (auto *fnTy = t->getAs<FunctionType>())
    t = getFunctionType(fnTy->getArgs(), fnTy->getReturnType());
  else if (auto *s = t->getAs<StructType>())
    t = getStructType(*s->getDecl(), s->getTypeArgs());
  else if (auto *p = t->getAs<OutParamType>())
    t = getOutParamType(p->getParamType());
  else if (auto *trait = t->getAs<TraitType>())
    t = getTraitType(*trait->decl, trait->args);

  for (auto &arg : t->args)
    arg = instantiate(arg, substitution);

  return t;
}
} // namespace res
} // namespace yl
