#include <algorithm>
#include <iostream>
#include <sstream>

#include "cfg.h"
#include "diag.h"
#include "lexer.h"
#include "res.h"
#include "utils.h"

namespace yl {
namespace res {
res::Type *Substitution::getSelfType() const {
  for (auto &&[from, to] : *this)
    if (from->getDecl()->isImplicitSelf)
      return to;

  return nullptr;
}

std::string Substitution::getString() const {
  std::stringstream ss;

  using MappingType = std::pair<const TypeParamType *, const yl::res::Type *>;
  std::vector<MappingType> mappings;
  for (auto &&[from, to] : *this)
    mappings.emplace_back(from, to);

  auto cmp = [](const MappingType &lhs, const MappingType &rhs) {
    SourceLocation lhsLoc = lhs.first->getDecl()->location;
    SourceLocation rhsLoc = rhs.first->getDecl()->location;

    if (lhsLoc.line != rhsLoc.line)
      return lhsLoc.line < rhsLoc.line;

    return lhsLoc.col < rhsLoc.col;
  };

  std::sort(mappings.begin(), mappings.end(), cmp);

  ss << "[";
  for (auto it = mappings.begin(); it != mappings.end(); ++it) {
    if (it != mappings.begin())
      ss << ", ";

    auto &&[from, to] = *it;
    ss << from->getName() << " = " << to->getName();
  }
  ss << "]";

  return ss.str();
}

void TranslationUnit::dump(size_t level) const {
  for (auto &&trait : traits)
    trait->dump(level);

  for (auto &&s : structs)
    s->dump(level);

  for (auto &&extension : extensions)
    extension->dump(level);

  for (auto &&fn : functions)
    fn->dump(level);
}

void Context::ExtensionCache::insertIfMissing(
    Type *type,
    TraitType *trait,
    int depth,
    QueryResult<TypeExtension *> result) {
  std::stringstream ss;
  buildKey(type, trait, depth, ss);
  size_t key = hash(ss.str());

  if (!count(key))
    emplace(key, std::move(result));
}

std::optional<Context::QueryResult<TypeExtension *>>
Context::ExtensionCache::get(Type *type, TraitType *trait, int depth) const {
  std::stringstream ss;
  buildKey(type, trait, depth, ss);
  size_t key = hash(ss.str());

  if (!count(key))
    return std::nullopt;

  return find(key)->second;
}

void Context::ExtensionCache::buildKey(Type *type,
                                       TraitType *trait,
                                       int depth,
                                       std::stringstream &ss) const {
  ss << type->getName() << ':';
  if (trait)
    ss << trait->getName();

  for (auto &&t : std::initializer_list<Type *>{type, trait}) {
    ss << ':';

    if (!t)
      continue;

    if (auto *u = t->getAs<res::UninferredType>())
      ss << 't' << std::get<size_t>(u->metadata);

    for (auto &&a : t->args)
      if (auto *u = a->getAs<res::UninferredType>())
        ss << 't' << std::get<size_t>(u->metadata);
  }

  ss << ':' << depth;
}

void Context::add(std::unique_ptr<Stmt> stmt) {
  statements.emplace_back(std::move(stmt));
}

void Context::add(std::unique_ptr<Decl> decl) {
  decls.emplace_back(std::move(decl));
}

void Context::add(std::unique_ptr<Type> type) {
  types.emplace_back(std::move(type));
}

void Context::add(std::unique_ptr<Block> block) {
  blocks.emplace_back(std::move(block));
}

void Context::add(std::unique_ptr<TraitConformance> conformance) {
  conformances.emplace_back(std::move(conformance));
}

void Context::add(std::unique_ptr<TypeExtension> extension) {
  extensions.emplace_back(std::move(extension));
}

bool Context::occurs(Type *type, Type *in) const {
  if (!type->getAs<res::UninferredType>() && !type->getAs<res::TypeParamType>())
    return false;

  in = in->getRootType();

  if (type->isSameKind(in))
    return true;

  for (auto &&arg : in->args)
    if (occurs(type, arg))
      return true;

  return false;
}

void Context::doUnify(Type *t1, Type *t2, UnifyResult &result) {
  t1 = t1->getRootType();
  t2 = t2->getRootType();

  if (t1 == t2)
    return;

  if (auto *u = t1->getAs<UninferredType>();
      u && !t2->getAs<res::AnyTraitType>() && !t2->getAs<res::RefType>() &&
      !occurs(u, t2)) {
    u->setParent(t2);
    result.inferredTypes.emplace_back(u);
    return;
  }

  if (t2->getAs<UninferredType>())
    return doUnify(t2, t1, result);

  if (!t1->isSameKind(t2))
    result.state = QueryState::Error;

  size_t i = 0;
  while (i < t1->args.size() && result.state == QueryState::Success) {
    doUnify(t1->args[i], t2->args[i], result);
    if (result.state == QueryState::Error)
      break;

    ++i;
  }

  if (result.state == QueryState::Error)
    result.diags.emplace_back(
        err::unificationError().with(t1->getName()).with(t2->getName()));
}

void Context::processObligations(UnifyResult &result, bool allowAmbiguity) {
  assert(result.state == QueryState::Success &&
         "processing obligations after failed unification");

  size_t i = 0;
  while (i != result.inferredTypes.size()) {
    auto *u = result.inferredTypes[i++];

    if (auto *root = u->getRootType()->getAs<res::UninferredType>()) {
      for (auto &&trait : u->obligations)
        root->addObligation(trait);
      result.propagatedObligations[root] += u->obligations.size();
      continue;
    }

    for (auto &&trait : u->obligations) {
      QueryResult<TraitType *> queryResult = querySatisfyingTraits(u, trait);

      if (queryResult.state == QueryState::Success) {
        doUnify(trait, queryResult.items[0], result);
        continue;
      }

      for (auto &&error : queryResult.diags)
        result.diags.emplace_back(error);

      result.state = queryResult.state;
      if (result.state == QueryState::Ambiguous && allowAmbiguity)
        continue;

      return;
    }
  }
}

Context::QueryResult<AssociatedDeclRef> Context::queryAssociatedDecls(
    std::string identifier, Type *type, TraitType *trait) {
  QueryResult<AssociatedDeclRef> result;

  if (!trait) {
    std::vector<AssociatedDeclRef> candidates;
    for (auto &&trait : getEveryConformance(type))
      if (auto *decl = trait->getDecl()->lookupFunction(identifier))
        candidates.push_back({nullptr, decl, trait->getSub()});

    if (candidates.size() > 1) {
      result.state = res::Context::QueryState::Ambiguous;
      result.diags.emplace_back(err::ambiguousAssociatedFn());
      return result;
    }

    if (candidates.size() == 1) {
      result.items.emplace_back(std::move(candidates.front()));
      return result;
    }

    std::vector<std::pair<res::TypeExtension *, res::Substitution>>
        applicableExtensions;
    for (auto &&extension : queryExtensions(type, nullptr).items)
      if (extension->getFunction(identifier))
        applicableExtensions.emplace_back(
            extension, getUninferredInstantiation(extension));

    if (applicableExtensions.size() > 1) {
      result.state = res::Context::QueryState::Ambiguous;
      result.diags.emplace_back(err::ambiguousAssociatedFn());
      return result;
    }

    if (applicableExtensions.size() == 1) {
      auto &&[extension, sub] = applicableExtensions.front();
      result.items.push_back({instantiate(extension->type, sub),
                              extension->getFunction(identifier), sub});
      return result;
    }

    std::vector<res::TraitDecl *> applicableTraits;
    for (auto &&trait : getTU()->traits) {
      if (!trait->lookupFunction(identifier))
        continue;

      res::Substitution traitSub = getUninferredInstantiation(trait);
      auto *traitType =
          instantiate(trait->getType(), traitSub)->getAs<res::TraitType>();

      auto applicableTraitQuery = queryExtensions(type, traitType);
      if (applicableTraitQuery.state == QueryState::Overflow) {
        result.state = applicableTraitQuery.state;
        result.diags = std::move(applicableTraitQuery.diags);
        return result;
      }

      if (applicableTraitQuery.state != QueryState::Error)
        applicableTraits.emplace_back(trait);
    }

    if (applicableTraits.size() > 1) {
      result.state = res::Context::QueryState::Ambiguous;

      for (auto &&trait : applicableTraits)
        result.diags.emplace_back(err::traitProvidesMethod()
                                      .with(trait->identifier)
                                      .with(identifier)
                                      .with(type->getName()));

      result.diags.emplace_back(err::multipleTraitsProvideMethod()
                                    .with(identifier)
                                    .with(type->getName()));
      return result;
    }

    if (applicableTraits.empty()) {
      result.state = res::Context::QueryState::Error;
      result.diags.emplace_back(
          err::memberLookupFailed().with(identifier).with(type->getName()));
      return result;
    }

    res::TraitDecl *applicableTrait = applicableTraits.front();
    res::Substitution traitSub = getUninferredInstantiation(applicableTrait);
    trait = instantiate(applicableTrait->getType(), traitSub)
                ->getAs<res::TraitType>();
  }

  res::Type *typeHint = nullptr;
  res::Decl *decl = trait->getDecl()->lookupFunction(identifier);
  Substitution sub = trait->getSub();

  if (!decl) {
    result.state = res::Context::QueryState::Error;
    result.diags.emplace_back(
        err::memberLookupFailed().with(identifier).with(trait->getName()));
    return result;
  }

  auto extensionQuery = queryExtensions(type, trait);
  if (extensionQuery.state == res::Context::QueryState::Ambiguous) {
    result.state = res::Context::QueryState::Ambiguous;

    if (extensionQuery.items.size() > 1) {
      result.diags.emplace_back(err::ambiguousAssociatedFn());
      return result;
    }

    for (auto &&err : extensionQuery.diags)
      result.diags.emplace_back(err);

    result.diags.emplace_back(err::annotationsNeededForRequirements());
    return result;
  }

  if (extensionQuery.state == QueryState::Success) {
    auto *extension = extensionQuery.items.front();

    Substitution extensionSub = getUninferredInstantiation(extension);
    typeHint = instantiate(extension->type, extensionSub);

    unify(instantiate(extension->trait, extensionSub), trait);

    if (auto *extensionFnDecl = extension->getFunction(identifier)) {
      decl = extensionFnDecl;
      sub = extensionSub;
    }
  }

  result.items.push_back({typeHint, decl, sub});
  return result;
}

Context::QueryResult<TraitType *>
Context::querySatisfyingTraits(Type *type, TraitType *trait) {
  QueryResult<TraitType *> result;

  for (auto &&conformingTrait : getEveryConformance(type)) {
    probe([&, this](UnifyResult &unifyResult) {
      doUnify(trait, conformingTrait, unifyResult);
      if (unifyResult.state == QueryState::Success)
        processObligations(unifyResult, false);

      if (unifyResult.state == QueryState::Success)
        result.items.emplace_back(conformingTrait);
    });
  }

  if (result.items.empty()) {
    auto extensionsResult = queryExtensions(type, trait);

    for (auto &&extension : extensionsResult.items) {
      auto sub = getUninferredInstantiation(extension);
      result.items.emplace_back(
          instantiate(extension->trait, sub)->getAs<res::TraitType>());
    }

    result.state = extensionsResult.state;

    if (result.state == QueryState::Ambiguous && result.items.size() == 1) {
      result.diags = std::move(extensionsResult.diags);
      return result;
    }

    if (result.state == QueryState::Overflow) {
      result.diags.emplace_back(
          err::overflow().with(type->getName()).with(trait->getName()));
      return result;
    }
  }

  if (result.items.empty()) {
    result.state = QueryState::Error;
    result.diags.emplace_back(err::unsatisfiedRequirement()
                                  .with(type->getName())
                                  .with(trait->getName()));
    return result;
  }

  if (result.items.size() > 1) {
    result.state = QueryState::Ambiguous;

    for (auto &&resultTrait : result.items)
      result.diags.emplace_back(err::ambiguousConformance()
                                    .with(resultTrait->getName())
                                    .with(type->getName())
                                    .with(trait->getName()));
    return result;
  }
  return result;
}

Context::QueryResult<TypeExtension *>
Context::queryExtensions(Type *type, TraitType *trait) {
  QueryResult<TypeExtension *> result;
  std::vector<diag::DiagBuilder> overflowDiags;

  if (extensionDepth == extensionDepthLimit) {
    result.state = QueryState::Overflow;
    return result;
  }

  if (auto cachedExtensions = extensionCache.get(type, trait, extensionDepth))
    return *cachedExtensions;

  for (auto &&extension : extensions) {
    ++extensionDepth;

    Substitution sub = getUninferredInstantiation(extension.get());

    probe([&, this](UnifyResult &unifyResult) {
      doUnify(type, instantiate(extension->type, sub), unifyResult);
      if (unifyResult.state == QueryState::Error)
        return;

      if (extension->trait && trait) {
        doUnify(trait, instantiate(extension->trait, sub), unifyResult);
        if (unifyResult.state == QueryState::Error)
          return;
      }

      processObligations(unifyResult, true);
      if (unifyResult.state == QueryState::Error)
        return;

      if (unifyResult.state == QueryState::Overflow) {
        overflowDiags = std::move(unifyResult.diags);
        return;
      }

      if ((extension->trait == nullptr) != (trait == nullptr))
        return;

      if (unifyResult.state == QueryState::Ambiguous) {
        result.state = unifyResult.state;
        result.diags = std::move(unifyResult.diags);
      }

      result.items.emplace_back(extension.get());
    });

    --extensionDepth;
    if (result.state == QueryState::Ambiguous)
      break;
  }

  if (result.items.empty()) {
    result.state = QueryState::Error;

    if (!overflowDiags.empty()) {
      result.state = QueryState::Overflow;
      result.diags = std::move(overflowDiags);
    }
  }

  if (result.items.size() > 1)
    result.state = QueryState::Ambiguous;

  extensionCache.insertIfMissing(type, trait, extensionDepth, result);
  return result;
}

bool Context::eq(Type *t1, Type *t2) const {
  t1 = t1->getRootType();
  t2 = t2->getRootType();

  if (!t1->isSameKind(t2))
    return false;

  for (size_t i = 0; i < t1->args.size(); ++i)
    if (!eq(t1->args[i], t2->args[i]))
      return false;

  return true;
}

std::vector<diag::DiagBuilder> Context::unify(Type *t1, Type *t2) {
  UnifyResult result;
  doUnify(t1, t2, result);

  if (result.state == QueryState::Success)
    processObligations(result, false);

  if (result.state == QueryState::Ambiguous)
    result.diags.emplace_back(err::annotationsNeededForRequirements());

  return result.diags;
}

template <typename Fn> void Context::probe(Fn &&fn) {
  UnifyResult result;

  fn(result);

  for (auto &&[type, cnt] : result.propagatedObligations)
    for (size_t i = 0; i < cnt; ++i)
      type->obligations.pop_back();

  for (auto &&type : result.inferredTypes)
    type->setParent(nullptr);
}

Type *Context::instantiate(Type *t, const Substitution &sub) {
  for (auto &&[from, to] : sub)
    if (eq(from->getRootType(), t->getRootType()))
      return to;

  if (auto *fnTy = t->getAs<FunctionType>())
    t = FunctionType::create(*this, fnTy->getArgs(), fnTy->getReturnType());
  else if (auto *s = t->getAs<StructType>())
    t = StructType::create(*this, s->getDecl(), s->getTypeArgs());
  else if (auto *r = t->getAs<RefType>())
    t = RefType::create(*this, r->getReferencedType(), r->isMutable());
  else if (auto *p = t->getAs<PointerType>())
    t = PointerType::create(*this, p->getPointeeType(), p->isMutable());
  else if (auto *trait = t->getAs<TraitType>())
    t = TraitType::create(*this, trait->getDecl(), trait->getTypeArgs());
  else if (auto *trait = t->getAs<AnyTraitType>())
    t = AnyTraitType::create(*this, trait->getDecl(), trait->getTypeArgs());

  for (auto &arg : t->args)
    arg = instantiate(arg, sub);

  return t;
}

bool Context::isInfiniteStructType(StructType *structType) const {
  const auto &typeParams = structType->getDecl()->typeParams;
  const auto &typeArgs = structType->getTypeArgs();

  for (size_t i = 0; i < typeParams.size(); ++i) {
    res::Type *typeParam = typeParams[i]->getType();
    res::Type *typeArg = typeArgs[i];

    if (!typeParam->isSameKind(typeArg) && occurs(typeParam, typeArg))
      return true;
  }

  return false;
}

Substitution Context::getUninferredInstantiation(GenericDeclContext *declCtx) {
  Substitution sub;
  for (auto &&typeParam : declCtx->typeParams)
    sub[typeParam->getType()->getAs<res::TypeParamType>()] =
        UninferredType::create(*this);

  for (auto &&typeParam : declCtx->typeParams) {
    if (typeParam->isImplicitSelf)
      continue;

    auto *tpType = typeParam->getType()->getAs<res::TypeParamType>();
    auto *probeType = sub[tpType]->getAs<res::UninferredType>();
    for (auto &&trait : getDirectConformance(tpType))
      probeType->addObligation(
          instantiate(trait, sub)->getAs<res::TraitType>());
  }

  return sub;
}

std::vector<TraitType *> Context::getDirectConformance(Type *type) {
  type = type->getRootType();

  if (auto *a = type->getAs<res::AnyTraitType>())
    return {a->withSelfType(this, a)};

  if (auto *t = type->getAs<TraitType>()) {
    res::TraitConformance *conformance = t->getDecl()->conformance;
    if (!conformance)
      return {};

    std::vector<TraitType *> traits;
    Substitution sub = type->getSub();

    for (auto &&trait : conformance->traits)
      traits.emplace_back(instantiate(trait, sub)->getAs<res::TraitType>());

    return traits;
  }

  if (auto *tp = type->getAs<TypeParamType>()) {
    const res::TypeParamDecl *decl = tp->getDecl();

    if (decl->isImplicitSelf) {
      auto *traitType = dynamic_cast<const res::TraitDecl *>(decl->declContext)
                            ->getType()
                            ->getAs<res::TraitType>();
      return {traitType};
    }

    res::TraitConformance *conformance = decl->conformance;
    if (!conformance)
      return {};

    return conformance->traits;
  }

  return {};
}

std::vector<TraitType *> Context::getEveryConformance(Type *type) {
  std::vector<TraitType *> result;

  for (auto &&trait : getDirectConformance(type)) {
    for (auto &&superTrait : getEveryConformance(trait)) {
      auto pred = [&](res::TraitType *t) { return eq(t, superTrait); };
      if (std::find_if(result.begin(), result.end(), pred) != result.end())
        continue;

      result.emplace_back(superTrait);
    }

    auto pred = [&](res::TraitType *t) { return eq(t, trait); };
    if (std::find_if(result.begin(), result.end(), pred) != result.end())
      continue;
    result.emplace_back(trait);
  }

  return result;
}

void Context::dumpEveryFunctionCFG() const {
  CFGBuilder builder;

  std::vector<CFG> cfgs;

  for (auto &&decl : decls)
    if (auto *fn = decl->getAs<res::FunctionDecl>())
      cfgs.emplace_back(builder.build(*fn));

  for (size_t i = 0; i < cfgs.size(); ++i) {
    cfgs[i].dump();

    if (i != cfgs.size() - 1)
      std::cerr << '\n';
  }
}

std::string ConstVal::asString() const {
  return std::visit(
      [](auto &&value) {
        std::stringstream ss;

        using T = std::decay_t<decltype(value)>;
        if constexpr (std::is_same_v<T, bool>)
          ss << (value ? "true" : "false");
        else if constexpr (std::is_same_v<T, double>)
          ss << value;

        return ss.str();
      },
      *this);
}

GenericDeclContext::GenericDeclContext(
    GenericDeclContext *parent, std::vector<res::TypeParamDecl *> typeParams)
    : parent(parent),
      typeParams(std::move(typeParams)) {
  for (auto &&tp : this->typeParams)
    tp->setDeclContext(this);
}

Type::Type(std::string name,
           std::variant<void *, size_t> metadata,
           std::vector<Type *> args)
    : baseName(std::move(name)),
      args(std::move(args)),
      metadata(metadata){};

std::string Type::getName() const {
  std::stringstream ss;
  ss << baseName;

  if (!args.empty()) {
    ss << '<';
    for (size_t i = 0; i < args.size(); ++i) {
      ss << args[i]->getName();

      if (i != args.size() - 1)
        ss << ',' << ' ';
    }
    ss << '>';
  }

  return ss.str();
}

bool Type::isSameKind(Type *other) const {
  return typeid(*this) == typeid(*other) && metadata == other->metadata;
}

UninferredType::UninferredType()
    : Type("_", nextId++){};

Type *UninferredType::getRootType() {
  if (parent)
    return parent->getRootType();
  return this;
}

std::string UninferredType::getName() const {
  if (parent)
    return parent->getName();
  return baseName;
};

void UninferredType::addObligation(res::TraitType *trait) {
  obligations.emplace_back(trait);
}

BuiltinUnitType::BuiltinUnitType()
    : Type("unit"){};

BuiltinNumberType::BuiltinNumberType()
    : Type("number"){};

BuiltinBoolType::BuiltinBoolType()
    : Type("bool"){};

TypeParamType::TypeParamType(TypeParamDecl *decl)
    : Type(decl->identifier, decl) {}

FunctionType::FunctionType(std::vector<Type *> args, Type *ret)
    : Type("fn", nullptr, std::move(args)) {
  this->metadata = this->args.size();
  this->args.emplace_back(ret);
}

std::string FunctionType::getName() const {
  std::stringstream ss;
  ss << '(';
  for (size_t i = 0; i < args.size() - 1; ++i) {
    ss << args[i]->getRootType()->getName();

    if (i < args.size() - 2)
      ss << ',' << ' ';
  }
  ss << ") -> " << getReturnType()->getName();

  return ss.str();
}

RefType::RefType(Type *referencedType, bool isMutable)
    : Type(isMutable ? "&mut " : "&", isMutable, {referencedType}){};

PointerType::PointerType(Type *pointeeType, bool isMutable)
    : Type(isMutable ? "*mut " : "*", isMutable, {pointeeType}){};

TraitType::TraitType(TraitDecl *decl, std::vector<Type *> args)
    : Type(decl->identifier, decl, std::move(args)) {}

std::string TraitType::getName() const {
  std::stringstream ss;
  ss << baseName;

  if (args.size() > 1) {
    ss << '<';
    for (size_t i = 1; i < args.size(); ++i) {
      ss << args[i]->getName();

      if (i < args.size() - 1)
        ss << ',' << ' ';
    }
    ss << '>';
  }

  return ss.str();
}

Substitution TraitType::getSub() const {
  Substitution res;

  for (size_t i = 0; i < args.size(); ++i)
    res[getDecl()->typeParams[i]->getType()->getAs<res::TypeParamType>()] =
        args[i];

  return res;
}

std::vector<std::pair<TraitType *, FunctionDecl *>>
TraitType::getVtableLayout(Context *ctx) {
  std::vector<std::pair<TraitType *, FunctionDecl *>> layout;

  for (auto &&trait : ctx->getEveryConformance(this))
    for (auto &&fn : trait->getDecl()->functions)
      layout.emplace_back(trait, fn);

  for (auto &&fn : getDecl()->functions)
    layout.emplace_back(this, fn);

  return layout;
}

StructType::StructType(StructDecl *decl, std::vector<Type *> typeArgs)
    : Type(decl->identifier, decl, std::move(typeArgs)){};

Substitution StructType::getSub() const {
  Substitution res;

  for (size_t i = 0; i < args.size(); ++i)
    res[getDecl()->typeParams[i]->getType()->getAs<res::TypeParamType>()] =
        args[i];

  return res;
}

AnyTraitType::AnyTraitType(TraitDecl *decl, std::vector<Type *> args)
    : Type("any " + decl->identifier, decl, std::move(args)) {}

Substitution AnyTraitType::getSub() const {
  Substitution res;

  for (size_t i = 0; i < args.size(); ++i)
    res[getDecl()->typeParams[i + 1]->getType()->getAs<res::TypeParamType>()] =
        args[i];

  return res;
}

TraitType *AnyTraitType::withSelfType(Context *ctx, Type *selfType) const {
  std::vector<Type *> traitArgs = {selfType};
  traitArgs.insert(traitArgs.end(), args.begin(), args.end());
  return TraitType::create(*ctx, getDecl(), std::move(traitArgs));
}

Block::Block(SourceLocation l, std::vector<Stmt *> s)
    : location(l),
      statements(std::move(s)) {}

void Block::dump(size_t level) const {
  std::cerr << indent(level) << "Block\n";

  for (auto &&stmt : statements)
    stmt->dump(level + 1);
}

IfStmt::IfStmt(SourceLocation l, Expr *c, Block *t, Block *f)
    : Stmt(l),
      condition(c),
      trueBlock(t),
      falseBlock(f) {}

void IfStmt::dump(size_t level) const {
  std::cerr << indent(level) << "IfStmt\n";

  condition->dump(level + 1);
  trueBlock->dump(level + 1);
  if (falseBlock)
    falseBlock->dump(level + 1);
}

WhileStmt::WhileStmt(SourceLocation l, Expr *c, Block *b)
    : Stmt(l),
      condition(c),
      body(b) {}

void WhileStmt::dump(size_t level) const {
  std::cerr << indent(level) << "WhileStmt\n";

  condition->dump(level + 1);
  body->dump(level + 1);
}

ParamDecl::ParamDecl(SourceLocation l,
                     std::string i,
                     GenericDeclContext *c,
                     bool m)
    : ValueDecl(l, std::move(i), c, m) {}

void ParamDecl::dump(size_t level) const {
  std::cerr << indent(level) << "ParamDecl @(" << this << ") " << identifier
            << " {" << getType()->getName() << '}' << '\n';
}

FieldDecl::FieldDecl(SourceLocation l, std::string i, GenericDeclContext *c)
    : ValueDecl(l, std::move(i), c, false) {}

void FieldDecl::dump(size_t level) const {
  std::cerr << indent(level) << "FieldDecl @(" << this << ") " << identifier
            << " {" << getType()->getName() << '}' << '\n';
}

VarDecl::VarDecl(SourceLocation location,
                 std::string identifier,
                 GenericDeclContext *declContext,
                 bool isMutable,
                 Expr *initializer)
    : ValueDecl(location, std::move(identifier), declContext, isMutable),
      initializer(initializer) {}

void VarDecl::dump(size_t level) const {
  std::cerr << indent(level) << "VarDecl @(" << this << ") " << identifier
            << " {" << getType()->getName() << '}' << '\n';

  if (initializer)
    initializer->dump(level + 1);
}

FunctionDecl::FunctionDecl(SourceLocation location,
                           std::string identifier,
                           GenericDeclContext *declContext,
                           std::vector<TypeParamDecl *> typeParams)
    : ValueDecl(location, std::move(identifier), declContext, false),
      GenericDeclContext(declContext, std::move(typeParams)) {}

void FunctionDecl::dump(size_t level) const {
  std::cerr << indent(level) << "FunctionDecl @(" << this << ") " << identifier
            << (!body ? " [incomplete]" : "") << " {" << getType()->getName()
            << '}' << '\n';

  for (auto &&typeParam : typeParams)
    typeParam->dump(level + 1);

  for (auto &&param : params)
    param->dump(level + 1);

  if (body)
    body->dump(level + 1);
}

TypeExtension::TypeExtension(SourceLocation location,
                             std::vector<TypeParamDecl *> typeParams,
                             GenericDeclContext *declContext,
                             Type *type,
                             TraitType *trait)
    : GenericDeclContext(declContext, std::move(typeParams)),
      location(location),
      type(type),
      trait(trait) {}

res::FunctionDecl *TypeExtension::getFunction(const std::string &id) const {
  for (auto &&function : functions)
    if (function->identifier == id)
      return function;

  return nullptr;
}

void TypeExtension::dump(size_t level) const {
  std::cerr << indent(level) << "TypeExtension " << type->getName();
  if (trait)
    std::cerr << " : " << trait->getName();
  std::cerr << '\n';

  for (auto &&typeParam : typeParams)
    typeParam->dump(level + 1);

  for (auto &&function : functions)
    function->dump(level + 1);
}

StructDecl::StructDecl(SourceLocation location,
                       std::string identifier,
                       GenericDeclContext *declContext,
                       std::vector<TypeParamDecl *> typeParams,
                       bool isLambda)
    : TypeDecl(location, std::move(identifier), declContext),
      GenericDeclContext(declContext, std::move(typeParams)),
      isLambda(isLambda) {}

res::FieldDecl *StructDecl::lookupField(const std::string &id) const {
  for (auto &&field : fields)
    if (field->identifier == id)
      return field;

  return nullptr;
}

void StructDecl::dump(size_t level) const {
  std::cerr << indent(level) << "StructDecl @(" << this << ") " << identifier
            << " {" << getType()->getName() << '}' << '\n';

  for (auto &&typeParam : typeParams)
    typeParam->dump(level + 1);

  for (auto &&field : fields)
    field->dump(level + 1);
}

TraitConformance::TraitConformance(SourceLocation l,
                                   res::Type *t,
                                   std::vector<res::TraitType *> ts)
    : location(l),
      type(t),
      traits(std::move(ts)) {}

void TraitConformance::dump(size_t level) const {
  std::cerr << indent(level) << "TraitConformance " << type->getName() << " : ";

  for (auto &&trait : traits) {
    std::cerr << trait->getName();

    if (trait != traits.back())
      std::cerr << " & ";
  }

  std::cerr << '\n';
}

TraitDecl::TraitDecl(SourceLocation l,
                     std::string i,
                     GenericDeclContext *c,
                     std::vector<TypeParamDecl *> p)
    : TypeDecl(l, std::move(i), c),
      GenericDeclContext(c, std::move(p)) {}

res::FunctionDecl *TraitDecl::lookupFunction(const std::string &id) const {
  for (auto &&function : functions)
    if (function->identifier == id)
      return function;

  return nullptr;
}

void TraitDecl::dump(size_t level) const {
  std::cerr << indent(level) << "TraitDecl @(" << this << ") " << identifier
            << '\n';

  if (conformance)
    conformance->dump(level + 1);

  for (auto &&typeParam : typeParams)
    typeParam->dump(level + 1);

  for (auto &&function : functions)
    function->dump(level + 1);
}

TypeParamDecl::TypeParamDecl(SourceLocation l, std::string i, bool s)
    : TypeDecl(l, std::move(i), nullptr),
      isImplicitSelf(s) {}

void TypeParamDecl::dump(size_t level) const {
  std::cerr << indent(level) << "TypeParamDecl @(" << this << ") " << identifier
            << " {" << getType()->getName() << "}\n";

  if (conformance)
    conformance->dump(level + 1);
}

NumberLiteral::NumberLiteral(SourceLocation location, double value)
    : Expr(location, Expr::ValueCategory::Rvalue),
      value(value) {}

void NumberLiteral::dump(size_t level) const {
  std::cerr << indent(level) << "NumberLiteral '" << value << "' {"
            << getType()->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';
}

BoolLiteral::BoolLiteral(SourceLocation location, bool value)
    : Expr(location, Expr::ValueCategory::Rvalue),
      value(value) {}

void BoolLiteral::dump(size_t level) const {
  std::cerr << indent(level) << "BoolLiteral '" << (value ? "true" : "false")
            << "' {" << getType()->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';
}

UnitLiteral::UnitLiteral(SourceLocation location)
    : Expr(location, Expr::ValueCategory::Rvalue) {}

void UnitLiteral::dump(size_t level) const {
  std::cerr << indent(level) << "UnitLiteral {" << getType()->getName() << '}'
            << '\n';
}

DeclRefExpr::DeclRefExpr(SourceLocation loc,
                         Decl *d,
                         Expr::ValueCategory valueCategory,
                         Substitution sub)
    : Expr(loc, valueCategory),
      decl(d),
      sub(sub) {}

void DeclRefExpr::dump(size_t level) const {
  std::cerr << indent(level) << "DeclRefExpr @(" << decl << ") "
            << decl->identifier << " {" << getType()->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';
}

CallExpr::CallExpr(SourceLocation location, Expr *callee)
    : Expr(location, Expr::ValueCategory::Rvalue),
      callee(callee){};

void CallExpr::dump(size_t level) const {
  std::cerr << indent(level) << "CallExpr"
            << " {" << getType()->getName() << '}' << '\n';

  callee->dump(level + 1);

  for (auto &&arg : arguments)
    arg->dump(level + 1);
}

MemberExpr::MemberExpr(SourceLocation location, Expr *base, DeclRefExpr *member)
    : Expr(location, base->valueCategory),
      base(base),
      member(member) {}

void MemberExpr::dump(size_t level) const {
  std::cerr << indent(level) << "MemberExpr @(" << member->decl << ')' << ' '
            << member->decl->identifier << " {" << getType()->getName() << '}'
            << '\n';

  base->dump(level + 1);
}

BinaryOperator::BinaryOperator(SourceLocation loc,
                               TokenKind op,
                               Expr *lhs,
                               Expr *rhs)
    : Expr(loc, Expr::ValueCategory::Rvalue),
      op(op),
      lhs(lhs),
      rhs(rhs) {}

void BinaryOperator::dump(size_t level) const {
  std::cerr << indent(level) << "BinaryOperator '" << getOpStr(op) << '\''
            << " {" << getType()->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';

  lhs->dump(level + 1);
  rhs->dump(level + 1);
}

UnaryOperator::UnaryOperator(SourceLocation loc,
                             TokenKind op,
                             Expr *e,
                             Expr::ValueCategory valueCategory)
    : Expr(loc, valueCategory),
      op(op),
      operand(e) {}

void UnaryOperator::dump(size_t level) const {
  std::cerr << indent(level) << "UnaryOperator '" << getOpStr(op) << '\''
            << " {" << getType()->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';

  operand->dump(level + 1);
}

DeclStmt::DeclStmt(SourceLocation location, VarDecl *varDecl)
    : Stmt(location),
      varDecl(varDecl) {}

void DeclStmt::dump(size_t level) const {
  std::cerr << indent(level) << "DeclStmt\n";

  varDecl->dump(level + 1);
}

Assignment::Assignment(SourceLocation location, Expr *assignee, Expr *expr)
    : Stmt(location),
      assignee(assignee),
      expr(expr) {}

void Assignment::dump(size_t level) const {
  std::cerr << indent(level) << "Assignment\n";

  assignee->dump(level + 1);
  expr->dump(level + 1);
}

ReturnStmt::ReturnStmt(SourceLocation location, Expr *expr)
    : Stmt(location),
      expr(expr) {}

void ReturnStmt::dump(size_t level) const {
  std::cerr << indent(level) << "ReturnStmt\n";

  if (expr)
    expr->dump(level + 1);
}

FieldInitStmt::FieldInitStmt(SourceLocation loc, FieldDecl *field, Expr *init)
    : Stmt(loc),
      field(field),
      initializer(init) {}

void FieldInitStmt::dump(size_t level) const {
  std::cerr << indent(level) << "FieldInitStmt @(" << field << ')' << ' '
            << field->identifier << '\n';

  initializer->dump(level + 1);
}

StructInstantiationExpr::StructInstantiationExpr(
    SourceLocation loc, DeclRefExpr *dre, std::vector<FieldInitStmt *> inits)
    : Expr(loc, Expr::ValueCategory::Rvalue),
      structPath(dre),
      fieldInitializers(std::move(inits)) {}

void StructInstantiationExpr::dump(size_t level) const {
  std::cerr << indent(level) << "StructInstantiationExpr"
            << " {" << getType()->getName() << '}' << '\n';

  structPath->dump(level + 1);

  for (auto &&field : fieldInitializers)
    field->dump(level + 1);
}

ImplicitDerefExpr::ImplicitDerefExpr(SourceLocation location, DeclRefExpr *dre)
    : Expr(location, dre->valueCategory),
      dre(dre) {}

void ImplicitDerefExpr::dump(size_t level) const {
  std::cerr << indent(level) << "ImplicitDerefExpr"
            << " {" << getType()->getName() << '}' << '\n';

  dre->dump(level + 1);
}

GCExpr::GCExpr(SourceLocation location, Expr *expr)
    : Expr(location, Expr::ValueCategory::Rvalue),
      expr(expr) {}

void GCExpr::dump(size_t level) const {
  std::cerr << indent(level) << "GCExpr"
            << " {" << getType()->getName() << '}' << '\n';

  expr->dump(level + 1);
}

LambdaExpr::LambdaExpr(SourceLocation location,
                       res::StructDecl *closure,
                       res::TypeExtension *ext,
                       std::vector<res::Expr *> fieldInits)
    : Expr(location, Expr::ValueCategory::Rvalue),
      fieldInits(std::move(fieldInits)),
      closure(closure),
      ext(ext) {}

void LambdaExpr::dump(size_t level) const {
  std::cerr << indent(level) << "LambdaExpr"
            << " {" << getType()->getName() << '}' << '\n';

  for (auto &&init : fieldInits)
    init->dump(level + 1);

  closure->dump(level + 1);
  ext->dump(level + 1);
}

ImplicitPtrToRefDecay::ImplicitPtrToRefDecay(SourceLocation location,
                                             res::Expr *expr)
    : Expr(location, Expr::ValueCategory::Rvalue),
      expr(expr) {}

void ImplicitPtrToRefDecay::dump(size_t level) const {
  std::cerr << indent(level) << "ImplicitPtrToRefDecay"
            << " {" << getType()->getName() << '}' << '\n';

  expr->dump(level + 1);
}

ImplicitAsRefExpr::ImplicitAsRefExpr(SourceLocation location, res::Expr *expr)
    : Expr(location, Expr::ValueCategory::Rvalue),
      expr(expr) {}

void ImplicitAsRefExpr::dump(size_t level) const {
  std::cerr << indent(level) << "ImplicitAsRefExpr"
            << " {" << getType()->getName() << '}' << '\n';

  expr->dump(level + 1);
}

MaterializeTemporaryExpr::MaterializeTemporaryExpr(SourceLocation location,
                                                   res::Expr *expr)
    : Expr(location, Expr::ValueCategory::MutLvalue),
      expr(expr) {}

void MaterializeTemporaryExpr::dump(size_t level) const {
  std::cerr << indent(level) << "MaterializeTemporaryExpr"
            << " {" << getType()->getName() << '}' << '\n';

  expr->dump(level + 1);
}

TraitObjectPromoExpr::TraitObjectPromoExpr(SourceLocation location,
                                           res::Expr *expr)
    : Expr(location, Expr::ValueCategory::Rvalue),
      expr(expr) {}

void TraitObjectPromoExpr::dump(size_t level) const {
  std::cerr << indent(level) << "TraitObjectPromoExpr"
            << " {" << getType()->getName() << '}' << '\n';

  expr->dump(level + 1);
}
} // namespace res
} // namespace yl
