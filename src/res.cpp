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
    if (auto *t = from->getAs<res::TypeParamType>();
        t && t->getDecl()->isImplicitSelf)
      return to;

  return nullptr;
}

void Substitution::dump() const {
  for (auto &&[from, to] : *this)
    std::cerr << from->getName() << " -> " << to->getName() << '\n';
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

std::vector<diag::DiagBuilder> Context::doUnify(
    Type *t1, Type *t2, std::vector<UninferredType *> &pendingUnifications) {
  t1 = t1->getRootType();
  t2 = t2->getRootType();

  if (t1 == t2)
    return {};

  if (auto *u = t1->getAs<UninferredType>();
      u && !t2->getAs<res::AnyTraitType>() && !t2->getAs<res::RefType>()) {
    u->setParent(t2);
    pendingUnifications.emplace_back(u);
    return {};
  }

  if (t2->getAs<UninferredType>())
    return doUnify(t2, t1, pendingUnifications);

  if (!t1->isSameKind(t2))
    return {err::unificationError().with(t1->getName()).with(t2->getName())};

  for (size_t i = 0; i < t1->args.size(); ++i) {
    auto errs = doUnify(t1->args[i], t2->args[i], pendingUnifications);
    if (!errs.empty()) {
      errs.emplace_back(
          err::unificationError().with(t1->getName()).with(t2->getName()));
      return errs;
    }
  }

  return {};
}

std::vector<diag::DiagBuilder> Context::doUnifyAndSolveConformance(
    Type *t1, Type *t2, std::vector<UninferredType *> &pendingUnifications) {
  auto errors = doUnify(t1, t2, pendingUnifications);

  size_t i = 0;
  while (i != pendingUnifications.size()) {
    auto *u = pendingUnifications[i++];

    if (auto *root = u->getRootType()->getAs<res::UninferredType>()) {
      // FIXME: don't duplicate obligations with add
      for (auto &&trait : obligations[u])
        addObligation(root, trait);
      continue;
    }

    std::vector<res::TraitType *> seen;

    for (auto &&trait : obligations[u]) {
      // FIXME: remove
      bool duplicate = false;
      for (auto &&s : seen) {
        if (eq(s, trait)) {
          duplicate = true;
          break;
        }
      }

      if (duplicate)
        continue;

      seen.emplace_back(trait);

      std::vector<TraitType *> candidates =
          getSatisfyingTraits(u, trait, false);

      if (candidates.empty()) {
        errors.emplace_back(err::unsatisfiedRequirement()
                                .with(u->getName())
                                .with(trait->getName()));
        continue;
      }

      if (candidates.size() > 1) {
        for (auto &&candidate : candidates)
          errors.emplace_back(err::ambigousConformance()
                                  .with(candidate->getName())
                                  .with(u->getName())
                                  .with(trait->getName()));
        continue;
      }

      doUnify(trait, candidates[0], pendingUnifications);
    }
  }

  return errors;
}

std::vector<std::pair<TypeExtension *, Substitution>>
Context::getEveryExtension(Type *type, bool isTopLevel) {
  std::vector<std::pair<TypeExtension *, Substitution>> matches;

  for (auto &&extension : extensions) {
    if (extensionStack.count(extension.get()))
      continue;

    EnterExtensionRAII enterThisExtension(this, extension.get(), isTopLevel);

    Substitution sub = getUninferredInstantiation(extension.get());
    if (probe(type, instantiate(extension->type, sub)).empty())
      matches.emplace_back(extension.get(), sub);
  }

  return matches;
}

std::vector<std::pair<TypeExtension *, Substitution>>
Context::getExtensions(Type *type, TraitType *trait, bool isTopLevel) {
  std::vector<std::pair<TypeExtension *, Substitution>> matches;

  for (auto &&[extension, sub] : getEveryExtension(type, isTopLevel)) {
    EnterExtensionRAII enterThisExtension(this, extension, isTopLevel);

    if (!extension->trait && !trait) {
      matches.emplace_back(extension, sub);
      continue;
    }

    if (extension->trait && trait)
      if (probe(trait, instantiate(extension->trait, sub)).empty())
        matches.emplace_back(extension, sub);
  }

  return matches;
}

std::vector<res::TraitType *> Context::getSatisfyingTraits(
    Type *type, TraitType *requirement, bool isTopLevel) {
  std::vector<TraitType *> candidates;

  for (auto &&trait : getEveryConformance(type))
    if (probe(trait, requirement).empty())
      candidates.emplace_back(trait);

  if (candidates.empty()) {
    auto extensions =
        getExtensions(type->getRootType(), requirement, isTopLevel);
    for (auto &&[extension, sub] : extensions)
      candidates.emplace_back(
          instantiate(extension->trait, sub)->getAs<res::TraitType>());
  }

  return candidates;
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
  std::vector<UninferredType *> pendingUnifications;
  return doUnifyAndSolveConformance(t1, t2, pendingUnifications);
}

std::vector<diag::DiagBuilder> Context::probe(Type *t1, Type *t2) {
  std::vector<UninferredType *> pendingUnifications;
  auto errors = doUnifyAndSolveConformance(t1, t2, pendingUnifications);

  for (auto &&pending : pendingUnifications) {
    if (auto *root = pending->getRootType()->getAs<res::UninferredType>()) {
      auto &rootObligations = obligations[root];
      rootObligations.resize(rootObligations.size() -
                             obligations[pending].size());
    }

    pending->setParent(nullptr);
  }

  return errors;
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

Substitution Context::instantiate(const Substitution &s,
                                  const Substitution &sub) {
  Substitution res;

  for (auto &&[from, to] : s)
    res[from] = instantiate(to, sub);

  return res;
}

Substitution Context::getUninferredInstantiation(GenericDeclContext *declCtx) {
  Substitution sub;
  for (auto &&typeParam : declCtx->typeParams) {
    auto *tpType = typeParam->getType();
    auto *probeType = UninferredType::create(*this);

    sub[tpType] = probeType;

    for (auto &&trait : getDirectConformance(tpType))
      addObligation(probeType,
                    instantiate(trait, sub)->getAs<res::TraitType>());
  }

  return sub;
}

std::vector<TraitType *> Context::getDirectConformance(Type *type) {
  type = type->getRootType();

  if (auto *a = type->getAs<res::AnyTraitType>())
    return {a->withSelfType(this, a)};

  res::TraitConformance *conformance = nullptr;

  if (auto *t = type->getAs<TraitType>())
    conformance = t->getDecl()->conformance;
  else if (auto *tp = type->getAs<TypeParamType>())
    conformance = tp->getDecl()->conformance;

  if (!conformance)
    return {};

  std::vector<TraitType *> traits;
  Substitution sub = type->getSub();

  for (auto &&trait : conformance->traits)
    traits.emplace_back(instantiate(trait, sub)->getAs<res::TraitType>());

  return traits;
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

void Context::addObligation(res::UninferredType *type, res::TraitType *trait) {
  obligations[type].emplace_back(trait);
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
    for (auto &&arg : args) {
      ss << arg->getName();

      if (arg != args.back())
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
    res[getDecl()->typeParams[i]->getType()] = args[i];

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
    res[getDecl()->typeParams[i]->getType()] = args[i];

  return res;
}

AnyTraitType::AnyTraitType(TraitDecl *decl, std::vector<Type *> args)
    : Type("any " + decl->identifier, decl, std::move(args)) {}

Substitution AnyTraitType::getSub() const {
  Substitution res;

  for (size_t i = 0; i < args.size(); ++i)
    res[getDecl()->typeParams[i + 1]->getType()] = args[i];

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
                             Type *type,
                             TraitType *trait)
    : GenericDeclContext(nullptr, std::move(typeParams)),
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
