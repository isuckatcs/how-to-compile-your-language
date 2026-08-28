#include <deque>
#include <iostream>
#include <set>

#include "cfg.h"
#include "mono.h"

namespace yl {
namespace mono {
void Context::dump() const {
  for (auto &&[id, functions] : vtables) {
    std::cerr << id << '\n';
    for (auto &&fn : functions)
      std::cerr << indent(1) << fn << '\n';
    std::cerr << '\n';
  }

  for (auto &&fn : functions)
    std::cerr << '(' << fn.decl->identifier << ", " << fn.sub.getString()
              << ") -> " << fn.name << '\n';
}
} // namespace mono

std::string Mangling::mangleMonoType(res::Context *resCtx,
                                     res::Type *type,
                                     const res::Substitution &sub) {
  type = resCtx->instantiate(type, sub);

  if (type->getAs<res::BuiltinUnitType>())
    return "u";

  if (type->getAs<res::BuiltinNumberType>())
    return "n";

  if (type->getAs<res::BuiltinBoolType>())
    return "b";

  if (const auto *p = type->getAs<res::PointerType>())
    return (p->isMutable() ? "m" : "p") +
           mangleMonoType(resCtx, p->getPointeeType(), sub);

  if (const auto *r = type->getAs<res::RefType>())
    return (r->isMutable() ? "i" : "r") +
           mangleMonoType(resCtx, r->getReferencedType(), sub);

  if (const auto *s = type->getAs<res::StructType>()) {
    static std::map<std::string, std::map<const res::StructDecl *, std::string>>
        lambdaNames;

    std::stringstream mangledName;

    const auto *sd = s->getDecl();
    std::string id = sd->identifier;

    if (sd->isLambda) {
      auto *parentFn = dynamic_cast<const res::FunctionDecl *>(sd->declContext);
      assert(parentFn && "lambda found outside function");

      std::string parentName = mangleFunctionSignature(resCtx, parentFn, sub);

      if (!lambdaNames.count(parentName) || !lambdaNames[parentName].count(sd))
        lambdaNames[parentName][sd] =
            "lambda_" + std::to_string(lambdaNames[parentName].size());

      mangledName << parentName;
      id = lambdaNames[parentName][sd];
    }

    mangledName << 'S' << id.size() << id
                << mangleGenericArgs(resCtx, s->getTypeArgs(), sub);
    return mangledName.str();
  }

  if (const auto *a = type->getAs<res::AnyTraitType>()) {
    const auto &id = a->getDecl()->identifier;

    std::stringstream mangledName;
    mangledName << 'A' << id.size() << id
                << mangleGenericArgs(resCtx, a->getTypeArgs(), sub);
    return mangledName.str();
  }

  if (const auto *tr = type->getAs<res::TraitType>()) {
    const auto &id = tr->getDecl()->identifier;

    std::stringstream mangledName;
    mangledName << 'T' << id.size() << id
                << mangleGenericArgs(resCtx, tr->getTypeArgs(), sub);
    return mangledName.str();
  }

  if (const auto *f = type->getAs<res::FunctionType>()) {
    std::stringstream mangledName;

    mangledName << 'F';
    for (auto &&arg : f->getArgs())
      mangledName << mangleMonoType(resCtx, arg, sub);
    mangledName << 'R' << mangleMonoType(resCtx, f->getReturnType(), sub);

    return mangledName.str();
  }

  llvm_unreachable("unexpected type in mangling");
}

std::string Mangling::mangleFunctionDecl(res::Context *resCtx,
                                         const res::FunctionDecl *fn,
                                         const res::Substitution &sub) {
  std::stringstream mangledName;
  mangledName << '_' << 'Y';
  mangledName << mangleFunctionSignature(resCtx, fn, sub);
  return mangledName.str();
}

std::string Mangling::mangleFunctionSignature(res::Context *resCtx,
                                              const res::FunctionDecl *fn,
                                              const res::Substitution &sub) {
  std::stringstream mangledName;

  if (auto *e = dynamic_cast<const res::TypeExtension *>(fn->declContext))
    mangledName << mangleMonoType(resCtx, e->trait ? e->trait : e->type, sub);
  else if (auto *t = dynamic_cast<const res::TraitDecl *>(fn->declContext))
    mangledName << mangleMonoType(resCtx, t->getType(), sub);

  const auto &identifier = fn->identifier;
  mangledName << identifier.size() << identifier;

  std::vector<res::Type *> typeArgs;
  for (auto &&tp : fn->typeParams)
    typeArgs.emplace_back(tp->getType());

  mangledName << mangleGenericArgs(resCtx, typeArgs, sub);
  return mangledName.str();
}

std::string Mangling::mangleGenericArgs(res::Context *resCtx,
                                        const std::vector<res::Type *> &args,
                                        const res::Substitution &sub) {
  if (args.empty())
    return "";

  std::stringstream mangledName;

  mangledName << 'G';
  for (auto &&arg : args)
    mangledName << mangleMonoType(resCtx, arg, sub);
  mangledName << 'E';

  return mangledName.str();
}

bool MonoCollector::processFunctionBody(const mono::Function &fn,
                                        size_t depth) {
  CFG cfg = CFGBuilder().build(*fn.decl);

  std::set<int> visited;
  std::vector<int> worklist;
  worklist.emplace_back(cfg.entry);

  while (!worklist.empty()) {
    int bb = worklist.back();
    worklist.pop_back();

    if (!visited.emplace(bb).second)
      continue;

    const auto &[preds, succs, stmts] = cfg.basicBlocks[bb];

    for (auto it = stmts.rbegin(); it != stmts.rend(); ++it) {
      if (auto *promo = dynamic_cast<const res::TraitObjectPromoExpr *>(*it)) {
        res::Type *resultType = resCtx->instantiate(promo->getType(), fn.sub);
        res::Type *originType =
            resCtx->instantiate(promo->expr->getType(), fn.sub);

        res::AnyTraitType *anyType = nullptr;
        res::Type *objectType = nullptr;

        if (auto *ptrType = resultType->getAs<res::PointerType>()) {
          objectType = originType->getAs<res::PointerType>()->getPointeeType();
          anyType = ptrType->getPointeeType()->getAs<res::AnyTraitType>();
        } else {
          objectType = originType->getAs<res::RefType>()->getReferencedType();
          anyType = resultType->getAs<res::RefType>()
                        ->getReferencedType()
                        ->getAs<res::AnyTraitType>();
        }

        monoCtx->vtableRefs[fn.id][promo] =
            generateVtable(anyType->withSelfType(resCtx, objectType));
        continue;
      }

      if (auto *lambda = dynamic_cast<const res::LambdaExpr *>(*it)) {
        monoCtx->mangledLambdas[fn.id][lambda] =
            monomorphize(lambda->getFunction(), fn.sub, depth);
        continue;
      }

      auto *dre = dynamic_cast<const res::DeclRefExpr *>(*it);
      if (!dre)
        continue;

      auto *fnDecl = dre->decl->getAs<res::FunctionDecl>();
      if (!fnDecl)
        continue;

      // This is a virtual function reference, don't monomorphize it.
      res::Substitution declSub = resCtx->instantiate(dre->sub, fn.sub);
      auto *selfType = declSub.getSelfType();
      if (selfType && selfType->getAs<res::AnyTraitType>())
        continue;

      if (depth + 1 > depthLimit) {
        err::monoOverflow()
            .at(dre->location)
            .with(fnDecl->identifier)
            .with(declSub.getString())
            .report(reporter);

        return false;
      }

      if (auto *trait = dynamic_cast<res::TraitDecl *>(fnDecl->declContext)) {
        auto *traitType = resCtx->instantiate(trait->getType(), declSub)
                              ->getAs<res::TraitType>();

        auto result =
            resCtx->queryExtensions(traitType->getTypeArgs()[0], traitType);
        assert(result.items.size() == 1 && "failed to find extension");
        const auto &extension = result.items.front();

        auto extensionSub = resCtx->getUninferredInstantiation(extension);
        resCtx->unify(traitType,
                      resCtx->instantiate(extension->trait, extensionSub));

        if (auto *extensionFn = extension->getFunction(fnDecl->identifier)) {
          fnDecl = extensionFn;
          declSub = resCtx->instantiate(extensionSub, fn.sub);
        }
      }

      const std::string &mangledName = monomorphize(fnDecl, declSub, depth + 1);
      monoCtx->mangledDeclRefs[fn.id][dre] = mangledName;
    }

    for (auto &&[succ, reachable] : succs)
      if (reachable)
        worklist.emplace_back(succ);
  }

  return true;
}

std::string MonoCollector::generateVtable(res::TraitType *trait) {
  std::string vtableId =
      "vtable." + Mangling::mangleMonoType(resCtx, trait, trait->getSub());
  if (monoCtx->vtables.count(vtableId))
    return vtableId;

  std::vector<std::string> functions;
  for (auto &&[layoutTrait, layoutFn] : trait->getVtableLayout(resCtx)) {
    auto result = resCtx->queryExtensions(trait->getTypeArgs()[0], layoutTrait);
    assert(result.items.size() == 1 && "failed to find extension");
    const auto &extension = result.items.front();

    auto extensionSub = resCtx->getUninferredInstantiation(extension);
    resCtx->unify(layoutTrait,
                  resCtx->instantiate(extension->trait, extensionSub));

    if (auto *extensionFn = extension->getFunction(layoutFn->identifier)) {
      functions.emplace_back(monomorphize(extensionFn, extensionSub, 0));
      continue;
    }

    functions.emplace_back(monomorphize(layoutFn, layoutTrait->getSub(), 0));
  }

  monoCtx->vtables[vtableId] = std::move(functions);
  return vtableId;
}

std::string MonoCollector::monomorphize(const res::FunctionDecl *fnDecl,
                                        res::Substitution sub,
                                        size_t depth) {
  std::string name = Mangling::mangleFunctionDecl(resCtx, fnDecl, sub);

  if (seen.emplace(name).second) {
    mono::Function function{monoCtx->functions.size(), fnDecl, name, sub};
    monoCtx->functions.emplace_back(function);
    worklist.emplace_back(function, depth);
  }

  return name;
}

void MonoCollector::processTopLevelFunctions() {
  const res::TranslationUnit *translationUnit = resCtx->getTU();
  std::vector<const res::FunctionDecl *> topLevelFunctions;

  for (auto &&e : translationUnit->extensions)
    if (e->typeParams.empty())
      for (auto &&fnDecl : e->functions)
        if (fnDecl->typeParams.empty())
          topLevelFunctions.emplace_back(fnDecl);

  for (auto &&fnDecl : translationUnit->functions)
    if (fnDecl->typeParams.empty())
      topLevelFunctions.emplace_back(fnDecl);

  res::Substitution emptySub;
  for (auto &&fnDecl : topLevelFunctions)
    monomorphize(fnDecl, emptySub, 0);
}

std::unique_ptr<mono::Context> MonoCollector::collectMonoFunctions() {
  auto ctx = std::make_unique<mono::Context>();
  ctx->resCtx = resCtx;

  monoCtx = ctx.get();
  processTopLevelFunctions();

  while (!worklist.empty()) {
    auto [fn, depth] = worklist.front();
    worklist.pop_front();

    if (!processFunctionBody(fn, depth))
      return nullptr;
  }

  return ctx;
}
} // namespace yl