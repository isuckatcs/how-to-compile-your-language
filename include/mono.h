#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_MONO_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_MONO_H

#include <deque>
#include <map>
#include <memory>
#include <set>
#include <vector>

#include "res.h"

namespace yl {
namespace mono {
struct Function final {
  const res::FunctionDecl *decl;
  std::string name;
  res::Substitution sub;

  std::map<const res::DeclRefExpr *, std::string> mangledDeclRefs;
  std::map<const res::LambdaExpr *, std::string> mangledLambdas;
  std::map<const res::TraitObjectPromoExpr *, std::string> vtableRefs;
};

struct Context final {
  res::Context *resCtx;
  std::vector<Function> functions;
  std::map<std::string, std::vector<std::string>> vtables;

  void dump() const;
};
} // namespace mono

struct Mangling {
  static std::string mangleMonoType(res::Context *resCtx,
                                    res::Type *type,
                                    const res::Substitution &sub);
  static std::string mangleFunctionDecl(res::Context *resCtx,
                                        const res::FunctionDecl *fn,
                                        const res::Substitution &sub);

private:
  static std::string mangleFunctionSignature(res::Context *resCtx,
                                             const res::FunctionDecl *fn,
                                             const res::Substitution &sub);
  static std::string mangleGenericArgs(res::Context *resCtx,
                                       const std::vector<res::Type *> &args,
                                       const res::Substitution &sub);
};

class MonoCollector final {
  const size_t depthLimit = 10;

  diag::DiagnosticReporter *reporter;
  res::Context *resCtx;
  mono::Context *monoCtx;

  std::deque<std::pair<mono::Function, size_t>> worklist;
  std::set<std::string> seen;

  void processTopLevelFunctions();
  bool processFunctionBody(mono::Function &fn, size_t depth);

  std::string generateVtable(res::TraitType *trait);
  std::string monomorphize(const res::FunctionDecl *fn,
                           res::Substitution sub,
                           size_t depth);

public:
  explicit MonoCollector(diag::DiagnosticReporter &reporter,
                         res::Context *resCtx)
      : reporter(&reporter),
        resCtx(resCtx) {}

  std::unique_ptr<mono::Context> collectMonoFunctions();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_MONO_H
