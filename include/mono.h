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
  size_t id;
  const res::FunctionDecl *decl;
  std::string name;
  res::Substitution sub;
};

struct Context final {
  res::Context *resCtx;
  std::vector<Function> functions;
  std::map<std::string, std::vector<std::string>> vtables;

  std::map<size_t, std::map<const res::DeclRefExpr *, std::string>>
      mangledDeclRefs;
  std::map<size_t, std::map<const res::LambdaExpr *, std::string>>
      mangledLambdas;
  std::map<size_t, std::map<const res::TraitObjectPromoExpr *, std::string>>
      vtableRefs;

  void dump() const;
};
} // namespace mono

struct Mangling {
  static std::string mangleMonoType(res::Type *type);
  static std::string mangleFunctionDecl(const res::FunctionDecl *fn,
                                        res::Type *parentMonoType,
                                        const res::Substitution &sub);

private:
  static std::string mangleGenericArgs(const std::vector<res::Type *> &args);
};

class MonoCollector final {
  const size_t depthLimit = 10;

  diag::DiagnosticReporter *reporter;
  res::Context *resCtx;
  mono::Context *monoCtx;

  std::deque<std::pair<mono::Function, size_t>> worklist;
  std::set<std::string> seen;

  void processTopLevelFunctions();
  bool processFunctionBody(const mono::Function &fn, size_t depth);

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
