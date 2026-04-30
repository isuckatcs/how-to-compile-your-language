#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_CONSTEXPR_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_CONSTEXPR_H

#include <unordered_map>
#include <variant>

#include "res.h"

namespace yl {
using ConstVal = std::variant<bool, double>;
using ConstExprValueStorage = std::unordered_map<const res::Expr *, ConstVal>;

class ConstantExpressionEvaluator {
  ConstExprValueStorage *results;
  bool shortCircuitLogicalOperators;

  bool evaluateBinaryOperator(const res::BinaryOperator &binop);
  bool evaluateUnaryOperator(const res::UnaryOperator &unop);
  bool evaluateGroupingExpr(const res::GroupingExpr &grouping);
  bool evaluatePathExpr(const res::PathExpr &path);
  bool evaluateDeclRefExpr(const res::DeclRefExpr &dre);

public:
  ConstantExpressionEvaluator(ConstExprValueStorage &results,
                              bool shortCircuitLogicalOperators)
      : results(&results),
        shortCircuitLogicalOperators(shortCircuitLogicalOperators){};

  const ConstExprValueStorage *getResults() const { return results; }

  bool evaluate(const res::Expr &expr);
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_CONSTEXPR_H
