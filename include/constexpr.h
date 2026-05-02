#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_CONSTEXPR_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_CONSTEXPR_H

#include "res.h"

namespace yl {

class ConstExprEvaluator {
  bool shortCircuitLogicalOperators;

  res::ConstVal evaluateBinaryOperator(const res::BinaryOperator &binop);
  res::ConstVal evaluateUnaryOperator(const res::UnaryOperator &unop);
  res::ConstVal evaluateDeclRefExpr(const res::DeclRefExpr &dre);

public:
  explicit ConstExprEvaluator(bool shortCircuitLogicalOperators)
      : shortCircuitLogicalOperators(shortCircuitLogicalOperators){};

  res::ConstVal evaluate(const res::Expr &expr);
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_CONSTEXPR_H
