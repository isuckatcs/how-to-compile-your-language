#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_CONSTEXPR_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_CONSTEXPR_H

#include <optional>

#include "res.h"

namespace yl {
class ConstantExpressionEvaluator {
  std::optional<double> evaluateBinaryOperator(const res::BinaryOperator &binop,
                                               bool allowSideEffects);
  std::optional<double> evaluateUnaryOperator(const res::UnaryOperator &unop,
                                              bool allowSideEffects);
  std::optional<double> evaluateDeclRefExpr(const res::DeclRefExpr &dre,
                                            bool allowSideEffects);

public:
  std::optional<double> evaluate(const res::Expr &expr, bool allowSideEffects);
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_CONSTEXPR_H
