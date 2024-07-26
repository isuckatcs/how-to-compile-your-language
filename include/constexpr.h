#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_CONSTEXPR_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_CONSTEXPR_H

#include <optional>

#include "ast.h"

namespace yl {
class ConstantExpressionEvaluator {
  std::optional<double>
  evaluateBinaryOperator(const ResolvedBinaryOperator &binop,
                         bool allowSideEffects);
  std::optional<double> evaluateUnaryOperator(const ResolvedUnaryOperator &unop,
                                              bool allowSideEffects);
  std::optional<double> evaluateDeclRefExpr(const ResolvedDeclRefExpr &dre,
                                            bool allowSideEffects);

public:
  std::optional<double> evaluate(const ResolvedExpr &expr,
                                 bool allowSideEffects);
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_CONSTEXPR_H
