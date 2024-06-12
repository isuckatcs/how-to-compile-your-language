#ifndef A_COMPILER_CONSTEXPR_H
#define A_COMPILER_CONSTEXPR_H

#include "ast.h"

#include <optional>

class ConstantExpressionEvaluator {
  std::optional<double>
  evaluateBinaryOperator(const ResolvedBinaryOperator &binop);

public:
  std::optional<double> evaluate(const ResolvedExpr &expr);
};

#endif // A_COMPILER_CONSTEXPR_H
