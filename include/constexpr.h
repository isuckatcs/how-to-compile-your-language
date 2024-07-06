#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_CONSTEXPR_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_CONSTEXPR_H

#include "ast.h"

#include <optional>

class ConstantExpressionEvaluator {
  std::optional<double>
  evaluateBinaryOperator(const ResolvedBinaryOperator &binop);
  std::optional<double> evaluateUnaryOperator(const ResolvedUnaryOperator &op);
  std::optional<double> evaluateDeclRefExpr(const ResolvedDeclRefExpr &dre);

public:
  std::optional<double> evaluate(const ResolvedExpr &expr);
};

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_CONSTEXPR_H
