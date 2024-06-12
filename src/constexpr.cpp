#include "constexpr.h"

#include <cassert>
#include <optional>

std::optional<double> ConstantExpressionEvaluator::evaluateBinaryOperator(
    const ResolvedBinaryOperator &binop) {
  std::optional<double> LHS = evaluate(*binop.LHS);
  if (!LHS)
    return std::nullopt;

  std::optional<double> RHS = evaluate(*binop.RHS);
  if (!RHS)
    return std::nullopt;

  switch (binop.op) {

  case TokenKind::plus:
    return *LHS + *RHS;
  case TokenKind::minus:
    return *LHS - *RHS;
  case TokenKind::asterisk:
    return *LHS * *RHS;
  case TokenKind::slash:
    return *LHS / *RHS;
  default:
    assert(false && "unexpected binary operator");
  }
}

std::optional<double>
ConstantExpressionEvaluator::evaluate(const ResolvedExpr &expr) {
  if (auto numberLiteral = dynamic_cast<const ResolvedNumberLiteral *>(&expr))
    return numberLiteral->value;

  if (auto groupingExpr = dynamic_cast<const ResolvedGroupingExpr *>(&expr))
    return evaluate(*groupingExpr->expr);

  if (auto binaryOperator = dynamic_cast<const ResolvedBinaryOperator *>(&expr))
    return evaluateBinaryOperator(*binaryOperator);

  return std::nullopt;
}