#include "constexpr.h"

#include <cassert>
#include <optional>

std::optional<double> ConstantExpressionEvaluator::evaluateBinaryOperator(
    const ResolvedBinaryOperator &binop) {
  std::optional<double> lhs = evaluate(*binop.lhs);
  if (!lhs)
    return std::nullopt;

  std::optional<double> rhs = evaluate(*binop.rhs);
  if (!rhs)
    return std::nullopt;

  switch (binop.op) {
  case TokenKind::Plus:
    return *lhs + *rhs;
  case TokenKind::Minus:
    return *lhs - *rhs;
  case TokenKind::Asterisk:
    return *lhs * *rhs;
  case TokenKind::Slash:
    return *lhs / *rhs;
  default:
    assert(false && "unexpected binary operator");
  }
}

std::optional<double>
ConstantExpressionEvaluator::evaluate(const ResolvedExpr &expr) {
  // FIXME: reenable this
  return std::nullopt;

  if (const auto *numberLiteral =
          dynamic_cast<const ResolvedNumberLiteral *>(&expr))
    return numberLiteral->value;

  if (const auto *groupingExpr =
          dynamic_cast<const ResolvedGroupingExpr *>(&expr))
    return evaluate(*groupingExpr->expr);

  if (const auto *binaryOperator =
          dynamic_cast<const ResolvedBinaryOperator *>(&expr))
    return evaluateBinaryOperator(*binaryOperator);

  return std::nullopt;
}