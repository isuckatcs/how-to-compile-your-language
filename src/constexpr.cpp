#include <cassert>
#include <optional>

#include "constexpr.h"

namespace {
std::optional<bool> toBool(std::optional<double> d) {
  if (!d)
    return std::nullopt;

  return d != 0.0;
}
} // namespace

namespace yl {
std::optional<double> ConstantExpressionEvaluator::evaluateBinaryOperator(
    const ResolvedBinaryOperator &binop) {
  std::optional<double> lhs = evaluate(*binop.lhs);

  if (binop.op == TokenKind::PipePipe) {
    // If the LHS of || is true, we don't need to evaluate the RHS.
    if (toBool(lhs).value_or(false))
      return 1.0;

    return toBool(evaluate(*binop.rhs));
  }

  if (binop.op == TokenKind::AmpAmp) {
    // If the LHS of && is false, we don't need to evaluate the RHS.
    if (binop.op == TokenKind::AmpAmp && !toBool(lhs).value_or(true))
      return 0.0;

    // If the LHS is unknown, but the RHS is false, the expression is false.
    std::optional<double> rhs = evaluate(*binop.rhs);
    if (!lhs) {
      if (rhs == 0.0)
        return rhs;

      return std::nullopt;
    }

    // Otherwise LHS is known to be true, so the result depends on the RHS.
    return toBool(rhs);
  }

  if (!lhs)
    return std::nullopt;

  std::optional<double> rhs = evaluate(*binop.rhs);
  if (!rhs)
    return std::nullopt;

  switch (binop.op) {
  case TokenKind::Asterisk:
    return *lhs * *rhs;
  case TokenKind::Slash:
    return *lhs / *rhs;
  case TokenKind::Plus:
    return *lhs + *rhs;
  case TokenKind::Minus:
    return *lhs - *rhs;
  case TokenKind::Lt:
    return *lhs < *rhs;
  case TokenKind::Gt:
    return *lhs > *rhs;
  default:
    assert(binop.op == TokenKind::EqualEqual && "unexpected binary operator");
    return *lhs == *rhs;
  }
}

std::optional<double> ConstantExpressionEvaluator::evaluateUnaryOperator(
    const ResolvedUnaryOperator &op) {
  std::optional<double> rhs = evaluate(*op.rhs);
  if (!rhs)
    return std::nullopt;

  assert(op.op == TokenKind::Excl && "unexpected unary operator");
  return !*toBool(rhs);
}

std::optional<double> ConstantExpressionEvaluator::evaluateDeclRefExpr(
    const ResolvedDeclRefExpr &dre) {
  // We only care about reference to immutable variables with an initializer.
  const auto *rvd = dynamic_cast<const ResolvedVarDecl *>(dre.decl);
  if (!rvd || rvd->isMutable || !rvd->initializer)
    return std::nullopt;

  return evaluate(*rvd->initializer);
}

std::optional<double>
ConstantExpressionEvaluator::evaluate(const ResolvedExpr &expr) {
  // Don't evaluate the same expression multiple times.
  if (std::optional<double> val = expr.getConstantValue())
    return val;

  if (const auto *numberLiteral =
          dynamic_cast<const ResolvedNumberLiteral *>(&expr))
    return numberLiteral->value;

  if (const auto *groupingExpr =
          dynamic_cast<const ResolvedGroupingExpr *>(&expr))
    return evaluate(*groupingExpr->expr);

  if (const auto *binaryOperator =
          dynamic_cast<const ResolvedBinaryOperator *>(&expr))
    return evaluateBinaryOperator(*binaryOperator);

  if (const auto *unaryOperator =
          dynamic_cast<const ResolvedUnaryOperator *>(&expr))
    return evaluateUnaryOperator(*unaryOperator);

  if (const auto *declRefExpr =
          dynamic_cast<const ResolvedDeclRefExpr *>(&expr))
    return evaluateDeclRefExpr(*declRefExpr);

  return std::nullopt;
}
} // namespace yl
