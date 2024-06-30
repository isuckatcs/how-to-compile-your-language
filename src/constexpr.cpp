#include "constexpr.h"

#include <cassert>
#include <optional>

namespace {
bool toBool(double d) { return d != 0.0; }
} // namespace

std::optional<double> ConstantExpressionEvaluator::evaluateBinaryOperator(
    const ResolvedBinaryOperator &binop) {
  std::optional<double> lhs = evaluate(*binop.lhs);
  if (!lhs)
    return std::nullopt;

  // If the LHS of || is true, we don't need to evaluate the RHS.
  if (binop.op == TokenKind::PipePipe && toBool(*lhs))
    return 1.0;

  // If the LHS of && is false, we don't need to evaluate the RHS.
  if (binop.op == TokenKind::AmpAmp && !toBool(*lhs))
    return 0.0;

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
  case TokenKind::EqualEqual:
    return *lhs == *rhs;
  case TokenKind::AmpAmp:
  case TokenKind::PipePipe:
    return toBool(*rhs); // The LHS is already handled.
  default:
    assert(false && "unexpected binary operator");
  }
}

std::optional<double> ConstantExpressionEvaluator::evaluateUnaryOperator(
    const ResolvedUnaryOperator &op) {
  std::optional<double> rhs = evaluate(*op.rhs);
  if (!rhs)
    return std::nullopt;

  if (op.op == TokenKind::Excl)
    return !toBool(*rhs);

  assert(false && "unexpected unary operator");
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