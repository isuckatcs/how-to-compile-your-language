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
    const res::BinaryOperator &binop, bool allowSideEffects) {
  std::optional<double> lhs = evaluate(*binop.lhs, allowSideEffects);

  if (!lhs && !allowSideEffects)
    return std::nullopt;

  if (binop.op == TokenKind::PipePipe) {
    // If the LHS of || is true, we don't need to evaluate the RHS.
    if (toBool(lhs) == true)
      return 1.0;

    // If the LHS is false, or side effects are allowed and the RHS is true, the
    // result is true.
    std::optional<double> rhs = evaluate(*binop.rhs, allowSideEffects);
    if (toBool(rhs) == true)
      return 1.0;

    // If both sides are known but none of them is true, the result is false.
    if (lhs && rhs)
      return 0.0;

    // Otherwise one of the sides is unknown, so the result is unknown too.
    return std::nullopt;
  }

  if (binop.op == TokenKind::AmpAmp) {
    // If the LHS of && is false, we don't need to evaluate the RHS.
    if (toBool(lhs) == false)
      return 0.0;

    std::optional<double> rhs = evaluate(*binop.rhs, allowSideEffects);
    if (toBool(rhs) == false)
      return 0.0;

    if (lhs && rhs)
      return 1.0;

    return std::nullopt;
  }

  if (!lhs)
    return std::nullopt;

  std::optional<double> rhs = evaluate(*binop.rhs, allowSideEffects);
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
  default:
    llvm_unreachable("unexpected binary operator");
  }
}

std::optional<double> ConstantExpressionEvaluator::evaluateUnaryOperator(
    const res::UnaryOperator &unop, bool allowSideEffects) {
  std::optional<double> operand = evaluate(*unop.operand, allowSideEffects);
  if (!operand)
    return std::nullopt;

  if (unop.op == TokenKind::Excl)
    return !*toBool(operand);

  if (unop.op == TokenKind::Minus)
    return -*operand;

  llvm_unreachable("unexpected unary operator");
}

std::optional<double>
ConstantExpressionEvaluator::evaluateDeclRefExpr(const res::DeclRefExpr &dre,
                                                 bool allowSideEffects) {
  // We only care about reference to immutable variables with an initializer.
  const auto *rvd = dre.decl->getAs<res::VarDecl>();
  if (!rvd || rvd->isMutable || !rvd->initializer)
    return std::nullopt;

  return evaluate(*rvd->initializer, allowSideEffects);
}

std::optional<double>
ConstantExpressionEvaluator::evaluate(const res::Expr &expr,
                                      bool allowSideEffects) {
  // Don't evaluate the same expression multiple times.
  if (std::optional<double> val = expr.getConstantValue())
    return val;

  if (const auto *numberLiteral =
          dynamic_cast<const res::NumberLiteral *>(&expr))
    return numberLiteral->value;

  if (const auto *groupingExpr = dynamic_cast<const res::GroupingExpr *>(&expr))
    return evaluate(*groupingExpr->expr, allowSideEffects);

  if (const auto *binaryOperator =
          dynamic_cast<const res::BinaryOperator *>(&expr))
    return evaluateBinaryOperator(*binaryOperator, allowSideEffects);

  if (const auto *unaryOperator =
          dynamic_cast<const res::UnaryOperator *>(&expr))
    return evaluateUnaryOperator(*unaryOperator, allowSideEffects);

  if (const auto *declRefExpr = dynamic_cast<const res::DeclRefExpr *>(&expr))
    return evaluateDeclRefExpr(*declRefExpr, allowSideEffects);

  return std::nullopt;
}
} // namespace yl
