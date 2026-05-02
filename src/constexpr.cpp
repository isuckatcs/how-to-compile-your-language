#include "constexpr.h"

namespace yl {
res::ConstVal
ConstExprEvaluator::evaluateBinaryOperator(const res::BinaryOperator &binop) {

  res::Expr *lhs = binop.lhs;
  res::Expr *rhs = binop.rhs;

  res::ConstVal lhsVal = evaluate(*lhs);

  if (shortCircuitLogicalOperators && !lhsVal.isKnown())
    return res::ConstVal();

  if (binop.op == TokenKind::PipePipe) {
    bool binopResult = lhsVal.isKnown() && std::get<bool>(lhsVal);
    if (shortCircuitLogicalOperators && binopResult)
      return true;

    res::ConstVal rhsVal = evaluate(*rhs);
    if (rhsVal.isKnown())
      binopResult |= std::get<bool>(rhsVal);

    if ((lhsVal.isKnown() && rhsVal.isKnown()) || binopResult)
      return binopResult;

    return res::ConstVal();
  }

  if (binop.op == TokenKind::AmpAmp) {
    bool binopResult = lhsVal.isKnown() ? std::get<bool>(lhsVal) : true;
    if (shortCircuitLogicalOperators && !binopResult)
      return false;

    res::ConstVal rhsVal = evaluate(*rhs);
    if (rhsVal.isKnown())
      binopResult &= std::get<bool>(rhsVal);

    if ((lhsVal.isKnown() && rhsVal.isKnown()) || !binopResult)
      return binopResult;

    return res::ConstVal();
  }

  res::ConstVal rhsVal = evaluate(*rhs);
  if (!lhsVal.isKnown() || !rhsVal.isKnown())
    return res::ConstVal();

  TokenKind op = binop.op;

  if (op == TokenKind::EqualEqual)
    return lhsVal == rhsVal;

  res::ConstVal result;
  double lhsDouble = std::get<double>(lhsVal);
  double rhsDouble = std::get<double>(rhsVal);

  if (op == TokenKind::Asterisk)
    result = lhsDouble * rhsDouble;

  if (op == TokenKind::Slash)
    result = lhsDouble / rhsDouble;

  if (op == TokenKind::Plus)
    result = lhsDouble + rhsDouble;

  if (op == TokenKind::Minus)
    result = lhsDouble - rhsDouble;

  if (op == TokenKind::Lt)
    result = lhsDouble < rhsDouble;

  if (op == TokenKind::Gt)
    result = lhsDouble > rhsDouble;

  return result;
}

res::ConstVal
ConstExprEvaluator::evaluateUnaryOperator(const res::UnaryOperator &unop) {
  res::ConstVal result = evaluate(*unop.operand);
  if (!result.isKnown())
    return result;

  if (unop.op == TokenKind::Excl)
    result = !std::get<bool>(result);
  else if (unop.op == TokenKind::Minus)
    result = -std::get<double>(result);

  return result;
}

res::ConstVal
ConstExprEvaluator::evaluateDeclRefExpr(const res::DeclRefExpr &dre) {
  const auto *varDecl = dre.decl->getAs<res::VarDecl>();
  if (!varDecl || varDecl->isMutable || !varDecl->initializer)
    return res::ConstVal();

  return evaluate(*varDecl->initializer);
}

res::ConstVal ConstExprEvaluator::evaluate(const res::Expr &expr) {
  if (auto *numberLit = dynamic_cast<const res::NumberLiteral *>(&expr))
    return numberLit->value;

  if (auto *boolLit = dynamic_cast<const res::BoolLiteral *>(&expr))
    return boolLit->value;

  if (auto *groupingExpr = dynamic_cast<const res::GroupingExpr *>(&expr))
    return evaluate(*groupingExpr->expr);

  if (auto *binop = dynamic_cast<const res::BinaryOperator *>(&expr))
    return evaluateBinaryOperator(*binop);

  if (auto *unop = dynamic_cast<const res::UnaryOperator *>(&expr))
    return evaluateUnaryOperator(*unop);

  if (auto *path = dynamic_cast<const res::PathExpr *>(&expr))
    return evaluate(*path->fragments.back());

  if (auto *declRefExpr = dynamic_cast<const res::DeclRefExpr *>(&expr))
    return evaluateDeclRefExpr(*declRefExpr);

  return res::ConstVal();
}
} // namespace yl
