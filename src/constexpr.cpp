#include "constexpr.h"

namespace yl {
bool ConstantExpressionEvaluator::evaluateBinaryOperator(
    const res::BinaryOperator &binop) {

  ConstVal *lhsResult = nullptr;
  ConstVal *rhsResult = nullptr;

  if (evaluate(*binop.lhs))
    lhsResult = &results->at(binop.lhs);

  if (shortCircuitLogicalOperators && !lhsResult)
    return false;

  if (binop.op == TokenKind::PipePipe) {
    bool binopResult = lhsResult && std::get<bool>(*lhsResult);
    if (shortCircuitLogicalOperators && binopResult)
      return results->emplace(&binop, true).second;

    if (evaluate(*binop.rhs)) {
      rhsResult = &results->at(binop.rhs);
      binopResult |= std::get<bool>(*rhsResult);
    }

    if ((lhsResult && rhsResult) || binopResult)
      return results->emplace(&binop, binopResult).second;

    return false;
  }

  if (binop.op == TokenKind::AmpAmp) {
    bool binopResult = lhsResult ? std::get<bool>(*lhsResult) : true;
    if (shortCircuitLogicalOperators && !binopResult)
      return results->emplace(&binop, false).second;

    if (evaluate(*binop.rhs)) {
      rhsResult = &results->at(binop.rhs);
      binopResult &= std::get<bool>(*rhsResult);
    }

    if ((lhsResult && rhsResult) || !binopResult)
      return results->emplace(&binop, binopResult).second;

    return false;
  }

  if (!lhsResult || !evaluate(*binop.rhs))
    return false;

  TokenKind op = binop.op;
  rhsResult = &results->at(binop.rhs);

  if (op == TokenKind::EqualEqual)
    return results->emplace(&binop, *lhsResult == *rhsResult).second;

  double lhs = std::get<double>(*lhsResult);
  double rhs = std::get<double>(*rhsResult);

  if (op == TokenKind::Asterisk)
    return results->emplace(&binop, lhs * rhs).second;

  if (op == TokenKind::Slash)
    return results->emplace(&binop, lhs / rhs).second;

  if (op == TokenKind::Plus)
    return results->emplace(&binop, lhs + rhs).second;

  if (op == TokenKind::Minus)
    return results->emplace(&binop, lhs - rhs).second;

  if (op == TokenKind::Lt)
    return results->emplace(&binop, lhs < rhs).second;

  if (op == TokenKind::Gt)
    return results->emplace(&binop, lhs > rhs).second;

  if (op == TokenKind::EqualEqual)
    return results->emplace(&binop, lhs == rhs).second;

  return false;
}

bool ConstantExpressionEvaluator::evaluateUnaryOperator(
    const res::UnaryOperator &unop) {
  if (!evaluate(*unop.operand))
    return false;

  ConstVal result = results->at(unop.operand);
  if (unop.op == TokenKind::Excl)
    return results->emplace(&unop, !std::get<bool>(result)).second;

  if (unop.op == TokenKind::Minus)
    return results->emplace(&unop, -std::get<double>(result)).second;

  return false;
}

bool ConstantExpressionEvaluator::evaluateGroupingExpr(
    const res::GroupingExpr &grouping) {
  if (!evaluate(*grouping.expr))
    return false;

  return results->emplace(&grouping, results->at(grouping.expr)).second;
}

bool ConstantExpressionEvaluator::evaluatePathExpr(const res::PathExpr &path) {
  if (!evaluate(*path.fragments.back()))
    return false;

  return results->emplace(&path, results->at(path.fragments.back())).second;
}

bool ConstantExpressionEvaluator::evaluateDeclRefExpr(
    const res::DeclRefExpr &dre) {
  const auto *varDecl = dre.decl->getAs<res::VarDecl>();
  if (!varDecl || varDecl->isMutable || !varDecl->initializer)
    return false;

  if (!evaluate(*varDecl->initializer))
    return false;

  return results->emplace(&dre, results->at(varDecl->initializer)).second;
}

bool ConstantExpressionEvaluator::evaluate(const res::Expr &expr) {
  if (results->count(&expr))
    return true;

  if (const auto *numberLit = dynamic_cast<const res::NumberLiteral *>(&expr))
    return results->emplace(numberLit, numberLit->value).second;

  if (const auto *boolLit = dynamic_cast<const res::BoolLiteral *>(&expr))
    return results->emplace(boolLit, boolLit->value).second;

  if (const auto *groupingExpr = dynamic_cast<const res::GroupingExpr *>(&expr))
    return evaluateGroupingExpr(*groupingExpr);

  if (const auto *binop = dynamic_cast<const res::BinaryOperator *>(&expr))
    return evaluateBinaryOperator(*binop);

  if (const auto *unop = dynamic_cast<const res::UnaryOperator *>(&expr))
    return evaluateUnaryOperator(*unop);

  if (const auto *path = dynamic_cast<const res::PathExpr *>(&expr))
    return evaluatePathExpr(*path);

  if (const auto *declRefExpr = dynamic_cast<const res::DeclRefExpr *>(&expr))
    return evaluateDeclRefExpr(*declRefExpr);

  return false;
}
} // namespace yl
