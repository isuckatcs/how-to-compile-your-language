#include "cfg.h"
#include "ast.h"

#include <cassert>

namespace {
const ResolvedBinaryOperator *getAsConditionalBinop(const ResolvedExpr *expr) {
  const auto *binop = dynamic_cast<const ResolvedBinaryOperator *>(expr);
  if (!binop)
    return nullptr;

  TokenKind op = binop->op;
  if (op == TokenKind::PipePipe || op == TokenKind::AmpAmp)
    return binop;

  return nullptr;
}
} // namespace

void CFG::dump(size_t) const {
  for (int i = basicBlocks.size() - 1; i >= 0; --i) {
    std::cout << '[' << i;
    if (i == entry)
      std::cout << " (entry)";
    else if (i == exit)
      std::cout << " (exit)";
    std::cout << ']' << '\n';

    std::cout << "  preds: ";
    for (auto &&[id, reachable] : basicBlocks[i].predecessors)
      std::cout << id << ((reachable) ? " " : "(U) ");
    std::cout << '\n';

    std::cout << "  succs: ";
    for (auto &&[id, reachable] : basicBlocks[i].successors)
      std::cout << id << ((reachable) ? " " : "(U) ");
    std::cout << '\n';

    const auto &statements = basicBlocks[i].statements;
    for (auto it = statements.rbegin(); it != statements.rend(); ++it)
      (*it)->dump(1);
    std::cout << '\n';
  }
}

int CFGBuilder::visit(const ResolvedIfStmt &stmt) {
  int exitBlock = currentBlock == -1 ? successorBlock : currentBlock;

  int elseBlock = exitBlock;
  if (stmt.falseBlock) {
    successorBlock = exitBlock;
    currentBlock = -1;

    elseBlock = visit(*stmt.falseBlock);

    if (elseBlock == -1)
      elseBlock = exitBlock;
  }

  int trueBlock = -1;
  successorBlock = exitBlock;
  currentBlock = -1;

  trueBlock = visit(*stmt.trueBlock);

  if (trueBlock == -1)
    trueBlock = exitBlock;

  if (const auto *binop = getAsConditionalBinop(stmt.condition.get()))
    return visitCondition(*binop, &stmt, trueBlock, elseBlock);

  std::optional<double> conditionVal = stmt.condition->getConstantValue();

  currentBlock = currentCFG.insertNewBlock();
  currentCFG.insertEdge(currentBlock, trueBlock,
                        conditionVal == 0 ? false : true);
  currentCFG.insertEdge(currentBlock, elseBlock,
                        conditionVal.value_or(0) != 0 ? false : true);

  currentCFG.insertStatement(currentBlock, &stmt);

  return visit(*stmt.condition);
}

int CFGBuilder::visit(const ResolvedWhileStmt &stmt) {
  int exitBlock = currentBlock != -1 ? currentBlock : successorBlock;

  int transitionBlock = currentCFG.insertNewBlock();

  successorBlock = transitionBlock;
  currentBlock = -1;

  int bodyBlock = visit(*stmt.body);

  if (bodyBlock == -1)
    bodyBlock = transitionBlock;

  successorBlock = bodyBlock;
  currentBlock = -1;

  if (const auto *binop = getAsConditionalBinop(stmt.condition.get())) {
    visitCondition(*binop, &stmt, bodyBlock, exitBlock);
  } else {
    std::optional<double> conditionVal = stmt.condition->getConstantValue();

    currentBlock = currentCFG.insertNewBlock();
    currentCFG.insertEdge(currentBlock, exitBlock,
                          conditionVal.value_or(0) != 0 ? false : true);
    currentCFG.insertEdge(currentBlock, bodyBlock,
                          conditionVal == 0 ? false : true);

    currentCFG.insertStatement(currentBlock, &stmt);
    visit(*stmt.condition);
  }
  currentCFG.insertEdge(transitionBlock, currentBlock, true);

  successorBlock = currentBlock;
  currentBlock = -1;

  return successorBlock;
}

int CFGBuilder::visit(const ResolvedDeclStmt &stmt) {
  autoCreateBlock();
  currentCFG.insertStatement(currentBlock, &stmt);

  if (const auto *init = stmt.varDecl->initializer.get())
    return visit(*init);

  return currentBlock;
}

int CFGBuilder::visit(const ResolvedAssignment &stmt) {
  autoCreateBlock();
  currentCFG.insertStatement(currentBlock, &stmt);

  return visit(*stmt.expr);
}

int CFGBuilder::visit(const ResolvedReturnStmt &stmt) {
  currentBlock = currentCFG.insertNewBlock();
  currentCFG.insertEdge(currentBlock, currentCFG.exit, true);

  currentCFG.insertStatement(currentBlock, &stmt);

  if (stmt.expr)
    return visit(*stmt.expr);

  return currentBlock;
}

int CFGBuilder::visit(const ResolvedExpr &expr) {
  autoCreateBlock();
  currentCFG.insertStatement(currentBlock, &expr);

  if (const auto *callExpr = dynamic_cast<const ResolvedCallExpr *>(&expr)) {
    for (auto it = callExpr->arguments.rbegin();
         it != callExpr->arguments.rend(); ++it)
      visit(**it);
    return currentBlock;
  }

  if (const auto *groupingExpr =
          dynamic_cast<const ResolvedGroupingExpr *>(&expr))
    return visit(*groupingExpr->expr);

  if (const auto *cond = getAsConditionalBinop(&expr))
    return visitCondition(*cond, nullptr, currentBlock, currentBlock);

  if (const auto *binaryOperator =
          dynamic_cast<const ResolvedBinaryOperator *>(&expr)) {
    visit(*binaryOperator->rhs);
    return visit(*binaryOperator->lhs);
  }

  if (const auto *unaryOperator =
          dynamic_cast<const ResolvedUnaryOperator *>(&expr))
    return visit(*unaryOperator->rhs);

  return currentBlock;
}

int CFGBuilder::visitCondition(const ResolvedBinaryOperator &cond,
                               const ResolvedStmt *term, int trueBlock,
                               int falseBlock) {
  int rhsBlock = -1;
  if (const auto *binop = getAsConditionalBinop(cond.rhs.get())) {
    visitCondition(*binop, term, trueBlock, falseBlock);
    rhsBlock = currentBlock;
  } else {
    // Create the block for the RHS.
    currentBlock = currentCFG.insertNewBlock();

    if (term)
      currentCFG.insertStatement(currentBlock, term);

    visit(*cond.rhs);
    rhsBlock = currentBlock;

    // From the RHS both the true and the false block is reachable.
    std::optional<double> conditionVal = cond.rhs->getConstantValue();
    currentCFG.insertEdge(rhsBlock, trueBlock,
                          conditionVal == 0 ? false : true);

    if (trueBlock != falseBlock)
      currentCFG.insertEdge(rhsBlock, falseBlock,
                            conditionVal.value_or(0) != 0 ? false : true);
  }

  bool isAnd = cond.op == TokenKind::AmpAmp;
  if (const auto *binop = getAsConditionalBinop(cond.lhs.get())) {
    // If LHS is another conditional OP and the current operator is && the false
    // branch is reachable if LHS is false and the current RHS is reachable if
    // the LHS is true. In case of ||, it's the opposite.
    if (isAnd)
      return visitCondition(*binop, &cond, rhsBlock, falseBlock);

    return visitCondition(*binop, &cond, trueBlock, rhsBlock);
  }

  // Create the block for the LHS.
  currentBlock = currentCFG.insertNewBlock();

  // If the current op is &&, the false block is reachable from the LHS if LHS
  // is false. In case of ||, the true block is reachable if the LHS is true.
  std::optional<double> conditionVal = cond.lhs->getConstantValue();

  bool firstReachable = isAnd ? (conditionVal.value_or(0) != 0 ? false : true)
                              : (conditionVal == 0 ? false : true);
  currentCFG.insertEdge(currentBlock, isAnd ? falseBlock : trueBlock,
                        firstReachable);

  bool secondReachable = isAnd ? (conditionVal == 0 ? false : true)
                               : (conditionVal.value_or(0) != 0 ? false : true);
  currentCFG.insertEdge(currentBlock, rhsBlock, secondReachable);

  // The current block when the function exits is the LHS block.
  currentCFG.insertStatement(currentBlock, &cond);
  return visit(*cond.lhs);
}

int CFGBuilder::visit(const ResolvedStmt &stmt) {
  if (auto *expr = dynamic_cast<const ResolvedExpr *>(&stmt))
    return visit(*expr);

  if (auto *ifStmt = dynamic_cast<const ResolvedIfStmt *>(&stmt))
    return visit(*ifStmt);

  if (auto *assignment = dynamic_cast<const ResolvedAssignment *>(&stmt))
    return visit(*assignment);

  if (auto *declStmt = dynamic_cast<const ResolvedDeclStmt *>(&stmt))
    return visit(*declStmt);

  if (auto *whileStmt = dynamic_cast<const ResolvedWhileStmt *>(&stmt))
    return visit(*whileStmt);

  auto *returnStmt = dynamic_cast<const ResolvedReturnStmt *>(&stmt);

  assert(returnStmt && "unexpected statement");

  return visit(*returnStmt);
}

int CFGBuilder::visit(const ResolvedBlock &block) {
  int b = currentBlock;

  for (auto it = block.statements.rbegin(); it != block.statements.rend(); ++it)
    b = visit(**it);

  return b;
}

CFG CFGBuilder::build(const ResolvedFunctionDecl &fn) {
  currentCFG = CFG{};

  // Exit
  successorBlock = currentCFG.insertNewBlock();
  currentCFG.exit = successorBlock;

  visit(*fn.body);

  // Entry
  int entry = currentCFG.entry = currentCFG.insertNewBlock();
  currentCFG.insertEdge(
      entry, currentBlock != -1 ? currentBlock : successorBlock, true);

  return currentCFG;
};
