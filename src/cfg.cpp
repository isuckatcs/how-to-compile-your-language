#include "cfg.h"
#include <cassert>

// FIXME: Refactor this source file.
void CFG::dump() const {
  for (int i = 0; i < basicBlocks.size(); ++i) {
    std::cout << '[' << i << ']' << '\n';

    std::cout << "  preds: ";
    for (auto &&pred : basicBlocks[i].predecessors)
      std::cout << pred << ' ';
    std::cout << '\n';

    std::cout << "  succs: ";
    for (auto &&succ : basicBlocks[i].successors)
      std::cout << succ << ' ';
    std::cout << '\n';

    // FIXME: make dump const
    for (auto &&stmt : basicBlocks[i].statements)
      ((ResolvedStmt *)stmt)->dump(1);
    std::cout << '\n';
  }
}

void CFGBuilder::visit(const ResolvedIfStmt &stmt) {

  int current = currentBlock;
  if (current != -1)
    successorBlock = current;

  if (stmt.falseBlock) {
    currentBlock = -1;
    visit(*stmt.falseBlock);
  } else if (stmt.falseBranch) {
    visit(*stmt.falseBranch);
  }

  int elseBlock = currentBlock == -1 ? current : currentBlock;

  if (current != -1)
    successorBlock = current;
  currentBlock = -1;
  visit(*stmt.trueBlock);

  const auto *binaryOperator =
      dynamic_cast<const ResolvedBinaryOperator *>(stmt.condition.get());
  if (binaryOperator && (binaryOperator->op == TokenKind::PipePipe ||
                         binaryOperator->op == TokenKind::AmpAmp)) {
    visitCondition(*binaryOperator, &stmt, currentBlock, elseBlock);
  } else {
    successorBlock = currentBlock;
    currentBlock = -1;
    autoCreateBlock();

    if (elseBlock != -1)
      currentCFG.insertEdge(currentBlock, elseBlock);

    visit(*stmt.condition);
  }
}

void CFGBuilder::visit(const ResolvedDeclStmt &stmt) {
  autoCreateBlock();
  currentCFG.insertStatement(currentBlock, &stmt);

  if (const auto *init = stmt.varDecl->initializer.get())
    visit(*init);
}

void CFGBuilder::visit(const ResolvedAssignment &stmt) {
  autoCreateBlock();
  currentCFG.insertStatement(currentBlock, &stmt);

  visit(*stmt.variable);
  visit(*stmt.expr);
}

void CFGBuilder::visit(const ResolvedExpr &expr) {
  autoCreateBlock();
  currentCFG.insertStatement(currentBlock, &expr);

  if (const auto *callExpr = dynamic_cast<const ResolvedCallExpr *>(&expr)) {
    for (auto it = callExpr->arguments.rbegin();
         it != callExpr->arguments.rend(); ++it)
      visit(**it);
  } else if (const auto *groupingExpr =
                 dynamic_cast<const ResolvedGroupingExpr *>(&expr)) {
    visit(*groupingExpr->expr);
  } else if (const auto *binaryOperator =
                 dynamic_cast<const ResolvedBinaryOperator *>(&expr)) {
    if (binaryOperator->op == TokenKind::PipePipe ||
        binaryOperator->op == TokenKind::AmpAmp)
      visitCondition(*binaryOperator, nullptr, currentBlock, currentBlock);
    else {
      visit(*binaryOperator->rhs);
      visit(*binaryOperator->lhs);
    }
  } else if (const auto *unaryOperator =
                 dynamic_cast<const ResolvedUnaryOperator *>(&expr)) {
    visit(*unaryOperator->rhs);
  }
}

std::pair<int, int>
CFGBuilder::visitCondition(const ResolvedBinaryOperator &cond,
                           const ResolvedStmt *term, int trueBlock,
                           int falseBlock) {
  currentBlock = currentCFG.insertNewBlock();

  if (term)
    currentCFG.insertStatement(currentBlock, term);

  visit(*cond.rhs);
  int rhsBlock = currentBlock;

  currentCFG.insertEdge(rhsBlock, trueBlock);
  if (trueBlock != falseBlock)
    currentCFG.insertEdge(rhsBlock, falseBlock);

  const auto *binaryOperator =
      dynamic_cast<const ResolvedBinaryOperator *>(cond.lhs.get());
  if (binaryOperator && (binaryOperator->op == TokenKind::PipePipe ||
                         binaryOperator->op == TokenKind::AmpAmp)) {
    return visitCondition(*binaryOperator, &cond, rhsBlock, falseBlock);
  }

  // FIXME: Remove this pattern.
  successorBlock = falseBlock;
  currentBlock = -1;
  autoCreateBlock();

  currentCFG.insertStatement(currentBlock, &cond);
  visit(*cond.lhs);
  currentCFG.insertEdge(currentBlock, rhsBlock);

  return {rhsBlock, falseBlock};
}

void CFGBuilder::visit(const ResolvedStmt &stmt) {
  if (auto *expr = dynamic_cast<const ResolvedExpr *>(&stmt))
    return visit(*expr);

  if (auto *ifStmt = dynamic_cast<const ResolvedIfStmt *>(&stmt))
    return visit(*ifStmt);

  if (auto *assignment = dynamic_cast<const ResolvedAssignment *>(&stmt))
    return visit(*assignment);

  if (auto *declStmt = dynamic_cast<const ResolvedDeclStmt *>(&stmt))
    return visit(*declStmt);

  assert(false && "unknown statement");
}

void CFGBuilder::visit(const ResolvedBlock &block) {
  for (auto it = block.statements.rbegin(); it != block.statements.rend(); ++it)
    visit(**it);
}

CFG CFGBuilder::build(const ResolvedFunctionDecl &fn) {
  currentCFG = CFG{};

  // Exit
  successorBlock = currentCFG.insertNewBlock();

  // FIXME: This crashes if the only statement in the body is an ifStmt.

  visit(*fn.body);

  // Entry
  // FIXME: Remove this pattern.
  successorBlock = currentBlock;
  currentBlock = -1;
  autoCreateBlock();

  currentCFG.dump();

  return currentCFG;
};