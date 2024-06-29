#include "cfg.h"
#include "ast.h"

#include <cassert>

// FIXME: Refactor this source file.
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
      std::cout << id << ((reachable) ? " " : " (U)");
    std::cout << '\n';

    std::cout << "  succs: ";
    for (auto &&[id, reachable] : basicBlocks[i].successors)
      std::cout << id << ((reachable) ? " " : " (U)");
    std::cout << '\n';

    const auto &statements = basicBlocks[i].statements;
    for (auto it = statements.rbegin(); it != statements.rend(); ++it)
      (*it)->dump(1);
    std::cout << '\n';
  }
}

void CFGBuilder::visit(const ResolvedIfStmt &stmt) {
  int exitBlock = currentBlock == -1 ? successorBlock : currentBlock;

  successorBlock = exitBlock;
  if (stmt.falseBlock) {
    currentBlock = -1;
    visit(*stmt.falseBlock);
  } else if (stmt.falseBranch) {
    visit(*stmt.falseBranch);
  }
  int elseBlock = currentBlock == -1 ? exitBlock : currentBlock;

  successorBlock = exitBlock;
  currentBlock = -1;
  visit(*stmt.trueBlock);

  // An empty block can be inserted if we need to differentiate the true and the
  // false branches.
  int trueBlock = currentBlock == -1 ? exitBlock : currentBlock;

  const auto *binaryOperator =
      dynamic_cast<const ResolvedBinaryOperator *>(stmt.condition.get());
  if (binaryOperator && (binaryOperator->op == TokenKind::PipePipe ||
                         binaryOperator->op == TokenKind::AmpAmp)) {
    visitCondition(
        *binaryOperator, &stmt,
        binaryOperator->op == TokenKind::PipePipe ? elseBlock : trueBlock,
        binaryOperator->op == TokenKind::PipePipe ? trueBlock : elseBlock);
  } else {
    successorBlock = trueBlock;
    currentBlock = -1;
    autoCreateBlock();
    currentCFG.insertEdge(currentBlock, elseBlock, true);

    currentCFG.insertStatement(currentBlock, &stmt);
    visit(*stmt.condition);
  }
}

void CFGBuilder::visit(const ResolvedWhileStmt &stmt) {
  int exitBlock = currentBlock != -1 ? currentBlock : successorBlock;

  int transitionBlock = currentCFG.insertNewBlock();

  successorBlock = transitionBlock;
  currentBlock = -1;
  visit(*stmt.body);

  int bodyBlock = currentBlock;
  successorBlock = bodyBlock;
  currentBlock = -1;

  const auto *binaryOperator =
      dynamic_cast<const ResolvedBinaryOperator *>(stmt.condition.get());
  if (binaryOperator && (binaryOperator->op == TokenKind::PipePipe ||
                         binaryOperator->op == TokenKind::AmpAmp)) {
    visitCondition(
        *binaryOperator, &stmt,
        binaryOperator->op == TokenKind::PipePipe ? exitBlock : bodyBlock,
        binaryOperator->op == TokenKind::PipePipe ? bodyBlock : exitBlock);
  } else {
    visit(*stmt.condition);
    currentCFG.insertEdge(currentBlock, exitBlock, true);
  }
  currentCFG.insertEdge(transitionBlock, currentBlock, true);

  successorBlock = currentBlock;
  currentBlock = -1;
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

void CFGBuilder::visit(const ResolvedReturnStmt &stmt) {
  // FIXME: Remove this pattern.
  currentBlock = -1;

  int currentSucc = successorBlock;
  successorBlock = currentCFG.exit;
  autoCreateBlock();
  successorBlock = currentSucc;

  currentCFG.insertStatement(currentBlock, &stmt);

  if (const auto &expr = stmt.expr.get())
    visit(*expr);
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

  currentCFG.insertEdge(rhsBlock, trueBlock, true);
  if (trueBlock != falseBlock)
    currentCFG.insertEdge(rhsBlock, falseBlock, true);

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
  currentCFG.insertEdge(currentBlock, rhsBlock, true);

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

  if (auto *whileStmt = dynamic_cast<const ResolvedWhileStmt *>(&stmt))
    return visit(*whileStmt);

  if (auto *returnStmt = dynamic_cast<const ResolvedReturnStmt *>(&stmt))
    return visit(*returnStmt);

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
  currentCFG.exit = successorBlock;

  visit(*fn.body);

  // Entry
  // FIXME: Remove this pattern.
  if (currentBlock != -1) {
    successorBlock = currentBlock;
    currentBlock = -1;
  }
  autoCreateBlock();
  currentCFG.entry = currentBlock;

  return currentCFG;
};