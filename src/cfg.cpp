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

  if (const auto *binop = getAsConditionalBinop(stmt.condition.get())) {
    visitCondition(*binop, &stmt, trueBlock, elseBlock);
  } else {
    currentBlock = currentCFG.insertNewBlock();
    currentCFG.insertEdge(currentBlock, trueBlock, true);
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

  int bodyBlock = currentBlock != -1 ? currentBlock : transitionBlock;
  successorBlock = bodyBlock;
  currentBlock = -1;

  if (const auto *binop = getAsConditionalBinop(stmt.condition.get())) {
    visitCondition(*binop, &stmt, bodyBlock, exitBlock);
  } else {
    currentBlock = currentCFG.insertNewBlock();
    currentCFG.insertEdge(currentBlock, exitBlock, true);
    currentCFG.insertEdge(currentBlock, bodyBlock, true);

    currentCFG.insertStatement(currentBlock, &stmt);
    visit(*stmt.condition);
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
  currentBlock = currentCFG.insertNewBlock();
  currentCFG.insertEdge(currentBlock, currentCFG.exit, true);

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
  } else if (const auto *cond = getAsConditionalBinop(&expr)) {
    visitCondition(*cond, nullptr, currentBlock, currentBlock);
  } else if (const auto *binaryOperator =
                 dynamic_cast<const ResolvedBinaryOperator *>(&expr)) {
    visit(*binaryOperator->rhs);
    visit(*binaryOperator->lhs);
  } else if (const auto *unaryOperator =
                 dynamic_cast<const ResolvedUnaryOperator *>(&expr)) {
    visit(*unaryOperator->rhs);
  }
}

void CFGBuilder::visitCondition(const ResolvedBinaryOperator &cond,
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
    currentCFG.insertEdge(rhsBlock, trueBlock, true);
    currentCFG.insertEdge(rhsBlock, falseBlock, true);
  }

  bool isAnd = cond.op == TokenKind::AmpAmp;
  if (const auto *binop = getAsConditionalBinop(cond.lhs.get())) {
    // If LHS is another conditional OP and the current operator is && the false
    // branch is reachable if LHS is false and the current RHS is reachable if
    // the LHS is true. In case of ||, it's the opposite.
    isAnd ? visitCondition(*binop, &cond, rhsBlock, falseBlock)
          : visitCondition(*binop, &cond, trueBlock, rhsBlock);
    return;
  }

  // Create the block for the LHS.
  currentBlock = currentCFG.insertNewBlock();

  // If the current op is &&, the false block is reachable from the LHS if LHS
  // is false. In case of ||, the true block is reachable if the LHS is true.
  currentCFG.insertEdge(currentBlock, isAnd ? falseBlock : trueBlock, true);
  currentCFG.insertEdge(currentBlock, rhsBlock, true);

  // The current block when the function exits is the LHS block.
  currentCFG.insertStatement(currentBlock, &cond);
  visit(*cond.lhs);
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
  int entry = currentCFG.entry = currentCFG.insertNewBlock();
  currentCFG.insertEdge(
      entry, currentBlock != -1 ? currentBlock : successorBlock, true);

  return currentCFG;
};
