#include <cassert>

#include "ast.h"
#include "cfg.h"

namespace yl {
namespace {
bool isTerminator(const ResolvedStmt &stmt) {
  return dynamic_cast<const ResolvedIfStmt *>(&stmt) ||
         dynamic_cast<const ResolvedWhileStmt *>(&stmt) ||
         dynamic_cast<const ResolvedReturnStmt *>(&stmt);
}
} // namespace

void CFG::dump(size_t) const {
  for (int i = basicBlocks.size() - 1; i >= 0; --i) {
    std::cerr << '[' << i;
    if (i == entry)
      std::cerr << " (entry)";
    else if (i == exit)
      std::cerr << " (exit)";
    std::cerr << ']' << '\n';

    std::cerr << "  preds: ";
    for (auto &&[id, reachable] : basicBlocks[i].predecessors)
      std::cerr << id << ((reachable) ? " " : "(U) ");
    std::cerr << '\n';

    std::cerr << "  succs: ";
    for (auto &&[id, reachable] : basicBlocks[i].successors)
      std::cerr << id << ((reachable) ? " " : "(U) ");
    std::cerr << '\n';

    const auto &statements = basicBlocks[i].statements;
    for (auto it = statements.rbegin(); it != statements.rend(); ++it)
      (*it)->dump(1);
    std::cerr << '\n';
  }
}

int CFGBuilder::insertIfStmt(const ResolvedIfStmt &stmt, int exit) {
  int falseBlock = exit;
  if (stmt.falseBlock)
    falseBlock = insertBlock(*stmt.falseBlock, exit);

  int trueBlock = insertBlock(*stmt.trueBlock, exit);
  int entry = cfg.insertNewBlock();

  std::optional<double> val = stmt.condition->getConstantValue();
  cfg.insertEdge(entry, trueBlock, val != 0);
  cfg.insertEdge(entry, falseBlock, val.value_or(0) == 0);

  cfg.insertStmt(&stmt, entry);
  return insertExpr(*stmt.condition, entry);
}

int CFGBuilder::insertWhileStmt(const ResolvedWhileStmt &stmt, int exit) {
  int latch = cfg.insertNewBlock();
  int body = insertBlock(*stmt.body, latch);

  int header = cfg.insertNewBlock();
  cfg.insertEdge(latch, header, true);

  std::optional<double> val = stmt.condition->getConstantValue();
  cfg.insertEdge(header, body, val != 0);
  cfg.insertEdge(header, exit, val.value_or(0) == 0);

  cfg.insertStmt(&stmt, header);
  insertExpr(*stmt.condition, header);

  return header;
}

int CFGBuilder::insertReturnStmt(const ResolvedReturnStmt &stmt, int block) {
  block = cfg.insertNewBlockBefore(cfg.exit, true);

  cfg.insertStmt(&stmt, block);
  if (stmt.expr)
    return insertExpr(*stmt.expr, block);

  return block;
}

int CFGBuilder::insertExpr(const ResolvedExpr &expr, int block) {
  cfg.insertStmt(&expr, block);

  if (const auto *call = dynamic_cast<const ResolvedCallExpr *>(&expr)) {
    for (auto it = call->arguments.rbegin(); it != call->arguments.rend(); ++it)
      insertExpr(**it, block);
    return block;
  }

  if (const auto *grouping = dynamic_cast<const ResolvedGroupingExpr *>(&expr))
    return insertExpr(*grouping->expr, block);

  if (const auto *binop = dynamic_cast<const ResolvedBinaryOperator *>(&expr))
    return insertExpr(*binop->rhs, block), insertExpr(*binop->lhs, block);

  if (const auto *unop = dynamic_cast<const ResolvedUnaryOperator *>(&expr))
    return insertExpr(*unop->rhs, block);

  return block;
}

int CFGBuilder::insertStmt(const ResolvedStmt &stmt, int block) {
  if (auto *ifStmt = dynamic_cast<const ResolvedIfStmt *>(&stmt))
    return insertIfStmt(*ifStmt, block);

  if (auto *whileStmt = dynamic_cast<const ResolvedWhileStmt *>(&stmt))
    return insertWhileStmt(*whileStmt, block);

  if (auto *expr = dynamic_cast<const ResolvedExpr *>(&stmt))
    return insertExpr(*expr, block);

  auto *returnStmt = dynamic_cast<const ResolvedReturnStmt *>(&stmt);
  assert(returnStmt && "unexpected statement");

  return insertReturnStmt(*returnStmt, block);
}

int CFGBuilder::insertBlock(const ResolvedBlock &block, int succ) {
  const auto &stmts = block.statements;

  bool insertNewBlock = true;
  for (auto it = stmts.rbegin(); it != stmts.rend(); ++it) {
    if (insertNewBlock && !isTerminator(**it))
      succ = cfg.insertNewBlockBefore(succ, true);

    insertNewBlock = dynamic_cast<const ResolvedWhileStmt *>(it->get());
    succ = insertStmt(**it, succ);
  }

  return succ;
}

CFG CFGBuilder::build(const ResolvedFunctionDecl &fn) {
  cfg = {};
  cfg.exit = cfg.insertNewBlock();

  int body = insertBlock(*fn.body, cfg.exit);

  cfg.entry = cfg.insertNewBlockBefore(body, true);
  return cfg;
};
} // namespace yl
