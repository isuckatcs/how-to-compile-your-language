#include <iostream>

#include "cfg.h"
#include "constexpr.h"
#include "res.h"

namespace yl {
namespace {
bool isTerminator(const res::Stmt &stmt) {
  return dynamic_cast<const res::IfStmt *>(&stmt) ||
         dynamic_cast<const res::WhileStmt *>(&stmt) ||
         dynamic_cast<const res::ReturnStmt *>(&stmt);
}
} // namespace

void CFG::dumpStmt(const res::Stmt *stmt, bool topLevel) const {
  if (auto *grouping = dynamic_cast<const res::GroupingExpr *>(stmt)) {
    dumpStmt(grouping->expr);
    return;
  }

  if (value.count(stmt)) {
    std::cerr << value[stmt];
    return;
  }

  if (topLevel)
    std::cerr << "  ";

  if (auto *ifStmt = dynamic_cast<const res::IfStmt *>(stmt)) {
    std::cerr << "if ";
    dumpStmt(ifStmt->condition);
    return;
  }

  if (auto *whileStmt = dynamic_cast<const res::WhileStmt *>(stmt)) {
    std::cerr << "while ";
    dumpStmt(whileStmt->condition);
    return;
  }

  if (auto *assignment = dynamic_cast<const res::Assignment *>(stmt)) {
    dumpStmt(assignment->assignee);
    std::cerr << " = ";
    dumpStmt(assignment->expr);
    return;
  }

  if (auto *retStmt = dynamic_cast<const res::ReturnStmt *>(stmt)) {
    std::cerr << "return ";
    dumpStmt(retStmt->expr);
    return;
  }

  if (auto *declStmt = dynamic_cast<const res::DeclStmt *>(stmt)) {
    auto *varDecl = declStmt->varDecl;
    std::cerr << (varDecl->isMutable ? "mut " : "let ") << varDecl->identifier;

    if (auto *init = varDecl->initializer) {
      std::cerr << " = ";
      dumpStmt(init);
    }

    return;
  }

  if (topLevel) {
    value[stmt] = '$' + std::to_string(value.size() + 1);
    std::cerr << value[stmt] << " = ";
  }

  if (auto *unit = dynamic_cast<const res::UnitLiteral *>(stmt)) {
    std::cerr << "unit";
    return;
  }

  if (auto *number = dynamic_cast<const res::NumberLiteral *>(stmt)) {
    std::cerr << number->value;
    return;
  }

  if (auto *boolLiteral = dynamic_cast<const res::BoolLiteral *>(stmt)) {
    std::cerr << (boolLiteral->value ? "true" : "false");
    return;
  }

  if (auto *callExpr = dynamic_cast<const res::CallExpr *>(stmt)) {
    dumpStmt(callExpr->callee);
    std::cerr << '(';
    for (int i = 0; i < callExpr->arguments.size(); ++i) {
      dumpStmt(callExpr->arguments[i]);

      if (i != callExpr->arguments.size() - 1)
        std::cerr << ", ";
    }
    std::cerr << ')';
  }

  if (auto *binop = dynamic_cast<const res::BinaryOperator *>(stmt)) {
    dumpStmt(binop->lhs);
    std::cerr << ' ' << getOpStr(binop->op) << ' ';
    dumpStmt(binop->rhs);
    return;
  }

  if (auto *unop = dynamic_cast<const res::UnaryOperator *>(stmt)) {
    std::cerr << getOpStr(unop->op);
    dumpStmt(unop->operand);
    return;
  }

  if (auto *sie = dynamic_cast<const res::StructInstantiationExpr *>(stmt)) {
    dumpStmt(sie->structPath);

    std::cerr << " { ";
    for (auto &&fieldInit : sie->fieldInitializers) {
      std::cerr << fieldInit->field->identifier << ": ";
      dumpStmt(fieldInit->initializer);

      if (fieldInit != sie->fieldInitializers.back())
        std::cerr << ", ";
    }
    std::cerr << " }";
    return;
  }

  if (auto *dre = dynamic_cast<const res::DeclRefExpr *>(stmt)) {
    std::cerr << dre->decl->identifier;
    return;
  }

  if (auto *memberExpr = dynamic_cast<const res::MemberExpr *>(stmt)) {
    dumpStmt(memberExpr->base);
    std::cerr << '.' << memberExpr->member->decl->identifier;
    return;
  }

  if (auto *lambda = dynamic_cast<const res::LambdaExpr *>(stmt)) {
    std::cerr << "->[";
    for (int i = 0; i < lambda->fieldInits.size(); ++i) {
      dumpStmt(lambda->fieldInits[i]);

      if (i != lambda->fieldInits.size() - 1)
        std::cerr << ',' << ' ';
    }
    std::cerr << "](...){...}";
  }
}

void CFG::dump() const {
  std::cerr << "fn " << fn->identifier << "(...) {\n";

  for (int i = basicBlocks.size() - 1; i >= 0; --i) {
    std::cerr << "bb" << i << ":\n";

    std::cerr << "  preds: ";
    for (auto &&[id, reachable] : basicBlocks[i].predecessors)
      std::cerr << id << ((reachable) ? " " : "(U) ");
    std::cerr << '\n';

    const auto &statements = basicBlocks[i].statements;
    for (auto it = statements.rbegin(); it != statements.rend(); ++it) {
      dumpStmt(*it, true);
      std::cerr << '\n';
    }

    std::cerr << "  succs: ";
    for (auto &&[id, reachable] : basicBlocks[i].successors)
      std::cerr << id << ((reachable) ? " " : "(U) ");
    std::cerr << '\n';

    if (i > 0)
      std::cerr << '\n';
  }
  std::cerr << "}\n";
}

int CFGBuilder::insertIfStmt(const res::IfStmt &stmt, int exit) {
  int falseBlock = exit;
  if (stmt.falseBlock)
    falseBlock = insertBlock(*stmt.falseBlock, exit);

  int trueBlock = insertBlock(*stmt.trueBlock, exit);
  int entry = cfg.insertNewBlock();

  if (trueBlock == falseBlock) {
    cfg.insertEdge(entry, exit, true);
    return insertExpr(*stmt.condition, entry);
  }

  bool trueReachable = true;
  bool falseReachable = true;

  res::ConstVal constVal = ConstExprEvaluator(false).evaluate(*stmt.condition);
  if (constVal.isKnown()) {
    bool condVal = std::get<bool>(constVal);

    trueReachable = condVal;
    falseReachable = !condVal;
  }

  cfg.insertEdge(entry, trueBlock, trueReachable);
  cfg.insertEdge(entry, falseBlock, falseReachable);

  cfg.insertStmt(&stmt, entry);
  return insertExpr(*stmt.condition, entry);
}

int CFGBuilder::insertWhileStmt(const res::WhileStmt &stmt, int exit) {
  int latch = cfg.insertNewBlock();
  int body = insertBlock(*stmt.body, latch);

  int header = cfg.insertNewBlock();
  cfg.insertEdge(latch, header, true);

  bool trueReachable = true;
  bool falseReachable = true;

  res::ConstVal constVal = ConstExprEvaluator(false).evaluate(*stmt.condition);
  if (constVal.isKnown()) {
    bool condVal = std::get<bool>(constVal);

    trueReachable = condVal;
    falseReachable = !condVal;
  }

  cfg.insertEdge(header, body, trueReachable);
  cfg.insertEdge(header, exit, falseReachable);

  cfg.insertStmt(&stmt, header);
  insertExpr(*stmt.condition, header);

  return header;
}

int CFGBuilder::insertDeclStmt(const res::DeclStmt &stmt, int block) {
  cfg.insertStmt(&stmt, block);

  if (const auto &init = stmt.varDecl->initializer)
    return insertExpr(*init, block);

  return block;
}

int CFGBuilder::insertAssignment(const res::Assignment &stmt, int block) {
  cfg.insertStmt(&stmt, block);

  if (auto *me = dynamic_cast<const res::MemberExpr *>(stmt.assignee))
    block = insertExpr(*me->base, block);

  return insertExpr(*stmt.expr, block);
}

int CFGBuilder::insertReturnStmt(const res::ReturnStmt &stmt, int block) {
  block = cfg.insertNewBlockBefore(cfg.exit, true);

  cfg.insertStmt(&stmt, block);
  if (stmt.expr)
    return insertExpr(*stmt.expr, block);

  return block;
}

int CFGBuilder::insertExpr(const res::Expr &expr, int block) {
  if (auto *grouping = dynamic_cast<const res::GroupingExpr *>(&expr))
    return insertExpr(*grouping->expr, block);

  cfg.insertStmt(&expr, block);

  if (auto *call = dynamic_cast<const res::CallExpr *>(&expr)) {
    block = insertExpr(*call->callee, block);
    for (auto it = call->arguments.rbegin(); it != call->arguments.rend(); ++it)
      block = insertExpr(**it, block);
    return block;
  }

  if (auto *memberExpr = dynamic_cast<const res::MemberExpr *>(&expr))
    return insertExpr(*memberExpr->base, block);

  if (auto *binop = dynamic_cast<const res::BinaryOperator *>(&expr))
    return insertExpr(*binop->rhs, block), insertExpr(*binop->lhs, block);

  if (auto *unop = dynamic_cast<const res::UnaryOperator *>(&expr))
    return insertExpr(*unop->operand, block);

  if (auto *sie = dynamic_cast<const res::StructInstantiationExpr *>(&expr)) {
    auto &fieldInits = sie->fieldInitializers;
    for (auto it = fieldInits.rbegin(); it != fieldInits.rend(); ++it)
      block = insertStmt(*(*it)->initializer, block);
    return block;
  }

  if (auto *lambda = dynamic_cast<const res::LambdaExpr *>(&expr)) {
    const auto &inits = lambda->fieldInits;
    for (auto it = inits.rbegin(); it != inits.rend(); ++it)
      block = insertExpr(**it, block);
  }

  return block;
}

int CFGBuilder::insertStmt(const res::Stmt &stmt, int block) {
  if (auto *ifStmt = dynamic_cast<const res::IfStmt *>(&stmt))
    return insertIfStmt(*ifStmt, block);

  if (auto *whileStmt = dynamic_cast<const res::WhileStmt *>(&stmt))
    return insertWhileStmt(*whileStmt, block);

  if (auto *expr = dynamic_cast<const res::Expr *>(&stmt))
    return insertExpr(*expr, block);

  if (auto *assignment = dynamic_cast<const res::Assignment *>(&stmt))
    return insertAssignment(*assignment, block);

  if (auto *declStmt = dynamic_cast<const res::DeclStmt *>(&stmt))
    return insertDeclStmt(*declStmt, block);

  if (auto *returnStmt = dynamic_cast<const res::ReturnStmt *>(&stmt))
    return insertReturnStmt(*returnStmt, block);

  if (auto *fieldInit = dynamic_cast<const res::FieldInitStmt *>(&stmt)) {
    cfg.insertStmt(fieldInit, block);
    return insertExpr(*fieldInit->initializer, block);
  }

  llvm_unreachable("unexpected expression");
}

int CFGBuilder::insertBlock(const res::Block &block, int succ) {
  const auto &stmts = block.statements;

  bool insertNewBlock = true;
  for (auto it = stmts.rbegin(); it != stmts.rend(); ++it) {
    if (insertNewBlock && !isTerminator(**it))
      succ = cfg.insertNewBlockBefore(succ, true);

    insertNewBlock = dynamic_cast<const res::WhileStmt *>(*it);
    succ = insertStmt(**it, succ);
  }

  return succ;
}

CFG CFGBuilder::build(const res::FunctionDecl &fn) {
  cfg = {};
  cfg.fn = &fn;

  cfg.exit = cfg.insertNewBlock();
  int body = insertBlock(*fn.body, cfg.exit);
  cfg.entry = cfg.insertNewBlockBefore(body, true);

  return cfg;
};
} // namespace yl
