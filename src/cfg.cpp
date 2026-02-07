#include <iostream>

#include "cfg.h"
#include "res.h"

namespace yl {
namespace {
bool isTerminator(const res::Stmt &stmt) {
  return dynamic_cast<const res::IfStmt *>(&stmt) ||
         dynamic_cast<const res::WhileStmt *>(&stmt) ||
         dynamic_cast<const res::ReturnStmt *>(&stmt);
}
} // namespace

void CFG::dump() const {
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

    std::unordered_map<const res::Stmt *, std::string> stmtToRef;

    const auto &statements = basicBlocks[i].statements;
    for (auto it = statements.rbegin(); it != statements.rend(); ++it) {
      std::cerr << ' ' << ' ' << stmtToRef.size() + 1 << ':' << ' ';

      if (auto *ifStmt = dynamic_cast<const res::IfStmt *>(*it)) {
        std::cerr << "if " << stmtToRef[ifStmt->condition];
      } else if (auto *whileStmt = dynamic_cast<const res::WhileStmt *>(*it)) {
        std::cerr << "while " << stmtToRef[whileStmt->condition];
      } else if (auto *assignment =
                     dynamic_cast<const res::Assignment *>(*it)) {
        std::cerr << stmtToRef[assignment->assignee] << " = "
                  << stmtToRef[assignment->expr];
      } else if (auto *declStmt = dynamic_cast<const res::DeclStmt *>(*it)) {
        std::cerr << (declStmt->varDecl->isMutable ? "mut " : "let ")
                  << declStmt->varDecl->identifier;

        if (const auto *init = declStmt->varDecl->initializer)
          std::cerr << " = " << stmtToRef[init];
      } else if (auto *retStmt = dynamic_cast<const res::ReturnStmt *>(*it)) {
        std::cerr << "return " << stmtToRef[retStmt->expr];
      } else if (auto *fi = dynamic_cast<const res::FieldInitStmt *>(*it)) {
        std::cerr << fi->field->identifier << ": "
                  << stmtToRef[fi->initializer];
      } else if (const auto *number =
                     dynamic_cast<const res::NumberLiteral *>(*it)) {
        std::cerr << number->value;
      } else if (const auto *callExpr =
                     dynamic_cast<const res::CallExpr *>(*it)) {
        std::cerr << stmtToRef[callExpr->callee] << ' ' << '(';
        for (int i = 0; i < callExpr->arguments.size(); ++i) {
          std::cerr << stmtToRef[callExpr->arguments[i]];

          if (i != callExpr->arguments.size() - 1)
            std::cerr << ',' << ' ';
        }
        std::cerr << ')';
      } else if (const auto *grouping =
                     dynamic_cast<const res::GroupingExpr *>(*it)) {
        std::cerr << '(' << stmtToRef[grouping->expr] << ')';
      } else if (const auto *binop =
                     dynamic_cast<const res::BinaryOperator *>(*it)) {
        std::cerr << stmtToRef[binop->lhs] << ' ' << getOpStr(binop->op) << ' '
                  << stmtToRef[binop->rhs];
      } else if (const auto *unop =
                     dynamic_cast<const res::UnaryOperator *>(*it)) {
        std::cerr << getOpStr(unop->op) << stmtToRef[unop->operand];
      } else if (const auto *si =
                     dynamic_cast<const res::StructInstantiationExpr *>(*it)) {
        std::cerr << stmtToRef[si->structDecl] << ' ' << '{';
        for (int i = 0; i < si->fieldInitializers.size(); ++i) {
          std::cerr << stmtToRef[si->fieldInitializers[i]];

          if (i != si->fieldInitializers.size() - 1)
            std::cerr << ',' << ' ';
        }
        std::cerr << '}';
      } else if (const auto *dre =
                     dynamic_cast<const res::DeclRefExpr *>(*it)) {
        std::cerr << dre->decl->identifier;
      } else if (const auto *memberExpr =
                     dynamic_cast<const res::MemberExpr *>(*it)) {
        std::cerr << stmtToRef[memberExpr->base] << '.'
                  << memberExpr->field->identifier;
      }

      stmtToRef[*it] = '[' + std::to_string(i) + '.' +
                       std::to_string(stmtToRef.size() + 1) + ']';
      std::cerr << '\n';
    }
    std::cerr << '\n';
  }
}

int CFGBuilder::insertIfStmt(const res::IfStmt &stmt, int exit) {
  int falseBlock = exit;
  if (stmt.falseBlock)
    falseBlock = insertBlock(*stmt.falseBlock, exit);

  int trueBlock = insertBlock(*stmt.trueBlock, exit);
  int entry = cfg.insertNewBlock();

  std::optional<double> val = cee.evaluate(*stmt.condition, true);
  cfg.insertEdge(entry, trueBlock, val != 0);
  cfg.insertEdge(entry, falseBlock, val.value_or(0) == 0);

  cfg.insertStmt(&stmt, entry);
  return insertExpr(*stmt.condition, entry);
}

int CFGBuilder::insertWhileStmt(const res::WhileStmt &stmt, int exit) {
  int latch = cfg.insertNewBlock();
  int body = insertBlock(*stmt.body, latch);

  int header = cfg.insertNewBlock();
  cfg.insertEdge(latch, header, true);

  std::optional<double> val = cee.evaluate(*stmt.condition, true);
  cfg.insertEdge(header, body, val != 0);
  cfg.insertEdge(header, exit, val.value_or(0) == 0);

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

  if (!dynamic_cast<const res::DeclRefExpr *>(stmt.assignee))
    block = insertExpr(*stmt.assignee, block);

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
  cfg.insertStmt(&expr, block);

  if (const auto *call = dynamic_cast<const res::CallExpr *>(&expr)) {
    block = insertExpr(*call->callee, block);
    for (auto it = call->arguments.rbegin(); it != call->arguments.rend(); ++it)
      block = insertExpr(**it, block);
    return block;
  }

  if (const auto *memberExpr = dynamic_cast<const res::MemberExpr *>(&expr))
    return insertExpr(*memberExpr->base, block);

  if (const auto *grouping = dynamic_cast<const res::GroupingExpr *>(&expr))
    return insertExpr(*grouping->expr, block);

  if (const auto *binop = dynamic_cast<const res::BinaryOperator *>(&expr))
    return insertExpr(*binop->rhs, block), insertExpr(*binop->lhs, block);

  if (const auto *unop = dynamic_cast<const res::UnaryOperator *>(&expr))
    return insertExpr(*unop->operand, block);

  if (const auto *structInst =
          dynamic_cast<const res::StructInstantiationExpr *>(&expr)) {
    block = insertStmt(*structInst->structDecl, block);
    for (auto it = structInst->fieldInitializers.rbegin();
         it != structInst->fieldInitializers.rend(); ++it)
      block = insertStmt(**it, block);
    return block;
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
  cfg.exit = cfg.insertNewBlock();

  int body = insertBlock(*fn.body, cfg.exit);

  cfg.entry = cfg.insertNewBlockBefore(body, true);
  return cfg;
};
} // namespace yl
