#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_CFG_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_CFG_H

#include <set>
#include <vector>

#include "constexpr.h"
#include "res.h"

namespace yl {
struct BasicBlock {
  std::set<std::pair<int, bool>> predecessors;
  std::set<std::pair<int, bool>> successors;
  std::vector<const res::Stmt *> statements;
};

struct CFG {
  std::vector<BasicBlock> basicBlocks;
  int entry = -1;
  int exit = -1;

  int insertNewBlock() {
    basicBlocks.emplace_back();
    return basicBlocks.size() - 1;
  };

  int insertNewBlockBefore(int before, bool reachable) {
    int b = insertNewBlock();
    insertEdge(b, before, reachable);
    return b;
  }

  void insertEdge(int from, int to, bool reachable) {
    basicBlocks[from].successors.emplace(std::make_pair(to, reachable));
    basicBlocks[to].predecessors.emplace(std::make_pair(from, reachable));
  }

  void insertStmt(const res::Stmt *stmt, int block) {
    basicBlocks[block].statements.emplace_back(stmt);
  }

  void dump() const;
};

class CFGBuilder {
  ConstantExpressionEvaluator cee;
  CFG cfg;

  int insertBlock(const res::Block &block, int successor);
  int insertIfStmt(const res::IfStmt &stmt, int exit);
  int insertWhileStmt(const res::WhileStmt &stmt, int exit);

  int insertStmt(const res::Stmt &stmt, int block);
  int insertDeclStmt(const res::DeclStmt &stmt, int block);
  int insertAssignment(const res::Assignment &stmt, int block);
  int insertReturnStmt(const res::ReturnStmt &stmt, int block);
  int insertExpr(const res::Expr &expr, int block);

public:
  CFG build(const res::FunctionDecl &fn);
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_CFG_H
