#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_CFG_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_CFG_H

#include <set>
#include <vector>

#include "ast.h"

namespace yl {
struct BasicBlock {
  std::set<std::pair<int, bool>> predecessors;
  std::set<std::pair<int, bool>> successors;
  std::vector<const ResolvedStmt *> statements;
};

struct CFG : public Dumpable {
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

  void insertStmt(const ResolvedStmt *stmt, int block) {
    basicBlocks[block].statements.emplace_back(stmt);
  }

  void dump(size_t = 0) const override;
};

class CFGBuilder {
  CFG cfg;

  int insertBlock(const ResolvedBlock &block, int successor);
  int insertIfStmt(const ResolvedIfStmt &stmt, int exit);
  int insertWhileStmt(const ResolvedWhileStmt &stmt, int exit);

  int insertStmt(const ResolvedStmt &stmt, int block);
  int insertReturnStmt(const ResolvedReturnStmt &stmt, int block);
  int insertExpr(const ResolvedExpr &expr, int block);

public:
  CFG build(const ResolvedFunctionDecl &fn);
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_CFG_H
