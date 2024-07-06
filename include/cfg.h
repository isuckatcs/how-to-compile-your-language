#ifndef A_COMPILER_CFG_H
#define A_COMPILER_CFG_H

#include <set>
#include <vector>

#include "ast.h"

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

  void insertEdge(int from, int to, bool reachable) {
    basicBlocks[from].successors.emplace(std::make_pair(to, reachable));
    basicBlocks[to].predecessors.emplace(std::make_pair(from, reachable));
  }

  void insertStatement(int block, const ResolvedStmt *statement) {
    basicBlocks[block].statements.emplace_back(statement);
  }

  void dump(size_t = 0) const override;
};

class CFGBuilder {
  CFG currentCFG;
  int currentBlock = -1;
  int successorBlock = -1;

  void autoCreateBlock() {
    if (currentBlock == -1) {
      currentBlock = currentCFG.insertNewBlock();
      currentCFG.insertEdge(currentBlock, successorBlock, true);
    }
  }

  int visit(const ResolvedBlock &block);

  int visit(const ResolvedStmt &stmt);
  int visit(const ResolvedIfStmt &stmt);
  int visit(const ResolvedWhileStmt &stmt);
  int visit(const ResolvedDeclStmt &stmt);
  int visit(const ResolvedAssignment &stmt);
  int visit(const ResolvedReturnStmt &stmt);
  int visit(const ResolvedExpr &expr);

  int visitCondition(const ResolvedBinaryOperator &cond,
                     const ResolvedStmt *term, int trueBlock, int falseBlock);

  template <typename T> int buildIntoNewBlock(T &&element, int successor);

public:
  CFG build(const ResolvedFunctionDecl &fn);
};

#endif // A_COMPILER_CFG_H
