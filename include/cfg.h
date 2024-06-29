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

class CFG : public Dumpable {
  std::vector<BasicBlock> basicBlocks;

public:
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

  void visit(const ResolvedBlock &block);

  void visit(const ResolvedStmt &stmt);
  void visit(const ResolvedIfStmt &stmt);
  void visit(const ResolvedWhileStmt &stmt);
  void visit(const ResolvedDeclStmt &stmt);
  void visit(const ResolvedAssignment &stmt);
  void visit(const ResolvedReturnStmt &stmt);

  void visit(const ResolvedExpr &expr);

  void visitCondition(const ResolvedBinaryOperator &cond,
                      const ResolvedStmt *term, int trueBlock, int falseBlock);

public:
  CFG build(const ResolvedFunctionDecl &fn);
};

#endif // A_COMPILER_CFG_H
