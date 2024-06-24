#ifndef A_COMPILER_CFG_H
#define A_COMPILER_CFG_H

#include <vector>

#include "ast.h"

struct BasicBlock {
  std::vector<int> predecessors;
  std::vector<int> successors;
  std::vector<const ResolvedStmt *> statements;
};

class CFG {
  std::vector<BasicBlock> basicBlocks;

public:
  int insertNewBlock() {
    basicBlocks.emplace_back();
    return basicBlocks.size() - 1;
  };

  void insertEdge(int from, int to) {
    basicBlocks[from].successors.emplace_back(to);
    basicBlocks[to].predecessors.emplace_back(from);
  }

  void insertStatement(int block, const ResolvedStmt *statement) {
    basicBlocks[block].statements.emplace_back(statement);
  }

  void dump() const;
};

class CFGBuilder {
  CFG currentCFG;
  int currentBlock = -1;
  int successorBlock = -1;

  void autoCreateBlock() {
    if (currentBlock == -1) {
      currentBlock = currentCFG.insertNewBlock();
      currentCFG.insertEdge(currentBlock, successorBlock);
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

  std::pair<int, int> visitCondition(const ResolvedBinaryOperator &cond,
                                     const ResolvedStmt *Term, int trueBlock,
                                     int falseBlock);

public:
  CFG build(const ResolvedFunctionDecl &fn);
};

#endif // A_COMPILER_CFG_H
