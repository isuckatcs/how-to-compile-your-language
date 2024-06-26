#ifndef A_COMPILER_CODEGEN_H
#define A_COMPILER_CODEGEN_H

#include "ast.h"

#include <map>
#include <memory>
#include <vector>

#include <llvm/IR/IRBuilder.h>

class Codegen {
  std::vector<std::unique_ptr<ResolvedFunctionDecl>> resolvedSourceFile;
  std::map<const ResolvedDecl *, llvm::Value *> declarations;

  // FIXME: Keep these here?
  llvm::Value *retVal = nullptr;
  llvm::BasicBlock *retBlock = nullptr;

  llvm::LLVMContext context;
  llvm::IRBuilder<> builder;
  std::unique_ptr<llvm::Module> module;

  llvm::Type *generateType(Type type);
  llvm::Instruction::BinaryOps getOperatorKind(TokenKind op);

  llvm::Value *generateStmt(const ResolvedStmt &stmt);
  llvm::Value *generateIfStmt(const ResolvedIfStmt &stmt);
  llvm::Value *generateWhileStmt(const ResolvedWhileStmt &stmt);
  llvm::Value *generateDeclStmt(const ResolvedDeclStmt &stmt);
  llvm::Value *generateAssignment(const ResolvedAssignment &stmt);
  llvm::Value *generateReturnStmt(const ResolvedReturnStmt &stmt);

  llvm::Value *generateExpr(const ResolvedExpr &expr);
  llvm::Value *generateCallExpr(const ResolvedCallExpr &call);

  void generateConditionalOperator(const ResolvedExpr &op,
                                   llvm::BasicBlock *trueBlock,
                                   llvm::BasicBlock *falseBlock);

  llvm::Value *generateBinaryOperator(const ResolvedBinaryOperator &binop);
  llvm::Value *generateUnaryOperator(const ResolvedUnaryOperator &unary);

  llvm::Value *doubleToBool(llvm::Value *v);
  llvm::Value *boolToDouble(llvm::Value *v);

  llvm::Function *getCurrentFunction();
  llvm::AllocaInst *allocateStackVariable(llvm::Function *function,
                                          const std::string_view identifier);

  void generateBlock(const ResolvedBlock &block);
  void generateFunctionBody(const ResolvedFunctionDecl &functionDecl);
  void generateFunction(const ResolvedFunctionDecl &functionDecl);

  void generateBuiltinPrintBody();
  void generateMainWrapper();

public:
  explicit Codegen(
      std::vector<std::unique_ptr<ResolvedFunctionDecl>> resolvedSourceFile);

  std::unique_ptr<llvm::Module> generateIR();
};

#endif // A_COMPILER_CODEGEN_H
