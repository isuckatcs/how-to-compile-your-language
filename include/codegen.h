#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H

#include <llvm/IR/IRBuilder.h>

#include <map>
#include <memory>
#include <vector>

#include "ast.h"

namespace yl {
class Codegen {
  std::vector<std::unique_ptr<ResolvedFunctionDecl>> resolvedTree;
  std::map<const ResolvedDecl *, llvm::Value *> declarations;

  llvm::Value *retVal = nullptr;
  llvm::BasicBlock *retBB = nullptr;
  llvm::Instruction *allocaInsertPoint;

  llvm::LLVMContext context;
  llvm::IRBuilder<> builder;
  std::unique_ptr<llvm::Module> module;

  llvm::Type *generateType(Type type);

  llvm::Value *generateStmt(const ResolvedStmt &stmt);
  llvm::Value *generateIfStmt(const ResolvedIfStmt &stmt);
  llvm::Value *generateWhileStmt(const ResolvedWhileStmt &stmt);
  llvm::Value *generateDeclStmt(const ResolvedDeclStmt &stmt);
  llvm::Value *generateAssignment(const ResolvedAssignment &stmt);
  llvm::Value *generateReturnStmt(const ResolvedReturnStmt &stmt);

  llvm::Value *generateExpr(const ResolvedExpr &expr);
  llvm::Value *generateCallExpr(const ResolvedCallExpr &call);
  llvm::Value *generateBinaryOperator(const ResolvedBinaryOperator &binop);
  llvm::Value *generateUnaryOperator(const ResolvedUnaryOperator &unop);

  void generateConditionalOperator(const ResolvedExpr &op,
                                   llvm::BasicBlock *trueBlock,
                                   llvm::BasicBlock *falseBlock);

  llvm::Value *doubleToBool(llvm::Value *v);
  llvm::Value *boolToDouble(llvm::Value *v);

  llvm::Function *getCurrentFunction();
  llvm::AllocaInst *allocateStackVariable(llvm::Function *function,
                                          const std::string_view identifier);

  void generateBlock(const ResolvedBlock &block);
  void generateFunctionBody(const ResolvedFunctionDecl &functionDecl);
  void generateFunctionDecl(const ResolvedFunctionDecl &functionDecl);

  void generateBuiltinPrintlnBody(const ResolvedFunctionDecl &prinln);
  void generateMainWrapper();

public:
  explicit Codegen(
      std::vector<std::unique_ptr<ResolvedFunctionDecl>> resolvedSourceFile,
      std::string_view sourcePath);

  std::unique_ptr<llvm::Module> generateIR();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H
