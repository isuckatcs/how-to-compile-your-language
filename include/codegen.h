#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H

#include <llvm/IR/IRBuilder.h>

#include <map>
#include <memory>
#include <vector>

#include "ast.h"

namespace yl {
class Codegen {
  std::vector<std::unique_ptr<ResolvedDecl>> resolvedTree;
  std::map<const ResolvedDecl *, llvm::Value *> declarations;

  llvm::Value *retVal = nullptr;
  llvm::BasicBlock *retBB = nullptr;
  llvm::Instruction *allocaInsertPoint;

  llvm::LLVMContext context;
  llvm::IRBuilder<> builder;
  llvm::Module module;

  llvm::Type *generateType(Type type);

  llvm::Value *generateStmt(const ResolvedStmt &stmt);
  llvm::Value *generateIfStmt(const ResolvedIfStmt &stmt);
  llvm::Value *generateWhileStmt(const ResolvedWhileStmt &stmt);
  llvm::Value *generateDeclStmt(const ResolvedDeclStmt &stmt);
  llvm::Value *generateAssignment(const ResolvedAssignment &stmt);
  llvm::Value *generateReturnStmt(const ResolvedReturnStmt &stmt);

  llvm::Value *generateExpr(const ResolvedExpr &expr, bool keepPointer = false);
  llvm::Value *generateDeclRefExpr(const ResolvedDeclRefExpr &dre,
                                   bool keepPointer = false);
  llvm::Value *generateCallExpr(const ResolvedCallExpr &call);
  llvm::Value *generateBinaryOperator(const ResolvedBinaryOperator &binop);
  llvm::Value *generateUnaryOperator(const ResolvedUnaryOperator &unop);
  llvm::Value *generateMemberExpr(const ResolvedMemberExpr &memberExpr,
                                  bool keepPointer = false);
  llvm::Value *
  generateTemporaryStruct(const ResolvedStructInstantiationExpr &sie);

  void generateConditionalOperator(const ResolvedExpr &op,
                                   llvm::BasicBlock *trueBlock,
                                   llvm::BasicBlock *falseBlock);

  llvm::Value *loadValue(llvm::Value *val, const Type &type);
  llvm::Value *storeValue(llvm::Value *val, llvm::Value *ptr, const Type &type);
  llvm::Value *doubleToBool(llvm::Value *v);
  llvm::Value *boolToDouble(llvm::Value *v);

  llvm::Function *getCurrentFunction();
  llvm::AllocaInst *allocateStackVariable(const std::string_view identifier,
                                          const Type &type);
  llvm::AttributeList constructAttrList(const ResolvedFunctionDecl *fn);

  void generateBlock(const ResolvedBlock &block);
  void generateFunctionBody(const ResolvedFunctionDecl &functionDecl);
  void generateFunctionDecl(const ResolvedFunctionDecl &functionDecl);
  void generateStructDecl(const ResolvedStructDecl &structDecl);
  void generateStructDefinition(const ResolvedStructDecl &structDecl);

  void generateBuiltinPrintlnBody(const ResolvedFunctionDecl &println);
  void generateMainWrapper();

public:
  Codegen(std::vector<std::unique_ptr<ResolvedDecl>> resolvedTree,
          std::string_view sourcePath);

  llvm::Module *generateIR();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H
