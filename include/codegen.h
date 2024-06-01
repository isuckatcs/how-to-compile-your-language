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

  llvm::LLVMContext Context;
  llvm::IRBuilder<> Builder;
  llvm::Module Module;

  llvm::Type *generateType(Type type);
  llvm::Instruction::BinaryOps getOperatorKind(TokenKind op);

  llvm::Value *generateExpr(const ResolvedExpr &expr);
  llvm::Value *generateCallExpr(const ResolvedCallExpr &call);
  llvm::Value *generateBinaryOperator(const ResolvedBinaryOperator &binop);

  void generateBlock(const ResolvedBlock &block);
  void generateFunctionBody(const ResolvedFunctionDecl &functionDecl);
  void generateFunction(const ResolvedFunctionDecl &functionDecl);

  void generateBuiltinPrintBody();
  void generateMainWrapper();

public:
  explicit Codegen(
      std::vector<std::unique_ptr<ResolvedFunctionDecl>> resolvedSourceFile)
      : resolvedSourceFile(std::move(resolvedSourceFile)), Builder(Context),
        Module("<translation_unit>", Context) {}

  void generateIR(std::string_view filePath);
};

#endif // A_COMPILER_CODEGEN_H
