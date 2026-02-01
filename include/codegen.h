#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <map>
#include <stack>

#include "res.h"

namespace yl {
class Codegen {
  const res::Context *resolvedTree;
  std::map<const res::Decl *, llvm::Value *> declarations;

  using InstantiationTy = std::vector<const res::Type *>;
  std::stack<InstantiationTy> instantiationContexts;
  std::vector<std::pair<const res::FunctionDecl *, InstantiationTy>>
      functionsToProcess;

  class InstantiationContextRAII {
    Codegen *codegen;

  public:
    explicit InstantiationContextRAII(Codegen *codegen,
                                      InstantiationTy instantiation)
        : codegen(codegen) {
      codegen->instantiationContexts.emplace(instantiation);
    }
    ~InstantiationContextRAII() { codegen->instantiationContexts.pop(); }
  };

  llvm::Value *retVal = nullptr;
  llvm::BasicBlock *retBB = nullptr;
  llvm::Instruction *allocaInsertPoint;

  llvm::LLVMContext context;
  llvm::IRBuilder<> builder;
  llvm::Module module;

  llvm::Type *generateType(const res::Type *type);
  llvm::FunctionType *generateFunctionType(const res::FunctionType *type);

  llvm::Value *generateStmt(const res::Stmt &stmt);
  llvm::Value *generateIfStmt(const res::IfStmt &stmt);
  llvm::Value *generateWhileStmt(const res::WhileStmt &stmt);
  llvm::Value *generateDeclStmt(const res::DeclStmt &stmt);
  llvm::Value *generateAssignment(const res::Assignment &stmt);
  llvm::Value *generateReturnStmt(const res::ReturnStmt &stmt);

  llvm::Value *generateExpr(const res::Expr &expr, bool keepPointer = false);
  llvm::Value *generateDeclRefExpr(const res::DeclRefExpr &dre,
                                   bool keepPointer = false);
  llvm::Value *generateCallExpr(const res::CallExpr &call);
  llvm::Value *generateBinaryOperator(const res::BinaryOperator &binop);
  llvm::Value *generateUnaryOperator(const res::UnaryOperator &unop);
  llvm::Value *generateMemberExpr(const res::MemberExpr &memberExpr,
                                  bool keepPointer = false);
  llvm::Value *generateTemporaryStruct(const res::StructInstantiationExpr &sie);

  void generateConditionalOperator(const res::Expr &op,
                                   llvm::BasicBlock *trueBlock,
                                   llvm::BasicBlock *falseBlock);

  llvm::Value *getDeclVal(const res::DeclRefExpr &dre);
  llvm::Value *loadValue(llvm::Value *val, llvm::Type *type);
  llvm::Value *storeValue(llvm::Value *val, llvm::Value *ptr, llvm::Type *type);
  llvm::Value *doubleToBool(llvm::Value *v);
  llvm::Value *boolToDouble(llvm::Value *v);
  void breakIntoBB(llvm::BasicBlock *targetBB);

  llvm::Function *getCurrentFunction();
  llvm::AllocaInst *allocateStackVariable(const std::string_view identifier,
                                          llvm::Type *type);
  llvm::AttributeList constructAttrList(const res::FunctionDecl *decl,
                                        const res::FunctionType *ty);

  void generateBlock(const res::Block &block);
  llvm::Function *generateFunctionDecl(const res::FunctionDecl &functionDecl,
                                       const res::FunctionType *functionType);
  void generateFunctionBody(const res::FunctionDecl &functionDecl);

  llvm::StructType *generateStruct(const res::StructType *resolvedStructTy);

  void generateBuiltinPrintlnBody(const res::FunctionDecl &println);
  void generateMainWrapper();

public:
  Codegen(const res::Context &resolvedCtx, std::string_view sourcePath);

  llvm::Module *generateIR();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H
