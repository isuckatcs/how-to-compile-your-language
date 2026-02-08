#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <map>
#include <queue>
#include <stack>

#include "res.h"

namespace yl {
class Codegen {
  struct PendingFunctionDescriptor {
    std::vector<const res::Type *> typeArgs;
    std::string mangledName;
    const res::FunctionDecl *decl;
  };

  const res::Context *resolvedTree;
  std::map<const res::Decl *, llvm::Value *> declarations;

  std::queue<PendingFunctionDescriptor> pendingFunctions;
  std::stack<std::vector<llvm::Type *>> instantiations;

  class EnterInstantiationRAII {
    Codegen *codegen;

  public:
    EnterInstantiationRAII(Codegen *codegen,
                           std::vector<const res::Type *> typeArgs)
        : codegen(codegen) {
      std::vector<llvm::Type *> instantiation;
      for (auto &&arg : typeArgs)
        instantiation.emplace_back(codegen->generateType(arg));

      codegen->instantiations.emplace(std::move(instantiation));
    }
    ~EnterInstantiationRAII() { codegen->instantiations.pop(); }
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

  llvm::Value *generateExpr(const res::Expr &expr);
  llvm::Value *generateDeclRefExpr(const res::DeclRefExpr &dre);
  llvm::Value *generateCallExpr(const res::CallExpr &call);
  llvm::Value *generateBinaryOperator(const res::BinaryOperator &binop);
  llvm::Value *generateUnaryOperator(const res::UnaryOperator &unop);
  llvm::Value *generateMemberExpr(const res::MemberExpr &memberExpr);
  llvm::Value *generateTemporaryStruct(const res::StructInstantiationExpr &sie);

  void generateConditionalOperator(const res::Expr &op,
                                   llvm::BasicBlock *trueBlock,
                                   llvm::BasicBlock *falseBlock);

  llvm::Value *generateExprAndLoadValue(const res::Expr &expr);
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
  llvm::Function *
  generateFunctionDecl(const res::FunctionDecl &decl,
                       const res::FunctionType *type,
                       const std::vector<const res::Type *> &typeArgs);
  void generateFunctionBody(const PendingFunctionDescriptor &fn);

  llvm::Type *generateStructType(const res::StructType *structTy);

  void generateBuiltinPrintlnBody(const res::FunctionDecl &println);
  void generateMainWrapper();

public:
  Codegen(const res::Context &resolvedCtx, std::string_view sourcePath);

  llvm::Module *generateIR();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H
