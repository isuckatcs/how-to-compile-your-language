#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <map>
#include <queue>
#include <set>

#include "res.h"

namespace yl {
class Codegen {
  class EnterMonoCtxRAII {
    Codegen *codegen;
    res::Substitution prevMonoCtx;

  public:
    EnterMonoCtxRAII(Codegen *codegen, res::Substitution sub)
        : codegen(codegen),
          prevMonoCtx(std::move(codegen->monoCtx)) {
      codegen->monoCtx = std::move(sub);
    }
    ~EnterMonoCtxRAII() { codegen->monoCtx = std::move(prevMonoCtx); }
  };

  struct PendingFunctionDescriptor {
    res::Substitution monoCtx;
    std::string mangledName;
    const res::FunctionDecl *decl;
  };

  res::TypeManager *typeMgr;
  const res::Context *resCtx;

  std::map<const res::Decl *, llvm::Value *> declarations;

  std::queue<PendingFunctionDescriptor> pendingFunctions;
  res::Substitution monoCtx;

  std::set<llvm::AllocaInst *> permanentRoots;
  std::vector<std::pair<llvm::AllocaInst *, bool>> temporaryRoots;

  llvm::Value *retVal = nullptr;
  llvm::BasicBlock *retBB = nullptr;

  llvm::Instruction *allocaInsertPoint;
  llvm::Instruction *rootMarkInsertPoint;

  llvm::LLVMContext context;
  llvm::IRBuilder<> builder;
  llvm::Module module;
  const llvm::DataLayout *dl;

  res::Type *getMonoType(const res::Type *type) const;

  llvm::Type *generateType(const res::Type *monoType);
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
  llvm::Value *generateStructInstExpr(const res::StructInstantiationExpr &sie);
  llvm::Value *generateGCExpr(const res::GCExpr &gcExpr);
  llvm::Value *generateLambdaExpr(const res::LambdaExpr &lambdaExpr);
  llvm::Value *materializeTemporary(const res::MaterializeTemporaryExpr &mte);
  llvm::Value *generateImplicitAsRef(const res::ImplicitAsRefExpr &ref);
  llvm::Value *generatePtrToRef(const res::ImplicitPtrToRefDecay &decay);
  llvm::Value *generateTraitObjectPromo(const res::TraitObjectPromoExpr &promo);

  llvm::Value *
  constructStruct(llvm::Value *storage,
                  const res::StructType *structTy,
                  std::map<const res::FieldDecl *, llvm::Value *> &fieldInits);

  llvm::Value *generateConstantValue(const res::ConstVal &constVal);

  void generateConditionalOperator(const res::Expr &op,
                                   llvm::BasicBlock *trueBlock,
                                   llvm::BasicBlock *falseBlock);

  llvm::Value *generateExprAndLoadValue(const res::Expr &expr);
  llvm::Value *loadValue(llvm::Value *val, llvm::Type *type);
  llvm::Value *storeValue(llvm::Value *val, llvm::Value *ptr, llvm::Type *type);
  void breakIntoBB(llvm::BasicBlock *targetBB);

  llvm::Function *getCurrentFunction();
  llvm::AllocaInst *allocateStackVariable(const std::string_view identifier,
                                          llvm::Type *type);
  llvm::Value *allocateHeapVariable(const res::Type *type);
  llvm::AttributeList constructAttrList(const res::FunctionType *ty,
                                        bool isVirtualCall = false);

  void generateBlock(const res::Block &block);

  llvm::Function *generateExtensionFnDecl(res::TraitType *trait,
                                          const res::FunctionDecl *fn);
  llvm::Function *generateFunctionDecl(const res::FunctionDecl &fn);
  void generateFunctionBody(const PendingFunctionDescriptor &fn);

  llvm::StructType *generateStructType(const res::StructType *structType);

  void generateBuiltinGCCollectBody(const res::FunctionDecl &gcCollect);
  void generateBuiltinPrintlnBody(const res::FunctionDecl &println);
  void generateMainWrapper();

  std::vector<size_t> getHeapPtrOffsets(const res::Type *type);
  llvm::Value *getTypeMetadata(const res::Type *type);
  void createTmpGCRootIfNeeded(llvm::Value *val, const res::Expr *resVal);
  void markIfGCRoot(llvm::AllocaInst *alloca, const res::Type *type);
  llvm::Function *getOrInsertGCAlloc();
  llvm::Function *getOrInsertGCMark();
  llvm::Function *getOrInsertGCSweep();

  llvm::Value *lookupCalleeFromVtable(const res::CallExpr *call,
                                      llvm::Value *receiver);
  llvm::Value *getVtable(res::TraitType *trait);

public:
  Codegen(const res::Context &resolvedCtx,
          const res::TypeManager &typeMgr,
          std::string_view sourcePath);

  llvm::Module *generateIR();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H
