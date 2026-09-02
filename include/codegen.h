#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <map>
#include <set>

#include "mono.h"
#include "res.h"

namespace yl {
class Codegen {
  class UseSubstitutionRAII {
    Codegen *codegen;
    res::Substitution prevSub;

  public:
    UseSubstitutionRAII(Codegen *codegen, res::Substitution sub)
        : codegen(codegen),
          prevSub(std::move(codegen->currentSub)) {
      codegen->currentSub = std::move(sub);
    }
    ~UseSubstitutionRAII() { codegen->currentSub = std::move(prevSub); }
  };

  mono::Context *monoCtx;
  res::Context *resCtx;

  std::map<const res::Decl *, llvm::Value *> declarations;

  res::Substitution currentSub;
  const mono::Function *currentMonoFn = nullptr;

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

  res::Type *getMonoType(res::Type *type) const;

  llvm::Type *generateType(res::Type *monoType);
  llvm::FunctionType *generateFunctionType(res::FunctionType *type);

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
                  res::StructType *structTy,
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
  llvm::Value *allocateHeapVariable(res::Type *type);
  llvm::AttributeList constructAttrList(const res::FunctionType *ty,
                                        bool isVirtualCall = false);

  void generateBlock(const res::Block &block);

  llvm::Function *generateFunctionDecl(const mono::Function &fn);
  void generateFunctionBody(const mono::Function &fn);

  llvm::StructType *generateStructType(const res::StructType *structType);

  void generateBuiltinNullBody();
  void generateBuiltinNullMutBody();
  void generateBuiltinGCCollectBody();
  void generateBuiltinPrintlnBody(const res::FunctionDecl &println);
  void generateMainWrapper();

  std::vector<size_t> getHeapPtrOffsets(res::Type *type);
  llvm::Value *getTypeMetadata(res::Type *type);
  void createTmpGCRootIfNeeded(llvm::Value *val, const res::Expr *resVal);
  void markIfGCRoot(llvm::AllocaInst *alloca, res::Type *type);
  llvm::Function *getOrInsertGCAlloc();
  llvm::Function *getOrInsertGCMark();
  llvm::Function *getOrInsertGCSweep();

  bool isVirtualCall(const res::CallExpr &call);
  llvm::Value *lookupCalleeFromVtable(const res::CallExpr *call,
                                      llvm::Value *receiver);
  llvm::Value *generateVtable(std::string vtableId);

public:
  Codegen(mono::Context &monoCtx, std::string_view sourcePath);

  llvm::Module *generateIR();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H
