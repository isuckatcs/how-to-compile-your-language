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
  struct InstCtxTy : std::map<const res::TypeParamDecl *, res::Type *> {
    const res::Type *getInstantiatedType(const res::Type *type) {
      if (const auto *typeParamTy = type->getAs<res::TypeParamType>()) {
        auto it = find(typeParamTy->decl);
        return it == end() ? nullptr : it->second;
      }

      return type;
    }
  };

  class EnterInstantiationRAII {
    Codegen *codegen;
    InstCtxTy instCtxSnapshot;

    void impl(std::vector<res::TypeParamDecl *> typeParams,
              std::vector<res::Type *> typeArgs) {
      for (size_t i = 0; i < typeParams.size(); ++i) {
        res::Type *type = typeArgs[i];
        if (const auto *typeParamTy = type->getAs<res::TypeParamType>()) {
          type = instCtxSnapshot[typeParamTy->decl];

          if (type == nullptr) {
            int x = 0;
          }
        }

        codegen->instCtx[typeParams[i]] = type;
      }
    }

  public:
    EnterInstantiationRAII(Codegen *codegen, const res::TraitType *t)
        : codegen(codegen),
          instCtxSnapshot(codegen->instCtx) {
      if (t)
        impl(t->getDecl()->typeParams, t->getTypeArgs());
    }

    EnterInstantiationRAII(Codegen *codegen, const res::StructType *st)
        : codegen(codegen),
          instCtxSnapshot(codegen->instCtx) {
      if (st)
        impl(st->getDecl()->typeParams, st->getTypeArgs());
    }

    // FIXME: remove/rework this utility
    EnterInstantiationRAII(Codegen *codegen, const res::Substitution &sub)
        : codegen(codegen),
          instCtxSnapshot(codegen->instCtx) {
      std::vector<res::TypeParamDecl *> typeParams;
      std::vector<res::Type *> typeArgs;

      for (auto &&[from, to] : sub) {
        typeParams.emplace_back(from->getAs<res::TypeParamType>()->decl);
        typeArgs.emplace_back(to);
      }

      impl(typeParams, typeArgs);
    }

    ~EnterInstantiationRAII() { codegen->instCtx = instCtxSnapshot; }
  };

  struct PendingFunctionDescriptor {
    InstCtxTy instCtxCapture;
    std::string mangledName;
    const res::FunctionDecl *decl;
  };

  res::TypeManager *typeMgr;
  const res::Context *resCtx;

  std::map<const res::Decl *, llvm::Value *> declarations;

  std::queue<PendingFunctionDescriptor> pendingFunctions;
  InstCtxTy instCtx;

  std::set<llvm::AllocaInst *> permanentRoots;
  std::map<llvm::AllocaInst *, bool> temporaryRoots;

  llvm::Value *retVal = nullptr;
  llvm::BasicBlock *retBB = nullptr;

  llvm::Instruction *allocaInsertPoint;
  llvm::Instruction *rootMarkInsertPoint;

  llvm::LLVMContext context;
  llvm::IRBuilder<> builder;
  llvm::Module module;
  const llvm::DataLayout *dl;

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
  llvm::Value *generateStructInstExpr(const res::StructInstantiationExpr &sie);
  llvm::Value *generateGCExpr(const res::GCExpr &gcExpr);
  llvm::Value *generateLambdaExpr(const res::LambdaExpr &lambdaExpr);
  llvm::Value *materializeTemporary(const res::MaterializeTemporaryExpr &mte);
  llvm::Value *generateImplicitBorrow(const res::ImplicitBorrowExpr &borrow);
  llvm::Value *generatePtrToBorrow(const res::ImplicitPtrToBorrowDecay &decay);
  llvm::Value *generateTraitObjectPromo(const res::TraitObjectPromoExpr &promo);

  llvm::Value *
  constructStruct(llvm::Value *storage,
                  const res::StructType *structTy,
                  std::map<const res::FieldDecl *, llvm::Value *> &fieldInits);

  llvm::Value *generateConstantValue(const res::ConstVal &constVal);
  bool isImplOf(const res::ExtensionDecl *impl, const res::TraitType *trait);

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
  llvm::Function *generateFunctionDecl(const res::FunctionDecl &decl,
                                       const res::FunctionType *type,
                                       const res::Type *path,
                                       std::vector<res::Type *> typeArgs);
  void generateFunctionBody(const PendingFunctionDescriptor &fn);

  llvm::Type *generateStructType(const res::StructType *structTy);

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
  llvm::Value *getVtable(const res::TraitType *trait, const res::Type *type);

public:
  Codegen(const res::Context &resolvedCtx,
          const res::TypeManager &typeMgr,
          std::string_view sourcePath);

  llvm::Module *generateIR();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H
