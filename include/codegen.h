#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <map>
#include <queue>

#include "constexpr.h"
#include "res.h"

namespace yl {
class Codegen {
  using InstCtxTy = std::map<const res::TypeParamDecl *, res::Type *>;

  class EnterInstantiationRAII {
    Codegen *codegen;
    InstCtxTy instCtxSnapshot;

    void impl(std::vector<res::TypeParamDecl *> typeParams,
              std::vector<res::Type *> typeArgs) {
      for (size_t i = 0; i < typeParams.size(); ++i) {
        res::Type *type = typeArgs[i];
        if (const auto *typeParamTy = type->getAs<res::TypeParamType>())
          type = instCtxSnapshot[typeParamTy->decl];

        codegen->instCtx[typeParams[i]] = type;
      }
    }

  public:
    EnterInstantiationRAII(Codegen *codegen, const res::StructType *st)
        : codegen(codegen),
          instCtxSnapshot(codegen->instCtx) {
      impl(st->getDecl()->typeParams, st->getTypeArgs());
    }

    EnterInstantiationRAII(Codegen *codegen, const res::DeclRefExpr *dre)
        : codegen(codegen),
          instCtxSnapshot(codegen->instCtx) {
      if (dre->trait)
        impl(dre->trait->getDecl()->typeParams, dre->trait->getTypeArgs());

      if (dre->parentTy)
        if (auto *st = dre->parentTy->getAs<res::StructType>())
          impl(st->getDecl()->typeParams, st->getTypeArgs());

      impl(dre->decl->typeParams, dre->typeArgs);
    }

    ~EnterInstantiationRAII() { codegen->instCtx = instCtxSnapshot; }
  };

  struct PendingFunctionDescriptor {
    InstCtxTy instCtxCapture;
    std::string mangledName;
    const res::FunctionDecl *decl;
  };

  res::Context *resCtx;
  ConstExprValueStorage *constExprVals;
  std::map<const res::Decl *, llvm::Value *> declarations;

  std::queue<PendingFunctionDescriptor> pendingFunctions;
  InstCtxTy instCtx;

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

  llvm::Value *generateConstantValue(const ConstVal &constVal);

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
  llvm::AttributeList constructAttrList(const res::FunctionType *ty);

  void generateBlock(const res::Block &block);
  llvm::Function *generateFunctionDecl(const res::FunctionDecl &decl,
                                       const res::FunctionType *type,
                                       const res::Type *path,
                                       std::vector<res::Type *> typeArgs);
  void generateFunctionBody(const PendingFunctionDescriptor &fn);

  llvm::Type *generateStructType(const res::StructType *structTy);

  void generateBuiltinPrintlnBody(const res::FunctionDecl &println);
  void generateMainWrapper();

public:
  Codegen(res::Context &resolvedCtx,
          ConstExprValueStorage *constExprVals,
          std::string_view sourcePath);

  llvm::Module *generateIR();
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H
