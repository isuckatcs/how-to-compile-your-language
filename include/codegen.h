#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_CODEGEN_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include <map>
#include <queue>

#include "res.h"

namespace yl {
class Codegen {
  using InstCtxTy = std::map<const res::TypeParamDecl *, const res::Type *>;

  class EnterInstantiationRAII {
    Codegen *codegen;
    InstCtxTy instCtxSnapshot;

    void impl(std::vector<const res::TypeParamDecl *> typeParams,
              std::vector<const res::Type *> typeArgs) {
      for (size_t i = 0; i < typeParams.size(); ++i) {
        const res::Type *type = typeArgs[i];
        if (const auto *typeParamTy = type->getAs<res::TypeParamType>())
          type = instCtxSnapshot[typeParamTy->decl];

        codegen->instCtx[typeParams[i]] = type;
      }
    }

  public:
    EnterInstantiationRAII(Codegen *codegen, const res::StructType *st)
        : codegen(codegen),
          instCtxSnapshot(codegen->instCtx) {
      impl(st->getDecl()->getTypeParams(), st->getTypeArgs());
    }

    EnterInstantiationRAII(Codegen *codegen, const res::DeclRefExpr *dre)
        : codegen(codegen),
          instCtxSnapshot(codegen->instCtx) {
      for (auto &&fragment : dre->getPath())
        if (const auto *st = fragment->getAs<res::StructType>())
          impl(st->getDecl()->getTypeParams(), st->getTypeArgs());
      impl(dre->decl->getTypeParams(), dre->getTypeArgs());
    }

    ~EnterInstantiationRAII() { codegen->instCtx = instCtxSnapshot; }
  };

  struct PendingFunctionDescriptor {
    InstCtxTy instCtxCapture;
    std::string mangledName;
    const res::FunctionDecl *decl;
  };

  const res::Context *resCtx;
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
  llvm::AttributeList constructAttrList(const res::FunctionType *ty);

  void generateBlock(const res::Block &block);
  llvm::Function *
  generateFunctionDecl(const res::FunctionDecl &decl,
                       const res::FunctionType *type,
                       const std::vector<const res::Type *> &path,
                       std::vector<const res::Type *> typeArgs);
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
