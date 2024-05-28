#ifndef A_COMPILER_CODEGEN_H
#define A_COMPILER_CODEGEN_H

#include "ast.h"

#include <map>
#include <memory>
#include <vector>

#include <llvm/IR/IRBuilder.h>

class Codegen {
  std::vector<std::unique_ptr<ResolvedFunctionDecl>> resolvedAST;

  std::map<const ResolvedDecl *, llvm::Value *> declarations;

  llvm::LLVMContext TheContext;
  llvm::IRBuilder<> Builder;
  llvm::Module TheModule;

  llvm::Value *GenerateNumberLiteral(const ResolvedNumberLiteral &num) {
    return llvm::ConstantFP::get(Builder.getDoubleTy(), num.value);
  }

  llvm::Value *GenerateCallExpr(const ResolvedCallExpr &call) {
    // FIXME: What is going on here?
    std::string id = call.callee->identifier;
    if (id == "main")
      id = "__main";
    if (id == "print")
      id = "printf";

    auto callee = TheModule.getFunction(id);

    std::vector<llvm::Value *> args;

    if (id == "printf") {
      args.emplace_back(Builder.CreateGlobalStringPtr("%f\n"));
    }

    for (auto &&arg : call.arguments)
      args.emplace_back(GenerateExpr(*arg));

    return Builder.CreateCall(callee, args);
  }

  llvm::Value *GenerateDeclRefExpr(const ResolvedDeclRefExpr &DRE) {
    return declarations[DRE.decl];
  }

  llvm::Value *GenerateExpr(const ResolvedExpr &expr) {
    if (auto *numLit = dynamic_cast<const ResolvedNumberLiteral *>(&expr))
      return GenerateNumberLiteral(*numLit);
    else if (auto *call = dynamic_cast<const ResolvedCallExpr *>(&expr))
      return GenerateCallExpr(*call);
    else
      return GenerateDeclRefExpr(
          dynamic_cast<const ResolvedDeclRefExpr &>(expr));
  }

  void GenerateBlock(const ResolvedBlock &block) {
    for (auto &&stmt : block.expressions) {
      GenerateExpr(*stmt);
    }
  }

  llvm::Type *GenerateType(Type type) {
    switch (type) {
    case Type::NUMBER:
      return Builder.getDoubleTy();
    case Type::VOID:
      return Builder.getVoidTy();
    }

    return nullptr;
  }

  llvm::Function *GenerateBuiltinPrint() {
    auto functionType = llvm::FunctionType::get(Builder.getInt32Ty(),
                                                {Builder.getInt8PtrTy()}, true);
    return llvm::Function::Create(functionType, llvm::Function::ExternalLinkage,
                                  "printf", TheModule);
  }

  void GenerateFunctionBody(const ResolvedFunctionDecl &functionDecl) {
    if (functionDecl.identifier == "print")
      return;

    auto *function = TheModule.getFunction(
        functionDecl.identifier == "main" ? "__main" : functionDecl.identifier);

    assert(function);

    auto *BB = llvm::BasicBlock::Create(TheContext, "", function);
    Builder.SetInsertPoint(BB);

    GenerateBlock(*functionDecl.body);

    Builder.CreateRetVoid();
  }

  llvm::Function *GenerateFunction(const ResolvedFunctionDecl &functionDecl) {
    if (functionDecl.identifier == "print")
      return GenerateBuiltinPrint();

    auto returnType = GenerateType(functionDecl.type);

    std::vector<llvm::Type *> paramTypes;
    for (auto &&param : functionDecl.params)
      paramTypes.emplace_back(GenerateType(param->type));

    auto functionType = llvm::FunctionType::get(returnType, paramTypes, false);
    auto function = llvm::Function::Create(
        functionType, llvm::Function::ExternalLinkage,
        functionDecl.identifier == "main" ? "__main" : functionDecl.identifier,
        TheModule);

    int idx = 0;
    for (auto &&arg : function->args()) {
      arg.setName(functionDecl.params[idx]->identifier);
      declarations[functionDecl.params[idx].get()] = &arg;
      ++idx;
    }

    return function;
  }

public:
  explicit Codegen(
      std::vector<std::unique_ptr<ResolvedFunctionDecl>> resolvedAST)
      : resolvedAST(std::move(resolvedAST)), Builder(TheContext),
        TheModule("<translation unit>", TheContext) {}

  void GenerateCode() {
    for (auto &&function : resolvedAST)
      GenerateFunction(*function);

    for (auto &&function : resolvedAST)
      GenerateFunctionBody(*function);

    // Generate main wrapper
    auto functionType =
        llvm::FunctionType::get(Builder.getInt32Ty(), {}, false);
    auto main = llvm::Function::Create(
        functionType, llvm::Function::ExternalLinkage, "main", TheModule);
    auto *BB = llvm::BasicBlock::Create(TheContext, "", main);
    Builder.SetInsertPoint(BB);

    Builder.CreateCall(TheModule.getFunction("__main"));
    Builder.CreateRet(
        llvm::ConstantInt::get(Builder.getInt32Ty(), llvm::APInt(32, 0, true)));

    std::error_code errorCode;
    llvm::raw_fd_ostream f{"tmp.ll", errorCode};
    TheModule.dump();

    TheModule.print(f, nullptr);
  }
};

#endif // A_COMPILER_CODEGEN_H
