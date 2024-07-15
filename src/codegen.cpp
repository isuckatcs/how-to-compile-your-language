#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/Host.h>

#include "codegen.h"

namespace yl {
Codegen::Codegen(
    std::vector<std::unique_ptr<ResolvedFunctionDecl>> resolvedSourceFile,
    std::string_view sourcePath)
    : resolvedTree(std::move(resolvedSourceFile)), builder(context),
      module(std::make_unique<llvm::Module>("<translation_unit>", context)) {
  module->setSourceFileName(sourcePath);
  module->setTargetTriple(llvm::sys::getDefaultTargetTriple());
}

llvm::Type *Codegen::generateType(Type type) {
  if (type.kind == Type::Kind::Number)
    return builder.getDoubleTy();

  return builder.getVoidTy();
}

llvm::Value *Codegen::generateStmt(const ResolvedStmt &stmt) {
  if (auto *expr = dynamic_cast<const ResolvedExpr *>(&stmt))
    return generateExpr(*expr);

  if (auto *returnStmt = dynamic_cast<const ResolvedReturnStmt *>(&stmt))
    return generateReturnStmt(*returnStmt);

  llvm_unreachable("unknown statement");
  return nullptr;
}

llvm::Value *Codegen::generateReturnStmt(const ResolvedReturnStmt &stmt) {
  if (stmt.expr)
    builder.CreateStore(generateExpr(*stmt.expr), retVal);

  assert(retBB && "function with return stmt doesn't have a return block");
  return builder.CreateBr(retBB);
}

llvm::Value *Codegen::generateExpr(const ResolvedExpr &expr) {
  if (auto *number = dynamic_cast<const ResolvedNumberLiteral *>(&expr))
    return llvm::ConstantFP::get(builder.getDoubleTy(), number->value);

  if (auto *dre = dynamic_cast<const ResolvedDeclRefExpr *>(&expr))
    return builder.CreateLoad(builder.getDoubleTy(), declarations[dre->decl]);

  if (auto *call = dynamic_cast<const ResolvedCallExpr *>(&expr))
    return generateCallExpr(*call);

  if (auto *grouping = dynamic_cast<const ResolvedGroupingExpr *>(&expr))
    return generateExpr(*grouping->expr);

  if (auto *binop = dynamic_cast<const ResolvedBinaryOperator *>(&expr))
    return generateBinaryOperator(*binop);

  if (auto *unop = dynamic_cast<const ResolvedUnaryOperator *>(&expr))
    return generateUnaryOperator(*unop);

  llvm_unreachable("unknown expression encountered");
  return nullptr;
}

llvm::Value *Codegen::generateCallExpr(const ResolvedCallExpr &call) {
  llvm::Function *callee = module->getFunction(call.callee->identifier);

  std::vector<llvm::Value *> args;
  for (auto &&arg : call.arguments)
    args.emplace_back(generateExpr(*arg));

  return builder.CreateCall(callee, args);
}

llvm::Value *Codegen::generateUnaryOperator(const ResolvedUnaryOperator &unop) {
  llvm::Value *rhs = generateExpr(*unop.rhs);

  if (unop.op == TokenKind::Excl)
    return boolToDouble(builder.CreateNot(doubleToBool(rhs)));

  if (unop.op == TokenKind::Minus)
    return builder.CreateFNeg(rhs);

  llvm_unreachable("unknown unary op");
  return nullptr;
}

void Codegen::generateConditionalOperator(const ResolvedExpr &op,
                                          llvm::BasicBlock *trueBB,
                                          llvm::BasicBlock *falseBB) {
  const auto *binop = dynamic_cast<const ResolvedBinaryOperator *>(&op);

  if (binop && binop->op == TokenKind::PipePipe) {
    llvm::BasicBlock *nextBB =
        llvm::BasicBlock::Create(context, "or.lhs.false", trueBB->getParent());
    generateConditionalOperator(*binop->lhs, trueBB, nextBB);

    builder.SetInsertPoint(nextBB);
    generateConditionalOperator(*binop->rhs, trueBB, falseBB);
    return;
  }

  if (binop && binop->op == TokenKind::AmpAmp) {
    llvm::BasicBlock *nextBB =
        llvm::BasicBlock::Create(context, "and.lhs.true", trueBB->getParent());
    generateConditionalOperator(*binop->lhs, nextBB, falseBB);

    builder.SetInsertPoint(nextBB);
    generateConditionalOperator(*binop->rhs, trueBB, falseBB);
    return;
  }

  llvm::Value *val = doubleToBool(generateExpr(op));
  builder.CreateCondBr(val, trueBB, falseBB);
  return;
};

llvm::Value *
Codegen::generateBinaryOperator(const ResolvedBinaryOperator &binop) {
  TokenKind op = binop.op;

  if (op == TokenKind::AmpAmp || op == TokenKind::PipePipe) {
    llvm::Function *function = getCurrentFunction();
    bool isOr = op == TokenKind::PipePipe;

    auto *rhsTag = isOr ? "or.rhs" : "and.rhs";
    auto *mergeTag = isOr ? "or.merge" : "and.merge";

    auto *rhsBB = llvm::BasicBlock::Create(context, rhsTag, function);
    auto *mergeBB = llvm::BasicBlock::Create(context, mergeTag, function);

    llvm::BasicBlock *trueBB = isOr ? mergeBB : rhsBB;
    llvm::BasicBlock *falseBB = isOr ? rhsBB : mergeBB;
    generateConditionalOperator(*binop.lhs, trueBB, falseBB);

    builder.SetInsertPoint(rhsBB);
    llvm::Value *rhs = doubleToBool(generateExpr(*binop.rhs));
    builder.CreateBr(mergeBB);

    rhsBB = builder.GetInsertBlock();
    builder.SetInsertPoint(mergeBB);
    llvm::PHINode *phi = builder.CreatePHI(builder.getInt1Ty(), 2);

    for (auto it = pred_begin(mergeBB); it != pred_end(mergeBB); ++it) {
      if (*it == rhsBB)
        phi->addIncoming(rhs, rhsBB);
      else
        phi->addIncoming(builder.getInt1(isOr), *it);
    }

    return boolToDouble(phi);
  }

  llvm::Value *lhs = generateExpr(*binop.lhs);
  llvm::Value *rhs = generateExpr(*binop.rhs);

  if (op == TokenKind::Lt)
    return boolToDouble(builder.CreateFCmpOLT(lhs, rhs));

  if (op == TokenKind::Gt)
    return boolToDouble(builder.CreateFCmpOGT(lhs, rhs));

  if (op == TokenKind::EqualEqual)
    return boolToDouble(builder.CreateFCmpOEQ(lhs, rhs));

  if (op == TokenKind::Plus)
    return builder.CreateFAdd(lhs, rhs);

  if (op == TokenKind::Minus)
    return builder.CreateFSub(lhs, rhs);

  if (op == TokenKind::Asterisk)
    return builder.CreateFMul(lhs, rhs);

  if (op == TokenKind::Slash)
    return builder.CreateFDiv(lhs, rhs);

  llvm_unreachable("unexpected binary operator");
  return nullptr;
}

llvm::Value *Codegen::doubleToBool(llvm::Value *v) {
  return builder.CreateFCmpONE(
      v, llvm::ConstantFP::get(builder.getDoubleTy(), 0.0), "to.bool");
}

llvm::Value *Codegen::boolToDouble(llvm::Value *v) {
  return builder.CreateUIToFP(v, builder.getDoubleTy(), "to.double");
}

llvm::Function *Codegen::getCurrentFunction() {
  return builder.GetInsertBlock()->getParent();
};

llvm::AllocaInst *
Codegen::allocateStackVariable(llvm::Function *function,
                               const std::string_view identifier) {
  llvm::IRBuilder<> tmpBuilder(context);
  tmpBuilder.SetInsertPoint(allocaInsertPoint);

  return tmpBuilder.CreateAlloca(tmpBuilder.getDoubleTy(), nullptr, identifier);
}

void Codegen::generateBlock(const ResolvedBlock &block) {
  for (auto &&stmt : block.statements) {
    generateStmt(*stmt);

    // After a return statement we clear the insertion point, so that
    // no other instructions are inserted into the current block and break.
    // The break ensures that no other instruction is generated that will be
    // inserted regardless of there is no insertion point and crash (e.g.:
    // CreateStore, CreateLoad).
    if (dynamic_cast<const ResolvedReturnStmt *>(stmt.get())) {
      builder.ClearInsertionPoint();
      break;
    }
  }
}

void Codegen::generateFunctionBody(const ResolvedFunctionDecl &functionDecl) {
  auto *function = module->getFunction(functionDecl.identifier);

  auto *entryBB = llvm::BasicBlock::Create(context, "entry", function);
  builder.SetInsertPoint(entryBB);

  // Note: llvm:Instruction has a protected destructor.
  llvm::Value *undef = llvm::UndefValue::get(builder.getInt32Ty());
  allocaInsertPoint = new llvm::BitCastInst(undef, undef->getType(),
                                            "alloca.placeholder", entryBB);

  bool isVoid = functionDecl.type.kind == Type::Kind::Void;
  if (!isVoid)
    retVal = allocateStackVariable(function, "retval");
  retBB = llvm::BasicBlock::Create(context, "return");

  int idx = 0;
  for (auto &&arg : function->args()) {
    const auto *paramDecl = functionDecl.params[idx].get();
    arg.setName(paramDecl->identifier);

    llvm::Value *var = allocateStackVariable(function, paramDecl->identifier);
    builder.CreateStore(&arg, var);

    declarations[paramDecl] = var;
    ++idx;
  }

  if (functionDecl.identifier == "println")
    generateBuiltinPrintlnBody(functionDecl);
  else
    generateBlock(*functionDecl.body);

  if (retBB->hasNPredecessorsOrMore(1)) {
    builder.CreateBr(retBB);
    retBB->insertInto(function);
    builder.SetInsertPoint(retBB);
  }

  allocaInsertPoint->eraseFromParent();
  allocaInsertPoint = nullptr;

  if (isVoid) {
    builder.CreateRetVoid();
    return;
  }

  builder.CreateRet(builder.CreateLoad(builder.getDoubleTy(), retVal));
}

void Codegen::generateBuiltinPrintlnBody(const ResolvedFunctionDecl &println) {
  auto *type = llvm::FunctionType::get(builder.getInt32Ty(),
                                       {builder.getInt8PtrTy()}, true);
  auto *printf = llvm::Function::Create(type, llvm::Function::ExternalLinkage,
                                        "printf", *module);
  auto *format = builder.CreateGlobalStringPtr("%.15g\n");

  llvm::Value *param = builder.CreateLoad(
      builder.getDoubleTy(), declarations[println.params[0].get()]);

  builder.CreateCall(printf, {format, param});
}

void Codegen::generateMainWrapper() {
  auto *builtinMain = module->getFunction("main");
  builtinMain->setName("__builtin_main");

  auto *main = llvm::Function::Create(
      llvm::FunctionType::get(builder.getInt32Ty(), {}, false),
      llvm::Function::ExternalLinkage, "main", *module);

  auto *entry = llvm::BasicBlock::Create(context, "entry", main);
  builder.SetInsertPoint(entry);

  builder.CreateCall(builtinMain);
  builder.CreateRet(llvm::ConstantInt::getSigned(builder.getInt32Ty(), 0));
}

void Codegen::generateFunctionDecl(const ResolvedFunctionDecl &functionDecl) {
  auto *retType = generateType(functionDecl.type);

  std::vector<llvm::Type *> paramTypes;
  for (auto &&param : functionDecl.params)
    paramTypes.emplace_back(generateType(param->type));

  auto *type = llvm::FunctionType::get(retType, paramTypes, false);

  llvm::Function::Create(type, llvm::Function::ExternalLinkage,
                         functionDecl.identifier, *module);
}

std::unique_ptr<llvm::Module> Codegen::generateIR() {
  for (auto &&function : resolvedTree)
    generateFunctionDecl(*function);

  for (auto &&function : resolvedTree)
    generateFunctionBody(*function);

  generateMainWrapper();

  return std::move(module);
}
} // namespace yl
