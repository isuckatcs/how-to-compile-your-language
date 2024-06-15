#include "codegen.h"

#include <llvm/Support/Host.h>

Codegen::Codegen(
    std::vector<std::unique_ptr<ResolvedFunctionDecl>> resolvedSourceFile)
    : resolvedSourceFile(std::move(resolvedSourceFile)), Builder(Context),
      Module("<translation_unit>", Context) {
  Module.setTargetTriple(llvm::sys::getDefaultTargetTriple());
}

llvm::Type *Codegen::generateType(Type type) {
  if (type == Type::NUMBER)
    return Builder.getDoubleTy();

  return Builder.getVoidTy();
}

llvm::Instruction::BinaryOps Codegen::getOperatorKind(TokenKind op) {
  if (op == TokenKind::plus)
    return llvm::BinaryOperator::FAdd;
  if (op == TokenKind::minus)
    return llvm::BinaryOperator::FSub;
  if (op == TokenKind::asterisk)
    return llvm::BinaryOperator::FMul;
  if (op == TokenKind::slash)
    return llvm::BinaryOperator::FDiv;

  llvm_unreachable("unknown operator");
}

llvm::Value *Codegen::generateStmt(const ResolvedStmt &stmt) {
  if (auto *expr = dynamic_cast<const ResolvedExpr *>(&stmt))
    return generateExpr(*expr);

  if (auto *ifStmt = dynamic_cast<const ResolvedIfStmt *>(&stmt))
    return generateIfStmt(*ifStmt);

  llvm_unreachable("unknown statement");
}

llvm::Value *Codegen::generateIfStmt(const ResolvedIfStmt &stmt) {
  llvm::Function *parentFunction = Builder.GetInsertBlock()->getParent();

  llvm::Value *cond = generateExpr(*stmt.condition);
  llvm::BasicBlock *thenBB =
      llvm::BasicBlock::Create(Context, "then", parentFunction);
  llvm::BasicBlock *elseBB = nullptr;
  llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(Context, "merge");

  bool hasElseBranch = stmt.falseBlock || stmt.falseBranch;
  if (hasElseBranch)
    elseBB = llvm::BasicBlock::Create(Context, "else");

  Builder.CreateCondBr(doubleToBool(cond), thenBB,
                       hasElseBranch ? elseBB : mergeBB);

  Builder.SetInsertPoint(thenBB);
  generateBlock(*stmt.trueBlock);
  Builder.CreateBr(mergeBB);

  if (hasElseBranch) {
    elseBB->insertInto(parentFunction);
    Builder.SetInsertPoint(elseBB);

    if (stmt.falseBlock)
      generateBlock(*stmt.falseBlock);
    else
      generateIfStmt(*stmt.falseBranch);

    Builder.CreateBr(mergeBB);
  }

  mergeBB->insertInto(parentFunction);
  Builder.SetInsertPoint(mergeBB);
  return nullptr;
}

llvm::Value *Codegen::generateExpr(const ResolvedExpr &expr) {
  if (std::optional<double> val = expr.getConstantValue())
    return llvm::ConstantFP::get(Builder.getDoubleTy(), *val);

  if (auto *numLit = dynamic_cast<const ResolvedNumberLiteral *>(&expr))
    return llvm::ConstantFP::get(Builder.getDoubleTy(), numLit->value);

  if (auto *declRefExpr = dynamic_cast<const ResolvedDeclRefExpr *>(&expr))
    return declarations[declRefExpr->decl];

  if (auto *call = dynamic_cast<const ResolvedCallExpr *>(&expr))
    return generateCallExpr(*call);

  if (auto *grouping = dynamic_cast<const ResolvedGroupingExpr *>(&expr))
    return generateExpr(*grouping->expr);

  if (auto *binop = dynamic_cast<const ResolvedBinaryOperator *>(&expr))
    return generateBinaryOperator(*binop);

  if (auto *unop = dynamic_cast<const ResolvedUnaryOperator *>(&expr))
    return generateUnaryOperator(*unop);

  llvm_unreachable("unknown expression encountered");
}

llvm::Value *Codegen::generateCallExpr(const ResolvedCallExpr &call) {
  llvm::Function *callee = Module.getFunction(call.callee->identifier);

  std::vector<llvm::Value *> args;
  for (auto &&arg : call.arguments)
    args.emplace_back(generateExpr(*arg));

  return Builder.CreateCall(callee, args);
}

// FIXME: Refactor!!!
llvm::Value *
Codegen::generateUnaryOperator(const ResolvedUnaryOperator &unary) {
  llvm::Value *RHS = generateExpr(*unary.RHS);

  if (unary.op == TokenKind::excl)
    return boolToDouble(Builder.CreateNot(doubleToBool(RHS)));

  llvm_unreachable("unknown unary op");
}

llvm::Value *
Codegen::generateBinaryOperator(const ResolvedBinaryOperator &binop) {
  llvm::Value *LHS = generateExpr(*binop.LHS);
  llvm::Value *RHS = generateExpr(*binop.RHS);

  // FIXME: Refactor this!!!
  if (binop.op == TokenKind::lt)
    return boolToDouble(Builder.CreateFCmpOLT(LHS, RHS));
  if (binop.op == TokenKind::gt)
    return boolToDouble(Builder.CreateFCmpOGT(LHS, RHS));
  if (binop.op == TokenKind::equalequal)
    return boolToDouble(Builder.CreateFCmpOEQ(LHS, RHS));
  if (binop.op == TokenKind::ampamp) {
    return boolToDouble(
        Builder.CreateLogicalAnd(doubleToBool(LHS), doubleToBool(RHS)));
  }
  if (binop.op == TokenKind::pipepipe)
    return boolToDouble(
        Builder.CreateLogicalOr(doubleToBool(LHS), doubleToBool(RHS)));

  return Builder.CreateBinOp(getOperatorKind(binop.op), LHS, RHS);
}

llvm::Value *Codegen::doubleToBool(llvm::Value *V) {
  return Builder.CreateFCmpONE(
      V, llvm::ConstantFP::get(Builder.getDoubleTy(), 0.0), "toBool");
}

llvm::Value *Codegen::boolToDouble(llvm::Value *V) {
  return Builder.CreateUIToFP(V, Builder.getDoubleTy(), "toDouble");
}

void Codegen::generateBlock(const ResolvedBlock &block) {
  for (auto &&stmt : block.statements)
    generateStmt(*stmt);
}

void Codegen::generateFunctionBody(const ResolvedFunctionDecl &functionDecl) {
  auto *function = Module.getFunction(functionDecl.identifier);
  auto *BB = llvm::BasicBlock::Create(Context, "", function);

  Builder.SetInsertPoint(BB);

  if (functionDecl.identifier == "print")
    generateBuiltinPrintBody();
  else
    generateBlock(*functionDecl.body);

  Builder.CreateRetVoid();
}

void Codegen::generateBuiltinPrintBody() {
  auto *functionType = llvm::FunctionType::get(Builder.getInt32Ty(),
                                               {Builder.getInt8PtrTy()}, true);
  auto *printf = llvm::Function::Create(
      functionType, llvm::Function::ExternalLinkage, "printf", Module);

  auto *formatStr = Builder.CreateGlobalStringPtr("%f\n");
  llvm::Value *param;
  for (auto &&fn : resolvedSourceFile) {
    if (fn->identifier != "print")
      continue;

    param = declarations[fn->params[0].get()];
  }

  Builder.CreateCall(printf, {formatStr, param});
}

void Codegen::generateMainWrapper() {
  auto *builtinMain = Module.getFunction("main");
  builtinMain->setName("__builtin_main");

  auto *main = llvm::Function::Create(
      llvm::FunctionType::get(Builder.getInt32Ty(), {}, false),
      llvm::Function::ExternalLinkage, "main", Module);

  auto *BB = llvm::BasicBlock::Create(Context, "", main);

  Builder.SetInsertPoint(BB);
  Builder.CreateCall(builtinMain);
  Builder.CreateRet(
      llvm::ConstantInt::get(Builder.getInt32Ty(), llvm::APInt(32, 0, true)));
}

void Codegen::generateFunction(const ResolvedFunctionDecl &functionDecl) {
  auto returnType = generateType(functionDecl.type);

  std::vector<llvm::Type *> paramTypes;
  for (auto &&param : functionDecl.params)
    paramTypes.emplace_back(generateType(param->type));

  auto functionType = llvm::FunctionType::get(returnType, paramTypes, false);
  auto function =
      llvm::Function::Create(functionType, llvm::Function::ExternalLinkage,
                             functionDecl.identifier, Module);

  int idx = 0;
  for (auto &&arg : function->args()) {
    const auto &paramDecl = functionDecl.params[idx];

    arg.setName(paramDecl->identifier);
    declarations[paramDecl.get()] = &arg;

    ++idx;
  }
}

void Codegen::generateIR(std::string_view filePath) {
  for (auto &&function : resolvedSourceFile)
    generateFunction(*function);

  for (auto &&function : resolvedSourceFile)
    generateFunctionBody(*function);

  generateMainWrapper();

  Module.dump();

  std::error_code errorCode;
  llvm::raw_fd_ostream f{filePath, errorCode};
  Module.print(f, nullptr);
}