#include "codegen.h"

#include <llvm/Support/Host.h>

Codegen::Codegen(
    std::vector<std::unique_ptr<ResolvedFunctionDecl>> resolvedSourceFile)
    : resolvedSourceFile(std::move(resolvedSourceFile)), builder(context),
      module("<translation_unit>", context) {
  module.setTargetTriple(llvm::sys::getDefaultTargetTriple());
}

llvm::Type *Codegen::generateType(Type type) {
  if (type == Type::Number)
    return builder.getDoubleTy();

  return builder.getVoidTy();
}

llvm::Instruction::BinaryOps Codegen::getOperatorKind(TokenKind op) {
  if (op == TokenKind::Plus)
    return llvm::BinaryOperator::FAdd;
  if (op == TokenKind::Minus)
    return llvm::BinaryOperator::FSub;
  if (op == TokenKind::Asterisk)
    return llvm::BinaryOperator::FMul;
  if (op == TokenKind::Slash)
    return llvm::BinaryOperator::FDiv;

  llvm_unreachable("unknown operator");
}

llvm::Value *Codegen::generateStmt(const ResolvedStmt &stmt) {
  if (auto *expr = dynamic_cast<const ResolvedExpr *>(&stmt))
    return generateExpr(*expr);

  if (auto *ifStmt = dynamic_cast<const ResolvedIfStmt *>(&stmt))
    return generateIfStmt(*ifStmt);

  if (auto *declStmt = dynamic_cast<const ResolvedDeclStmt *>(&stmt))
    return generateDeclStmt(*declStmt);

  if (auto *assignment = dynamic_cast<const ResolvedAssignment *>(&stmt))
    return generateAssignment(*assignment);

  llvm_unreachable("unknown statement");
}

llvm::Value *Codegen::generateIfStmt(const ResolvedIfStmt &stmt) {
  llvm::Function *parentFunction = builder.GetInsertBlock()->getParent();

  llvm::Value *cond = generateExpr(*stmt.condition);
  llvm::BasicBlock *thenBB =
      llvm::BasicBlock::Create(context, "then", parentFunction);
  llvm::BasicBlock *elseBB = nullptr;
  llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "merge");

  bool hasElseBranch = stmt.falseBlock || stmt.falseBranch;
  if (hasElseBranch)
    elseBB = llvm::BasicBlock::Create(context, "else");

  builder.CreateCondBr(doubleToBool(cond), thenBB,
                       hasElseBranch ? elseBB : mergeBB);

  builder.SetInsertPoint(thenBB);
  generateBlock(*stmt.trueBlock);
  builder.CreateBr(mergeBB);

  if (hasElseBranch) {
    elseBB->insertInto(parentFunction);
    builder.SetInsertPoint(elseBB);

    if (stmt.falseBlock)
      generateBlock(*stmt.falseBlock);
    else
      generateIfStmt(*stmt.falseBranch);

    builder.CreateBr(mergeBB);
  }

  mergeBB->insertInto(parentFunction);
  builder.SetInsertPoint(mergeBB);
  return nullptr;
}

llvm::Value *Codegen::generateDeclStmt(const ResolvedDeclStmt &stmt) {
  llvm::Function *parentFunction = builder.GetInsertBlock()->getParent();

  const auto &varDecl = stmt.varDecl;
  llvm::AllocaInst *localVar = allocateStackVariable(parentFunction, *varDecl);

  if (const auto &init = varDecl->initializer) {
    llvm::Value *initVal = generateExpr(*init);
    builder.CreateStore(initVal, localVar);
  }

  declarations[varDecl.get()] = localVar;

  return nullptr;
}

llvm::Value *Codegen::generateAssignment(const ResolvedAssignment &stmt) {
  llvm::Value *lhs = generateExpr(*stmt.expr);

  builder.CreateStore(lhs, declarations[stmt.variable->decl]);

  return nullptr;
}

llvm::Value *Codegen::generateExpr(const ResolvedExpr &expr) {
  if (std::optional<double> val = expr.getConstantValue())
    return llvm::ConstantFP::get(builder.getDoubleTy(), *val);

  if (auto *numLit = dynamic_cast<const ResolvedNumberLiteral *>(&expr))
    return llvm::ConstantFP::get(builder.getDoubleTy(), numLit->value);

  if (auto *declRefExpr = dynamic_cast<const ResolvedDeclRefExpr *>(&expr))
    return builder.CreateLoad(builder.getDoubleTy(),
                              declarations[declRefExpr->decl]);

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
  llvm::Function *callee = module.getFunction(call.callee->identifier);

  std::vector<llvm::Value *> args;
  for (auto &&arg : call.arguments)
    args.emplace_back(generateExpr(*arg));

  return builder.CreateCall(callee, args);
}

// FIXME: Refactor!!!
llvm::Value *
Codegen::generateUnaryOperator(const ResolvedUnaryOperator &unary) {
  llvm::Value *rhs = generateExpr(*unary.rhs);

  if (unary.op == TokenKind::Excl)
    return boolToDouble(builder.CreateNot(doubleToBool(rhs)));

  llvm_unreachable("unknown unary op");
}

llvm::Value *
Codegen::generateBinaryOperator(const ResolvedBinaryOperator &binop) {
  llvm::Value *lhs = generateExpr(*binop.lhs);
  llvm::Value *rhs = generateExpr(*binop.rhs);

  // FIXME: Refactor this!!!
  if (binop.op == TokenKind::Lt)
    return boolToDouble(builder.CreateFCmpOLT(lhs, rhs));
  if (binop.op == TokenKind::Gt)
    return boolToDouble(builder.CreateFCmpOGT(lhs, rhs));
  if (binop.op == TokenKind::EqualEqual)
    return boolToDouble(builder.CreateFCmpOEQ(lhs, rhs));
  if (binop.op == TokenKind::AmpAmp) {
    return boolToDouble(
        builder.CreateLogicalAnd(doubleToBool(lhs), doubleToBool(rhs)));
  }
  if (binop.op == TokenKind::PipePipe)
    return boolToDouble(
        builder.CreateLogicalOr(doubleToBool(lhs), doubleToBool(rhs)));

  return builder.CreateBinOp(getOperatorKind(binop.op), lhs, rhs);
}

llvm::Value *Codegen::doubleToBool(llvm::Value *v) {
  return builder.CreateFCmpONE(
      v, llvm::ConstantFP::get(builder.getDoubleTy(), 0.0), "toBool");
}

llvm::Value *Codegen::boolToDouble(llvm::Value *v) {
  return builder.CreateUIToFP(v, builder.getDoubleTy(), "toDouble");
}

llvm::AllocaInst *Codegen::allocateStackVariable(llvm::Function *function,
                                                 const ResolvedDecl &decl) {
  // FIXME: fix the ordering
  llvm::IRBuilder<> tmpBuilder(context);
  tmpBuilder.SetInsertPoint(&function->getEntryBlock(),
                            function->getEntryBlock().begin());

  return tmpBuilder.CreateAlloca(tmpBuilder.getDoubleTy(), nullptr,
                                 decl.identifier);
}

void Codegen::generateBlock(const ResolvedBlock &block) {
  for (auto &&stmt : block.statements)
    generateStmt(*stmt);
}

void Codegen::generateFunctionBody(const ResolvedFunctionDecl &functionDecl) {
  auto *function = module.getFunction(functionDecl.identifier);
  auto *bb = llvm::BasicBlock::Create(context, "", function);

  builder.SetInsertPoint(bb);

  int idx = 0;
  for (auto &&arg : function->args()) {
    const auto &paramDecl = functionDecl.params[idx];
    arg.setName(paramDecl->identifier);

    llvm::AllocaInst *stackParam = allocateStackVariable(function, *paramDecl);
    declarations[paramDecl.get()] = stackParam;
    builder.CreateStore(&arg, stackParam);
    ++idx;
  }

  if (functionDecl.identifier == "print")
    generateBuiltinPrintBody();
  else
    generateBlock(*functionDecl.body);

  builder.CreateRetVoid();
}

void Codegen::generateBuiltinPrintBody() {
  auto *functionType = llvm::FunctionType::get(builder.getInt32Ty(),
                                               {builder.getInt8PtrTy()}, true);
  auto *printf = llvm::Function::Create(
      functionType, llvm::Function::ExternalLinkage, "printf", module);

  auto *formatStr = builder.CreateGlobalStringPtr("%f\n");
  llvm::Value *param;
  for (auto &&fn : resolvedSourceFile) {
    if (fn->identifier != "print")
      continue;

    param = builder.CreateLoad(builder.getDoubleTy(),
                               declarations[fn->params[0].get()]);
  }

  builder.CreateCall(printf, {formatStr, param});
}

void Codegen::generateMainWrapper() {
  auto *builtinMain = module.getFunction("main");
  builtinMain->setName("__builtin_main");

  auto *main = llvm::Function::Create(
      llvm::FunctionType::get(builder.getInt32Ty(), {}, false),
      llvm::Function::ExternalLinkage, "main", module);

  auto *bb = llvm::BasicBlock::Create(context, "", main);

  builder.SetInsertPoint(bb);
  builder.CreateCall(builtinMain);
  builder.CreateRet(
      llvm::ConstantInt::get(builder.getInt32Ty(), llvm::APInt(32, 0, true)));
}

void Codegen::generateFunction(const ResolvedFunctionDecl &functionDecl) {
  auto *returnType = generateType(functionDecl.type);

  std::vector<llvm::Type *> paramTypes;
  for (auto &&param : functionDecl.params)
    paramTypes.emplace_back(generateType(param->type));

  auto *functionType = llvm::FunctionType::get(returnType, paramTypes, false);
  auto *function =
      llvm::Function::Create(functionType, llvm::Function::ExternalLinkage,
                             functionDecl.identifier, module);
}

void Codegen::generateIR(std::string_view filePath) {
  for (auto &&function : resolvedSourceFile)
    generateFunction(*function);

  for (auto &&function : resolvedSourceFile)
    generateFunctionBody(*function);

  generateMainWrapper();

  module.dump();

  std::error_code errorCode;
  llvm::raw_fd_ostream f{filePath, errorCode};
  module.print(f, nullptr);
}