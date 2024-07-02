#include "codegen.h"

#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>

// FIXME: Move this to utility
namespace {
const ResolvedBinaryOperator *getAsConditionalBinop(const ResolvedExpr *expr) {
  const auto *binop = dynamic_cast<const ResolvedBinaryOperator *>(expr);
  if (!binop)
    return nullptr;

  TokenKind op = binop->op;
  if (op == TokenKind::PipePipe || op == TokenKind::AmpAmp)
    return binop;

  return nullptr;
}
} // namespace

Codegen::Codegen(
    std::vector<std::unique_ptr<ResolvedFunctionDecl>> resolvedSourceFile)
    : resolvedSourceFile(std::move(resolvedSourceFile)), builder(context),
      module(std::make_unique<llvm::Module>("<translation_unit>", context)) {}

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

  if (auto *whileStmt = dynamic_cast<const ResolvedWhileStmt *>(&stmt))
    return generateWhileStmt(*whileStmt);

  if (auto *returnStmt = dynamic_cast<const ResolvedReturnStmt *>(&stmt))
    return generateReturnStmt(*returnStmt);

  llvm_unreachable("unknown statement");
}

llvm::Value *Codegen::generateIfStmt(const ResolvedIfStmt &stmt) {
  llvm::Function *parentFunction = getCurrentFunction();

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

llvm::Value *Codegen::generateWhileStmt(const ResolvedWhileStmt &stmt) {
  llvm::Function *parentFunction = getCurrentFunction();

  llvm::BasicBlock *header =
      llvm::BasicBlock::Create(context, "whileCond", parentFunction);
  llvm::BasicBlock *body =
      llvm::BasicBlock::Create(context, "whileBody", parentFunction);
  llvm::BasicBlock *exit =
      llvm::BasicBlock::Create(context, "whileExit", parentFunction);

  builder.CreateBr(header);

  builder.SetInsertPoint(header);
  llvm::Value *cond = generateExpr(*stmt.condition);
  builder.CreateCondBr(doubleToBool(cond), body, exit);

  builder.SetInsertPoint(body);
  generateBlock(*stmt.body);
  builder.CreateBr(header);

  builder.SetInsertPoint(exit);
  return nullptr;
}

llvm::Value *Codegen::generateDeclStmt(const ResolvedDeclStmt &stmt) {
  llvm::Function *parentFunction = getCurrentFunction();

  const auto &varDecl = stmt.varDecl;
  llvm::AllocaInst *localVar =
      allocateStackVariable(parentFunction, varDecl->identifier);

  if (const auto &init = varDecl->initializer) {
    llvm::Value *initVal = generateExpr(*init);
    builder.CreateStore(initVal, localVar);
  }

  declarations[varDecl.get()] = localVar;

  return nullptr;
}

llvm::Value *Codegen::generateAssignment(const ResolvedAssignment &stmt) {
  llvm::Value *lhs = generateExpr(*stmt.expr);
  return builder.CreateStore(lhs, declarations[stmt.variable->decl]);
}

llvm::Value *Codegen::generateReturnStmt(const ResolvedReturnStmt &stmt) {
  llvm::Value *ret = nullptr;
  if (stmt.expr)
    ret = generateExpr(*stmt.expr);

  if (ret)
    builder.CreateStore(ret, retVal);

  assert(retBlock && "function without return block");
  return builder.CreateBr(retBlock);
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
  llvm::Function *callee = module->getFunction(call.callee->identifier);

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

void Codegen::generateConditionalOperator(const ResolvedExpr &op,
                                          llvm::BasicBlock *trueBlock,
                                          llvm::BasicBlock *falseBlock) {
  if (const auto *condBinop = getAsConditionalBinop(&op)) {
    if (condBinop->op == TokenKind::PipePipe) {
      llvm::BasicBlock *nextBlock = llvm::BasicBlock::Create(
          context, "or.lhs.false", trueBlock->getParent());
      generateConditionalOperator(*condBinop->lhs, trueBlock, nextBlock);

      builder.SetInsertPoint(nextBlock);
      generateConditionalOperator(*condBinop->rhs, trueBlock, falseBlock);
      return;
    }

    if (condBinop->op == TokenKind::AmpAmp) {
      llvm::BasicBlock *nextBlock = llvm::BasicBlock::Create(
          context, "and.lhs.true", trueBlock->getParent());
      generateConditionalOperator(*condBinop->lhs, nextBlock, falseBlock);

      builder.SetInsertPoint(nextBlock);
      generateConditionalOperator(*condBinop->rhs, trueBlock, falseBlock);
      return;
    }
  }

  llvm::Value *val = doubleToBool(generateExpr(op));
  builder.CreateCondBr(val, trueBlock, falseBlock);
};

llvm::Value *
Codegen::generateBinaryOperator(const ResolvedBinaryOperator &binop) {

  TokenKind op = binop.op;
  if (op == TokenKind::AmpAmp || op == TokenKind::PipePipe) {
    bool isOr = op == TokenKind::PipePipe;

    llvm::BasicBlock *rhsBlock =
        llvm::BasicBlock::Create(context, isOr ? "or.rhs" : "and.rhs",
                                 builder.GetInsertBlock()->getParent());
    llvm::BasicBlock *mergeBlock =
        llvm::BasicBlock::Create(context, isOr ? "or.merge" : "and.merge",
                                 builder.GetInsertBlock()->getParent());

    generateConditionalOperator(*binop.lhs, isOr ? mergeBlock : rhsBlock,
                                isOr ? rhsBlock : mergeBlock);
    builder.SetInsertPoint(rhsBlock);

    llvm::Value *rhs = doubleToBool(generateExpr(*binop.rhs));
    builder.CreateBr(mergeBlock);

    builder.SetInsertPoint(mergeBlock);

    llvm::PHINode *phi = builder.CreatePHI(builder.getInt1Ty(), 0);

    for (llvm::pred_iterator pi = pred_begin(mergeBlock),
                             pe = pred_end(mergeBlock);
         pi != pe; ++pi)
      phi->addIncoming(builder.getInt1(isOr), *pi);
    phi->addIncoming(rhs, rhsBlock);

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

  return builder.CreateBinOp(getOperatorKind(op), lhs, rhs);
}

llvm::Value *Codegen::doubleToBool(llvm::Value *v) {
  return builder.CreateFCmpONE(
      v, llvm::ConstantFP::get(builder.getDoubleTy(), 0.0), "toBool");
}

llvm::Value *Codegen::boolToDouble(llvm::Value *v) {
  return builder.CreateUIToFP(v, builder.getDoubleTy(), "toDouble");
}

llvm::Function *Codegen::getCurrentFunction() {
  llvm::BasicBlock *currentBlock = builder.GetInsertBlock();
  if (!currentBlock)
    return nullptr;

  return currentBlock->getParent();
};

llvm::AllocaInst *
Codegen::allocateStackVariable(llvm::Function *function,
                               const std::string_view identifier) {
  // FIXME: fix the ordering
  llvm::IRBuilder<> tmpBuilder(context);
  tmpBuilder.SetInsertPoint(&function->getEntryBlock(),
                            function->getEntryBlock().begin());

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

  // FIXME: This should disappear with return optimization.
  if (const llvm::BasicBlock *bb = builder.GetInsertBlock();
      bb && bb->empty() && !retBlock->hasNPredecessors(0))
    builder.CreateUnreachable();
}

// FIXME: Optimize return stmt emission.
void Codegen::generateFunctionBody(const ResolvedFunctionDecl &functionDecl) {
  auto *function = module->getFunction(functionDecl.identifier);
  auto *bb = llvm::BasicBlock::Create(context, "", function);

  builder.SetInsertPoint(bb);

  bool isVoidFunction = functionDecl.type == Type::Void;
  if (!isVoidFunction)
    retVal = allocateStackVariable(function, "retval");
  retBlock = llvm::BasicBlock::Create(context, "return");

  int idx = 0;
  for (auto &&arg : function->args()) {
    const auto &paramDecl = functionDecl.params[idx];
    arg.setName(paramDecl->identifier);

    llvm::AllocaInst *stackParam =
        allocateStackVariable(function, paramDecl->identifier);
    declarations[paramDecl.get()] = stackParam;
    builder.CreateStore(&arg, stackParam);
    ++idx;
  }

  if (functionDecl.identifier == "print")
    generateBuiltinPrintBody();
  else
    generateBlock(*functionDecl.body);

  if (retBlock->hasNPredecessorsOrMore(1)) {
    retBlock->insertInto(function);
    builder.SetInsertPoint(retBlock);
  }

  if (isVoidFunction)
    builder.CreateRetVoid();
  else
    builder.CreateRet(builder.CreateLoad(builder.getDoubleTy(), retVal));
}

void Codegen::generateBuiltinPrintBody() {
  auto *functionType = llvm::FunctionType::get(builder.getInt32Ty(),
                                               {builder.getInt8PtrTy()}, true);
  auto *printf = llvm::Function::Create(
      functionType, llvm::Function::ExternalLinkage, "printf", *module);

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
  auto *builtinMain = module->getFunction("main");
  builtinMain->setName("__builtin_main");

  auto *main = llvm::Function::Create(
      llvm::FunctionType::get(builder.getInt32Ty(), {}, false),
      llvm::Function::ExternalLinkage, "main", *module);

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
                             functionDecl.identifier, *module);
}

std::unique_ptr<llvm::Module> Codegen::generateIR() {
  for (auto &&function : resolvedSourceFile)
    generateFunction(*function);

  for (auto &&function : resolvedSourceFile)
    generateFunctionBody(*function);

  generateMainWrapper();

  return std::move(module);
}
