#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/Host.h>

#include "codegen.h"

namespace yl {
Codegen::Codegen(std::vector<std::unique_ptr<res::Decl>> resolvedTree,
                 std::string_view sourcePath)
    : resolvedTree(std::move(resolvedTree)),
      builder(context),
      module("<translation_unit>", context) {
  module.setSourceFileName(sourcePath);
  module.setTargetTriple(llvm::sys::getDefaultTargetTriple());
}

llvm::Type *Codegen::generateType(res::Type type) {
  if (type.kind == res::Type::Kind::Number)
    return builder.getDoubleTy();

  if (type.kind == res::Type::Kind::Struct)
    return llvm::StructType::getTypeByName(context, "struct." + type.name);

  return builder.getVoidTy();
}

llvm::Value *Codegen::generateStmt(const res::Stmt &stmt) {
  if (auto *expr = dynamic_cast<const res::Expr *>(&stmt))
    return generateExpr(*expr);

  if (auto *ifStmt = dynamic_cast<const res::IfStmt *>(&stmt))
    return generateIfStmt(*ifStmt);

  if (auto *declStmt = dynamic_cast<const res::DeclStmt *>(&stmt))
    return generateDeclStmt(*declStmt);

  if (auto *assignment = dynamic_cast<const res::Assignment *>(&stmt))
    return generateAssignment(*assignment);

  if (auto *whileStmt = dynamic_cast<const res::WhileStmt *>(&stmt))
    return generateWhileStmt(*whileStmt);

  if (auto *returnStmt = dynamic_cast<const res::ReturnStmt *>(&stmt))
    return generateReturnStmt(*returnStmt);

  llvm_unreachable("unknown statement");
}

llvm::Value *Codegen::generateIfStmt(const res::IfStmt &stmt) {
  llvm::Function *function = getCurrentFunction();

  auto *trueBB = llvm::BasicBlock::Create(context, "if.true");
  auto *exitBB = llvm::BasicBlock::Create(context, "if.exit");

  llvm::BasicBlock *elseBB = exitBB;
  if (stmt.falseBlock)
    elseBB = llvm::BasicBlock::Create(context, "if.false");

  llvm::Value *cond = generateExpr(*stmt.condition);
  builder.CreateCondBr(doubleToBool(cond), trueBB, elseBB);

  trueBB->insertInto(function);
  builder.SetInsertPoint(trueBB);
  generateBlock(*stmt.trueBlock);
  breakIntoBB(exitBB);

  if (stmt.falseBlock) {
    elseBB->insertInto(function);
    builder.SetInsertPoint(elseBB);
    generateBlock(*stmt.falseBlock);
    breakIntoBB(exitBB);
  }

  exitBB->insertInto(function);
  builder.SetInsertPoint(exitBB);
  return nullptr;
}

llvm::Value *Codegen::generateWhileStmt(const res::WhileStmt &stmt) {
  llvm::Function *function = getCurrentFunction();

  auto *header = llvm::BasicBlock::Create(context, "while.cond", function);
  auto *body = llvm::BasicBlock::Create(context, "while.body", function);
  auto *exit = llvm::BasicBlock::Create(context, "while.exit", function);

  builder.CreateBr(header);

  builder.SetInsertPoint(header);
  llvm::Value *cond = generateExpr(*stmt.condition);
  builder.CreateCondBr(doubleToBool(cond), body, exit);

  builder.SetInsertPoint(body);
  generateBlock(*stmt.body);
  breakIntoBB(header);

  builder.SetInsertPoint(exit);
  return nullptr;
}

llvm::Value *Codegen::generateDeclStmt(const res::DeclStmt &stmt) {
  const auto *decl = stmt.varDecl.get();
  llvm::AllocaInst *var = allocateStackVariable(decl->identifier, decl->type);

  if (const auto &init = decl->initializer)
    storeValue(generateExpr(*init), var, init->type);

  declarations[decl] = var;
  return nullptr;
}

llvm::Value *Codegen::generateAssignment(const res::Assignment &stmt) {
  llvm::Value *val = generateExpr(*stmt.expr);
  return storeValue(val, generateExpr(*stmt.assignee, true),
                    stmt.assignee->type);
}

llvm::Value *Codegen::generateReturnStmt(const res::ReturnStmt &stmt) {
  if (stmt.expr)
    storeValue(generateExpr(*stmt.expr), retVal, stmt.expr->type);

  assert(retBB && "function with return stmt doesn't have a return block");
  breakIntoBB(retBB);
  return nullptr;
}

llvm::Value *Codegen::generateMemberExpr(const res::MemberExpr &memberExpr,
                                         bool keepPointer) {
  llvm::Value *base = generateExpr(*memberExpr.base, true);
  llvm::Value *field = builder.CreateStructGEP(
      generateType(memberExpr.base->type), base, memberExpr.field->index);

  return keepPointer ? field : loadValue(field, memberExpr.field->type);
}

llvm::Value *
Codegen::generateTemporaryStruct(const res::StructInstantiationExpr &sie) {
  res::Type structType = sie.type;
  llvm::Value *tmp =
      allocateStackVariable(structType.name + ".tmp", structType);

  std::map<const res::FieldDecl *, llvm::Value *> initializerVals;
  for (auto &&initStmt : sie.fieldInitializers)
    initializerVals[initStmt->field] = generateExpr(*initStmt->initializer);

  size_t idx = 0;
  for (auto &&field : sie.structDecl->fields) {
    llvm::Value *dst =
        builder.CreateStructGEP(generateType(structType), tmp, idx++);
    storeValue(initializerVals[field.get()], dst, field->type);
  }

  return tmp;
}

llvm::Value *Codegen::generateExpr(const res::Expr &expr, bool keepPointer) {
  if (auto *number = dynamic_cast<const res::NumberLiteral *>(&expr))
    return llvm::ConstantFP::get(builder.getDoubleTy(), number->value);

  if (auto val = expr.getConstantValue())
    return llvm::ConstantFP::get(builder.getDoubleTy(), *val);

  if (auto *dre = dynamic_cast<const res::DeclRefExpr *>(&expr))
    return generateDeclRefExpr(*dre, keepPointer);

  if (auto *call = dynamic_cast<const res::CallExpr *>(&expr))
    return generateCallExpr(*call);

  if (auto *grouping = dynamic_cast<const res::GroupingExpr *>(&expr))
    return generateExpr(*grouping->expr);

  if (auto *binop = dynamic_cast<const res::BinaryOperator *>(&expr))
    return generateBinaryOperator(*binop);

  if (auto *unop = dynamic_cast<const res::UnaryOperator *>(&expr))
    return generateUnaryOperator(*unop);

  if (auto *me = dynamic_cast<const res::MemberExpr *>(&expr))
    return generateMemberExpr(*me, keepPointer);

  if (auto *sie = dynamic_cast<const res::StructInstantiationExpr *>(&expr))
    return generateTemporaryStruct(*sie);

  llvm_unreachable("unexpected expression");
}

llvm::Value *Codegen::generateDeclRefExpr(const res::DeclRefExpr &dre,
                                          bool keepPointer) {
  const res::Decl *decl = dre.decl;
  llvm::Value *val = declarations[decl];

  keepPointer |= dynamic_cast<const res::ParamDecl *>(decl) && !decl->isMutable;
  keepPointer |= dre.type.kind == res::Type::Kind::Struct;

  return keepPointer ? val : loadValue(val, dre.type);
}

llvm::Value *Codegen::generateCallExpr(const res::CallExpr &call) {
  const res::FunctionDecl *calleeDecl = call.callee;
  llvm::Function *callee = module.getFunction(calleeDecl->identifier);

  bool isReturningStruct = calleeDecl->type.kind == res::Type::Kind::Struct;
  llvm::Value *retVal = nullptr;
  std::vector<llvm::Value *> args;

  if (isReturningStruct)
    retVal = args.emplace_back(
        allocateStackVariable("struct.ret.tmp", calleeDecl->type));

  size_t argIdx = 0;
  for (auto &&arg : call.arguments) {
    llvm::Value *val = generateExpr(*arg);

    if (arg->type.kind == res::Type::Kind::Struct &&
        calleeDecl->params[argIdx]->isMutable) {
      llvm::Value *tmpVar = allocateStackVariable("struct.arg.tmp", arg->type);
      storeValue(val, tmpVar, arg->type);
      val = tmpVar;
    }

    args.emplace_back(val);
    ++argIdx;
  }

  llvm::CallInst *callInst = builder.CreateCall(callee, args);
  callInst->setAttributes(constructAttrList(calleeDecl));

  return isReturningStruct ? retVal : callInst;
}

llvm::Value *Codegen::generateUnaryOperator(const res::UnaryOperator &unop) {
  llvm::Value *rhs = generateExpr(*unop.operand);

  if (unop.op == TokenKind::Excl)
    return boolToDouble(builder.CreateNot(doubleToBool(rhs)));

  if (unop.op == TokenKind::Minus)
    return builder.CreateFNeg(rhs);

  llvm_unreachable("unknown unary op");
}

void Codegen::generateConditionalOperator(const res::Expr &op,
                                          llvm::BasicBlock *trueBB,
                                          llvm::BasicBlock *falseBB) {
  llvm::Function *function = getCurrentFunction();
  const auto *binop = dynamic_cast<const res::BinaryOperator *>(&op);

  if (binop && binop->op == TokenKind::PipePipe) {
    llvm::BasicBlock *nextBB =
        llvm::BasicBlock::Create(context, "or.lhs.false", function);
    generateConditionalOperator(*binop->lhs, trueBB, nextBB);

    builder.SetInsertPoint(nextBB);
    generateConditionalOperator(*binop->rhs, trueBB, falseBB);
    return;
  }

  if (binop && binop->op == TokenKind::AmpAmp) {
    llvm::BasicBlock *nextBB =
        llvm::BasicBlock::Create(context, "and.lhs.true", function);
    generateConditionalOperator(*binop->lhs, nextBB, falseBB);

    builder.SetInsertPoint(nextBB);
    generateConditionalOperator(*binop->rhs, trueBB, falseBB);
    return;
  }

  llvm::Value *val = doubleToBool(generateExpr(op));
  builder.CreateCondBr(val, trueBB, falseBB);
};

llvm::Value *Codegen::generateBinaryOperator(const res::BinaryOperator &binop) {
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

    assert(!builder.GetInsertBlock()->getTerminator() &&
           "a binop terminated the current block");
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
}

llvm::Value *Codegen::loadValue(llvm::Value *v, const res::Type &type) {
  if (type.kind == res::Type::Kind::Number)
    return builder.CreateLoad(builder.getDoubleTy(), v);

  return v;
}

llvm::Value *
Codegen::storeValue(llvm::Value *val, llvm::Value *ptr, const res::Type &type) {
  if (type.kind != res::Type::Kind::Struct)
    return builder.CreateStore(val, ptr);

  const llvm::DataLayout &dl = module.getDataLayout();
  const llvm::StructLayout *sl =
      dl.getStructLayout(static_cast<llvm::StructType *>(generateType(type)));

  return builder.CreateMemCpy(ptr, sl->getAlignment(), val, sl->getAlignment(),
                              sl->getSizeInBytes());
}

llvm::Value *Codegen::doubleToBool(llvm::Value *v) {
  return builder.CreateFCmpONE(
      v, llvm::ConstantFP::get(builder.getDoubleTy(), 0.0), "to.bool");
}

llvm::Value *Codegen::boolToDouble(llvm::Value *v) {
  return builder.CreateUIToFP(v, builder.getDoubleTy(), "to.double");
}

void Codegen::breakIntoBB(llvm::BasicBlock *targetBB) {
  llvm::BasicBlock *currentBB = builder.GetInsertBlock();

  if (currentBB && !currentBB->getTerminator())
    builder.CreateBr(targetBB);

  builder.ClearInsertionPoint();
}

llvm::Function *Codegen::getCurrentFunction() {
  return builder.GetInsertBlock()->getParent();
};

llvm::AllocaInst *
Codegen::allocateStackVariable(const std::string_view identifier,
                               const res::Type &type) {
  llvm::IRBuilder<> tmpBuilder(context);
  tmpBuilder.SetInsertPoint(allocaInsertPoint);

  return tmpBuilder.CreateAlloca(generateType(type), nullptr, identifier);
}

llvm::AttributeList Codegen::constructAttrList(const res::FunctionDecl *fn) {
  bool isReturningStruct = fn->type.kind == res::Type::Kind::Struct;
  std::vector<llvm::AttributeSet> argsAttrSets;

  if (isReturningStruct) {
    llvm::AttrBuilder retAttrs(context);
    retAttrs.addStructRetAttr(generateType(fn->type));
    argsAttrSets.emplace_back(llvm::AttributeSet::get(context, retAttrs));
  }

  for (auto &&param : fn->params) {
    llvm::AttrBuilder paramAttrs(context);
    if (param->type.kind == res::Type::Kind::Struct) {
      if (param->isMutable)
        paramAttrs.addByValAttr(generateType(param->type));
      else
        paramAttrs.addAttribute(llvm::Attribute::ReadOnly);
    }
    argsAttrSets.emplace_back(llvm::AttributeSet::get(context, paramAttrs));
  }

  return llvm::AttributeList::get(context, llvm::AttributeSet{},
                                  llvm::AttributeSet{}, argsAttrSets);
}

void Codegen::generateBlock(const res::Block &block) {
  for (auto &&stmt : block.statements) {
    generateStmt(*stmt);

    // We exited the current basic block for some reason, so there is
    // no need for generating the remaining instructions.
    if (!builder.GetInsertBlock())
      break;
  }
}

void Codegen::generateFunctionBody(const res::FunctionDecl &functionDecl) {
  auto *function = module.getFunction(functionDecl.identifier);

  auto *entryBB = llvm::BasicBlock::Create(context, "entry", function);
  builder.SetInsertPoint(entryBB);

  // Note: llvm:Instruction has a protected destructor.
  llvm::Value *undef = llvm::UndefValue::get(builder.getInt32Ty());
  allocaInsertPoint = new llvm::BitCastInst(undef, undef->getType(),
                                            "alloca.placeholder", entryBB);

  bool returnsVoid = functionDecl.type.kind != res::Type::Kind::Number;
  if (!returnsVoid)
    retVal = allocateStackVariable("retval", functionDecl.type);
  retBB = llvm::BasicBlock::Create(context, "return");

  int idx = 0;
  for (auto &&arg : function->args()) {
    if (arg.hasStructRetAttr()) {
      arg.setName("ret");
      retVal = &arg;
      continue;
    }

    const auto *paramDecl = functionDecl.params[idx].get();
    arg.setName(paramDecl->identifier);

    llvm::Value *declVal = &arg;
    if (paramDecl->type.kind != res::Type::Kind::Struct &&
        paramDecl->isMutable) {
      declVal = allocateStackVariable(paramDecl->identifier, paramDecl->type);
      storeValue(&arg, declVal, paramDecl->type);
    }

    declarations[paramDecl] = declVal;
    ++idx;
  }

  if (functionDecl.identifier == "println")
    generateBuiltinPrintlnBody(functionDecl);
  else
    generateBlock(*functionDecl.body);

  if (retBB->hasNPredecessorsOrMore(1)) {
    breakIntoBB(retBB);
    retBB->insertInto(function);
    builder.SetInsertPoint(retBB);
  }

  allocaInsertPoint->eraseFromParent();
  allocaInsertPoint = nullptr;

  if (returnsVoid) {
    builder.CreateRetVoid();
    return;
  }

  builder.CreateRet(loadValue(retVal, functionDecl.type));
}

void Codegen::generateBuiltinPrintlnBody(const res::FunctionDecl &println) {
  auto *type = llvm::FunctionType::get(builder.getInt32Ty(),
                                       {builder.getInt8PtrTy()}, true);
  auto *printf = llvm::Function::Create(type, llvm::Function::ExternalLinkage,
                                        "printf", module);
  auto *format = builder.CreateGlobalStringPtr("%.15g\n");

  llvm::Value *param = declarations[println.params[0].get()];

  builder.CreateCall(printf, {format, param});
}

void Codegen::generateMainWrapper() {
  auto *builtinMain = module.getFunction("main");
  builtinMain->setName("__builtin_main");

  auto *main = llvm::Function::Create(
      llvm::FunctionType::get(builder.getInt32Ty(), {}, false),
      llvm::Function::ExternalLinkage, "main", module);

  auto *entry = llvm::BasicBlock::Create(context, "entry", main);
  builder.SetInsertPoint(entry);

  builder.CreateCall(builtinMain);
  builder.CreateRet(llvm::ConstantInt::getSigned(builder.getInt32Ty(), 0));
}

void Codegen::generateFunctionDecl(const res::FunctionDecl &functionDecl) {
  llvm::Type *retType = generateType(functionDecl.type);
  std::vector<llvm::Type *> paramTypes;

  if (functionDecl.type.kind == res::Type::Kind::Struct) {
    paramTypes.emplace_back(llvm::PointerType::get(retType, 0));
    retType = builder.getVoidTy();
  }

  for (auto &&param : functionDecl.params) {
    llvm::Type *paramType = generateType(param->type);
    if (param->type.kind == res::Type::Kind::Struct)
      paramType = llvm::PointerType::get(paramType, 0);
    paramTypes.emplace_back(paramType);
  }

  auto *type = llvm::FunctionType::get(retType, paramTypes, false);
  auto *fn = llvm::Function::Create(type, llvm::Function::ExternalLinkage,
                                    functionDecl.identifier, module);
  fn->setAttributes(constructAttrList(&functionDecl));
}

void Codegen::generateStructDecl(const res::StructDecl &structDecl) {
  llvm::StructType::create(context, "struct." + structDecl.identifier);
}

void Codegen::generateStructDefinition(const res::StructDecl &structDecl) {
  auto *type = static_cast<llvm::StructType *>(generateType(structDecl.type));

  std::vector<llvm::Type *> fieldTypes;
  for (auto &&field : structDecl.fields) {
    llvm::Type *t = generateType(field->type);
    fieldTypes.emplace_back(t);
  }

  type->setBody(fieldTypes);
}

llvm::Module *Codegen::generateIR() {
  for (auto &&decl : resolvedTree) {
    if (const auto *fn = dynamic_cast<const res::FunctionDecl *>(decl.get()))
      generateFunctionDecl(*fn);
    else if (const auto *sd = dynamic_cast<const res::StructDecl *>(decl.get()))
      generateStructDecl(*sd);
    else
      llvm_unreachable("unexpected top level declaration");
  }

  for (auto &&decl : resolvedTree) {
    if (const auto *fn = dynamic_cast<const res::FunctionDecl *>(decl.get()))
      generateFunctionBody(*fn);
    else if (const auto *sd = dynamic_cast<const res::StructDecl *>(decl.get()))
      generateStructDefinition(*sd);
    else
      llvm_unreachable("unexpected top level declaration");
  }

  generateMainWrapper();

  return &module;
}
} // namespace yl
