#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/TargetParser/Host.h>

#include <sstream>

#include "codegen.h"

namespace yl {
namespace {
class Mangling {
  static std::string mangleType(const res::Type *type) {
    type = type->getRootType();

    std::stringstream mangledName;
    if (type->isBuiltinVoid()) {
      mangledName << 'v';
    } else if (type->isBuiltinNumber()) {
      mangledName << 'n';
    } else if (type->isStructType()) {
      const auto *s = static_cast<const res::StructType *>(type);
      const auto &id = s->getDecl()->identifier;
      mangledName << 'S' << id.size() << id;

      size_t typeArgCnt = s->getTypeArgCount();
      if (typeArgCnt > 0) {
        std::vector<const res::Type *> genericArgs;
        for (size_t i = 0; i < typeArgCnt; ++i)
          genericArgs.emplace_back(s->getTypeArg(i));
        mangledName << mangleGenericArgs(genericArgs);
      }
    } else if (type->isFunctionType()) {
      const auto *f = static_cast<const res::FunctionType *>(type);
      mangledName << 'F';
      for (size_t i = 0; i < f->getArgCount(); ++i)
        mangledName << mangleType(f->getArgType(i));
      mangledName << 'R' << mangleType(f->getReturnType());
    } else {
      llvm_unreachable("unexpected type in mangling");
    }

    return mangledName.str();
  }

  static std::string
  mangleGenericArgs(const std::vector<const res::Type *> &genericArgs) {
    std::stringstream mangledName;

    mangledName << 'G';
    for (auto &&type : genericArgs)
      mangledName << mangleType(type);
    mangledName << 'E';

    return mangledName.str();
  }

public:
  static std::string mangleSymbol(const res::Decl *decl,
                                  const std::vector<res::Type *> &genericArgs) {
    std::stringstream mangledName;

    const auto &identifier = decl->identifier;
    mangledName << '_' << 'Y' << 'l' << identifier.size() << identifier;

    if (!genericArgs.empty())
      mangledName << mangleGenericArgs(std::vector<const res::Type *>(
          genericArgs.begin(), genericArgs.end()));

    return mangledName.str();
  }
};
} // namespace

Codegen::Codegen(const res::Context &resolvedCtx, std::string_view sourcePath)
    : resolvedTree(&resolvedCtx),
      builder(context),
      module("<translation_unit>", context) {
  module.setSourceFileName(sourcePath);
  module.setTargetTriple(llvm::sys::getDefaultTargetTriple());
}

llvm::Type *Codegen::generateType(const res::Type *type) {
  if (type->isBuiltinNumber())
    return builder.getDoubleTy();

  if (type->isBuiltinVoid())
    return builder.getVoidTy();

  // FIXME: handle generics
  if (type->isStructType()) {
    auto structId =
        static_cast<const res::StructType *>(type)->getDecl()->identifier;
    return llvm::StructType::getTypeByName(context, "struct." + structId);
  }

  // FIXME: cache this to avoid regeneration every time
  if (type->isFunctionType()) {
    const auto *fnTy = static_cast<const res::FunctionType *>(type);

    llvm::Type *res;
    std::vector<llvm::Type *> args;

    if (fnTy->getReturnType()->isStructType()) {
      args.emplace_back(llvm::PointerType::get(context, 0));
      res = builder.getVoidTy();
    } else if (fnTy->getReturnType()->isFunctionType()) {
      res = llvm::PointerType::get(context, 0);
    } else {
      res = generateType(fnTy->getReturnType());
    }

    for (size_t i = 0; i < fnTy->getArgCount(); ++i) {
      const auto &arg = fnTy->getArgType(i);
      if (arg->isStructType() || arg->isFunctionType())
        args.emplace_back(llvm::PointerType::get(context, 0));
      else
        args.emplace_back(generateType(arg));
    }

    return llvm::FunctionType::get(res, args, false);
  }

  llvm_unreachable("unexpected type encountered");
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
  const res::VarDecl *decl = stmt.varDecl;

  llvm::AllocaInst *var = allocateStackVariable(
      decl->identifier, generateType(resolvedTree->getType(decl)));

  if (const auto &init = decl->initializer)
    storeValue(generateExpr(*init), var,
               generateType(resolvedTree->getType(init)));

  declarations[decl] = var;
  return nullptr;
}

llvm::Value *Codegen::generateAssignment(const res::Assignment &stmt) {
  llvm::Value *val = generateExpr(*stmt.expr);
  return storeValue(val, generateExpr(*stmt.assignee, true),
                    generateType(resolvedTree->getType(stmt.assignee)));
}

llvm::Value *Codegen::generateReturnStmt(const res::ReturnStmt &stmt) {
  if (stmt.expr)
    storeValue(generateExpr(*stmt.expr), retVal,
               generateType(resolvedTree->getType(stmt.expr)));

  assert(retBB && "function with return stmt doesn't have a return block");
  breakIntoBB(retBB);
  return nullptr;
}

llvm::Value *Codegen::generateMemberExpr(const res::MemberExpr &memberExpr,
                                         bool keepPointer) {
  llvm::Value *base = generateExpr(*memberExpr.base, true);
  llvm::Value *field = builder.CreateStructGEP(
      generateType(resolvedTree->getType(memberExpr.base)), base,
      memberExpr.field->index);

  return keepPointer
             ? field
             : loadValue(field,
                         generateType(resolvedTree->getType(memberExpr.field)));
}

llvm::Value *
Codegen::generateTemporaryStruct(const res::StructInstantiationExpr &sie) {
  const res::Type *ty = resolvedTree->getType(sie.structDecl);
  assert(ty->isStructType());
  const res::StructType *structType = static_cast<const res::StructType *>(ty);

  llvm::Value *tmp = allocateStackVariable(
      structType->getDecl()->identifier + ".tmp", generateType(structType));

  std::map<const res::FieldDecl *, llvm::Value *> initializerVals;
  for (auto &&initStmt : sie.fieldInitializers)
    initializerVals[initStmt->field] = generateExpr(*initStmt->initializer);

  size_t idx = 0;
  for (auto &&field : structType->getDecl()->fields) {
    llvm::Value *dst =
        builder.CreateStructGEP(generateType(structType), tmp, idx++);
    storeValue(initializerVals[field], dst,
               generateType(resolvedTree->getType(field)));
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
  const res::Type *type = resolvedTree->getType(dre.decl);
  llvm::Value *val = declarations[decl];

  // FIXME: revisit
  keepPointer |= dynamic_cast<const res::ParamDecl *>(decl) &&
                 !static_cast<const res::ParamDecl *>(decl)->isMutable;
  keepPointer |= type->isStructType();
  keepPointer |= type->isFunctionType();

  return keepPointer ? val : loadValue(val, generateType(type));
}

llvm::Value *Codegen::generateCallExpr(const res::CallExpr &call) {
  llvm::Value *callee = generateExpr(*call.callee);

  // FIXME: this is wrong
  const yl::res::FunctionDecl *calleeDecl = nullptr;
  if (callee->hasName()) {
    for (auto &&fn : resolvedTree->getFunctions()) {
      if (fn->identifier == callee->getName())
        calleeDecl = fn;
    }
  }

  const res::Type *resultTy = resolvedTree->getType(&call);

  bool isReturningStruct = resultTy->isStructType();
  llvm::Value *retVal = nullptr;
  std::vector<llvm::Value *> args;

  if (isReturningStruct)
    retVal = args.emplace_back(
        allocateStackVariable("struct.ret.tmp", generateType(resultTy)));

  size_t argIdx = 0;
  for (auto &&arg : call.arguments) {
    const res::Type *argTy = resolvedTree->getType(arg);
    llvm::Value *val = generateExpr(*arg);

    if (argTy->isStructType()
        // FIXME: keep this optimization?
        && calleeDecl->params[argIdx]->isMutable) {
      llvm::Value *tmpVar =
          allocateStackVariable("struct.arg.tmp", generateType(argTy));
      storeValue(val, tmpVar, generateType(argTy));
      val = tmpVar;
    }

    args.emplace_back(val);
    ++argIdx;
  }

  llvm::CallInst *callInst =
      builder.CreateCall(llvm::cast<llvm::FunctionType>(
                             generateType(resolvedTree->getType(call.callee))),
                         callee, args);
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

llvm::Value *Codegen::loadValue(llvm::Value *v, llvm::Type *type) {
  if (!type->isPointerTy())
    return builder.CreateLoad(type, v);

  return v;
}

llvm::Value *
Codegen::storeValue(llvm::Value *val, llvm::Value *ptr, llvm::Type *type) {
  if (!type->isStructTy())
    return builder.CreateStore(val, ptr);

  const llvm::DataLayout &dl = module.getDataLayout();
  const llvm::StructLayout *sl =
      dl.getStructLayout(llvm::cast<llvm::StructType>(type));

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
                               llvm::Type *type) {
  llvm::IRBuilder<> tmpBuilder(context);
  tmpBuilder.SetInsertPoint(allocaInsertPoint);

  return tmpBuilder.CreateAlloca(type, nullptr, identifier);
}

llvm::AttributeList Codegen::constructAttrList(const res::FunctionDecl *fn) {
  std::vector<llvm::AttributeSet> argsAttrSets;

  const auto *fnTy =
      static_cast<const res::FunctionType *>(resolvedTree->getType(fn));
  if (fnTy->getReturnType()->isStructType()) {
    llvm::AttrBuilder retAttrs(context);
    retAttrs.addStructRetAttr(generateType(fnTy->getReturnType()));
    argsAttrSets.emplace_back(llvm::AttributeSet::get(context, retAttrs));
  }

  for (auto &&param : fn->params) {
    const auto *paramTy = resolvedTree->getType(param);
    llvm::AttrBuilder paramAttrs(context);
    if (paramTy->isStructType()) {
      if (param->isMutable)
        paramAttrs.addByValAttr(generateType(paramTy));
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
  llvm::Function *function = module.getFunction(functionDecl.identifier);
  llvm::FunctionType *functionTy = function->getFunctionType();
  llvm::Type *returnTy = functionTy->getReturnType();

  auto *entryBB = llvm::BasicBlock::Create(context, "entry", function);
  builder.SetInsertPoint(entryBB);

  // Note: llvm:Instruction has a protected destructor.
  llvm::Value *undef = llvm::UndefValue::get(builder.getInt32Ty());
  allocaInsertPoint = new llvm::BitCastInst(undef, undef->getType(),
                                            "alloca.placeholder", entryBB);

  if (!returnTy->isVoidTy())
    retVal = allocateStackVariable("retval", returnTy);
  retBB = llvm::BasicBlock::Create(context, "return");

  int idx = 0;
  for (auto &&arg : function->args()) {
    if (arg.hasStructRetAttr()) {
      arg.setName("ret");
      retVal = &arg;
      continue;
    }

    const res::ParamDecl *paramDecl = functionDecl.params[idx];
    const res::Type *paramType = resolvedTree->getType(paramDecl);
    arg.setName(paramDecl->identifier);

    llvm::Value *argVal;
    if (!paramDecl->isMutable || paramType->isStructType()) {
      argVal = &arg;
    } else {
      argVal = allocateStackVariable(paramDecl->identifier, arg.getType());
      storeValue(&arg, argVal, arg.getType());
    }

    declarations[paramDecl] = argVal;
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

  if (returnTy->isVoidTy())
    builder.CreateRetVoid();
  else
    builder.CreateRet(loadValue(retVal, returnTy));
}

void Codegen::generateBuiltinPrintlnBody(const res::FunctionDecl &println) {
  auto *type =
      llvm::FunctionType::get(builder.getInt32Ty(), {builder.getPtrTy()}, true);
  auto *printf = llvm::Function::Create(type, llvm::Function::ExternalLinkage,
                                        "printf", module);
  auto *format = builder.CreateGlobalString("%.15g\n");

  llvm::Value *param = declarations[println.params[0]];

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
  auto *type = generateType(resolvedTree->getType(&functionDecl));
  assert(type->isFunctionTy());

  auto *fn = llvm::Function::Create(static_cast<llvm::FunctionType *>(type),
                                    llvm::Function::ExternalLinkage,
                                    functionDecl.identifier, module);
  fn->setAttributes(constructAttrList(&functionDecl));
  declarations[&functionDecl] = fn;
}

void Codegen::generateStructDecl(const res::StructDecl &structDecl) {
  llvm::StructType::create(context, "struct." + structDecl.identifier);
}

void Codegen::generateStructDefinition(const res::StructDecl &structDecl) {
  auto *type = static_cast<llvm::StructType *>(
      generateType(resolvedTree->getType(&structDecl)));

  std::vector<llvm::Type *> fieldTypes;
  for (auto &&field : structDecl.fields)
    fieldTypes.emplace_back(generateType(resolvedTree->getType(field)));

  type->setBody(fieldTypes);
}

llvm::Module *Codegen::generateIR() {
  for (auto &&sd : resolvedTree->getStructs())
    generateStructDecl(*sd);
  for (auto &&sd : resolvedTree->getStructs())
    generateStructDefinition(*sd);

  for (auto &&fn : resolvedTree->getFunctions())
    generateFunctionDecl(*fn);
  for (auto &&fn : resolvedTree->getFunctions())
    generateFunctionBody(*fn);

  generateMainWrapper();

  return &module;
}
} // namespace yl
