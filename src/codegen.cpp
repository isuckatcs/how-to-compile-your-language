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
    if (type->getAs<res::BuiltinUnitType>()) {
      mangledName << 'u';
    } else if (type->getAs<res::BuiltinNumberType>()) {
      mangledName << 'n';
    } else if (const auto *s = type->getAs<res::StructType>()) {
      const auto &id = s->getDecl()->identifier;
      mangledName << 'S' << id.size() << id
                  << mangleGenericArgs(s->getTypeArgs());
    } else if (const auto *f = type->getAs<res::FunctionType>()) {
      mangledName << 'F';
      for (auto &&arg : f->getArgs())
        mangledName << mangleType(arg);
      mangledName << 'R' << mangleType(f->getReturnType());
    } else {
      llvm_unreachable("unexpected type in mangling");
    }

    return mangledName.str();
  }

  static std::string
  mangleGenericArgs(const std::vector<const res::Type *> &genericArgs) {
    if (genericArgs.empty())
      return "";

    std::stringstream mangledName;

    mangledName << 'G';
    for (auto &&type : genericArgs)
      mangledName << mangleType(type);
    mangledName << 'E';

    return mangledName.str();
  }

public:
  static std::string
  mangleFunction(const res::FunctionDecl *fn,
                 const std::vector<const res::Type *> &genericArgs) {
    std::stringstream mangledName;

    const auto &identifier = fn->identifier;
    if (fn->isGeneric())
      mangledName << '_' << 'Y' << identifier.size();
    mangledName << identifier << mangleGenericArgs(genericArgs);
    return mangledName.str();
  }
};
} // namespace

Codegen::Codegen(const res::Context &resolvedCtx, std::string_view sourcePath)
    : resCtx(&resolvedCtx),
      builder(context),
      module("<translation_unit>", context) {
  module.setSourceFileName(sourcePath);
  module.setTargetTriple(llvm::sys::getDefaultTargetTriple());
}

llvm::Type *Codegen::generateType(const res::Type *type) {
  type = type->getRootType();

  if (type->getAs<res::BuiltinNumberType>())
    return builder.getDoubleTy();

  if (type->getAs<res::BuiltinUnitType>())
    return builder.getVoidTy();

  if (const auto *s = type->getAs<res::StructType>())
    return generateStructType(s);

  if (type->getAs<res::FunctionType>())
    return llvm::PointerType::get(context, 0);

  if (const auto *typeParamTy = type->getAs<res::TypeParamType>()) {
    if (instantiations.empty())
      assert(false && "type param found outside an instantiation");

    size_t idx = typeParamTy->decl->index;

    const auto &currentInstantiation = instantiations.top();
    if (idx >= currentInstantiation.size())
      assert(false && "type argument is not in the current instantiation");

    return currentInstantiation[idx];
  }

  llvm_unreachable("unexpected type encountered");
}

llvm::FunctionType *
Codegen::generateFunctionType(const res::FunctionType *type) {
  std::vector<llvm::Type *> args;

  llvm::Type *res = generateType(type->getReturnType());
  if (res->isStructTy()) {
    args.emplace_back(llvm::PointerType::get(context, 0));
    res = builder.getVoidTy();
  }

  for (auto &&arg : type->getArgs()) {
    llvm::Type *argTy = generateType(arg);
    if (argTy->isVoidTy())
      continue;

    args.emplace_back(argTy->isStructTy() ? llvm::PointerType::get(context, 0)
                                          : argTy);
  }

  return llvm::FunctionType::get(res, args, false);
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
  llvm::Type *declTy = generateType(resCtx->getType(decl));

  const res::Expr *initExpr = decl->initializer;
  llvm::Value *initVal =
      initExpr ? generateExprAndLoadValue(*initExpr) : nullptr;

  if (declTy->isVoidTy())
    return nullptr;

  llvm::AllocaInst *var = allocateStackVariable(decl->identifier, declTy);
  if (initExpr)
    storeValue(initVal, var, declTy);

  declarations[decl] = var;
  return nullptr;
}

llvm::Value *Codegen::generateAssignment(const res::Assignment &stmt) {
  llvm::Value *val = generateExprAndLoadValue(*stmt.expr);
  return storeValue(val, generateExpr(*stmt.assignee),
                    generateType(resCtx->getType(stmt.assignee)));
}

llvm::Value *Codegen::generateReturnStmt(const res::ReturnStmt &stmt) {
  if (stmt.expr)
    storeValue(generateExprAndLoadValue(*stmt.expr), retVal,
               generateType(resCtx->getType(stmt.expr)));

  assert(retBB && "function with return stmt doesn't have a return block");
  breakIntoBB(retBB);
  return nullptr;
}

llvm::Value *Codegen::generateMemberExpr(const res::MemberExpr &memberExpr) {
  llvm::Value *base = generateExpr(*memberExpr.base);

  const auto *structTy =
      resCtx->getType(memberExpr.base)->getAs<res::StructType>();
  llvm::Type *baseTy = generateType(structTy);

  if (generateType(resCtx->getType(&memberExpr))->isVoidTy())
    return nullptr;

  unsigned index = 0;
  EnterInstantiationRAII structInst(this, structTy->getTypeArgs());
  for (auto &&field : structTy->getDecl()->fields) {
    if (field == memberExpr.field)
      break;

    if (!generateType(resCtx->getType(field))->isVoidTy())
      ++index;
  }

  return builder.CreateStructGEP(baseTy, base, index);
}

llvm::Value *
Codegen::generateTemporaryStruct(const res::StructInstantiationExpr &sie) {
  std::map<const res::FieldDecl *, std::pair<llvm::Value *, llvm::Type *>>
      fieldInits;

  for (auto &&initStmt : sie.fieldInitializers) {
    llvm::Value *init = generateExprAndLoadValue(*initStmt->initializer);
    llvm::Type *ty = generateType(resCtx->getType(initStmt->initializer));

    if (!ty->isVoidTy())
      fieldInits[initStmt->field] = {init, ty};
  }

  if (fieldInits.empty())
    return nullptr;

  const auto *structTy =
      resCtx->getType(sie.structDecl)->getAs<res::StructType>();
  llvm::Type *type = generateType(structTy);
  llvm::Value *tmp =
      allocateStackVariable(sie.structDecl->decl->identifier + ".tmp", type);

  unsigned idx = 0;
  for (auto &&fieldDecl : structTy->getDecl()->fields) {
    if (!fieldInits.count(fieldDecl))
      continue;

    const auto &[init, ty] = fieldInits[fieldDecl];
    llvm::Value *field = builder.CreateStructGEP(type, tmp, idx++);
    storeValue(init, field, ty);
  }

  return tmp;
}

llvm::Value *Codegen::generateExpr(const res::Expr &expr) {
  if (auto *number = dynamic_cast<const res::NumberLiteral *>(&expr))
    return llvm::ConstantFP::get(builder.getDoubleTy(), number->value);

  if (auto *unit = dynamic_cast<const res::UnitLiteral *>(&expr))
    return nullptr;

  if (auto val = expr.getConstantValue())
    return llvm::ConstantFP::get(builder.getDoubleTy(), *val);

  if (auto *dre = dynamic_cast<const res::DeclRefExpr *>(&expr))
    return generateDeclRefExpr(*dre);

  if (auto *call = dynamic_cast<const res::CallExpr *>(&expr))
    return generateCallExpr(*call);

  if (auto *grouping = dynamic_cast<const res::GroupingExpr *>(&expr))
    return generateExpr(*grouping->expr);

  if (auto *binop = dynamic_cast<const res::BinaryOperator *>(&expr))
    return generateBinaryOperator(*binop);

  if (auto *unop = dynamic_cast<const res::UnaryOperator *>(&expr))
    return generateUnaryOperator(*unop);

  if (auto *me = dynamic_cast<const res::MemberExpr *>(&expr))
    return generateMemberExpr(*me);

  if (auto *sie = dynamic_cast<const res::StructInstantiationExpr *>(&expr))
    return generateTemporaryStruct(*sie);

  llvm_unreachable("unexpected expression");
}

llvm::Value *Codegen::generateDeclRefExpr(const res::DeclRefExpr &dre) {
  const res::FunctionDecl *fnDecl = dre.decl->getAs<res::FunctionDecl>();
  if (!fnDecl)
    return declarations[dre.decl];

  return generateFunctionDecl(*fnDecl,
                              resCtx->getType(&dre)->getAs<res::FunctionType>(),
                              dre.getTypeArgs());
}

llvm::Value *Codegen::generateCallExpr(const res::CallExpr &call) {
  llvm::Type *retTy = generateType(resCtx->getType(&call));
  llvm::Value *retVal = nullptr;
  std::vector<llvm::Value *> args;

  bool isReturningStruct = retTy->isStructTy();
  if (isReturningStruct)
    retVal = args.emplace_back(allocateStackVariable("struct.ret.tmp", retTy));

  size_t argIdx = 0;
  for (auto &&arg : call.arguments) {
    llvm::Value *argVal = generateExprAndLoadValue(*arg);
    llvm::Type *argTy = generateType(resCtx->getType(arg));

    if (argTy->isVoidTy())
      continue;

    if (argTy->isStructTy()) {
      llvm::Value *tmpVar = allocateStackVariable("struct.arg.tmp", argTy);
      storeValue(argVal, tmpVar, argTy);
      argVal = tmpVar;
    }

    args.emplace_back(argVal);
    ++argIdx;
  }

  const auto *fnTy = resCtx->getType(call.callee)->getAs<res::FunctionType>();

  llvm::Value *callee = generateExprAndLoadValue(*call.callee);
  llvm::CallInst *callInst =
      builder.CreateCall(generateFunctionType(fnTy), callee, args);
  callInst->setAttributes(constructAttrList(fnTy));

  return isReturningStruct ? retVal : callInst;
}

llvm::Value *Codegen::generateUnaryOperator(const res::UnaryOperator &unop) {
  llvm::Value *rhs = generateExprAndLoadValue(*unop.operand);

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
    llvm::Value *rhs = doubleToBool(generateExprAndLoadValue(*binop.rhs));

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

  llvm::Value *lhs = generateExprAndLoadValue(*binop.lhs);
  llvm::Value *rhs = generateExprAndLoadValue(*binop.rhs);

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

llvm::Value *Codegen::generateExprAndLoadValue(const res::Expr &expr) {
  llvm::Value *val = generateExpr(expr);
  if (!val)
    return nullptr;

  llvm::Type *type = generateType(resCtx->getType(&expr));
  if (!expr.isLvalue() || type->isStructTy() || llvm::isa<llvm::Argument>(val))
    return val;

  return builder.CreateLoad(type, val);
}

llvm::Value *
Codegen::storeValue(llvm::Value *val, llvm::Value *ptr, llvm::Type *type) {
  if (type->isVoidTy())
    return nullptr;

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

llvm::AttributeList Codegen::constructAttrList(const res::FunctionType *ty) {
  std::vector<llvm::AttributeSet> argsAttrSets;

  if (llvm::Type *retTy = generateType(ty->getReturnType());
      retTy && retTy->isStructTy()) {
    llvm::AttrBuilder retAttrs(context);
    retAttrs.addStructRetAttr(retTy);
    argsAttrSets.emplace_back(llvm::AttributeSet::get(context, retAttrs));
  }

  for (auto &&argTy : ty->getArgs()) {
    llvm::Type *llvmTy = generateType(argTy);
    if (llvmTy->isVoidTy())
      continue;

    llvm::AttrBuilder paramAttrs(context);
    if (llvmTy->isStructTy())
      paramAttrs.addByValAttr(llvmTy);

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

void Codegen::generateFunctionBody(const PendingFunctionDescriptor &fn) {
  auto [instantiation, mangledName, functionDecl] = fn;
  EnterInstantiationRAII fnInstantiation(this, instantiation);

  llvm::Function *function = module.getFunction(mangledName);
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

  bool isReturningStruct =
      !function->arg_empty() && function->getArg(0)->hasStructRetAttr();

  unsigned nonVoidArgIdx = 0;
  if (isReturningStruct) {
    llvm::Argument *sretArg = function->getArg(0);

    sretArg->setName("ret");
    retVal = sretArg;

    ++nonVoidArgIdx;
  }

  for (auto &&paramDecl : functionDecl->params) {
    llvm::Type *argTy = generateType(resCtx->getType(paramDecl));
    if (argTy->isVoidTy())
      continue;

    llvm::Argument *arg = function->getArg(nonVoidArgIdx);
    arg->setName(paramDecl->identifier);

    llvm::Value *argVal;
    if (!paramDecl->isMutable || arg->hasByValAttr()) {
      argVal = arg;
    } else {
      argVal = allocateStackVariable(paramDecl->identifier, arg->getType());
      storeValue(arg, argVal, arg->getType());
    }

    declarations[paramDecl] = argVal;
    ++nonVoidArgIdx;
  }

  if (functionDecl->identifier == "println")
    generateBuiltinPrintlnBody(*functionDecl);
  else
    generateBlock(*functionDecl->body);

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
    builder.CreateRet(builder.CreateLoad(returnTy, retVal));
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

llvm::Function *
Codegen::generateFunctionDecl(const res::FunctionDecl &fn,
                              const res::FunctionType *type,
                              const std::vector<const res::Type *> &typeArgs) {
  std::string name = Mangling::mangleFunction(&fn, typeArgs);
  if (auto *function = module.getFunction(name))
    return function;

  EnterInstantiationRAII fnInstantiation(this, typeArgs);

  llvm::FunctionType *fnTy = generateFunctionType(type);
  auto *function = llvm::Function::Create(fnTy, llvm::Function::ExternalLinkage,
                                          name, module);
  function->setAttributes(constructAttrList(type));

  pendingFunctions.push({typeArgs, name, &fn});
  return function;
}

llvm::Type *Codegen::generateStructType(const res::StructType *structTy) {
  EnterInstantiationRAII structInstantiation(this, structTy->getTypeArgs());

  std::vector<llvm::Type *> fieldTypes;
  for (auto &&field : structTy->getDecl()->fields) {
    llvm::Type *fieldTy = generateType(resCtx->getType(field));
    if (fieldTy->isVoidTy())
      continue;

    fieldTypes.emplace_back(fieldTy);
  }

  if (fieldTypes.empty())
    return builder.getVoidTy();

  return llvm::StructType::get(context, fieldTypes);
}

llvm::Module *Codegen::generateIR() {
  for (auto &&fn : resCtx->getFunctions())
    if (!fn->isGeneric())
      generateFunctionDecl(*fn, resCtx->getType(fn)->getAs<res::FunctionType>(),
                           {});

  while (!pendingFunctions.empty()) {
    generateFunctionBody(pendingFunctions.front());
    pendingFunctions.pop();
  }

  generateMainWrapper();
  return &module;
}
} // namespace yl
