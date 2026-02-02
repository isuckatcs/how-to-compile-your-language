#include <iostream>
#include <sstream>

#include "lexer.h"
#include "res.h"
#include "utils.h"

namespace yl {
namespace res {
bool Type::isBuiltinVoid() const {
  auto *builtinTy = getAs<BuiltinType>();
  return builtinTy && builtinTy->isVoid();
}

bool Type::isBuiltinNumber() const {
  auto *builtinTy = getAs<BuiltinType>();
  return builtinTy && builtinTy->isNumber();
}

void Block::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "Block\n";

  for (auto &&stmt : statements)
    stmt->dump(ctx, level + 1);
}

void IfStmt::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "IfStmt\n";

  condition->dump(ctx, level + 1);
  trueBlock->dump(ctx, level + 1);
  if (falseBlock)
    falseBlock->dump(ctx, level + 1);
}

void WhileStmt::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "WhileStmt\n";

  condition->dump(ctx, level + 1);
  body->dump(ctx, level + 1);
}

void ParamDecl::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "ParamDecl @(" << this << ") " << identifier
            << " {" << ctx.getType(this)->getName() << '}' << '\n';
}

void FieldDecl::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "FieldDecl @(" << this << ") " << identifier
            << " {" << ctx.getType(this)->getName() << '}' << '\n';
}

void VarDecl::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "VarDecl @(" << this << ") " << identifier
            << " {" << ctx.getType(this)->getName() << '}' << '\n';

  if (initializer)
    initializer->dump(ctx, level + 1);
}

void FunctionDecl::setBody(Block *body) {
  assert(!isComplete && "setting body on an already complete function");
  assert(body && "function body cannot be null");

  this->body = body;
  isComplete = true;
}

void FunctionDecl::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "FunctionDecl @(" << this << ") " << identifier
            << (!isComplete ? " [incomplete]" : "") << " {"
            << ctx.getType(this)->getName() << '}' << '\n';

  for (auto &&typeArg : typeArguments)
    typeArg->dump(ctx, level + 1);

  for (auto &&param : params)
    param->dump(ctx, level + 1);

  if (body)
    body->dump(ctx, level + 1);
}

void StructDecl::setFields(std::vector<FieldDecl *> fields) {
  assert(!isComplete && "setting fields on already complete struct");

  this->fields = std::move(fields);
  isComplete = true;
}

void StructDecl::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "StructDecl @(" << this << ") " << identifier
            << (!isComplete ? " [incomplete]" : "") << " {"
            << ctx.getType(this)->getName() << '}' << '\n';

  for (auto &&typeArg : typeArguments)
    typeArg->dump(ctx, level + 1);

  for (auto &&field : fields)
    field->dump(ctx, level + 1);
}

void TypeArgumentDecl::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "TypeArgumentDecl @(" << this << ") "
            << identifier << '\n';
}

void NumberLiteral::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "NumberLiteral '" << value << "' {"
            << ctx.getType(this)->getName() << '}' << '\n';

  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';
}

void DeclRefExpr::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "DeclRefExpr @(" << decl << ") "
            << decl->identifier;
  if (!typeArgs.empty()) {
    std::cerr << '<';
    for (int i = 0; i < typeArgs.size(); ++i) {
      std::cerr << typeArgs[i]->getRootType()->getName();

      if (i < typeArgs.size() - 1)
        std::cerr << ',';
    }
    std::cerr << '>';
  }

  std::cerr << " {" << ctx.getType(this)->getName() << '}' << '\n';

  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';
}

void CallExpr::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "CallExpr"
            << " {" << ctx.getType(this)->getName() << '}' << '\n';

  callee->dump(ctx, level + 1);

  for (auto &&arg : arguments)
    arg->dump(ctx, level + 1);
}

void MemberExpr::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "MemberExpr @(" << field << ')' << ' '
            << field->identifier << " {" << ctx.getType(this)->getName() << '}'
            << '\n';

  base->dump(ctx, level + 1);
}

void GroupingExpr::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "GroupingExpr"
            << " {" << ctx.getType(this)->getName() << '}' << '\n';

  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';

  expr->dump(ctx, level + 1);
}

void BinaryOperator::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "BinaryOperator '" << getOpStr(op) << '\''
            << " {" << ctx.getType(this)->getName() << '}' << '\n';

  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';

  lhs->dump(ctx, level + 1);
  rhs->dump(ctx, level + 1);
}

void UnaryOperator::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "UnaryOperator '" << getOpStr(op) << '\''
            << " {" << ctx.getType(this)->getName() << '}' << '\n';

  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';

  operand->dump(ctx, level + 1);
}

void DeclStmt::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "DeclStmt\n";

  varDecl->dump(ctx, level + 1);
}

void Assignment::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "Assignment\n";

  assignee->dump(ctx, level + 1);
  expr->dump(ctx, level + 1);
}

void ReturnStmt::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "ReturnStmt\n";

  if (expr)
    expr->dump(ctx, level + 1);
}

void FieldInitStmt::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "FieldInitStmt @(" << field << ')' << ' '
            << field->identifier << '\n';

  initializer->dump(ctx, level + 1);
}

void StructInstantiationExpr::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "StructInstantiationExpr"
            << " {" << ctx.getType(this)->getName() << '}' << '\n';

  structDecl->dump(ctx, level + 1);

  for (auto &&field : fieldInitializers)
    field->dump(ctx, level + 1);
}

std::string FunctionType::getName() const {
  std::stringstream ss;
  ss << '(';
  for (int i = 0; i < args.size(); ++i) {
    ss << args[i]->getRootType()->getName();

    if (i < args.size() - 1)
      ss << ',';
  }
  ss << ") -> " << ret->getRootType()->getName();

  return ss.str();
}

std::string StructType::getName() const {
  std::stringstream ss;
  ss << decl->identifier;

  if (!typeArgs.empty()) {
    ss << '<';
    for (int i = 0; i < typeArgs.size(); ++i) {
      ss << typeArgs[i]->getName();

      if (i < typeArgs.size() - 1)
        ss << ',';
    }
    ss << '>';
  }

  return ss.str();
}

UninferredType *Context::getNewUninferredType() {
  return typeVariables
      .emplace_back(std::unique_ptr<UninferredType>(
          new UninferredType(typeVariables.size())))
      .get();
}

BuiltinType *Context::getBuiltinType(const BuiltinType::Kind kind) {
  switch (kind) {
  case BuiltinType::Kind::Void:
    return voidTy.get();
  case BuiltinType::Kind::Number:
    return numberTy.get();
  }
}

FunctionType *Context::getUninferredFunctionType(size_t argCount) {
  std::vector<Type *> args;
  for (size_t i = 0; i < argCount; ++i)
    args.emplace_back(getNewUninferredType());

  return functionTys
      .emplace_back(std::unique_ptr<FunctionType>(
          new FunctionType(args, getNewUninferredType())))
      .get();
}

StructType *Context::getUninferredStructType(const res::StructDecl &decl) {
  std::vector<Type *> types;
  for (size_t i = 0; i < decl.typeArguments.size(); ++i)
    types.emplace_back(getNewUninferredType());

  return structTys
      .emplace_back(
          std::unique_ptr<StructType>(new StructType(decl, std::move(types))))
      .get();
}

TypeArgumentType *Context::getTypeArgumentType(const TypeArgumentDecl &decl) {
  return typeArgTys
      .try_emplace(
          &decl, std::unique_ptr<TypeArgumentType>(new TypeArgumentType(decl)))
      .first->second.get();
}

bool Context::unify(Type *t1, Type *t2) {
  t1 = t1->getRootType();
  t2 = t2->getRootType();

  if (auto *u = t1->getAs<UninferredType>()) {
    u->infer(t2);
    return true;
  }

  if (t2->getAs<UninferredType>())
    return unify(t2, t1);

  // FIXME: is there a way to unify these similar to HM monotypes?
  if (auto *fn1 = t1->getAs<FunctionType>()) {
    auto *fn2 = t2->getAs<FunctionType>();
    if (!fn2)
      return false;

    if (fn1->getArgCount() != fn2->getArgCount())
      return false;

    for (int i = 0; i < fn1->getArgCount(); ++i)
      if (!unify(fn1->getArgType(i), fn2->getArgType(i)))
        return false;

    return unify(fn1->getReturnType(), fn2->getReturnType());
  }

  if (auto *s1 = t1->getAs<StructType>()) {
    auto *s2 = t2->getAs<StructType>();
    if (!s2)
      return false;

    if (s1->decl != s2->decl)
      return false;

    if (s1->typeArgs.size() != s2->typeArgs.size())
      return false;

    for (size_t i = 0; i < s1->typeArgs.size(); ++i)
      if (!unify(s1->typeArgs[i], s2->typeArgs[i]))
        return false;

    return true;
  }

  return t1 == t2;
}

std::vector<res::Type *> Context::createInstantiation(const Decl *decl) {
  size_t typeArgsCnt = 0;
  if (auto *fnDecl = decl->getAs<FunctionDecl>())
    typeArgsCnt = fnDecl->typeArguments.size();

  if (auto *structDecl = decl->getAs<StructDecl>())
    typeArgsCnt = structDecl->typeArguments.size();

  std::vector<res::Type *> instantiation(typeArgsCnt);
  for (size_t i = 0; i < typeArgsCnt; ++i)
    instantiation[i] = getNewUninferredType();

  return instantiation;
}

Type *Context::instantiate(Type *t,
                           const std::vector<res::Type *> &instantiation) {
  t = t->getRootType();

  if (auto *fnTy = t->getAs<FunctionType>()) {
    auto *instantiatedTy = getUninferredFunctionType(fnTy->getArgCount());

    for (size_t i = 0; i < fnTy->getArgCount(); ++i)
      unify(instantiatedTy->getArgType(i),
            instantiate(fnTy->getArgType(i), instantiation));

    unify(instantiatedTy->getReturnType(),
          instantiate(fnTy->getReturnType(), instantiation));
    return instantiatedTy;
  }

  if (auto *structTy = t->getAs<StructType>()) {
    std::vector<Type *> tyArgs = structTy->getTypeArgs();

    auto *instantiatedTy = getUninferredStructType(*structTy->getDecl());
    std::vector<Type *> instantiatedTyArgs = instantiatedTy->getTypeArgs();

    for (size_t i = 0; i < tyArgs.size(); ++i)
      unify(instantiatedTyArgs[i], instantiate(tyArgs[i], instantiation));

    return instantiatedTy;
  }

  if (auto *typeArgTy = t->getAs<TypeArgumentType>())
    return instantiation[typeArgTy->decl->index];

  return t;
}

void Context::dump() {
  for (auto &&decl : structs)
    decl->dump(*this, 0);

  for (auto &&decl : functions)
    decl->dump(*this, 0);
}
} // namespace res
} // namespace yl
