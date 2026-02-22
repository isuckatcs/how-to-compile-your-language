#include <iostream>
#include <sstream>

#include "lexer.h"
#include "res.h"
#include "utils.h"

namespace yl {
namespace res {
void Block::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "Block\n";

  for (auto &&stmt : statements)
    stmt->dump(ctx, level + 1);
}

void IfStmt::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "IfStmt\n";

  condition->dump(ctx, level + 1);
  trueBlock->dump(ctx, level + 1);
  if (falseBlock)
    falseBlock->dump(ctx, level + 1);
}

void WhileStmt::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "WhileStmt\n";

  condition->dump(ctx, level + 1);
  body->dump(ctx, level + 1);
}

void ParamDecl::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "ParamDecl @(" << this << ") " << identifier
            << " {" << ctx.getType(this)->getName() << '}' << '\n';
}

void FieldDecl::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "FieldDecl @(" << this << ") " << identifier
            << " {" << ctx.getType(this)->getName() << '}' << '\n';
}

void VarDecl::dump(const Context &ctx, size_t level) const {
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

void FunctionDecl::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "FunctionDecl @(" << this << ") " << identifier
            << (!isComplete ? " [incomplete]" : "") << " {"
            << ctx.getType(this)->getName() << '}' << '\n';

  for (auto &&typeParam : typeParams)
    typeParam->dump(ctx, level + 1);

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

void StructDecl::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "StructDecl @(" << this << ") " << identifier
            << (!isComplete ? " [incomplete]" : "") << " {"
            << ctx.getType(this)->getName() << '}' << '\n';

  for (auto &&typeParam : typeParams)
    typeParam->dump(ctx, level + 1);

  for (auto &&field : fields)
    field->dump(ctx, level + 1);
}

void TypeParamDecl::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "TypeParamDecl @(" << this << ") " << identifier
            << '\n';
}

void NumberLiteral::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "NumberLiteral '" << value << "' {"
            << ctx.getType(this)->getName() << '}' << '\n';

  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';
}

void UnitLiteral::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "UnitLiteral {" << ctx.getType(this)->getName()
            << '}' << '\n';
}

void DeclRefExpr::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "DeclRefExpr @(" << decl << ") "
            << decl->identifier;

  std::cerr << " {" << ctx.getType(this)->getName() << '}' << '\n';

  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';
}

void CallExpr::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "CallExpr"
            << " {" << ctx.getType(this)->getName() << '}' << '\n';

  callee->dump(ctx, level + 1);

  for (auto &&arg : arguments)
    arg->dump(ctx, level + 1);
}

void MemberExpr::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "MemberExpr @(" << field << ')' << ' '
            << field->identifier << " {" << ctx.getType(this)->getName() << '}'
            << '\n';

  base->dump(ctx, level + 1);
}

void GroupingExpr::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "GroupingExpr"
            << " {" << ctx.getType(this)->getName() << '}' << '\n';

  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';

  expr->dump(ctx, level + 1);
}

void BinaryOperator::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "BinaryOperator '" << getOpStr(op) << '\''
            << " {" << ctx.getType(this)->getName() << '}' << '\n';

  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';

  lhs->dump(ctx, level + 1);
  rhs->dump(ctx, level + 1);
}

void UnaryOperator::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "UnaryOperator '" << getOpStr(op) << '\''
            << " {" << ctx.getType(this)->getName() << '}' << '\n';

  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';

  operand->dump(ctx, level + 1);
}

void DeclStmt::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "DeclStmt\n";

  varDecl->dump(ctx, level + 1);
}

void Assignment::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "Assignment\n";

  assignee->dump(ctx, level + 1);
  expr->dump(ctx, level + 1);
}

void ReturnStmt::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "ReturnStmt\n";

  if (expr)
    expr->dump(ctx, level + 1);
}

void FieldInitStmt::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "FieldInitStmt @(" << field << ')' << ' '
            << field->identifier << '\n';

  initializer->dump(ctx, level + 1);
}

void StructInstantiationExpr::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "StructInstantiationExpr"
            << " {" << ctx.getType(this)->getName() << '}' << '\n';

  structDecl->dump(ctx, level + 1);

  for (auto &&field : fieldInitializers)
    field->dump(ctx, level + 1);
}

void ImplicitDerefExpr::dump(const Context &ctx, size_t level) const {
  std::cerr << indent(level) << "ImplicitDerefExpr"
            << " {" << ctx.getType(this)->getName() << '}' << '\n';

  outParamRef->dump(ctx, level + 1);
}

std::string FunctionType::getName() const {
  std::stringstream ss;
  ss << '(';
  for (int i = 0; i < args.size() - 1; ++i) {
    ss << args[i]->getRootType()->getName();

    if (i < args.size() - 2)
      ss << ',' << ' ';
  }
  ss << ") -> " << getReturnType()->getName();

  return ss.str();
}

std::string StructType::getName() const {
  std::stringstream ss;
  ss << decl->identifier;

  if (!args.empty()) {
    ss << '<';
    for (int i = 0; i < args.size(); ++i) {
      ss << args[i]->getName();

      if (i < args.size() - 1)
        ss << ',' << ' ';
    }
    ss << '>';
  }

  return ss.str();
}

UninferredType *Context::getNewUninferredType() {
  auto *typeVariable =
      new UninferredType("t" + std::to_string(typeVariableCount++));
  types.emplace_back(std::unique_ptr<UninferredType>());
  return typeVariable;
}

FunctionType *Context::getFunctionType(std::vector<Type *> args, Type *ret) {
  args.emplace_back(ret);
  auto *fnTy = new FunctionType(std::move(args));
  types.emplace_back(std::unique_ptr<FunctionType>(fnTy));
  return fnTy;
}

StructType *Context::getStructType(const res::StructDecl &decl,
                                   std::vector<Type *> typeArgs) {
  auto *structTy = new StructType(decl, std::move(typeArgs));
  types.emplace_back(std::unique_ptr<StructType>(structTy));
  return structTy;
}

TypeParamType *Context::getTypeParamType(const TypeParamDecl &decl) {
  return typeParamTys
      .try_emplace(&decl,
                   std::unique_ptr<TypeParamType>(new TypeParamType(decl)))
      .first->second.get();
}

PointerType *Context::getPointerType(Type *pointeeType) {
  auto *ptrTy = new PointerType(pointeeType);
  types.emplace_back(std::unique_ptr<PointerType>(ptrTy));
  return ptrTy;
}

bool Context::unify(Type *t1, Type *t2) {
  t1 = t1->getRootType();
  t2 = t2->getRootType();

  if (t1 == t2)
    return true;

  if (auto *u = t1->getAs<UninferredType>()) {
    if (auto *ptrTy = t2->getAs<PointerType>()) {
      u->infer(ptrTy->getPointeeType());
      return false;
    }

    u->infer(t2);
    return true;
  }

  if (t2->getAs<UninferredType>())
    return unify(t2, t1);

  if (t1->name != t2->name || t1->args.size() != t2->args.size())
    return false;

  for (size_t i = 0; i < t1->args.size(); ++i)
    if (!unify(t1->args[i], t2->args[i]))
      return false;

  return true;
}

Context::SubstitutionTy Context::createSubstitution(const Decl *decl) {
  size_t typeParamCnt = 0;
  if (auto *fnDecl = decl->getAs<FunctionDecl>())
    typeParamCnt = fnDecl->typeParams.size();

  if (auto *structDecl = decl->getAs<StructDecl>())
    typeParamCnt = structDecl->typeParams.size();

  std::vector<res::Type *> substitution(typeParamCnt);
  for (size_t i = 0; i < typeParamCnt; ++i)
    substitution[i] = getNewUninferredType();

  return substitution;
}

Type *Context::instantiate(Type *t, const SubstitutionTy &substitution) {
  if (auto *typeParamTy = t->getAs<TypeParamType>())
    return substitution[typeParamTy->decl->index];

  Type *instTy = t;
  if (auto *fnTy = t->getAs<FunctionType>())
    instTy = getFunctionType(fnTy->getArgs(), fnTy->getReturnType());

  if (auto *s = t->getAs<StructType>())
    instTy = getStructType(*s->getDecl(), s->getTypeArgs());

  for (auto &arg : instTy->args)
    arg = instantiate(arg, substitution);

  return instTy;
}

void Context::dump() {
  for (auto &&decl : structs)
    decl->dump(*this, 0);

  for (auto &&decl : functions)
    decl->dump(*this, 0);
}
} // namespace res
} // namespace yl
