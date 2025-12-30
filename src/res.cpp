#include <iostream>
#include <sstream>

#include "lexer.h"
#include "res.h"
#include "utils.h"

#define printType(node)                                                        \
  if (auto *ty = ctx.getType(node))                                            \
    std::cerr << ' ' << '{' << ty->asString() << '}';

namespace yl {
namespace res {
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
  std::cerr << indent(level) << "ParamDecl @(" << this << ") " << identifier;
  printType(this);
  std::cerr << '\n';
}

void FieldDecl::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "FieldDecl @(" << this << ") " << identifier;
  printType(this);
  std::cerr << '\n';
}

void VarDecl::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "VarDecl @(" << this << ") " << identifier;
  printType(this);
  std::cerr << '\n';

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
            << (!isComplete ? " [incomplete]" : "");
  printType(this);
  std::cerr << '\n';

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
            << (!isComplete ? " [incomplete]" : "");
  printType(this);
  std::cerr << '\n';

  for (auto &&field : fields)
    field->dump(ctx, level + 1);
}

void NumberLiteral::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "NumberLiteral '" << value << '\'';
  printType(this);
  std::cerr << '\n';

  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';
}

void DeclRefExpr::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "DeclRefExpr @(" << decl << ") "
            << decl->identifier;
  printType(this);
  std::cerr << '\n';

  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';
}

void CallExpr::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "CallExpr";
  printType(this);
  std::cerr << '\n';

  callee->dump(ctx, level + 1);

  for (auto &&arg : arguments)
    arg->dump(ctx, level + 1);
}

void MemberExpr::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "MemberExpr @(" << field << ')' << ' '
            << field->identifier;
  printType(this);
  std::cerr << '\n';

  base->dump(ctx, level + 1);
}

void GroupingExpr::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "GroupingExpr";
  printType(this);
  std::cerr << '\n';

  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';

  expr->dump(ctx, level + 1);
}

void BinaryOperator::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "BinaryOperator '" << getOpStr(op) << '\'';
  printType(this);
  std::cerr << '\n';

  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';

  lhs->dump(ctx, level + 1);
  rhs->dump(ctx, level + 1);
}

void UnaryOperator::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "UnaryOperator '" << getOpStr(op) << '\'';
  printType(this);
  std::cerr << '\n';

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
  std::cerr << indent(level) << "StructInstantiationExpr @(" << structDecl
            << ')';
  printType(this);
  std::cerr << '\n';

  for (auto &&field : fieldInitializers)
    field->dump(ctx, level + 1);
}

bool UninferredType::operator==(const Type &b) const {
  const auto *uninferred = dynamic_cast<const UninferredType *>(&b);

  return uninferred && uninferred->id == id;
}

std::string UninferredType::asString() const {
  return "T" + std::to_string(id);
}

void UninferredType::dump() const {
  std::cerr << "UninferredType " << asString() << '\n';
}

bool BuiltinType::operator==(const Type &b) const {
  const auto *builtin = dynamic_cast<const BuiltinType *>(&b);

  return builtin && builtin->kind == kind;
}

std::string BuiltinType::asString() const {
  return kind == BuiltinType::Kind::Number ? "number" : "void";
}

void BuiltinType::dump() const {
  std::cerr << "BuiltinType " << asString() << '\n';
}

bool StructType::operator==(const Type &b) const {
  const auto *st = dynamic_cast<const StructType *>(&b);

  return st && st->decl == decl;
}

std::string StructType::asString() const { return decl->identifier; }

void StructType::dump() const {
  std::cerr << "StructType " << asString() << '\n';
}

bool FunctionType::operator==(const Type &b) const {
  const auto *function = dynamic_cast<const FunctionType *>(&b);

  if (!function || function->args.size() != args.size() || function->ret != ret)
    return false;

  for (size_t i = 0; i < args.size(); ++i) {
    if (function->args[i] != args[i])
      return false;
  }

  return true;
}

std::string FunctionType::asString() const {
  std::stringstream ss;
  ss << '(';
  for (int i = 0; i < args.size(); ++i) {
    ss << args[i]->asString();

    if (i < args.size() - 1)
      ss << ',';
  }
  ss << ") -> " << ret->asString();

  return ss.str();
}

void FunctionType::dump() const {
  std::cerr << "FunctionType " << asString() << '\n';
}

void Context::replace(const Type *t1, const Type *t2) {
  for (auto &m : environment)
    if (m.second == t1)
      m.second = t2;

  for (auto &type : types) {
    if (auto *fn = dynamic_cast<FunctionType *>(type.get())) {
      for (int i = 0; i < fn->args.size(); ++i)
        if (fn->args[i] == t1)
          fn->args[i] = t2;

      if (fn->ret == t1)
        fn->ret = t2;
    }
  }

  auto it = types.begin();
  while (it != types.end() && it->get() != t1)
    ++it;
  if (it != types.end())
    types.erase(it);
}

const Type *Context::getNewUninferredType() {
  return getType<UninferredType>(uninferredTypeIdx++);
}

const Type *Context::getBuiltinType(const BuiltinType::Kind kind) {
  return getType<BuiltinType>(kind);
}

const Type *Context::getFunctionType(std::vector<const Type *> args,
                                     const Type *ret) {
  return getType<FunctionType>(std::move(args), ret);
}

const Type *Context::getStructType(const res::StructDecl &decl) {
  return getType<StructType>(decl);
}

bool Context::unify(const Type *t1, const Type *t2) {
  if (dynamic_cast<const UninferredType *>(t1)) {
    replace(t1, t2);
    return true;
  }

  if (dynamic_cast<const UninferredType *>(t2))
    return unify(t2, t1);

  if (const auto *fn1 = dynamic_cast<const FunctionType *>(t1)) {
    const auto *fn2 = dynamic_cast<const FunctionType *>(t2);
    if (!fn2)
      return false;

    if (fn1->args.size() != fn2->args.size())
      return false;

    for (int i = 0; i < fn1->args.size(); ++i)
      if (!unify(fn1->args[i], fn2->args[i]))
        return false;

    return unify(fn1->ret, fn2->ret);
  }

  return t1 == t2;
}

void Context::dump() {
  for (auto &&decl : structs)
    decl->dump(*this, 0);

  for (auto &&decl : functions)
    decl->dump(*this, 0);
}
} // namespace res
} // namespace yl
