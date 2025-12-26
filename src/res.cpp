#include <iostream>
#include <sstream>

#include "lexer.h"
#include "res.h"
#include "utils.h"

namespace yl {
namespace res {
void Block::dump(size_t level) const {
  std::cerr << indent(level) << "Block\n";

  for (auto &&stmt : statements)
    stmt->dump(level + 1);
}

void IfStmt::dump(size_t level) const {
  std::cerr << indent(level) << "IfStmt\n";

  condition->dump(level + 1);
  trueBlock->dump(level + 1);
  if (falseBlock)
    falseBlock->dump(level + 1);
}

void WhileStmt::dump(size_t level) const {
  std::cerr << indent(level) << "WhileStmt\n";

  condition->dump(level + 1);
  body->dump(level + 1);
}

void ParamDecl::dump(size_t level) const {
  std::cerr << indent(level) << "ParamDecl: @(" << this << ") " << identifier
            << ':' << '\n';
}

void FieldDecl::dump(size_t level) const {
  std::cerr << indent(level) << "FieldDecl: @(" << this << ") " << identifier
            << '\n';
}

void VarDecl::dump(size_t level) const {
  std::cerr << indent(level) << "VarDecl: @(" << this << ") " << identifier
            << ':' << '\n';
  if (initializer)
    initializer->dump(level + 1);
}

void FunctionDecl::setBody(Block *body) {
  assert(!isComplete && "setting body on an already complete function");
  assert(body && "function body cannot be null");

  this->body = body;
  isComplete = true;
}

void FunctionDecl::dump(size_t level) const {
  std::cerr << indent(level) << "FunctionDecl: @(" << this << ") " << identifier
            << (!isComplete ? " [incomplete]" : "") << ':' << '\n';

  for (auto &&param : params)
    param->dump(level + 1);

  if (body)
    body->dump(level + 1);
}

void StructDecl::setFields(std::vector<FieldDecl *> fields) {
  assert(!isComplete && "setting fields on already complete struct");

  this->fields = std::move(fields);
  isComplete = true;
}

void StructDecl::dump(size_t level) const {
  std::cerr << indent(level) << "StructDecl: @(" << this << ") " << identifier
            << (!isComplete ? " [incomplete]" : "") << ':' << '\n';

  for (auto &&field : fields)
    field->dump(level + 1);
}

void NumberLiteral::dump(size_t level) const {
  std::cerr << indent(level) << "NumberLiteral: '" << value << "'\n";
  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';
}

void DeclRefExpr::dump(size_t level) const {
  std::cerr << indent(level) << "DeclRefExpr: @(" << decl << ") "
            << decl->identifier << '\n';
  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';
}

void CallExpr::dump(size_t level) const {
  std::cerr << indent(level) << "CallExpr:\n";
  callee->dump(level + 1);

  for (auto &&arg : arguments)
    arg->dump(level + 1);
}

void MemberExpr::dump(size_t level) const {
  std::cerr << indent(level) << "MemberExpr: @(" << field << ')' << ' '
            << field->identifier << '\n';

  base->dump(level + 1);
}

void GroupingExpr::dump(size_t level) const {
  std::cerr << indent(level) << "GroupingExpr:\n";
  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';

  expr->dump(level + 1);
}

void BinaryOperator::dump(size_t level) const {
  std::cerr << indent(level) << "BinaryOperator: '" << getOpStr(op) << '\''
            << '\n';

  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';

  lhs->dump(level + 1);
  rhs->dump(level + 1);
}

void UnaryOperator::dump(size_t level) const {
  std::cerr << indent(level) << "UnaryOperator: '" << getOpStr(op) << '\''
            << '\n';

  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';

  operand->dump(level + 1);
}

void DeclStmt::dump(size_t level) const {
  std::cerr << indent(level) << "DeclStmt:\n";
  varDecl->dump(level + 1);
}

void Assignment::dump(size_t level) const {
  std::cerr << indent(level) << "Assignment:\n";
  assignee->dump(level + 1);
  expr->dump(level + 1);
}

void ReturnStmt::dump(size_t level) const {
  std::cerr << indent(level) << "ReturnStmt\n";

  if (expr)
    expr->dump(level + 1);
}

void FieldInitStmt::dump(size_t level) const {
  std::cerr << indent(level) << "FieldInitStmt: @(" << field << ')' << ' '
            << field->identifier << '\n';

  initializer->dump(level + 1);
}

void StructInstantiationExpr::dump(size_t level) const {
  std::cerr << indent(level) << "StructInstantiationExpr: @(" << structDecl
            << ')' << '\n';

  for (auto &&field : fieldInitializers)
    field->dump(level + 1);
}

bool UninferredType::operator==(const Type &b) const {
  const auto *uninferred = dynamic_cast<const UninferredType *>(&b);

  return uninferred && uninferred->id == id;
}

std::string UninferredType::asString() const {
  return "T" + std::to_string(id);
}

void UninferredType::dump() const {
  std::cerr << "UninferredType: " << asString();
}

bool BuiltinType::operator==(const Type &b) const {
  const auto *builtin = dynamic_cast<const BuiltinType *>(&b);

  return builtin && builtin->kind == kind;
}

std::string BuiltinType::asString() const {
  return kind == BuiltinType::Kind::Number ? "number" : "void";
}

void BuiltinType::dump() const { std::cerr << "BuiltinType: " << asString(); }

bool StructType::operator==(const Type &b) const {
  const auto *st = dynamic_cast<const StructType *>(&b);

  return st && st->decl == decl;
}

std::string StructType::asString() const { return decl->identifier; }

void StructType::dump() const { std::cerr << "StructType: " << asString(); }

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

void FunctionType::dump() const { std::cerr << "FunctionType: " << asString(); }

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
  std::cerr << "Context\n";
  std::cerr << "==========================\n";
  for (auto &&type : types) {
    type->dump();
    std::cerr << '\n';
  }

  std::cerr << "--------------------------\n";
  for (auto &&[node, type] : environment) {

    if (const auto *e = std::get_if<const res::Expr *>(&node); e)
      (*e)->dump(0);
    if (const auto *d = std::get_if<const res::Decl *>(&node); d)
      (*d)->dump(0);
    std::cerr << "->\n";
    type->dump();
    std::cerr << "\n--------------------------\n";
  }
}
} // namespace res
} // namespace yl
