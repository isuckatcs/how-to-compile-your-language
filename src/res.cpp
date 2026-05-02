#include <iostream>
#include <sstream>

#include "lexer.h"
#include "res.h"
#include "utils.h"

namespace yl {
namespace res {
std::string ConstVal::asString() const {
  return std::visit(
      [](auto &&value) {
        std::stringstream ss;

        using T = std::decay_t<decltype(value)>;
        if constexpr (std::is_same_v<T, std::monostate>)
          ss << "unknown";
        else if constexpr (std::is_same_v<T, bool>)
          ss << (value ? "true" : "false");
        else if constexpr (std::is_same_v<T, double>)
          ss << value;

        return ss.str();
      },
      *this);
}

bool DeclContext::insertDecl(res::Decl *decl) {
  bool isValueDecl = decl->getAs<res::ValueDecl>();

  for (auto &&currentDecl : decls)
    if (currentDecl->identifier == decl->identifier &&
        isValueDecl == !!currentDecl->getAs<res::ValueDecl>())
      return false;

  decls.emplace_back(decl);
  return true;
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
            << " {" << ctx.getTypeMgr().getType(this)->getName() << '}' << '\n';
}

void FieldDecl::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "FieldDecl @(" << this << ") " << identifier
            << " {" << ctx.getTypeMgr().getType(this)->getName() << '}' << '\n';
}

void VarDecl::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "VarDecl @(" << this << ") " << identifier
            << " {" << ctx.getTypeMgr().getType(this)->getName() << '}' << '\n';

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
            << ctx.getTypeMgr().getType(this)->getName() << '}' << '\n';

  if (implements)
    std::cerr << indent(level) << "| implements '" << implements->identifier
              << "' @(" << implements << ")\n";

  for (auto &&typeParam : typeParams)
    typeParam->dump(ctx, level + 1);

  for (auto &&param : params)
    param->dump(ctx, level + 1);

  if (body)
    body->dump(ctx, level + 1);
}

void ImplDecl::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "ImplDecl @(" << this << ") "
            << traitInstance->decl->identifier << " {"
            << ctx.getTypeMgr().getType(this)->getName() << '}' << '\n';

  traitInstance->dump(ctx, level + 1);

  for (auto &&decl : decls)
    decl->dump(ctx, level + 1);
}

void StructDecl::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "StructDecl @(" << this << ") " << identifier
            << " {" << ctx.getTypeMgr().getType(this)->getName() << '}' << '\n';

  for (auto &&typeParam : typeParams)
    typeParam->dump(ctx, level + 1);

  for (auto &&decl : decls)
    decl->dump(ctx, level + 1);
}

void TraitInstance::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "TraitInstance @(" << decl
            << ") " + decl->identifier << " {"
            << ctx.getTypeMgr().getType(this)->getName() << "}\n";
}

void TraitDecl::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "TraitDecl @(" << this << ") " << identifier
            << '\n';

  for (auto &&trait : traits)
    trait->dump(ctx, level + 1);

  for (auto &&typeParam : typeParams)
    typeParam->dump(ctx, level + 1);

  for (auto &&decl : decls)
    decl->dump(ctx, level + 1);
}

void TypeParamDecl::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "TypeParamDecl @(" << this << ") " << identifier
            << " {" << ctx.getTypeMgr().getType(this)->getName() << "}\n";

  for (auto &&trait : traits)
    trait->dump(ctx, level + 1);
}

void NumberLiteral::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "NumberLiteral '" << value << "' {"
            << ctx.getTypeMgr().getType(this)->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';
}

void BoolLiteral::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "BoolLiteral '" << value << "' {"
            << ctx.getTypeMgr().getType(this)->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';
}

void UnitLiteral::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "UnitLiteral {"
            << ctx.getTypeMgr().getType(this)->getName() << '}' << '\n';
}

void DeclRefExpr::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "DeclRefExpr @(" << decl << ") ";
  if (trait)
    std::cerr << trait->getName() << ':' << ':';
  std::cerr << decl->identifier;

  std::cerr << " {" << ctx.getTypeMgr().getType(this)->getName() << '}' << '\n';
}

void PathExpr::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "PathExpr"
            << " {" << ctx.getTypeMgr().getType(this)->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';

  for (auto &&fragment : fragments)
    fragment->dump(ctx, level + 1);
}

void CallExpr::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "CallExpr"
            << " {" << ctx.getTypeMgr().getType(this)->getName() << '}' << '\n';

  callee->dump(ctx, level + 1);

  for (auto &&arg : arguments)
    arg->dump(ctx, level + 1);
}

void MemberExpr::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "MemberExpr @(" << member << ')' << ' '
            << member->decl->identifier << " {"
            << ctx.getTypeMgr().getType(this)->getName() << '}' << '\n';

  base->dump(ctx, level + 1);
}

void GroupingExpr::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "GroupingExpr"
            << " {" << ctx.getTypeMgr().getType(this)->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';

  expr->dump(ctx, level + 1);
}

void BinaryOperator::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "BinaryOperator '" << getOpStr(op) << '\''
            << " {" << ctx.getTypeMgr().getType(this)->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';

  lhs->dump(ctx, level + 1);
  rhs->dump(ctx, level + 1);
}

void UnaryOperator::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "UnaryOperator '" << getOpStr(op) << '\''
            << " {" << ctx.getTypeMgr().getType(this)->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';

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
            << " {" << ctx.getTypeMgr().getType(this)->getName() << '}' << '\n';

  structPath->dump(ctx, level + 1);

  for (auto &&field : fieldInitializers)
    field->dump(ctx, level + 1);
}

void ImplicitDerefExpr::dump(Context &ctx, size_t level) const {
  std::cerr << indent(level) << "ImplicitDerefExpr"
            << " {" << ctx.getTypeMgr().getType(this)->getName() << '}' << '\n';

  outParamRef->dump(ctx, level + 1);
}

void Context::dump() const {
  for (auto &&trait : traits)
    trait->dump(*(Context *)this, 0);

  for (auto &&decl : structs)
    decl->dump(*(Context *)this, 0);

  for (auto &&decl : functions)
    decl->dump(*(Context *)this, 0);
}
} // namespace res
} // namespace yl
