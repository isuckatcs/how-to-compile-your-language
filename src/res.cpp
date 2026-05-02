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
  std::cerr << indent(level) << "ParamDecl @(" << this << ") " << identifier
            << " {" << getType()->getName() << '}' << '\n';
}

void FieldDecl::dump(size_t level) const {
  std::cerr << indent(level) << "FieldDecl @(" << this << ") " << identifier
            << " {" << getType()->getName() << '}' << '\n';
}

void VarDecl::dump(size_t level) const {
  std::cerr << indent(level) << "VarDecl @(" << this << ") " << identifier
            << " {" << getType()->getName() << '}' << '\n';

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
  std::cerr << indent(level) << "FunctionDecl @(" << this << ") " << identifier
            << (!isComplete ? " [incomplete]" : "") << " {"
            << getType()->getName() << '}' << '\n';

  if (implements)
    std::cerr << indent(level) << "| implements '" << implements->identifier
              << "' @(" << implements << ")\n";

  for (auto &&typeParam : typeParams)
    typeParam->dump(level + 1);

  for (auto &&param : params)
    param->dump(level + 1);

  if (body)
    body->dump(level + 1);
}

void ImplDecl::dump(size_t level) const {
  std::cerr << indent(level) << "ImplDecl @(" << this << ") "
            << traitInstance->decl->identifier << " {" << getType()->getName()
            << '}' << '\n';

  traitInstance->dump(level + 1);

  for (auto &&decl : decls)
    decl->dump(level + 1);
}

void StructDecl::dump(size_t level) const {
  std::cerr << indent(level) << "StructDecl @(" << this << ") " << identifier
            << " {" << getType()->getName() << '}' << '\n';

  for (auto &&typeParam : typeParams)
    typeParam->dump(level + 1);

  for (auto &&decl : decls)
    decl->dump(level + 1);
}

void TraitInstance::dump(size_t level) const {
  std::cerr << indent(level) << "TraitInstance @(" << decl
            << ") " + decl->identifier << " {" << getType()->getName() << "}\n";
}

void TraitDecl::dump(size_t level) const {
  std::cerr << indent(level) << "TraitDecl @(" << this << ") " << identifier
            << '\n';

  for (auto &&trait : traits)
    trait->dump(level + 1);

  for (auto &&typeParam : typeParams)
    typeParam->dump(level + 1);

  for (auto &&decl : decls)
    decl->dump(level + 1);
}

void TypeParamDecl::dump(size_t level) const {
  std::cerr << indent(level) << "TypeParamDecl @(" << this << ") " << identifier
            << " {" << getType()->getName() << "}\n";

  for (auto &&trait : traits)
    trait->dump(level + 1);
}

void NumberLiteral::dump(size_t level) const {
  std::cerr << indent(level) << "NumberLiteral '" << value << "' {"
            << getType()->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';
}

void BoolLiteral::dump(size_t level) const {
  std::cerr << indent(level) << "BoolLiteral '" << (value ? "true" : "false")
            << "' {" << getType()->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';
}

void UnitLiteral::dump(size_t level) const {
  std::cerr << indent(level) << "UnitLiteral {" << getType()->getName() << '}'
            << '\n';
}

void DeclRefExpr::dump(size_t level) const {
  std::cerr << indent(level) << "DeclRefExpr @(" << decl << ") ";
  if (trait)
    std::cerr << trait->getName() << ':' << ':';
  std::cerr << decl->identifier;

  std::cerr << " {" << getType()->getName() << '}' << '\n';
}

void PathExpr::dump(size_t level) const {
  std::cerr << indent(level) << "PathExpr"
            << " {" << getType()->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';

  for (auto &&fragment : fragments)
    fragment->dump(level + 1);
}

void CallExpr::dump(size_t level) const {
  std::cerr << indent(level) << "CallExpr"
            << " {" << getType()->getName() << '}' << '\n';

  callee->dump(level + 1);

  for (auto &&arg : arguments)
    arg->dump(level + 1);
}

void MemberExpr::dump(size_t level) const {
  std::cerr << indent(level) << "MemberExpr @(" << member << ')' << ' '
            << member->decl->identifier << " {" << getType()->getName() << '}'
            << '\n';

  base->dump(level + 1);
}

void GroupingExpr::dump(size_t level) const {
  std::cerr << indent(level) << "GroupingExpr"
            << " {" << getType()->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';

  expr->dump(level + 1);
}

void BinaryOperator::dump(size_t level) const {
  std::cerr << indent(level) << "BinaryOperator '" << getOpStr(op) << '\''
            << " {" << getType()->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';

  lhs->dump(level + 1);
  rhs->dump(level + 1);
}

void UnaryOperator::dump(size_t level) const {
  std::cerr << indent(level) << "UnaryOperator '" << getOpStr(op) << '\''
            << " {" << getType()->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';

  operand->dump(level + 1);
}

void DeclStmt::dump(size_t level) const {
  std::cerr << indent(level) << "DeclStmt\n";

  varDecl->dump(level + 1);
}

void Assignment::dump(size_t level) const {
  std::cerr << indent(level) << "Assignment\n";

  assignee->dump(level + 1);
  expr->dump(level + 1);
}

void ReturnStmt::dump(size_t level) const {
  std::cerr << indent(level) << "ReturnStmt\n";

  if (expr)
    expr->dump(level + 1);
}

void FieldInitStmt::dump(size_t level) const {
  std::cerr << indent(level) << "FieldInitStmt @(" << field << ')' << ' '
            << field->identifier << '\n';

  initializer->dump(level + 1);
}

void StructInstantiationExpr::dump(size_t level) const {
  std::cerr << indent(level) << "StructInstantiationExpr"
            << " {" << getType()->getName() << '}' << '\n';

  structPath->dump(level + 1);

  for (auto &&field : fieldInitializers)
    field->dump(level + 1);
}

void ImplicitDerefExpr::dump(size_t level) const {
  std::cerr << indent(level) << "ImplicitDerefExpr"
            << " {" << getType()->getName() << '}' << '\n';

  outParamRef->dump(level + 1);
}

void Context::dump() const {
  for (auto &&trait : traits)
    trait->dump(0);

  for (auto &&decl : structs)
    decl->dump(0);

  for (auto &&decl : functions)
    decl->dump(0);
}
} // namespace res
} // namespace yl
