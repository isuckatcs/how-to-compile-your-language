#include <iostream>

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

void FunctionDecl::dump(size_t level) const {
  std::cerr << indent(level) << "FunctionDecl: @(" << this << ") " << identifier
            << ':' << '\n';

  for (auto &&param : params)
    param->dump(level + 1);

  body->dump(level + 1);
}

void StructDecl::dump(size_t level) const {
  std::cerr << indent(level) << "StructDecl: @(" << this << ") " << identifier
            << ':' << '\n';

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
  std::cerr << indent(level) << "CallExpr: @(" << callee << ") "
            << callee->identifier << '\n';

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
} // namespace res
} // namespace yl
