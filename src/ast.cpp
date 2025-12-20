#include <iostream>

#include "ast.h"
#include "lexer.h"
#include "utils.h"

namespace yl {
namespace ast {
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

void ReturnStmt::dump(size_t level) const {
  std::cerr << indent(level) << "ReturnStmt\n";

  if (expr)
    expr->dump(level + 1);
}

void FieldInitStmt::dump(size_t level) const {
  std::cerr << indent(level) << "FieldInitStmt: " << identifier << '\n';
  initializer->dump(level + 1);
}

void StructInstantiationExpr::dump(size_t level) const {
  std::cerr << indent(level) << "StructInstantiationExpr: " << identifier
            << '\n';

  for (auto &&field : fieldInitializers)
    field->dump(level + 1);
}

void NumberLiteral::dump(size_t level) const {
  std::cerr << indent(level) << "NumberLiteral: '" << value << "'\n";
}

void DeclRefExpr::dump(size_t level) const {
  std::cerr << indent(level) << "DeclRefExpr: " << identifier << '\n';
}

void CallExpr::dump(size_t level) const {
  std::cerr << indent(level) << "CallExpr:\n";

  callee->dump(level + 1);

  for (auto &&arg : arguments)
    arg->dump(level + 1);
}

void MemberExpr::dump(size_t level) const {
  std::cerr << indent(level) << "MemberExpr: ." << field << '\n';

  base->dump(level + 1);
}

void GroupingExpr::dump(size_t level) const {
  std::cerr << indent(level) << "GroupingExpr:\n";

  expr->dump(level + 1);
}

void BinaryOperator::dump(size_t level) const {
  std::cerr << indent(level) << "BinaryOperator: '" << getOpStr(op) << '\''
            << '\n';

  lhs->dump(level + 1);
  rhs->dump(level + 1);
}

void UnaryOperator::dump(size_t level) const {
  std::cerr << indent(level) << "UnaryOperator: '" << getOpStr(op) << '\''
            << '\n';

  operand->dump(level + 1);
}

void FieldDecl::dump(size_t level) const {
  std::cerr << indent(level) << "FieldDecl: " << identifier << ':' << type.name
            << '\n';
}

void StructDecl::dump(size_t level) const {
  std::cerr << indent(level) << "StructDecl: " << identifier << '\n';

  for (auto &&field : fields)
    field->dump(level + 1);
}

void ParamDecl::dump(size_t level) const {
  std::cerr << indent(level) << "ParamDecl: " << identifier << ':' << type.name
            << '\n';
}

void VarDecl::dump(size_t level) const {
  std::cerr << indent(level) << "VarDecl: " << identifier;
  if (type)
    std::cerr << ':' << type->name;
  std::cerr << '\n';

  if (initializer)
    initializer->dump(level + 1);
}

void FunctionDecl::dump(size_t level) const {
  std::cerr << indent(level) << "FunctionDecl: " << identifier << ':'
            << type.name << '\n';

  for (auto &&param : params)
    param->dump(level + 1);

  body->dump(level + 1);
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
} // namespace ast
} // namespace yl
