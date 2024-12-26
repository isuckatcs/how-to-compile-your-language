#include <iostream>

#include "ast.h"

namespace yl {
namespace {
std::string_view getOpStr(TokenKind op) {
  if (op == TokenKind::Plus)
    return "+";
  if (op == TokenKind::Minus)
    return "-";
  if (op == TokenKind::Asterisk)
    return "*";
  if (op == TokenKind::Slash)
    return "/";
  if (op == TokenKind::EqualEqual)
    return "==";
  if (op == TokenKind::AmpAmp)
    return "&&";
  if (op == TokenKind::PipePipe)
    return "||";
  if (op == TokenKind::Lt)
    return "<";
  if (op == TokenKind::Gt)
    return ">";
  if (op == TokenKind::Excl)
    return "!";

  llvm_unreachable("unexpected operator");
}

std::string indent(size_t level) { return std::string(level * 2, ' '); }
} // namespace

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

void MemberInitStmt::dump(size_t level) const {
  std::cerr << indent(level) << "MemberInitStmt: " << identifier << '\n';
  initializer->dump(level + 1);
}

void StructInstantiationExpr::dump(size_t level) const {
  std::cerr << indent(level) << "StructInstantiationExpr: " << identifier
            << '\n';

  for (auto &&member : memberInitializers)
    member->dump(level + 1);
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
  std::cerr << indent(level) << "MemberExpr: ." << member << '\n';

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

void MemberDecl::dump(size_t level) const {
  std::cerr << indent(level) << "MemberDecl: " << identifier << ':' << type.name
            << '\n';
}

void StructDecl::dump(size_t level) const {
  std::cerr << indent(level) << "StructDecl: " << identifier << '\n';

  for (auto &&member : members)
    member->dump(level + 1);
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
  variable->dump(level + 1);
  expr->dump(level + 1);
}

void ResolvedBlock::dump(size_t level) const {
  std::cerr << indent(level) << "ResolvedBlock\n";

  for (auto &&stmt : statements)
    stmt->dump(level + 1);
}

void ResolvedIfStmt::dump(size_t level) const {
  std::cerr << indent(level) << "ResolvedIfStmt\n";

  condition->dump(level + 1);
  trueBlock->dump(level + 1);
  if (falseBlock)
    falseBlock->dump(level + 1);
}

void ResolvedWhileStmt::dump(size_t level) const {
  std::cerr << indent(level) << "ResolvedWhileStmt\n";

  condition->dump(level + 1);
  body->dump(level + 1);
}

void ResolvedParamDecl::dump(size_t level) const {
  std::cerr << indent(level) << "ResolvedParamDecl: @(" << this << ") "
            << identifier << ':' << '\n';
}

void ResolvedMemberDecl::dump(size_t level) const {
  std::cerr << indent(level) << "ResolvedMemberDecl: @(" << this << ") "
            << identifier << '\n';
}

void ResolvedVarDecl::dump(size_t level) const {
  std::cerr << indent(level) << "ResolvedVarDecl: @(" << this << ") "
            << identifier << ':' << '\n';
  if (initializer)
    initializer->dump(level + 1);
}

void ResolvedFunctionDecl::dump(size_t level) const {
  std::cerr << indent(level) << "ResolvedFunctionDecl: @(" << this << ") "
            << identifier << ':' << '\n';

  for (auto &&param : params)
    param->dump(level + 1);

  body->dump(level + 1);
}

void ResolvedStructDecl::dump(size_t level) const {
  std::cerr << indent(level) << "ResolvedStructDecl: @(" << this << ") "
            << identifier << ':' << '\n';

  for (auto &&member : members)
    member->dump(level + 1);
}

void ResolvedNumberLiteral::dump(size_t level) const {
  std::cerr << indent(level) << "ResolvedNumberLiteral: '" << value << "'\n";
  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';
}

void ResolvedDeclRefExpr::dump(size_t level) const {
  std::cerr << indent(level) << "ResolvedDeclRefExpr: @(" << decl << ") "
            << decl->identifier << '\n';
  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';
}

void ResolvedCallExpr::dump(size_t level) const {
  std::cerr << indent(level) << "ResolvedCallExpr: @(" << callee << ") "
            << callee->identifier << '\n';
  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';

  for (auto &&arg : arguments)
    arg->dump(level + 1);
}

void ResolvedGroupingExpr::dump(size_t level) const {
  std::cerr << indent(level) << "ResolvedGroupingExpr:\n";
  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';

  expr->dump(level + 1);
}

void ResolvedBinaryOperator::dump(size_t level) const {
  std::cerr << indent(level) << "ResolvedBinaryOperator: '" << getOpStr(op)
            << '\'' << '\n';

  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';

  lhs->dump(level + 1);
  rhs->dump(level + 1);
}

void ResolvedUnaryOperator::dump(size_t level) const {
  std::cerr << indent(level) << "ResolvedUnaryOperator: '" << getOpStr(op)
            << '\'' << '\n';

  if (auto val = getConstantValue())
    std::cerr << indent(level) << "| value: " << *val << '\n';

  operand->dump(level + 1);
}

void ResolvedDeclStmt::dump(size_t level) const {
  std::cerr << indent(level) << "ResolvedDeclStmt:\n";
  varDecl->dump(level + 1);
}

void ResolvedAssignment::dump(size_t level) const {
  std::cerr << indent(level) << "ResolvedAssignment:\n";
  variable->dump(level + 1);
  expr->dump(level + 1);
}

void ResolvedReturnStmt::dump(size_t level) const {
  std::cerr << indent(level) << "ResolvedReturnStmt\n";

  if (expr)
    expr->dump(level + 1);
}
} // namespace yl
