#include <iostream>
#include <sstream>

#include "lexer.h"
#include "res.h"
#include "utils.h"

namespace yl {
namespace res {
void TranslationUnit::dump(size_t level) const {
  for (auto &&trait : getAll<res::TraitDecl>())
    trait->dump(0);

  for (auto &&s : getAll<res::StructDecl>())
    s->dump(0);

  for (auto &&extension : extensions)
    extension->dump(0);

  for (auto &&fn : getAll<res::FunctionDecl>())
    fn->dump(0);
}

void Context::add(std::unique_ptr<Stmt> stmt) {
  statements.emplace_back(std::move(stmt));
}

void Context::add(std::unique_ptr<Decl> decl) {
  decls.emplace_back(std::move(decl));
}

void Context::add(std::unique_ptr<Block> block) {
  blocks.emplace_back(std::move(block));
}

void Context::add(std::unique_ptr<TraitConformance> conformance) {
  conformances.emplace_back(std::move(conformance));
}

void Context::add(std::unique_ptr<TypeExtension> extension) {
  extensions.emplace_back(std::move(extension));
}

std::string ConstVal::asString() const {
  return std::visit(
      [](auto &&value) {
        std::stringstream ss;

        using T = std::decay_t<decltype(value)>;
        if constexpr (std::is_same_v<T, bool>)
          ss << (value ? "true" : "false");
        else if constexpr (std::is_same_v<T, double>)
          ss << value;

        return ss.str();
      },
      *this);
}

GenericDeclContext::GenericDeclContext(
    GenericDeclContext *parent, std::vector<res::TypeParamDecl *> typeParams)
    : parent(parent),
      typeParams(std::move(typeParams)) {
  for (auto &&tp : this->typeParams)
    tp->setDeclContext(this);
}

std::vector<res::Decl *>
GenericDeclContext::lookupDirect(const std::string id) const {
  std::vector<res::Decl *> result;

  for (auto &&d : decls)
    if (d->identifier == id)
      result.emplace_back(d);

  return result;
}

Block::Block(SourceLocation l, std::vector<Stmt *> s)
    : location(l),
      statements(std::move(s)) {}

void Block::dump(size_t level) const {
  std::cerr << indent(level) << "Block\n";

  for (auto &&stmt : statements)
    stmt->dump(level + 1);
}

IfStmt::IfStmt(SourceLocation l, Expr *c, Block *t, Block *f)
    : Stmt(l),
      condition(c),
      trueBlock(t),
      falseBlock(f) {}

void IfStmt::dump(size_t level) const {
  std::cerr << indent(level) << "IfStmt\n";

  condition->dump(level + 1);
  trueBlock->dump(level + 1);
  if (falseBlock)
    falseBlock->dump(level + 1);
}

WhileStmt::WhileStmt(SourceLocation l, Expr *c, Block *b)
    : Stmt(l),
      condition(c),
      body(b) {}

void WhileStmt::dump(size_t level) const {
  std::cerr << indent(level) << "WhileStmt\n";

  condition->dump(level + 1);
  body->dump(level + 1);
}

ParamDecl::ParamDecl(SourceLocation l,
                     std::string i,
                     GenericDeclContext *c,
                     bool m)
    : ValueDecl(l, std::move(i), c, m) {}

void ParamDecl::dump(size_t level) const {
  std::cerr << indent(level) << "ParamDecl @(" << this << ") " << identifier
            << " {" << getType()->getName() << '}' << '\n';
}

FieldDecl::FieldDecl(SourceLocation l, std::string i, GenericDeclContext *c)
    : ValueDecl(l, std::move(i), c, false) {}

void FieldDecl::dump(size_t level) const {
  std::cerr << indent(level) << "FieldDecl @(" << this << ") " << identifier
            << " {" << getType()->getName() << '}' << '\n';
}

VarDecl::VarDecl(SourceLocation location,
                 std::string identifier,
                 GenericDeclContext *declContext,
                 bool isMutable,
                 Expr *initializer)
    : ValueDecl(location, std::move(identifier), declContext, isMutable),
      initializer(initializer) {}

void VarDecl::dump(size_t level) const {
  std::cerr << indent(level) << "VarDecl @(" << this << ") " << identifier
            << " {" << getType()->getName() << '}' << '\n';

  if (initializer)
    initializer->dump(level + 1);
}

FunctionDecl::FunctionDecl(SourceLocation location,
                           std::string identifier,
                           GenericDeclContext *declContext,
                           std::vector<TypeParamDecl *> typeParams)
    : ValueDecl(location, std::move(identifier), declContext, false),
      GenericDeclContext(declContext, std::move(typeParams)) {}

void FunctionDecl::dump(size_t level) const {
  std::cerr << indent(level) << "FunctionDecl @(" << this << ") " << identifier
            << (!body ? " [incomplete]" : "") << " {" << getType()->getName()
            << '}' << '\n';

  for (auto &&typeParam : typeParams)
    typeParam->dump(level + 1);

  for (auto &&param : params)
    param->dump(level + 1);

  if (body)
    body->dump(level + 1);
}

TypeExtension::TypeExtension(SourceLocation location,
                             std::vector<TypeParamDecl *> typeParams,
                             Type *type,
                             TraitType *trait)
    : GenericDeclContext(nullptr, std::move(typeParams)),
      location(location),
      type(type),
      trait(trait) {}

void TypeExtension::dump(size_t level) const {
  std::cerr << indent(level) << "TypeExtension " << type->getName() << " : "
            << trait->getName() << '\n';

  for (auto &&typeParam : typeParams)
    typeParam->dump(level + 1);

  for (auto &&decl : decls)
    decl->dump(level + 1);
}

StructDecl::StructDecl(SourceLocation location,
                       std::string identifier,
                       GenericDeclContext *declContext,
                       std::vector<TypeParamDecl *> typeParams,
                       bool isLambda)
    : TypeDecl(location, std::move(identifier), declContext),
      GenericDeclContext(declContext, std::move(typeParams)),
      isLambda(isLambda) {}

void StructDecl::dump(size_t level) const {
  std::cerr << indent(level) << "StructDecl @(" << this << ") " << identifier
            << " {" << getType()->getName() << '}' << '\n';

  for (auto &&typeParam : typeParams)
    typeParam->dump(level + 1);

  for (auto &&decl : decls)
    decl->dump(level + 1);
}

TraitConformance::TraitConformance(SourceLocation l,
                                   res::Type *t,
                                   std::vector<res::TraitType *> ts)
    : location(l),
      type(t),
      traits(std::move(ts)) {}

void TraitConformance::dump(size_t level) const {
  std::cerr << indent(level) << "TraitConformance " << type->getName() << " : ";

  for (auto &&trait : traits) {
    std::cerr << trait->getName();

    if (trait != traits.back())
      std::cerr << " & ";
  }

  std::cerr << '\n';
}

TraitDecl::TraitDecl(SourceLocation l,
                     std::string i,
                     GenericDeclContext *c,
                     std::vector<TypeParamDecl *> p)
    : TypeDecl(l, std::move(i), c),
      GenericDeclContext(c, std::move(p)) {}

void TraitDecl::dump(size_t level) const {
  std::cerr << indent(level) << "TraitDecl @(" << this << ") " << identifier
            << '\n';

  if (conformance)
    conformance->dump(level + 1);

  for (auto &&typeParam : typeParams)
    typeParam->dump(level + 1);

  for (auto &&decl : decls)
    decl->dump(level + 1);
}

TypeParamDecl::TypeParamDecl(SourceLocation l, std::string i, bool s)
    : TypeDecl(l, std::move(i), nullptr),
      isImplicitSelf(s) {}

void TypeParamDecl::dump(size_t level) const {
  std::cerr << indent(level) << "TypeParamDecl @(" << this << ") " << identifier
            << " {" << getType()->getName() << "}\n";

  if (conformance)
    conformance->dump(level + 1);
}

NumberLiteral::NumberLiteral(SourceLocation location, double value)
    : Expr(location, Expr::Kind::Rvalue),
      value(value) {}

void NumberLiteral::dump(size_t level) const {
  std::cerr << indent(level) << "NumberLiteral '" << value << "' {"
            << getType()->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';
}

BoolLiteral::BoolLiteral(SourceLocation location, bool value)
    : Expr(location, Expr::Kind::Rvalue),
      value(value) {}

void BoolLiteral::dump(size_t level) const {
  std::cerr << indent(level) << "BoolLiteral '" << (value ? "true" : "false")
            << "' {" << getType()->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';
}

UnitLiteral::UnitLiteral(SourceLocation location)
    : Expr(location, Expr::Kind::Rvalue) {}

void UnitLiteral::dump(size_t level) const {
  std::cerr << indent(level) << "UnitLiteral {" << getType()->getName() << '}'
            << '\n';
}

DeclRefExpr::DeclRefExpr(SourceLocation loc,
                         Decl *d,
                         Expr::Kind kind,
                         Substitution sub)
    : Expr(loc, kind),
      decl(d),
      sub(sub) {}

Type *DeclRefExpr::getReceiverType() const {
  for (auto &&[from, to] : sub)
    if (auto *t = from->getAs<res::TypeParamType>();
        t && t->decl->isImplicitSelf)
      return to;

  return nullptr;
}

void DeclRefExpr::dump(size_t level) const {
  std::cerr << indent(level) << "DeclRefExpr @(" << decl << ") "
            << decl->identifier << " {" << getType()->getName() << '}' << '\n';
}

CallExpr::CallExpr(SourceLocation location,
                   Expr *callee,
                   std::vector<Expr *> args)
    : Expr(location, Expr::Kind::Rvalue),
      callee(callee),
      arguments(std::move(args)){};

bool CallExpr::isVirtual() const {
  // FIXME: revisit
  const auto *dre = dynamic_cast<const res::DeclRefExpr *>(callee);
  return dre && dre->getReceiverType() &&
         dre->getReceiverType()->getAs<res::AnyTraitType>();
}

void CallExpr::dump(size_t level) const {
  std::cerr << indent(level) << "CallExpr"
            << " {" << getType()->getName() << '}' << '\n';

  callee->dump(level + 1);

  for (auto &&arg : arguments)
    arg->dump(level + 1);
}

MemberExpr::MemberExpr(SourceLocation location, Expr *base, DeclRefExpr *member)
    : Expr(location, !base->isLvalue() ? Expr::Kind::MutLvalue : base->kind),
      base(base),
      member(member) {}

void MemberExpr::dump(size_t level) const {
  std::cerr << indent(level) << "MemberExpr @(" << member->decl << ')' << ' '
            << member->decl->identifier << " {" << getType()->getName() << '}'
            << '\n';

  base->dump(level + 1);
}

GroupingExpr::GroupingExpr(SourceLocation location, Expr *expr)
    : Expr(location, expr->kind),
      expr(expr) {}

void GroupingExpr::dump(size_t level) const {
  std::cerr << indent(level) << "GroupingExpr"
            << " {" << getType()->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';

  expr->dump(level + 1);
}

BinaryOperator::BinaryOperator(SourceLocation loc,
                               TokenKind op,
                               Expr *lhs,
                               Expr *rhs)
    : Expr(loc, Expr::Kind::Rvalue),
      op(op),
      lhs(lhs),
      rhs(rhs) {}

void BinaryOperator::dump(size_t level) const {
  std::cerr << indent(level) << "BinaryOperator '" << getOpStr(op) << '\''
            << " {" << getType()->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';

  lhs->dump(level + 1);
  rhs->dump(level + 1);
}

UnaryOperator::UnaryOperator(SourceLocation loc,
                             TokenKind op,
                             Expr *e,
                             Expr::Kind kind)
    : Expr(loc, kind),
      op(op),
      operand(e) {}

void UnaryOperator::dump(size_t level) const {
  std::cerr << indent(level) << "UnaryOperator '" << getOpStr(op) << '\''
            << " {" << getType()->getName() << '}' << '\n';

  if (constVal.isKnown())
    std::cerr << indent(level) << "| value: " << constVal.asString() << '\n';

  operand->dump(level + 1);
}

DeclStmt::DeclStmt(SourceLocation location, VarDecl *varDecl)
    : Stmt(location),
      varDecl(varDecl) {}

void DeclStmt::dump(size_t level) const {
  std::cerr << indent(level) << "DeclStmt\n";

  varDecl->dump(level + 1);
}

Assignment::Assignment(SourceLocation location, Expr *assignee, Expr *expr)
    : Stmt(location),
      assignee(assignee),
      expr(expr) {}

void Assignment::dump(size_t level) const {
  std::cerr << indent(level) << "Assignment\n";

  assignee->dump(level + 1);
  expr->dump(level + 1);
}

ReturnStmt::ReturnStmt(SourceLocation location, Expr *expr)
    : Stmt(location),
      expr(expr) {}

void ReturnStmt::dump(size_t level) const {
  std::cerr << indent(level) << "ReturnStmt\n";

  if (expr)
    expr->dump(level + 1);
}

FieldInitStmt::FieldInitStmt(SourceLocation loc, FieldDecl *field, Expr *init)
    : Stmt(loc),
      field(field),
      initializer(init) {}

void FieldInitStmt::dump(size_t level) const {
  std::cerr << indent(level) << "FieldInitStmt @(" << field << ')' << ' '
            << field->identifier << '\n';

  initializer->dump(level + 1);
}

StructInstantiationExpr::StructInstantiationExpr(
    SourceLocation loc, DeclRefExpr *dre, std::vector<FieldInitStmt *> inits)
    : Expr(loc, Expr::Kind::Rvalue),
      structPath(dre),
      fieldInitializers(std::move(inits)) {}

void StructInstantiationExpr::dump(size_t level) const {
  std::cerr << indent(level) << "StructInstantiationExpr"
            << " {" << getType()->getName() << '}' << '\n';

  structPath->dump(level + 1);

  for (auto &&field : fieldInitializers)
    field->dump(level + 1);
}

ImplicitDerefExpr::ImplicitDerefExpr(SourceLocation location, DeclRefExpr *dre)
    : Expr(location, dre->kind),
      dre(dre) {}

void ImplicitDerefExpr::dump(size_t level) const {
  std::cerr << indent(level) << "ImplicitDerefExpr"
            << " {" << getType()->getName() << '}' << '\n';

  dre->dump(level + 1);
}

GCExpr::GCExpr(SourceLocation location, Expr *expr)
    : Expr(location, Expr::Kind::Rvalue),
      expr(expr) {}

void GCExpr::dump(size_t level) const {
  std::cerr << indent(level) << "GCExpr"
            << " {" << getType()->getName() << '}' << '\n';

  expr->dump(level + 1);
}

LambdaExpr::LambdaExpr(SourceLocation location,
                       res::StructDecl *closure,
                       res::FunctionDecl *method,
                       std::vector<res::Expr *> fieldInits)
    : Expr(location, Expr::Kind::Rvalue),
      closure(closure),
      method(method),
      fieldInits(std::move(fieldInits)) {}

void LambdaExpr::dump(size_t level) const {
  std::cerr << indent(level) << "LambdaExpr"
            << " {" << getType()->getName() << '}' << '\n';

  for (auto &&init : fieldInits)
    init->dump(level + 1);

  closure->dump(level + 1);
}

ImplicitPtrToRefDecay::ImplicitPtrToRefDecay(SourceLocation location,
                                             res::Expr *expr)
    : Expr(location, Expr::Kind::Rvalue),
      expr(expr) {}

void ImplicitPtrToRefDecay::dump(size_t level) const {
  std::cerr << indent(level) << "ImplicitPtrToRefDecay"
            << " {" << getType()->getName() << '}' << '\n';

  expr->dump(level + 1);
}

ImplicitAsRefExpr::ImplicitAsRefExpr(SourceLocation location, res::Expr *expr)
    : Expr(location, Expr::Kind::Rvalue),
      expr(expr) {}

void ImplicitAsRefExpr::dump(size_t level) const {
  std::cerr << indent(level) << "ImplicitAsRefExpr"
            << " {" << getType()->getName() << '}' << '\n';

  expr->dump(level + 1);
}

MaterializeTemporaryExpr::MaterializeTemporaryExpr(SourceLocation location,
                                                   res::Expr *expr)
    : Expr(location, Expr::Kind::MutLvalue),
      expr(expr) {}

void MaterializeTemporaryExpr::dump(size_t level) const {
  std::cerr << indent(level) << "MaterializeTemporaryExpr"
            << " {" << getType()->getName() << '}' << '\n';

  expr->dump(level + 1);
}

TraitObjectPromoExpr::TraitObjectPromoExpr(SourceLocation location,
                                           res::Expr *expr)
    : Expr(location, Expr::Kind::Rvalue),
      expr(expr) {}

void TraitObjectPromoExpr::dump(size_t level) const {
  std::cerr << indent(level) << "TraitObjectPromoExpr"
            << " {" << getType()->getName() << '}' << '\n';

  expr->dump(level + 1);
}
} // namespace res
} // namespace yl
