#include <cassert>

#include "parser.h"
#include "utils.h"

#define matchOrReturn(tok, msg)                                                \
  if (nextToken.kind != tok)                                                   \
    return report(nextToken.location, msg);

namespace yl {
namespace {
constexpr int getTokPrecedence(TokenKind tok) {
  switch (tok) {
  case TokenKind::Asterisk:
  case TokenKind::Slash:
    return 6;
  case TokenKind::Plus:
  case TokenKind::Minus:
    return 5;
  case TokenKind::Lt:
  case TokenKind::Gt:
    return 4;
  case TokenKind::EqualEqual:
    return 3;
  case TokenKind::AmpAmp:
    return 2;
  case TokenKind::PipePipe:
    return 1;
  default:
    return -1;
  }
}

constexpr bool isTopLevelToken(TokenKind tok) {
  return tok == TokenKind::Eof || tok == TokenKind::KwFn ||
         tok == TokenKind::KwStruct || tok == TokenKind::KwTrait;
}

}; // namespace

// Synchronization points:
// - start of a function decl
// - start of a struct decl
// - start of a trait decl
// - end of the current block
// - ';'
// - EOF
void Parser::synchronize() {
  incompleteAST = true;

  int braces = 0;
  while (true) {
    TokenKind kind = nextToken.kind;

    if (kind == TokenKind::Lbrace) {
      ++braces;
    } else if (kind == TokenKind::Rbrace) {
      if (braces == 0)
        break;

      if (braces == 1) {
        eatNextToken(); // eat '}'
        break;
      }

      --braces;
    } else if (kind == TokenKind::Semi && braces == 0) {
      eatNextToken(); // eat ';'
      break;
    } else if (isTopLevelToken(kind))
      break;

    eatNextToken();
  }
}

// <fieldDecl>
//  ::= <identifier> ':' <type>
std::unique_ptr<ast::FieldDecl> Parser::parseFieldDecl() {
  matchOrReturn(TokenKind::Identifier, "expected field declaration");

  SourceLocation location = nextToken.location;
  assert(nextToken.value && "identifier token without value");

  std::string identifier = *nextToken.value;
  eatNextToken(); // eat identifier

  matchOrReturn(TokenKind::Colon, "expected ':'");
  eatNextToken(); // eat :

  varOrReturn(type, parseType());

  return std::make_unique<ast::FieldDecl>(location, std::move(identifier),
                                          std::move(type));
};

// <typeParamList>
//  ::= '<' <typeParamDecl> (',' <typeParamDecl>)* ','? '>'
std::unique_ptr<std::vector<std::unique_ptr<ast::TypeParamDecl>>>
Parser::parseTypeParamList() {
  if (nextToken.kind != TokenKind::Lt)
    return std::make_unique<std::vector<std::unique_ptr<ast::TypeParamDecl>>>();

  return parseListWithTrailingComma<ast::TypeParamDecl>(
      {TokenKind::Lt, "expected '<'"}, &Parser::parseTypeParamDecl,
      {TokenKind::Gt, "expected ',', ':' or '>'"}, false);
}

// <typeList>
//  ::= '<' <type> (',' <type>)* ','? '>'
std::unique_ptr<std::vector<std::unique_ptr<ast::Type>>>
Parser::parseTypeList() {
  return parseListWithTrailingComma<ast::Type>(
      {TokenKind::Lt, "expected '<'"}, &Parser::parseType,
      {TokenKind::Gt, "expected ',' or '>'"}, false);
}

// <typeParamDecl>
//  ::= <identifier>
std::unique_ptr<ast::TypeParamDecl> Parser::parseTypeParamDecl() {
  matchOrReturn(TokenKind::Identifier, "expected type parameter declaration");

  SourceLocation location = nextToken.location;
  assert(nextToken.value && "identifier token without value");

  std::string identifier = *nextToken.value;
  eatNextToken(); // eat identifier

  varOrReturn(traits, parseTraitList());

  return std::make_unique<ast::TypeParamDecl>(location, std::move(identifier),
                                              std::move(traits));
}

// <structDecl>
//  ::= 'struct' <identifier> <typeParamList>? '{' <memberList>? '}'
//
// <memberList>
//  ::= (<fieldList> | <implDecl> | <functionDecl>)*
//
// <fieldList>
//  ::= <fieldDecl> (',' <fieldDecl>)* ','?
//
// <memberFunctionList>
//  ::= <functionDecl>*
std::unique_ptr<ast::StructDecl> Parser::parseStructDecl() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat struct

  matchOrReturn(TokenKind::Identifier, "expected identifier");

  assert(nextToken.value && "identifier token without value");
  std::string structIdentifier = *nextToken.value;
  eatNextToken(); // eat identifier

  varOrReturn(typeParamList, parseTypeParamList());

  matchOrReturn(TokenKind::Lbrace, "expected '{'");
  eatNextToken(); // eat '{'

  std::vector<std::unique_ptr<ast::Decl>> decls;

  while (true) {
    std::unique_ptr<ast::Decl> decl = nullptr;

    if (nextToken.kind == TokenKind::Identifier) {
      decl = parseFieldDecl();
      if (decl && nextToken.kind == TokenKind::Comma)
        eatNextToken(); // eat ','
    } else if (nextToken.kind == TokenKind::KwFn)
      decl = parseFunctionDecl();
    else if (nextToken.kind == TokenKind::KwImpl)
      decl = parseImplDecl();
    else
      break;

    if (!decl) {
      synchronize();
      continue;
    }

    decls.emplace_back(std::move(decl));
  }

  matchOrReturn(TokenKind::Rbrace, "expected identifier, 'fn', 'impl' or '}'");
  eatNextToken(); // eat '}'

  return std::make_unique<ast::StructDecl>(
      location, structIdentifier, std::move(*typeParamList), std::move(decls));
}

// <traitDecl>
//     ::= 'trait' <identifier> <typeParamList>? <traitList>? '{'
//     <traitFunctionDecl>* '}'

// <traitFunctionDecl>
//     ::= <functionHeader> (';' | <block>)
std::unique_ptr<ast::TraitDecl> Parser::parseTraitDecl() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat 'trait'

  matchOrReturn(TokenKind::Identifier, "expected identifier");

  assert(nextToken.value && "identifier token without value");
  std::string identifier = *nextToken.value;
  eatNextToken(); // eat identifier

  varOrReturn(typeParamList, parseTypeParamList());
  varOrReturn(traitList, parseTraitList());

  matchOrReturn(TokenKind::Lbrace, traitList->traits.empty()
                                       ? "expected ':' or '{'"
                                       : "expected '&' or '{'");
  eatNextToken(); // eat '{'

  std::vector<std::unique_ptr<ast::FunctionDecl>> memberFunctions;

  while (nextToken.kind == TokenKind::KwFn) {
    if (auto fn = withRestrictions(FunctionWithoutBodyAllowed,
                                   &Parser::parseFunctionDecl)) {
      memberFunctions.emplace_back(std::move(fn));
      continue;
    }

    synchronize();
  }

  matchOrReturn(TokenKind::Rbrace, "expected 'fn' or '}'");
  eatNextToken(); // eat '}'

  return std::make_unique<ast::TraitDecl>(
      location, identifier, std::move(*typeParamList),
      std::move(memberFunctions), std::move(traitList));
}

// <implDecl>
//  ::= <implIdentifier> (';' | ('{' <functionDecl>* '}'))
//
// <implIdentifier>
//  ::= 'impl' <userDefinedDeclInstance>
std::unique_ptr<ast::ImplDecl> Parser::parseImplDecl() {
  matchOrReturn(TokenKind::KwImpl, "expected 'impl'");
  eatNextToken(); // eat 'impl'

  varOrReturn(owningTrait, parseTraitInstance());

  std::vector<std::unique_ptr<ast::FunctionDecl>> functionImpls;
  bool hasBody = nextToken.kind == TokenKind::Lbrace;

  if (!hasBody && nextToken.kind != TokenKind::Semi)
    return report(nextToken.location, "expected ';' or '{'");

  eatNextToken(); // eat ';' or '{'

  if (hasBody) {
    while (true) {
      if (nextToken.kind == TokenKind::Rbrace)
        break;

      if (auto fn = parseFunctionDecl())
        functionImpls.emplace_back(std::move(fn));
      else
        synchronize();
    }

    matchOrReturn(TokenKind::Rbrace, "expected '}'");
    eatNextToken(); // eat '}'
  }

  return std::make_unique<ast::ImplDecl>(std::move(owningTrait),
                                         std::move(functionImpls));
}

// <functionDecl>
//  ::= <functionHeader> <block>

// <functionHeader>
//  ::= 'fn' <identifier> <typeParamList>? <parameterList> ':' <type>?
//
// <parameterList>
//  ::= '(' (<paramDecl> (',' <paramDecl>)* ','?)? ')'
std::unique_ptr<ast::FunctionDecl> Parser::parseFunctionDecl() {
  matchOrReturn(TokenKind::KwFn, "expected 'fn'");
  eatNextToken(); // eat fn

  return parseFunctionSignature();
}

std::unique_ptr<ast::FunctionDecl> Parser::parseFunctionSignature() {
  SourceLocation location = nextToken.location;
  matchOrReturn(TokenKind::Identifier, "expected identifier");

  assert(nextToken.value && "identifier token without value");
  std::string functionIdentifier = *nextToken.value;
  eatNextToken(); // eat identifier

  varOrReturn(typeParamList, parseTypeParamList());

  varOrReturn(parameterList,
              parseListWithTrailingComma<ast::ParamDecl>(
                  {TokenKind::Lpar, "expected '('"}, &Parser::parseParamDecl,
                  {TokenKind::Rpar, "expected ')'"}));

  TokenKind nextTokenKind = nextToken.kind;
  if (nextTokenKind != TokenKind::Colon && nextTokenKind != TokenKind::Semi &&
      nextTokenKind != TokenKind::Lbrace)
    return report(nextToken.location, "expected ':', ';' or '{'");

  std::unique_ptr<ast::Type> type;
  if (nextTokenKind == TokenKind::Colon) {
    eatNextToken(); // eat ':'

    type = parseType();
    if (!type)
      return nullptr;
  }

  std::unique_ptr<ast::Block> block = nullptr;
  if (nextToken.kind == TokenKind::Lbrace) {
    varOrReturn(b, parseBlock());
    block = std::move(b);
  } else if (restrictions & FunctionWithoutBodyAllowed) {
    matchOrReturn(TokenKind::Semi, "expected ';' or '{'");
    eatNextToken(); // eat ';'
  } else {
    return report(nextToken.location, "expected function body");
  }

  return std::make_unique<ast::FunctionDecl>(
      location, functionIdentifier, std::move(type), std::move(*typeParamList),
      std::move(*parameterList), std::move(block));
}

// <paramDecl>
//  ::= 'mut'? <identifier> ':' <type>
std::unique_ptr<ast::ParamDecl> Parser::parseParamDecl() {
  SourceLocation location = nextToken.location;

  bool isMut = nextToken.kind == TokenKind::KwMut;
  if (isMut)
    eatNextToken(); // eat 'mut'

  matchOrReturn(TokenKind::Identifier, "expected parameter declaration");
  assert(nextToken.value && "identifier token without value");

  std::string identifier = *nextToken.value;
  eatNextToken(); // eat identifier

  matchOrReturn(TokenKind::Colon, "expected ':'");
  eatNextToken(); // eat :

  varOrReturn(type, parseType());

  return std::make_unique<ast::ParamDecl>(location, std::move(identifier),
                                          std::move(type), isMut);
}

// <varDecl>
//  ::= <identifier> (':' <type>)? ('=' <expr>)?
std::unique_ptr<ast::VarDecl> Parser::parseVarDecl(bool isLet) {
  SourceLocation location = nextToken.location;

  assert(nextToken.value && "identifier token without value");

  std::string identifier = *nextToken.value;
  eatNextToken(); // eat identifier

  std::unique_ptr<ast::Type> type;
  if (nextToken.kind == TokenKind::Colon) {
    eatNextToken(); // eat ':'

    type = parseType();
    if (!type)
      return nullptr;
  }

  if (nextToken.kind != TokenKind::Equal)
    return std::make_unique<ast::VarDecl>(location, identifier, std::move(type),
                                          !isLet);
  eatNextToken(); // eat '='

  varOrReturn(initializer, parseExpr());

  return std::make_unique<ast::VarDecl>(location, identifier, std::move(type),
                                        !isLet, std::move(initializer));
}

// <block>
//  ::= '{' <statement>* '}'
std::unique_ptr<ast::Block> Parser::parseBlock() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat '{'

  std::vector<std::unique_ptr<ast::Stmt>> expressions;
  while (true) {
    if (nextToken.kind == TokenKind::Rbrace)
      break;

    if (isTopLevelToken(nextToken.kind))
      return report(nextToken.location, "expected '}' at the end of a block");

    std::unique_ptr<ast::Stmt> stmt = parseStmt();
    if (!stmt) {
      synchronize();
      continue;
    }

    expressions.emplace_back(std::move(stmt));
  }

  eatNextToken(); // eat '}'

  return std::make_unique<ast::Block>(location, std::move(expressions));
}

// <ifStatement>
//  ::= 'if' <expr> <block> ('else' (<ifStatement> | <block>))?
std::unique_ptr<ast::IfStmt> Parser::parseIfStmt() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat 'if'

  varOrReturn(condition,
              withRestrictions(StructNotAllowed, &Parser::parseExpr));

  matchOrReturn(TokenKind::Lbrace, "expected 'if' body");

  varOrReturn(trueBlock, parseBlock());

  if (nextToken.kind != TokenKind::KwElse)
    return std::make_unique<ast::IfStmt>(location, std::move(condition),
                                         std::move(trueBlock));
  eatNextToken(); // eat 'else'

  std::unique_ptr<ast::Block> falseBlock;
  if (nextToken.kind == TokenKind::KwIf) {
    varOrReturn(elseIf, parseIfStmt());

    SourceLocation loc = elseIf->location;
    std::vector<std::unique_ptr<ast::Stmt>> stmts;
    stmts.emplace_back(std::move(elseIf));

    falseBlock = std::make_unique<ast::Block>(loc, std::move(stmts));
  } else {
    matchOrReturn(TokenKind::Lbrace, "expected 'else' body");
    falseBlock = parseBlock();
  }

  if (!falseBlock)
    return nullptr;

  return std::make_unique<ast::IfStmt>(location, std::move(condition),
                                       std::move(trueBlock),
                                       std::move(falseBlock));
}

// <whileStatement>
//  ::= 'while' <expr> <block>
std::unique_ptr<ast::WhileStmt> Parser::parseWhileStmt() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat 'while'

  varOrReturn(cond, withRestrictions(StructNotAllowed, &Parser::parseExpr));

  matchOrReturn(TokenKind::Lbrace, "expected 'while' body");
  varOrReturn(body, parseBlock());

  return std::make_unique<ast::WhileStmt>(location, std::move(cond),
                                          std::move(body));
}

// <declStmt>
//  ::= ('let'|'mut') <varDecl>  ';'
std::unique_ptr<ast::DeclStmt> Parser::parseDeclStmt() {
  Token tok = nextToken;
  eatNextToken(); // eat 'let' | 'mut'

  matchOrReturn(TokenKind::Identifier, "expected identifier");
  varOrReturn(varDecl, parseVarDecl(tok.kind == TokenKind::KwLet));

  matchOrReturn(TokenKind::Semi, "expected ';' after declaration");
  eatNextToken(); // eat ';'

  return std::make_unique<ast::DeclStmt>(tok.location, std::move(varDecl));
}

// <returnStmt>
//  ::= 'return' <expr> ';'
std::unique_ptr<ast::ReturnStmt> Parser::parseReturnStmt() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat 'return'

  std::unique_ptr<ast::Expr> expr;
  if (nextToken.kind != TokenKind::Semi) {
    expr = parseExpr();
    if (!expr)
      return nullptr;
  }

  matchOrReturn(TokenKind::Semi,
                "expected ';' at the end of a return statement");
  eatNextToken(); // eat ';'

  return std::make_unique<ast::ReturnStmt>(location, std::move(expr));
}

// <fieldInit>
//  ::= <identifier> ':' <expr>
std::unique_ptr<ast::FieldInitStmt> Parser::parseFieldInitStmt() {
  matchOrReturn(TokenKind::Identifier, "expected field initialization");

  SourceLocation location = nextToken.location;
  assert(nextToken.value && "identifier token without value");

  std::string identifier = *nextToken.value;
  eatNextToken(); // eat identifier

  matchOrReturn(TokenKind::Colon, "expected ':'");
  eatNextToken(); // eat ':'

  varOrReturn(init, parseExpr());

  return std::make_unique<ast::FieldInitStmt>(location, std::move(identifier),
                                              std::move(init));
}

// <statement>
//  ::= <expr> ';'
//  |   <returnStmt>
//  |   <ifStatement>
//  |   <whileStatement>
//  |   <assignment>
//  |   <declStmt>
std::unique_ptr<ast::Stmt> Parser::parseStmt() {
  if (nextToken.kind == TokenKind::KwIf)
    return parseIfStmt();

  if (nextToken.kind == TokenKind::KwWhile)
    return parseWhileStmt();

  if (nextToken.kind == TokenKind::KwReturn)
    return parseReturnStmt();

  if (nextToken.kind == TokenKind::KwLet || nextToken.kind == TokenKind::KwMut)
    return parseDeclStmt();

  return parseAssignmentOrExpr();
}

// <statement>
//  ::= <expr> ';'
//  |   ...
//
// <assignment>
//  ::= <expr> '=' <expr> ';'
std::unique_ptr<ast::Stmt> Parser::parseAssignmentOrExpr() {
  varOrReturn(lhs, parseExpr());

  if (nextToken.kind != TokenKind::Equal) {
    matchOrReturn(TokenKind::Semi, "expected ';' at the end of expression");
    eatNextToken(); // eat ';'

    return lhs;
  }

  SourceLocation location = nextToken.location;
  eatNextToken(); // eat '='

  varOrReturn(rhs, parseExpr());

  matchOrReturn(TokenKind::Semi, "expected ';' at the end of assignment");
  eatNextToken(); // eat ';'

  return std::make_unique<ast::Assignment>(location, std::move(lhs),
                                           std::move(rhs));
}

std::unique_ptr<ast::Expr> Parser::parseExpr() {
  varOrReturn(lhs, parsePrefixExpr());
  return parseExprRHS(std::move(lhs), 0);
}

std::unique_ptr<ast::Expr> Parser::parseExprRHS(std::unique_ptr<ast::Expr> lhs,
                                                int precedence) {
  while (true) {
    Token op = nextToken;
    int curOpPrec = getTokPrecedence(op.kind);

    if (curOpPrec < precedence)
      return lhs;
    eatNextToken(); // eat operator

    varOrReturn(rhs, parsePrefixExpr());

    if (curOpPrec < getTokPrecedence(nextToken.kind)) {
      rhs = parseExprRHS(std::move(rhs), curOpPrec + 1);
      if (!rhs)
        return nullptr;
    }

    lhs = std::make_unique<ast::BinaryOperator>(op.location, std::move(lhs),
                                                std::move(rhs), op.kind);
  }
}

// <prefixExpression>
//  ::= ('!' | '-' | '&')* <postfixExpression>
std::unique_ptr<ast::Expr> Parser::parsePrefixExpr() {
  Token tok = nextToken;

  if (tok.kind != TokenKind::Excl && tok.kind != TokenKind::Minus &&
      tok.kind != TokenKind::Amp)
    return parsePostfixExpr();
  eatNextToken(); // eat '!', '-' or '&'

  varOrReturn(rhs, parsePrefixExpr());

  return std::make_unique<ast::UnaryOperator>(tok.location, std::move(rhs),
                                              tok.kind);
}

// <postfixExpression>
//  ::= <primaryExpression> (<argumentList> | <memberExpr>)*
//
// <argumentList>
//  ::= '(' (<expr> (',' <expr>)* ','?)? ')'
//
// <memberExpr>
//  ::= '.' <declRefExpr>
std::unique_ptr<ast::Expr> Parser::parsePostfixExpr() {
  varOrReturn(expr, parsePrimary());

  while (true) {
    SourceLocation location = nextToken.location;

    if (nextToken.kind == TokenKind::Lpar) {
      varOrReturn(argumentList,
                  parseListWithTrailingComma<ast::Expr>(
                      {TokenKind::Lpar, "expected '('"}, &Parser::parseExpr,
                      {TokenKind::Rpar, "expected ')'"}));

      expr = std::make_unique<ast::CallExpr>(location, std::move(expr),
                                             std::move(*argumentList));
      continue;
    }

    if (nextToken.kind == TokenKind::Dot) {
      eatNextToken(); // eat '.'

      varOrReturn(member, parseDeclRefExpr());

      expr = std::make_unique<ast::MemberExpr>(location, std::move(expr),
                                               std::move(member));
      continue;
    }

    break;
  }

  return expr;
}

// <primaryExpression>
//  ::= 'unit'
//  |   <numberLiteral>
//  |   <pathExpr> <fieldInitList>?
//  |   '(' <expr> ')'
//
// <fieldInitList>
//  ::= '{' (<fieldInit> (',' <fieldInit>)* ','?)? '}'
std::unique_ptr<ast::Expr> Parser::parsePrimary() {
  SourceLocation location = nextToken.location;

  if (nextToken.kind == TokenKind::Lpar) {
    eatNextToken(); // eat '('

    varOrReturn(expr, withNoRestrictions(&Parser::parseExpr));

    matchOrReturn(TokenKind::Rpar, "expected ')'");
    eatNextToken(); // eat ')'

    return std::make_unique<ast::GroupingExpr>(location, std::move(expr));
  }

  if (nextToken.kind == TokenKind::KwUnit) {
    auto literal = std::make_unique<ast::UnitLiteral>(location);
    eatNextToken(); // eat unit
    return literal;
  }

  if (nextToken.kind == TokenKind::Number) {
    auto literal =
        std::make_unique<ast::NumberLiteral>(location, *nextToken.value);
    eatNextToken(); // eat number
    return literal;
  }

  if (nextToken.kind == TokenKind::Identifier ||
      nextToken.kind == TokenKind::KwSelf) {
    varOrReturn(path, parsePathExpr());
    if (restrictions & StructNotAllowed || nextToken.kind != TokenKind::Lbrace)
      return path;

    location = nextToken.location;
    auto fieldInitList = parseListWithTrailingComma<ast::FieldInitStmt>(
        {TokenKind::Lbrace, "expected '{'"}, &Parser::parseFieldInitStmt,
        {TokenKind::Rbrace, "expected ',' or '}'"});

    if (!fieldInitList) {
      synchronizeOn({TokenKind::Rbrace});
      eatNextToken(); // eat '}'
      return nullptr;
    }

    return std::make_unique<ast::StructInstantiationExpr>(
        location, std::move(path), std::move(*fieldInitList));
  }

  return report(location, "expected expression");
}

// <pathExpr>
//  ::= <declRefExpr> ('::' (<implSpecifier> '::')? <declRefExpr>)*
std::unique_ptr<ast::PathExpr> Parser::parsePathExpr() {
  std::vector<ast::PathExpr::ImplXDecl> path;

  varOrReturn(dre, parseDeclRefExpr());
  path.emplace_back(nullptr, std::move(dre));

  while (true) {
    if (nextToken.kind != TokenKind::ColonColon)
      break;
    eatNextToken(); // eat '::'

    std::unique_ptr<ast::ImplSpecifier> impl;
    if (nextToken.kind == TokenKind::KwImpl) {
      impl = parseImplSpecifier();
      if (!impl)
        return nullptr;

      matchOrReturn(TokenKind::ColonColon, "expected '::'");
      eatNextToken(); // eat '::'
    }

    varOrReturn(dre, parseDeclRefExpr());
    path.emplace_back(std::move(impl), std::move(dre));
  }

  return std::make_unique<ast::PathExpr>(std::move(path));
}

std::unique_ptr<ast::DeclRefExpr> Parser::parseDeclRefExpr() {
  SourceLocation location = nextToken.location;

  if (nextToken.kind != TokenKind::Identifier &&
      nextToken.kind != TokenKind::KwSelf)
    return report(location, "expected identifier or 'Self'");

  assert(nextToken.value && "identifier without value");
  std::string identifier = *nextToken.value;
  eatNextToken(); // eat identifier or 'Self'

  std::unique_ptr<ast::TypeArgumentList> typeArgsList = nullptr;
  if (nextToken.kind == TokenKind::At) {
    varOrReturn(parsedTypeArgList, parseTypeArgumentList());
    typeArgsList = std::move(parsedTypeArgList);
  }

  return std::make_unique<ast::DeclRefExpr>(location, std::move(identifier),
                                            std::move(typeArgsList));
}

std::unique_ptr<ast::TypeArgumentList> Parser::parseTypeArgumentList() {
  matchOrReturn(TokenKind::At, "expected '@'");

  SourceLocation location = nextToken.location;
  eatNextToken(); // eat '@'

  varOrReturn(args, parseTypeList());

  return std::make_unique<ast::TypeArgumentList>(location, std::move(*args));
}

// <traitList>
//  ::= ':' <userDefinedDeclInstance> ('&' <userDefinedDeclInstance>)*
std::unique_ptr<ast::TraitList> Parser::parseTraitList() {
  std::vector<std::unique_ptr<ast::TraitInstance>> traits;

  if (nextToken.kind == TokenKind::Colon) {
    eatNextToken(); // eat ':'

    while (true) {
      matchOrReturn(TokenKind::Identifier, "expected identifier");
      varOrReturn(trait, parseTraitInstance());
      traits.emplace_back(std::move(trait));

      if (nextToken.kind != TokenKind::Amp)
        break;

      eatNextToken(); // eat '&'
    }
  }

  return std::make_unique<ast::TraitList>(std::move(traits));
}

// <TList>
//  ::= <openingToken> (<T> (',' <T>)* ','?)? <closingToken>
template <typename T, typename F>
std::unique_ptr<std::vector<std::unique_ptr<T>>>
Parser::parseListWithTrailingComma(
    std::pair<TokenKind, const char *> openingToken,
    F parser,
    std::pair<TokenKind, const char *> closingToken,
    bool allowEmpty) {
  matchOrReturn(openingToken.first, openingToken.second);
  eatNextToken(); // eat openingToken

  std::vector<std::unique_ptr<T>> list;
  while (true) {
    if (allowEmpty && nextToken.kind == closingToken.first)
      break;

    varOrReturn(init, (this->*parser)());
    list.emplace_back(std::move(init));

    if (nextToken.kind != TokenKind::Comma)
      break;
    eatNextToken(); // eat ','

    if (!allowEmpty && nextToken.kind == closingToken.first)
      break;
  }

  matchOrReturn(closingToken.first, closingToken.second);
  eatNextToken(); // eat closingToken

  return std::make_unique<decltype(list)>(std::move(list));
}

// <type>
//  ::= <builtinType>
//  |   <userDefinedDeclInstance>
//  |   <functionType>
//  |   <outParamType>
//
// <builtinType>
//  ::= 'number'
//  |   'unit'
//  |   'Self'
//
// <userDefinedDeclInstance>
//  ::= <identifier> <typeList>?
//
// <functionType>
//  ::= '(' <type> (',' <type>)* ','? ')' -> type
//
// <outParamType>
//  ::= '&' <type>
std::unique_ptr<ast::Type> Parser::parseType() {
  SourceLocation location = nextToken.location;

  if (nextToken.kind == TokenKind::KwNumber) {
    eatNextToken(); // eat 'number'
    return std::make_unique<ast::BuiltinType>(location,
                                              ast::BuiltinType::Kind::Number);
  }

  if (nextToken.kind == TokenKind::KwUnit) {
    eatNextToken(); // eat 'unit'
    return std::make_unique<ast::BuiltinType>(location,
                                              ast::BuiltinType::Kind::Unit);
  }

  if (nextToken.kind == TokenKind::KwSelf) {
    eatNextToken(); // eat 'Self'
    return std::make_unique<ast::BuiltinType>(location,
                                              ast::BuiltinType::Kind::Self);
  }

  if (nextToken.kind == TokenKind::Identifier)
    return parseUserDefinedType();

  if (nextToken.kind == TokenKind::Lpar) {
    SourceLocation location = nextToken.location;
    auto argumentList = parseListWithTrailingComma<ast::Type>(
        {TokenKind::Lpar, "expected '('"}, &Parser::parseType,
        {TokenKind::Rpar, "expected ')'"});

    matchOrReturn(TokenKind::Arrow, "expected '->'");
    eatNextToken(); // eat '->'

    varOrReturn(returnType, parseType());
    return std::make_unique<ast::FunctionType>(
        location, std::move(*argumentList), std::move(returnType));
  }

  if (nextToken.kind == TokenKind::Amp) {
    SourceLocation location = nextToken.location;
    eatNextToken(); // eat '&'

    varOrReturn(referencedType, parseType());
    return std::make_unique<ast::OutParamType>(location,
                                               std::move(referencedType));
  }

  return report(nextToken.location, "expected type specifier");
};

template <typename T> std::unique_ptr<T> Parser::parseIdentifierWithTypelist() {
  SourceLocation location = nextToken.location;

  matchOrReturn(TokenKind::Identifier, "expected identifier");
  assert(nextToken.value && "identifier without value");

  std::string identifier = *nextToken.value;
  eatNextToken(); // eat identifier

  std::vector<std::unique_ptr<ast::Type>> types;
  if (nextToken.kind == TokenKind::Lt) {
    varOrReturn(typeArguments, parseTypeList());
    types = std::move(*typeArguments);
  }

  return std::make_unique<T>(location, std::move(identifier), std::move(types));
}

std::unique_ptr<ast::UserDefinedType> Parser::parseUserDefinedType() {
  return parseIdentifierWithTypelist<ast::UserDefinedType>();
}

std::unique_ptr<ast::TraitInstance> Parser::parseTraitInstance() {
  return parseIdentifierWithTypelist<ast::TraitInstance>();
}

std::unique_ptr<ast::ImplSpecifier> Parser::parseImplSpecifier() {
  SourceLocation location = nextToken.location;

  matchOrReturn(TokenKind::KwImpl, "expected 'impl'");
  eatNextToken(); // eat 'impl'

  varOrReturn(trait, parseTraitInstance());
  return std::make_unique<ast::ImplSpecifier>(location, std::move(trait));
}

// <sourceFile>
//     ::= (<traitDecl> | <structDecl> | <functionDecl>)* EOF
std::pair<ast::Context, bool> Parser::parseSourceFile() {
  ast::Context ctx;

  while (nextToken.kind != TokenKind::Eof) {
    if (nextToken.kind == TokenKind::KwFn) {
      if (auto fn = parseFunctionDecl()) {
        ctx.addFunctionDecl(std::move(fn));
        continue;
      }
    } else if (nextToken.kind == TokenKind::KwStruct) {
      if (auto st = parseStructDecl()) {
        ctx.addStructDecl(std::move(st));
        continue;
      }
    } else if (nextToken.kind == TokenKind::KwTrait) {
      if (auto trait = parseTraitDecl()) {
        ctx.addTraitDecl(std::move(trait));
        continue;
      }
    } else {
      report(nextToken.location,
             "expected function, struct or trait declaration on the top level");
    }

    synchronizeOn({TokenKind::KwFn, TokenKind::KwStruct, TokenKind::KwTrait});
    continue;
  }

  assert(nextToken.kind == TokenKind::Eof && "expected to see end of file");

  // Only the lexer and the parser has access to the tokens, so to report an
  // error on the EOF token, we look for main() here.
  bool hasMainFunction = false;
  for (auto &&decl : ctx.functions)
    hasMainFunction |= decl->identifier == "main";

  if (!hasMainFunction && !incompleteAST)
    report(nextToken.location, "main function not found");

  return {std::move(ctx), !incompleteAST && hasMainFunction};
}
} // namespace yl
