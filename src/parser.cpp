#include <iostream>

#include "parser.h"

std::nullptr_t TheParser::error(SourceLocation location,
                                std::string_view message) {
  const auto &[file, line, col] = location;

  std::cerr << file << ':' << line << ':' << col << ": error: " << message
            << '\n';

  return nullptr;
}

bool TheParser::skipUntil(TokenKind kind) {
  while (true) {
    if (nextToken.kind == kind)
      return true;

    if (nextToken.kind == TokenKind::rbrace)
      return false;

    if (nextToken.kind == TokenKind::eof)
      return false;

    eatNextToken();
  }
}

// <IdentifierExpression>
//  ::= <DeclRefExpr>
//  ::= identifier ( )
//  ::= identifier ( (Expr ,)* Expr )
std::unique_ptr<Expr> TheParser::parseIdentifierExpr() {
  SourceLocation location = nextToken.location;
  std::string identifier = *nextToken.value;

  auto declRefExpr = std::make_unique<DeclRefExpr>(location, identifier);

  eatNextToken(); // eat identifier

  if (nextToken.kind != TokenKind::lpar)
    return declRefExpr;

  location = nextToken.location;
  eatNextToken(); // eat '('

  std::vector<std::unique_ptr<Expr>> arguments;
  while (true) {
    if (nextToken.kind == TokenKind::rpar)
      break;

    if (std::unique_ptr<Expr> expr = parseExpr())
      arguments.emplace_back(std::move(expr));
    else
      return nullptr;

    if (nextToken.kind == TokenKind::comma)
      eatNextToken(); // eat ,
  }

  if (nextToken.kind != TokenKind::rpar)
    return error(nextToken.location, "expected ')' in function call");
  eatNextToken(); // eat ')'

  return std::make_unique<CallExpr>(location, std::move(declRefExpr),
                                    std::move(arguments));
}

// <Expr>
//  ::= <NumberLiteral>
//  ::= <StringLiteral>
//  ::= <IdentifierExpression>
std::unique_ptr<Expr> TheParser::parseExpr() {
  if (nextToken.kind == TokenKind::number) {
    auto literal =
        std::make_unique<NumberLiteral>(nextToken.location, *nextToken.value);
    eatNextToken(); // eat NumberLiteral
    return literal;
  } else if (nextToken.kind == TokenKind::string) {
    auto literal =
        std::make_unique<StringLiteral>(nextToken.location, *nextToken.value);
    eatNextToken(); // eat StringLiteral
    return literal;
  } else if (nextToken.kind == TokenKind::identifier)
    return parseIdentifierExpr();
  else
    return error(nextToken.location, "expected expression");
}

// Stmt
//  ::= Expr ;
std::unique_ptr<Stmt> TheParser::parseStmt() {
  std::unique_ptr<Expr> expr = parseExpr();

  if (!expr) {
    if (skipUntil(TokenKind::semi))
      eatNextToken(); // eat ';'
    return nullptr;
  }

  if (nextToken.kind != TokenKind::semi)
    return error(nextToken.location, "expected ';' at the end of statement");

  eatNextToken(); // eat ';'

  return expr;
}

// <Block>
//  ::= { <Stmt>* }
std::unique_ptr<Block> TheParser::parseBlock() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat '{'

  std::vector<std::unique_ptr<Stmt>> statements;
  while (true) {
    if (nextToken.kind == TokenKind::rbrace)
      break;

    if (nextToken.kind == TokenKind::eof)
      return error(nextToken.location, "expected '}' at the end of a block");

    std::unique_ptr<Stmt> stmt = parseStmt();
    if (!stmt)
      return nullptr;

    statements.emplace_back(std::move(stmt));
  }

  eatNextToken(); // eat '}'

  return std::make_unique<Block>(location, std::move(statements));
}

// <FunctionDecl>
//  ::= fn identifier ( ) : identifier <Block>
//  ::= fn identifier ( (<ParamDecl> ,)* <ParamDecl> ) : identifier <Block>
std::unique_ptr<FunctionDecl> TheParser::parseFunctionDecl() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat fn

  if (nextToken.kind != TokenKind::identifier)
    return error(nextToken.location, "expected identifier");
  std::string functionIdentifier = *nextToken.value;
  eatNextToken(); // eat identifier

  if (nextToken.kind != TokenKind::lpar)
    return error(nextToken.location, "expected '('");
  eatNextToken(); // eat '('

  std::vector<std::unique_ptr<ParamDecl>> params;
  while (true) {
    if (nextToken.kind == TokenKind::rpar)
      break;

    if (nextToken.kind != TokenKind::identifier)
      return error(nextToken.location, "expected parameter declaration");

    if (auto paramDecl = parseParamDecl())
      params.emplace_back(std::move(paramDecl));
    else
      return nullptr;

    if (nextToken.kind == TokenKind::comma)
      eatNextToken(); // eat ','
  }

  if (nextToken.kind != TokenKind::rpar)
    return error(nextToken.location, "expected ')'");
  eatNextToken(); // eat ')'

  if (nextToken.kind != TokenKind::colon)
    return error(nextToken.location, "expected ':'");
  eatNextToken(); // eat ':'

  if (nextToken.kind != TokenKind::identifier)
    return error(nextToken.location, "expected type specifier");
  std::string type = *nextToken.value;
  eatNextToken(); // eat identifier

  if (nextToken.kind != TokenKind::lbrace)
    return error(nextToken.location, "expected function body");

  std::unique_ptr<Block> block = parseBlock();

  if (!block)
    return nullptr;

  return std::make_unique<FunctionDecl>(location, functionIdentifier, type,
                                        std::move(params), std::move(block));
}

// <ParamDecl>
//  ::= identifier : identifier
std::unique_ptr<ParamDecl> TheParser::parseParamDecl() {
  SourceLocation location = nextToken.location;

  std::string identifier = *nextToken.value;
  eatNextToken(); // eat identifier

  if (nextToken.kind != TokenKind::colon)
    return error(nextToken.location, "expected ':'");
  eatNextToken(); // eat :

  if (nextToken.kind != TokenKind::identifier)
    return error(nextToken.location, "expected type specifier for parameter");

  std::string type = *nextToken.value;
  eatNextToken(); // eat identifier

  return std::make_unique<ParamDecl>(location, std::move(identifier),
                                     std::move(type));
};

// <TopLevel>
//  ::= <FunctionDecl>
std::vector<std::unique_ptr<FunctionDecl>> TheParser::parseTopLevel() {
  std::vector<std::unique_ptr<FunctionDecl>> functions;

  while (nextToken.kind != TokenKind::eof) {
    if (nextToken.kind != TokenKind::fn) {
      error(nextToken.location,
            "only function definitions are allowed on the top level");
      return {};
    }

    if (auto fn = parseFunctionDecl())
      functions.emplace_back(std::move(fn));
    else
      return {};
  }

  return functions;
}
