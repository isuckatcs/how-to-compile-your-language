#include "parser.h"

namespace {
int getTokPrecedence(TokenKind tok) {
  if (tok == TokenKind::asterisk || tok == TokenKind::slash)
    return 6;

  if (tok == TokenKind::plus || tok == TokenKind::minus)
    return 5;

  if (tok == TokenKind::lt || tok == TokenKind::gt)
    return 4;

  if (tok == TokenKind::equalequal)
    return 3;

  if (tok == TokenKind::ampamp)
    return 2;

  if (tok == TokenKind::pipepipe)
    return 1;

  return -1;
}
}; // namespace

// <functionDecl>
//  ::= 'fn' <identifier> <parameterList> ':' <type> <block>
std::unique_ptr<FunctionDecl> TheParser::parseFunctionDecl() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat fn

  if (nextToken.kind != TokenKind::identifier)
    return error(nextToken.location, "expected identifier");
  std::string functionIdentifier = *nextToken.value;
  eatNextToken(); // eat identifier

  std::optional<ParameterList> parameterList = parseParameterList();
  if (!parameterList)
    return nullptr;

  if (nextToken.kind != TokenKind::colon)
    return error(nextToken.location, "expected ':'");
  eatNextToken(); // eat ':'

  std::optional<std::string> type = parseType();
  if (!type)
    return nullptr;

  if (nextToken.kind != TokenKind::lbrace)
    return error(nextToken.location, "expected function body");

  std::unique_ptr<Block> block = parseBlock();

  if (!block)
    return nullptr;

  return std::make_unique<FunctionDecl>(location, functionIdentifier, *type,
                                        std::move(*parameterList),
                                        std::move(block));
}

// <paramDecl>
//  ::= <identifier> ':' <type>
std::unique_ptr<ParamDecl> TheParser::parseParamDecl() {
  SourceLocation location = nextToken.location;

  std::string identifier = *nextToken.value;
  eatNextToken(); // eat identifier

  if (nextToken.kind != TokenKind::colon)
    return error(nextToken.location, "expected ':'");
  eatNextToken(); // eat :

  std::optional<std::string> type = parseType();
  if (!type)
    return nullptr;

  return std::make_unique<ParamDecl>(location, std::move(identifier),
                                     std::move(*type));
}

// <block>
//  ::= '{' (<expr> ';')* '}'
std::unique_ptr<Block> TheParser::parseBlock() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat '{'

  std::vector<std::unique_ptr<Expr>> expressions;
  while (true) {
    if (nextToken.kind == TokenKind::rbrace)
      break;

    if (nextToken.kind == TokenKind::eof)
      return error(nextToken.location, "expected '}' at the end of a block");

    std::unique_ptr<Expr> expr = parseExpr();
    if (!expr)
      return nullptr;

    if (nextToken.kind != TokenKind::semi)
      return error(nextToken.location,
                   "expected ';' at the end of an expression");
    eatNextToken(); // eat ';'

    expressions.emplace_back(std::move(expr));
  }

  eatNextToken(); // eat '}'

  return std::make_unique<Block>(location, std::move(expressions));
}

std::unique_ptr<Expr> TheParser::parseExpr() {
  auto LHS = parsePrefixExpr();
  if (!LHS)
    return nullptr;
  return parseExprRHS(std::move(LHS), 0);
}

std::unique_ptr<Expr> TheParser::parseExprRHS(std::unique_ptr<Expr> LHS,
                                              int precedence) {
  while (true) {
    TokenKind op = nextToken.kind;
    int curOpPrec = getTokPrecedence(op);

    if (curOpPrec < precedence)
      return LHS;
    eatNextToken(); // eat opearator

    auto RHS = parsePrefixExpr();
    if (!RHS)
      return nullptr;

    if (curOpPrec < getTokPrecedence(nextToken.kind)) {
      RHS = parseExprRHS(std::move(RHS), curOpPrec + 1);
      if (!RHS)
        return nullptr;
    }

    LHS = std::make_unique<BinaryOperator>(LHS->location, std::move(LHS),
                                           std::move(RHS), op);
  }
}

std::unique_ptr<Expr> TheParser::parsePrefixExpr() {
  Token tok = nextToken;

  if (tok.kind != TokenKind::excl)
    return parsePrimary();
  eatNextToken(); // eat !

  auto RHS = parsePrefixExpr();
  if (!RHS)
    return nullptr;

  return std::make_unique<UnaryOperator>(tok.location, std::move(RHS),
                                         tok.kind);
}

// <primaryExpr>
//  ::= <numberLiteral>
//  |   <declRefExpr>
//  |   <callExpr>
//  |   '(' <expr> ')'
//
// <numberLiteral>
//  ::= <number>
//
// <declRefExpr>
//  ::= <identifier>
//
// <callExpr>
//  ::= <declRefExpr> <argumentList>
std::unique_ptr<Expr> TheParser::parsePrimary() {
  SourceLocation location = nextToken.location;

  if (nextToken.kind == TokenKind::lpar) {
    eatNextToken(); // eat '('

    auto expr = parseExpr();
    if (!expr)
      return nullptr;

    if (nextToken.kind != TokenKind::rpar)
      return error(nextToken.location, "expected ')'");
    eatNextToken(); // eat ')'

    return std::make_unique<GroupingExpr>(location, std::move(expr));
  }

  if (nextToken.kind == TokenKind::number) {
    auto literal = std::make_unique<NumberLiteral>(location, *nextToken.value);
    eatNextToken(); // eat NumberLiteral
    return literal;
  }

  if (nextToken.kind == TokenKind::identifier) {
    auto declRefExpr =
        std::make_unique<DeclRefExpr>(location, *nextToken.value);
    eatNextToken(); // eat identifier

    if (nextToken.kind != TokenKind::lpar)
      return declRefExpr;
    location = nextToken.location;

    std::optional<ArgumentList> argumentList = parseArgumentList();
    if (!argumentList)
      return nullptr;

    return std::make_unique<CallExpr>(location, std::move(declRefExpr),
                                      std::move(*argumentList));
  }

  return error(location, "expected expression");
}

// <parameterList>
//  ::= '(' (<paramDecl> (',' <paramDecl>)*)? ')'
std::optional<TheParser::ParameterList> TheParser::parseParameterList() {
  if (nextToken.kind != TokenKind::lpar) {
    error(nextToken.location, "expected '('");
    return std::nullopt;
  }
  eatNextToken(); // eat '('

  std::vector<std::unique_ptr<ParamDecl>> parameterList;

  if (nextToken.kind == TokenKind::rpar) {
    eatNextToken(); // eat ')'
    return parameterList;
  }

  while (true) {
    if (nextToken.kind != TokenKind::identifier) {
      error(nextToken.location, "expected parameter declaration");
      return std::nullopt;
    }

    auto paramDecl = parseParamDecl();
    if (!paramDecl)
      return std::nullopt;
    parameterList.emplace_back(std::move(paramDecl));

    if (nextToken.kind != TokenKind::comma)
      break;
    eatNextToken(); // eat ','
  }

  if (nextToken.kind != TokenKind::rpar) {
    error(nextToken.location, "expected ')'");
    return std::nullopt;
  }
  eatNextToken(); // eat ')'

  return parameterList;
}

// <argumentList>
//  ::= '(' (<expr> (',' <expr>)*)? ')'
std::optional<TheParser::ArgumentList> TheParser::parseArgumentList() {
  if (nextToken.kind != TokenKind::lpar) {
    error(nextToken.location, "expected '('");
    return std::nullopt;
  }
  eatNextToken(); // eat '('

  std::vector<std::unique_ptr<Expr>> argumentList;

  if (nextToken.kind == TokenKind::rpar) {
    eatNextToken(); // eat ')'
    return argumentList;
  }

  while (true) {
    auto expr = parseExpr();
    if (!expr)
      return std::nullopt;
    argumentList.emplace_back(std::move(expr));

    if (nextToken.kind != TokenKind::comma)
      break;
    eatNextToken(); // eat ','
  }

  if (nextToken.kind != TokenKind::rpar) {
    error(nextToken.location, "expected ')'");
    return std::nullopt;
  }
  eatNextToken(); // eat ')'

  return argumentList;
}

// <type>
//  ::= 'number'
//  |   'void'
//  |   <identifier>
std::optional<std::string> TheParser::parseType() {
  TokenKind kind = nextToken.kind;

  if (kind != TokenKind::kw_number && kind != TokenKind::kw_void &&
      kind != TokenKind::identifier) {
    error(nextToken.location, "expected type specifier");
    return std::nullopt;
  }

  std::string value = *nextToken.value;
  eatNextToken(); // eat 'number' | 'void' | identifier

  return value;
};

// <sourceFile>
//     ::= <functionDecl>* EOF
std::vector<std::unique_ptr<FunctionDecl>> TheParser::parseSourceFile() {
  std::vector<std::unique_ptr<FunctionDecl>> functions;

  while (nextToken.kind != TokenKind::eof) {
    if (nextToken.kind != TokenKind::kw_fn) {
      error(nextToken.location,
            "only function definitions are allowed on the top level");
      return {};
    }

    auto fn = parseFunctionDecl();
    if (!fn)
      return {};
    functions.emplace_back(std::move(fn));
  }

  return functions;
}
