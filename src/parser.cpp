#include "parser.h"
#include <cassert>

namespace {
int getTokPrecedence(TokenKind tok) {
  if (tok == TokenKind::Asterisk || tok == TokenKind::Slash)
    return 6;

  if (tok == TokenKind::Plus || tok == TokenKind::Minus)
    return 5;

  if (tok == TokenKind::Lt || tok == TokenKind::Gt)
    return 4;

  if (tok == TokenKind::EqualEqual)
    return 3;

  if (tok == TokenKind::AmpAmp)
    return 2;

  if (tok == TokenKind::PipePipe)
    return 1;

  return -1;
}
}; // namespace

// <functionDecl>
//  ::= 'fn' <identifier> <parameterList> ':' <type> <block>
std::unique_ptr<FunctionDecl> TheParser::parseFunctionDecl() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat fn

  if (nextToken.kind != TokenKind::Identifier)
    return error(nextToken.location, "expected identifier");

  assert(nextToken.value && "identifier token without value");
  std::string functionIdentifier = *nextToken.value;
  eatNextToken(); // eat identifier

  std::optional<ParameterList> parameterList = parseParameterList();
  if (!parameterList)
    return nullptr;

  if (nextToken.kind != TokenKind::Colon)
    return error(nextToken.location, "expected ':'");
  eatNextToken(); // eat ':'

  std::optional<std::string> type = parseType();
  if (!type)
    return nullptr;

  if (nextToken.kind != TokenKind::Lbrace)
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
  assert(nextToken.value && "identifier token without value");

  std::string identifier = *nextToken.value;
  eatNextToken(); // eat identifier

  if (nextToken.kind != TokenKind::Colon)
    return error(nextToken.location, "expected ':'");
  eatNextToken(); // eat :

  std::optional<std::string> type = parseType();
  if (!type)
    return nullptr;

  return std::make_unique<ParamDecl>(location, std::move(identifier),
                                     std::move(*type));
}

// <varDecl>
//  ::= <identifier> ':' <type> ('=' <expr>)?
std::unique_ptr<VarDecl> TheParser::parseVarDecl(bool isLet) {
  SourceLocation location = nextToken.location;

  assert(nextToken.value && "identifier token without value");

  std::string identifier = *nextToken.value;
  eatNextToken(); // eat identifier

  if (nextToken.kind != TokenKind::Colon)
    return error(nextToken.location, "expected ':'");
  eatNextToken(); // eat ':'

  std::optional<std::string> type = parseType();
  if (!type)
    return nullptr;

  if (nextToken.kind != TokenKind::Equal)
    return std::make_unique<VarDecl>(location, identifier, *type, isLet);
  eatNextToken(); // eat '='

  auto initializer = parseExpr();
  if (!initializer)
    return nullptr;

  return std::make_unique<VarDecl>(location, identifier, *type, isLet,
                                   std::move(initializer));
}

// <block>
//  ::= '{' <statement>* '}'
std::unique_ptr<Block> TheParser::parseBlock() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat '{'

  std::vector<std::unique_ptr<Stmt>> expressions;
  while (true) {
    if (nextToken.kind == TokenKind::Rbrace)
      break;

    if (nextToken.kind == TokenKind::Eof)
      return error(nextToken.location, "expected '}' at the end of a block");

    std::unique_ptr<Stmt> stmt = parseStmt();
    if (!stmt)
      return nullptr;

    expressions.emplace_back(std::move(stmt));
  }

  eatNextToken(); // eat '}'

  return std::make_unique<Block>(location, std::move(expressions));
}

// <ifStatement>
//  ::= 'if' <expr> <block> ('else' (<ifStatement> | <block>))?
std::unique_ptr<IfStmt> TheParser::parseIfStmt() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat 'if'

  std::unique_ptr<Expr> condition = parseExpr();
  if (!condition)
    return nullptr;

  if (nextToken.kind != TokenKind::Lbrace)
    return error(nextToken.location, "expected 'if' body");

  std::unique_ptr<Block> trueBranch = parseBlock();
  if (!trueBranch)
    return nullptr;

  if (nextToken.kind != TokenKind::KwElse)
    return std::make_unique<IfStmt>(location, std::move(condition),
                                    std::move(trueBranch));
  eatNextToken(); // eat 'else'

  if (nextToken.kind == TokenKind::KwIf) {
    std::unique_ptr<IfStmt> elseIf = parseIfStmt();
    if (!elseIf)
      return nullptr;

    return std::make_unique<IfStmt>(location, std::move(condition),
                                    std::move(trueBranch), std::move(elseIf));
  }

  std::unique_ptr<Block> falseBlock = parseBlock();
  if (!falseBlock)
    return nullptr;

  return std::make_unique<IfStmt>(location, std::move(condition),
                                  std::move(trueBranch), std::move(falseBlock));
}

// <assignment>
//  ::= <declRefExpr> '=' <expr>
std::unique_ptr<BinaryOperator>
TheParser::parseAssignmentRHS(std::unique_ptr<Expr> lhs) {
  eatNextToken(); // eat '='

  auto rhs = parseExpr();
  if (!rhs)
    return nullptr;

  return std::make_unique<BinaryOperator>(lhs->location, std::move(lhs),
                                          std::move(rhs), TokenKind::Equal);
}

// <declStmt>
//  ::= ('let'|'var') <varDecl>
std::unique_ptr<DeclStmt> TheParser::parseDeclStmt() {
  Token tok = nextToken;
  eatNextToken(); // eat 'let' | 'var'

  auto varDecl = parseVarDecl(tok.kind == TokenKind::KwLet);
  if (!varDecl)
    return nullptr;

  return std::make_unique<DeclStmt>(tok.location, std::move(varDecl));
}

// <statement>
//  ::= <expr> ';'
//  |   <ifStatement>
//  |   <assignment> ';'
//  |   <declStmt> ';'
std::unique_ptr<Stmt> TheParser::parseStmt() {
  if (nextToken.kind == TokenKind::KwIf)
    return parseIfStmt();

  std::unique_ptr<Stmt> expr = nullptr;
  if (nextToken.kind == TokenKind::KwLet || nextToken.kind == TokenKind::KwVar)
    expr = parseDeclStmt();
  else {

    auto lhs = parsePrefixExpr();
    if (!lhs)
      return nullptr;

    if (nextToken.kind == TokenKind::Equal) {
      if (!dynamic_cast<const DeclRefExpr *>(lhs.get()))
        return error(nextToken.location,
                     "expected variable on the LHS of an assignment");

      expr = parseAssignmentRHS(std::move(lhs));
    } else {
      expr = parseExprRHS(std::move(lhs), 0);
    }
  }

  if (nextToken.kind != TokenKind::Semi)
    return error(nextToken.location,
                 "expected ';' at the end of an expression");
  eatNextToken(); // eat ';'

  return expr;
}

std::unique_ptr<Expr> TheParser::parseExpr() {
  auto lhs = parsePrefixExpr();
  if (!lhs)
    return nullptr;
  return parseExprRHS(std::move(lhs), 0);
}

std::unique_ptr<Expr> TheParser::parseExprRHS(std::unique_ptr<Expr> lhs,
                                              int precedence) {
  while (true) {
    TokenKind op = nextToken.kind;
    int curOpPrec = getTokPrecedence(op);

    if (curOpPrec < precedence)
      return lhs;
    eatNextToken(); // eat opearator

    auto rhs = parsePrefixExpr();
    if (!rhs)
      return nullptr;

    if (curOpPrec < getTokPrecedence(nextToken.kind)) {
      rhs = parseExprRHS(std::move(rhs), curOpPrec + 1);
      if (!rhs)
        return nullptr;
    }

    lhs = std::make_unique<BinaryOperator>(lhs->location, std::move(lhs),
                                           std::move(rhs), op);
  }
}

std::unique_ptr<Expr> TheParser::parsePrefixExpr() {
  Token tok = nextToken;

  if (tok.kind != TokenKind::Excl)
    return parsePrimary();
  eatNextToken(); // eat !

  auto rhs = parsePrefixExpr();
  if (!rhs)
    return nullptr;

  return std::make_unique<UnaryOperator>(tok.location, std::move(rhs),
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

  if (nextToken.kind == TokenKind::Lpar) {
    eatNextToken(); // eat '('

    auto expr = parseExpr();
    if (!expr)
      return nullptr;

    if (nextToken.kind != TokenKind::Rpar)
      return error(nextToken.location, "expected ')'");
    eatNextToken(); // eat ')'

    return std::make_unique<GroupingExpr>(location, std::move(expr));
  }

  if (nextToken.kind == TokenKind::Number) {
    auto literal = std::make_unique<NumberLiteral>(location, *nextToken.value);
    eatNextToken(); // eat NumberLiteral
    return literal;
  }

  if (nextToken.kind == TokenKind::Identifier) {
    auto declRefExpr =
        std::make_unique<DeclRefExpr>(location, *nextToken.value);
    eatNextToken(); // eat identifier

    if (nextToken.kind != TokenKind::Lpar)
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
  if (nextToken.kind != TokenKind::Lpar) {
    error(nextToken.location, "expected '('");
    return std::nullopt;
  }
  eatNextToken(); // eat '('

  std::vector<std::unique_ptr<ParamDecl>> parameterList;

  if (nextToken.kind == TokenKind::Rpar) {
    eatNextToken(); // eat ')'
    return parameterList;
  }

  while (true) {
    if (nextToken.kind != TokenKind::Identifier) {
      error(nextToken.location, "expected parameter declaration");
      return std::nullopt;
    }

    auto paramDecl = parseParamDecl();
    if (!paramDecl)
      return std::nullopt;
    parameterList.emplace_back(std::move(paramDecl));

    if (nextToken.kind != TokenKind::Comma)
      break;
    eatNextToken(); // eat ','
  }

  if (nextToken.kind != TokenKind::Rpar) {
    error(nextToken.location, "expected ')'");
    return std::nullopt;
  }
  eatNextToken(); // eat ')'

  return parameterList;
}

// <argumentList>
//  ::= '(' (<expr> (',' <expr>)*)? ')'
std::optional<TheParser::ArgumentList> TheParser::parseArgumentList() {
  if (nextToken.kind != TokenKind::Lpar) {
    error(nextToken.location, "expected '('");
    return std::nullopt;
  }
  eatNextToken(); // eat '('

  std::vector<std::unique_ptr<Expr>> argumentList;

  if (nextToken.kind == TokenKind::Rpar) {
    eatNextToken(); // eat ')'
    return argumentList;
  }

  while (true) {
    auto expr = parseExpr();
    if (!expr)
      return std::nullopt;
    argumentList.emplace_back(std::move(expr));

    if (nextToken.kind != TokenKind::Comma)
      break;
    eatNextToken(); // eat ','
  }

  if (nextToken.kind != TokenKind::Rpar) {
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

  if (kind != TokenKind::KwNumber && kind != TokenKind::KwVoid &&
      kind != TokenKind::Identifier) {
    error(nextToken.location, "expected type specifier");
    return std::nullopt;
  }

  assert(nextToken.value && "type token has no value");

  std::string value = *nextToken.value;
  eatNextToken(); // eat 'number' | 'void' | identifier

  return value;
};

// <sourceFile>
//     ::= <functionDecl>* EOF
std::vector<std::unique_ptr<FunctionDecl>> TheParser::parseSourceFile() {
  std::vector<std::unique_ptr<FunctionDecl>> functions;

  while (nextToken.kind != TokenKind::Eof) {
    if (nextToken.kind != TokenKind::KwFn) {
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
