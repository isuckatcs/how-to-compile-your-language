#include <cassert>

#include "parser.h"
#include "utils.h"

namespace yl {
// Synchronization points:
// - start of a function decl
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
    } else if (kind == TokenKind::KwFn || kind == TokenKind::Eof)
      break;

    eatNextToken();
  }
}

// <functionDecl>
//  ::= 'fn' <identifier> <parameterList> ':' <type> <block>
std::unique_ptr<FunctionDecl> Parser::parseFunctionDecl() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat fn

  if (nextToken.kind != TokenKind::Identifier)
    return report(nextToken.location, "expected identifier");

  assert(nextToken.value && "identifier token without value");
  std::string functionIdentifier = *nextToken.value;
  eatNextToken(); // eat identifier

  varOrReturn(parameterList, parseParameterList());

  if (nextToken.kind != TokenKind::Colon)
    return report(nextToken.location, "expected ':'");
  eatNextToken(); // eat ':'

  varOrReturn(type, parseType());

  if (nextToken.kind != TokenKind::Lbrace)
    return report(nextToken.location, "expected function body");

  varOrReturn(block, parseBlock());

  return std::make_unique<FunctionDecl>(location, functionIdentifier, *type,
                                        std::move(*parameterList),
                                        std::move(block));
}

// <paramDecl>
//  ::= <identifier> ':' <type>
std::unique_ptr<ParamDecl> Parser::parseParamDecl() {
  SourceLocation location = nextToken.location;
  assert(nextToken.value && "identifier token without value");

  std::string identifier = *nextToken.value;
  eatNextToken(); // eat identifier

  if (nextToken.kind != TokenKind::Colon)
    return report(nextToken.location, "expected ':'");
  eatNextToken(); // eat :

  varOrReturn(type, parseType());

  return std::make_unique<ParamDecl>(location, std::move(identifier),
                                     std::move(*type));
}

// <block>
//  ::= '{' <statement>* '}'
std::unique_ptr<Block> Parser::parseBlock() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat '{'

  std::vector<std::unique_ptr<Stmt>> expressions;
  while (true) {
    if (nextToken.kind == TokenKind::Rbrace)
      break;

    if (nextToken.kind == TokenKind::Eof || nextToken.kind == TokenKind::KwFn)
      return report(nextToken.location, "expected '}' at the end of a block");

    std::unique_ptr<Stmt> stmt = parseStmt();
    if (!stmt) {
      synchronize();
      continue;
    }

    expressions.emplace_back(std::move(stmt));
  }

  eatNextToken(); // eat '}'

  return std::make_unique<Block>(location, std::move(expressions));
}

// <returnStmt>
//  ::= 'return' <expr>? ';'
std::unique_ptr<ReturnStmt> Parser::parseReturnStmt() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat 'return'

  std::unique_ptr<Expr> expr;
  if (nextToken.kind != TokenKind::Semi) {
    expr = parseExpr();
    if (!expr)
      return nullptr;

    if (nextToken.kind != TokenKind::Semi)
      return report(nextToken.location,
                    "expected ';' at the end of a return statement");
  }

  eatNextToken(); // eat ';'

  return std::make_unique<ReturnStmt>(location, std::move(expr));
}

// <statement>
//  ::= <expr> ';'
//  |   <returnStmt>
std::unique_ptr<Stmt> Parser::parseStmt() {
  if (nextToken.kind == TokenKind::KwReturn)
    return parseReturnStmt();

  varOrReturn(expr, parseExpr());

  if (nextToken.kind != TokenKind::Semi)
    return report(nextToken.location, "expected ';' at the end of statement");
  eatNextToken(); // eat ';'

  return expr;
}

std::unique_ptr<Expr> Parser::parseExpr() { return parsePrimary(); }

// <primaryExpr>
//  ::= <numberLiteral>
//  |   <declRefExpr>
//  |   <callExpr>
//
// <numberLiteral>
//  ::= <number>
//
// <declRefExpr>
//  ::= <identifier>
//
// <callExpr>
//  ::= <declRefExpr> <argumentList>
std::unique_ptr<Expr> Parser::parsePrimary() {
  SourceLocation location = nextToken.location;

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

    varOrReturn(argumentList, parseArgumentList());

    return std::make_unique<CallExpr>(location, std::move(declRefExpr),
                                      std::move(*argumentList));
  }

  return report(location, "expected expression");
}

// <parameterList>
//  ::= '(' (<paramDecl> (',' <paramDecl>)*)? ')'
std::optional<Parser::ParameterList> Parser::parseParameterList() {
  if (nextToken.kind != TokenKind::Lpar) {
    report(nextToken.location, "expected '('");
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
      report(nextToken.location, "expected parameter declaration");
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
    report(nextToken.location, "expected ')'");
    return std::nullopt;
  }
  eatNextToken(); // eat ')'

  return parameterList;
}

// <argumentList>
//  ::= '(' (<expr> (',' <expr>)*)? ')'
std::optional<Parser::ArgumentList> Parser::parseArgumentList() {
  if (nextToken.kind != TokenKind::Lpar) {
    report(nextToken.location, "expected '('");
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
    report(nextToken.location, "expected ')'");
    return std::nullopt;
  }
  eatNextToken(); // eat ')'

  return argumentList;
}

// <type>
//  ::= 'number'
//  |   'void'
//  |   <identifier>
std::optional<Type> Parser::parseType() {
  TokenKind kind = nextToken.kind;

  if (kind == TokenKind::KwNumber) {
    eatNextToken(); // eat 'number'
    return Type::builtinNumber();
  }

  if (kind == TokenKind::KwVoid) {
    eatNextToken(); // eat 'void'
    return Type::builtinVoid();
  }

  if (kind == TokenKind::Identifier) {
    assert(nextToken.value && "identifier token has no value");
    auto t = Type::custom(*nextToken.value);
    eatNextToken(); // eat identifier
    return t;
  }

  report(nextToken.location, "expected type specifier");
  return std::nullopt;
};

// <sourceFile>
//     ::= <functionDecl>* EOF
std::pair<std::vector<std::unique_ptr<FunctionDecl>>, bool>
Parser::parseSourceFile() {
  std::vector<std::unique_ptr<FunctionDecl>> functions;

  while (nextToken.kind != TokenKind::Eof) {
    if (nextToken.kind != TokenKind::KwFn) {
      report(nextToken.location,
             "only function definitions are allowed on the top level");
      synchronizeOn(TokenKind::KwFn);
      continue;
    }

    auto fn = parseFunctionDecl();
    if (!fn) {
      synchronizeOn(TokenKind::KwFn);
      continue;
    }

    functions.emplace_back(std::move(fn));
  }

  assert(nextToken.kind == TokenKind::Eof && "expected to see end of file");

  return {std::move(functions), !incompleteAST};
}
} // namespace yl
