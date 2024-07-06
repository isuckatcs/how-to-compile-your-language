#include <cassert>

#include "parser.h"
#include "utils.h"

namespace yl {
namespace {
int getTokPrecedence(TokenKind tok) {
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
}; // namespace

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

// <varDecl>
//  ::= <identifier> ':' <type> ('=' <expr>)?
std::unique_ptr<VarDecl> Parser::parseVarDecl(bool isLet) {
  SourceLocation location = nextToken.location;

  assert(nextToken.value && "identifier token without value");

  std::string identifier = *nextToken.value;
  eatNextToken(); // eat identifier

  if (nextToken.kind != TokenKind::Colon)
    return report(nextToken.location, "expected ':'");
  eatNextToken(); // eat ':'

  varOrReturn(type, parseType());

  if (nextToken.kind != TokenKind::Equal)
    return std::make_unique<VarDecl>(location, identifier, *type, !isLet);
  eatNextToken(); // eat '='

  varOrReturn(initializer, parseExpr());

  return std::make_unique<VarDecl>(location, identifier, *type, !isLet,
                                   std::move(initializer));
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

// <ifStatement>
//  ::= 'if' <expr> <block> ('else' (<ifStatement> | <block>))?
std::unique_ptr<IfStmt> Parser::parseIfStmt() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat 'if'

  varOrReturn(condition, parseExpr());

  if (nextToken.kind != TokenKind::Lbrace)
    return report(nextToken.location, "expected 'if' body");

  varOrReturn(trueBranch, parseBlock());

  if (nextToken.kind != TokenKind::KwElse)
    return std::make_unique<IfStmt>(location, std::move(condition),
                                    std::move(trueBranch));
  eatNextToken(); // eat 'else'

  std::unique_ptr<Block> falseBlock;
  if (nextToken.kind == TokenKind::KwIf) {
    varOrReturn(elseIf, parseIfStmt());

    SourceLocation loc = elseIf->location;
    std::vector<std::unique_ptr<Stmt>> stmts;
    stmts.emplace_back(std::move(elseIf));

    falseBlock = std::make_unique<Block>(loc, std::move(stmts));
  } else {
    if (nextToken.kind != TokenKind::Lbrace)
      return report(nextToken.location, "expected 'else' body");

    falseBlock = parseBlock();
  }

  if (!falseBlock)
    return nullptr;

  return std::make_unique<IfStmt>(location, std::move(condition),
                                  std::move(trueBranch), std::move(falseBlock));
}

// <whileStatement>
//  ::= 'while' <expr> <block>
std::unique_ptr<WhileStmt> Parser::parseWhileStmt() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat 'while'

  varOrReturn(cond, parseExpr());

  if (nextToken.kind != TokenKind::Lbrace)
    return report(nextToken.location, "expected 'while' body");

  varOrReturn(body, parseBlock());

  return std::make_unique<WhileStmt>(location, std::move(cond),
                                     std::move(body));
}

// <assignment>
//  ::= <declRefExpr> '=' <expr>
std::unique_ptr<Assignment>
Parser::parseAssignmentRHS(std::unique_ptr<DeclRefExpr> lhs) {
  eatNextToken(); // eat '='

  varOrReturn(rhs, parseExpr());

  return std::make_unique<Assignment>(lhs->location, std::move(lhs),
                                      std::move(rhs));
}

// <declStmt>
//  ::= ('let'|'var') <varDecl>
std::unique_ptr<DeclStmt> Parser::parseDeclStmt() {
  Token tok = nextToken;
  eatNextToken(); // eat 'let' | 'var'

  if (nextToken.kind != TokenKind::Identifier)
    return report(nextToken.location, "expected identifier");

  varOrReturn(varDecl, parseVarDecl(tok.kind == TokenKind::KwLet));

  return std::make_unique<DeclStmt>(tok.location, std::move(varDecl));
}

// <returnStmt>
//  ::= 'return' <expr> ';'
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
//  |   <ifStatement>
//  |   <whileStatement>
//  |   <assignment> ';'
//  |   <declStmt> ';'
std::unique_ptr<Stmt> Parser::parseStmt() {
  if (nextToken.kind == TokenKind::KwIf)
    return parseIfStmt();

  if (nextToken.kind == TokenKind::KwWhile)
    return parseWhileStmt();

  if (nextToken.kind == TokenKind::KwReturn)
    return parseReturnStmt();

  std::unique_ptr<Stmt> expr = nullptr;
  if (nextToken.kind == TokenKind::KwLet || nextToken.kind == TokenKind::KwVar)
    expr = parseDeclStmt();
  else {
    varOrReturn(lhs, parsePrefixExpr());

    if (nextToken.kind != TokenKind::Equal)
      expr = parseExprRHS(std::move(lhs), 0);
    else if (auto *dre = dynamic_cast<DeclRefExpr *>(lhs.get())) {
      std::ignore = lhs.release();
      expr = parseAssignmentRHS(std::unique_ptr<DeclRefExpr>(dre));
    } else
      return report(nextToken.location,
                    "expected variable on the LHS of an assignment");
  }

  if (!expr)
    return nullptr;

  if (nextToken.kind != TokenKind::Semi)
    return report(nextToken.location, "expected ';' at the end of statement");
  eatNextToken(); // eat ';'

  return expr;
}

std::unique_ptr<Expr> Parser::parseExpr() {
  varOrReturn(lhs, parsePrefixExpr());
  return parseExprRHS(std::move(lhs), 0);
}

std::unique_ptr<Expr> Parser::parseExprRHS(std::unique_ptr<Expr> lhs,
                                           int precedence) {
  while (true) {
    TokenKind op = nextToken.kind;
    int curOpPrec = getTokPrecedence(op);

    if (curOpPrec < precedence)
      return lhs;
    eatNextToken(); // eat opearator

    varOrReturn(rhs, parsePrefixExpr());

    if (curOpPrec < getTokPrecedence(nextToken.kind)) {
      rhs = parseExprRHS(std::move(rhs), curOpPrec + 1);
      if (!rhs)
        return nullptr;
    }

    lhs = std::make_unique<BinaryOperator>(lhs->location, std::move(lhs),
                                           std::move(rhs), op);
  }
}

std::unique_ptr<Expr> Parser::parsePrefixExpr() {
  Token tok = nextToken;

  if (tok.kind != TokenKind::Excl)
    return parsePrimary();
  eatNextToken(); // eat !

  varOrReturn(rhs, parsePrefixExpr());

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
std::unique_ptr<Expr> Parser::parsePrimary() {
  SourceLocation location = nextToken.location;

  if (nextToken.kind == TokenKind::Lpar) {
    eatNextToken(); // eat '('

    varOrReturn(expr, parseExpr());

    if (nextToken.kind != TokenKind::Rpar)
      return report(nextToken.location, "expected ')'");
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
std::optional<std::string> Parser::parseType() {
  TokenKind kind = nextToken.kind;

  if (kind != TokenKind::KwNumber && kind != TokenKind::KwVoid &&
      kind != TokenKind::Identifier) {
    report(nextToken.location, "expected type specifier");
    return std::nullopt;
  }

  assert(nextToken.value && "type token has no value");

  std::string value = *nextToken.value;
  eatNextToken(); // eat 'number' | 'void' | identifier

  return value;
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

  // Only the lexer and the parser has access to the tokens, so to report an
  // error on the EOF token, we look for main() here.
  bool hasMainFunction = false;
  for (auto &&fn : functions)
    hasMainFunction |= fn->identifier == "main";

  if (!hasMainFunction && !incompleteAST)
    report(nextToken.location, "main function not found");

  return {std::move(functions), !incompleteAST && hasMainFunction};
}
} // namespace yl
