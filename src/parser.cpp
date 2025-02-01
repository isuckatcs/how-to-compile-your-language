#include <cassert>

#include "parser.h"
#include "utils.h"

#define matchOrReturn(tok, msg)                                                \
  if (nextToken.kind != tok)                                                   \
    return report(nextToken.location, msg);

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

  matchOrReturn(TokenKind::Identifier, "expected identifier");

  assert(nextToken.value && "identifier token without value");
  std::string functionIdentifier = *nextToken.value;
  eatNextToken(); // eat identifier

  varOrReturn(parameterList, parseParameterList());

  matchOrReturn(TokenKind::Colon, "expected ':'");
  eatNextToken(); // eat ':'

  varOrReturn(type, parseType());

  matchOrReturn(TokenKind::Lbrace, "expected function body");
  varOrReturn(block, parseBlock());

  return std::make_unique<FunctionDecl>(location, functionIdentifier, *type,
                                        std::move(*parameterList),
                                        std::move(block));
}

// <paramDecl>
//  ::= 'var'? <identifier> ':' <type>
std::unique_ptr<ParamDecl> Parser::parseParamDecl() {
  SourceLocation location = nextToken.location;

  bool isVar = nextToken.kind == TokenKind::KwVar;
  if (isVar)
    eatNextToken(); // eat 'var'

  matchOrReturn(TokenKind::Identifier, "expected parameter declaration");
  assert(nextToken.value && "identifier token without value");

  std::string identifier = *nextToken.value;
  eatNextToken(); // eat identifier

  matchOrReturn(TokenKind::Colon, "expected ':'");
  eatNextToken(); // eat :

  varOrReturn(type, parseType());

  return std::make_unique<ParamDecl>(location, std::move(identifier),
                                     std::move(*type), isVar);
}

// <varDecl>
//  ::= <identifier> (':' <type>)? ('=' <expr>)?
std::unique_ptr<VarDecl> Parser::parseVarDecl(bool isLet) {
  SourceLocation location = nextToken.location;

  assert(nextToken.value && "identifier token without value");

  std::string identifier = *nextToken.value;
  eatNextToken(); // eat identifier

  std::optional<Type> type;
  if (nextToken.kind == TokenKind::Colon) {
    eatNextToken(); // eat ':'

    type = parseType();
    if (!type)
      return nullptr;
  }

  if (nextToken.kind != TokenKind::Equal)
    return std::make_unique<VarDecl>(location, identifier, type, !isLet);
  eatNextToken(); // eat '='

  varOrReturn(initializer, parseExpr());

  return std::make_unique<VarDecl>(location, identifier, type, !isLet,
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

  matchOrReturn(TokenKind::Lbrace, "expected 'if' body");

  varOrReturn(trueBlock, parseBlock());

  if (nextToken.kind != TokenKind::KwElse)
    return std::make_unique<IfStmt>(location, std::move(condition),
                                    std::move(trueBlock));
  eatNextToken(); // eat 'else'

  std::unique_ptr<Block> falseBlock;
  if (nextToken.kind == TokenKind::KwIf) {
    varOrReturn(elseIf, parseIfStmt());

    SourceLocation loc = elseIf->location;
    std::vector<std::unique_ptr<Stmt>> stmts;
    stmts.emplace_back(std::move(elseIf));

    falseBlock = std::make_unique<Block>(loc, std::move(stmts));
  } else {
    matchOrReturn(TokenKind::Lbrace, "expected 'else' body");
    falseBlock = parseBlock();
  }

  if (!falseBlock)
    return nullptr;

  return std::make_unique<IfStmt>(location, std::move(condition),
                                  std::move(trueBlock), std::move(falseBlock));
}

// <whileStatement>
//  ::= 'while' <expr> <block>
std::unique_ptr<WhileStmt> Parser::parseWhileStmt() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat 'while'

  varOrReturn(cond, parseExpr());

  matchOrReturn(TokenKind::Lbrace, "expected 'while' body");
  varOrReturn(body, parseBlock());

  return std::make_unique<WhileStmt>(location, std::move(cond),
                                     std::move(body));
}

// <assignment>
//  ::= <declRefExpr> '=' <expr>
std::unique_ptr<Assignment>
Parser::parseAssignmentRHS(std::unique_ptr<DeclRefExpr> lhs) {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat '='

  varOrReturn(rhs, parseExpr());

  return std::make_unique<Assignment>(location, std::move(lhs), std::move(rhs));
}

// <declStmt>
//  ::= ('let'|'var') <varDecl>  ';'
std::unique_ptr<DeclStmt> Parser::parseDeclStmt() {
  Token tok = nextToken;
  eatNextToken(); // eat 'let' | 'var'

  matchOrReturn(TokenKind::Identifier, "expected identifier");
  varOrReturn(varDecl, parseVarDecl(tok.kind == TokenKind::KwLet));

  matchOrReturn(TokenKind::Semi, "expected ';' after declaration");
  eatNextToken(); // eat ';'

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
  }

  matchOrReturn(TokenKind::Semi,
                "expected ';' at the end of a return statement");
  eatNextToken(); // eat ';'

  return std::make_unique<ReturnStmt>(location, std::move(expr));
}

// <statement>
//  ::= <expr> ';'
//  |   <returnStmt>
//  |   <ifStatement>
//  |   <whileStatement>
//  |   <assignment>
//  |   <declStmt>
std::unique_ptr<Stmt> Parser::parseStmt() {
  if (nextToken.kind == TokenKind::KwIf)
    return parseIfStmt();

  if (nextToken.kind == TokenKind::KwWhile)
    return parseWhileStmt();

  if (nextToken.kind == TokenKind::KwReturn)
    return parseReturnStmt();

  if (nextToken.kind == TokenKind::KwLet || nextToken.kind == TokenKind::KwVar)
    return parseDeclStmt();

  return parseAssignmentOrExpr();
}

std::unique_ptr<Stmt> Parser::parseAssignmentOrExpr() {
  varOrReturn(lhs, parsePrefixExpr());

  if (nextToken.kind != TokenKind::Equal) {
    varOrReturn(expr, parseExprRHS(std::move(lhs), 0));

    matchOrReturn(TokenKind::Semi, "expected ';' at the end of expression");
    eatNextToken(); // eat ';'

    return expr;
  }

  auto *dre = dynamic_cast<DeclRefExpr *>(lhs.get());
  if (!dre)
    return report(lhs->location,
                  "expected variable on the LHS of an assignment");

  std::ignore = lhs.release();

  varOrReturn(assignment,
              parseAssignmentRHS(std::unique_ptr<DeclRefExpr>(dre)));

  matchOrReturn(TokenKind::Semi, "expected ';' at the end of assignment");
  eatNextToken(); // eat ';'

  return assignment;
}

std::unique_ptr<Expr> Parser::parseExpr() {
  varOrReturn(lhs, parsePrefixExpr());
  return parseExprRHS(std::move(lhs), 0);
}

std::unique_ptr<Expr> Parser::parseExprRHS(std::unique_ptr<Expr> lhs,
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

    lhs = std::make_unique<BinaryOperator>(op.location, std::move(lhs),
                                           std::move(rhs), op.kind);
  }
}

// <prefixExpression>
//  ::= ('!' | '-')* <primaryExpr>
std::unique_ptr<Expr> Parser::parsePrefixExpr() {
  Token tok = nextToken;

  if (tok.kind != TokenKind::Excl && tok.kind != TokenKind::Minus)
    return parsePostfixExpr();
  eatNextToken(); // eat '!' or '-'

  varOrReturn(rhs, parsePrefixExpr());

  return std::make_unique<UnaryOperator>(tok.location, std::move(rhs),
                                         tok.kind);
}

// <postfixExpression>
//     ::= <primaryExpression> <argumentList>

// <argumentList>
//     ::= '(' (<expr> (',' <expr>)* ','?)? ')'
std::unique_ptr<Expr> Parser::parsePostfixExpr() {
  varOrReturn(expr, parsePrimary());

  if (nextToken.kind != TokenKind::Lpar)
    return expr;

  SourceLocation location = nextToken.location;
  varOrReturn(argumentList, parseArgumentList());

  return std::make_unique<CallExpr>(location, std::move(expr),
                                    std::move(*argumentList));
}

// <primaryExpr>
//  ::= <numberLiteral>
//  |   <declRefExpr>
//  |   '(' <expr> ')'
//
// <numberLiteral>
//  ::= <number>
//
// <declRefExpr>
//  ::= <identifier>
//
std::unique_ptr<Expr> Parser::parsePrimary() {
  SourceLocation location = nextToken.location;

  if (nextToken.kind == TokenKind::Lpar) {
    eatNextToken(); // eat '('

    varOrReturn(expr, parseExpr());

    matchOrReturn(TokenKind::Rpar, "expected ')'");
    eatNextToken(); // eat ')'

    return std::make_unique<GroupingExpr>(location, std::move(expr));
  }

  if (nextToken.kind == TokenKind::Number) {
    auto literal = std::make_unique<NumberLiteral>(location, *nextToken.value);
    eatNextToken(); // eat number
    return literal;
  }

  if (nextToken.kind == TokenKind::Identifier) {
    auto declRefExpr =
        std::make_unique<DeclRefExpr>(location, *nextToken.value);
    eatNextToken(); // eat identifier
    return declRefExpr;
  }

  return report(location, "expected expression");
}

// <parameterList>
//  ::= '(' (<paramDecl> (',' <paramDecl>)* ','?)? ')'
std::unique_ptr<Parser::ParameterList> Parser::parseParameterList() {
  matchOrReturn(TokenKind::Lpar, "expected '('");
  eatNextToken(); // eat '('

  std::vector<std::unique_ptr<ParamDecl>> parameterList;

  while (true) {
    if (nextToken.kind == TokenKind::Rpar)
      break;

    varOrReturn(paramDecl, parseParamDecl());
    parameterList.emplace_back(std::move(paramDecl));

    if (nextToken.kind != TokenKind::Comma)
      break;
    eatNextToken(); // eat ','
  }

  matchOrReturn(TokenKind::Rpar, "expected ')'");
  eatNextToken(); // eat ')'

  return std::make_unique<ParameterList>(std::move(parameterList));
}

// <argumentList>
//  ::= '(' (<expr> (',' <expr>)* ','?)? ')'
std::unique_ptr<Parser::ArgumentList> Parser::parseArgumentList() {
  matchOrReturn(TokenKind::Lpar, "expected '('");
  eatNextToken(); // eat '('

  std::vector<std::unique_ptr<Expr>> argumentList;

  while (true) {
    if (nextToken.kind == TokenKind::Rpar)
      break;

    varOrReturn(expr, parseExpr());
    argumentList.emplace_back(std::move(expr));

    if (nextToken.kind != TokenKind::Comma)
      break;
    eatNextToken(); // eat ','
  }

  matchOrReturn(TokenKind::Rpar, "expected ')'");
  eatNextToken(); // eat ')'

  return std::make_unique<ArgumentList>(std::move(argumentList));
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
             "only function declarations are allowed on the top level");
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
