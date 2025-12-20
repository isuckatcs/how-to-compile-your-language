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
         tok == TokenKind::KwStruct;
}

}; // namespace

// Synchronization points:
// - start of a function decl
// - start of a struct decl
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
                                          std::move(*type));
};

// <structDecl>
//  ::= 'struct' <identifier> <fieldList>
//
// <fieldList>
//  ::= '{' (<fieldDecl> (',' <fieldDecl>)* ','?)? '}'
std::unique_ptr<ast::StructDecl> Parser::parseStructDecl() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat struct

  matchOrReturn(TokenKind::Identifier, "expected identifier");

  assert(nextToken.value && "identifier token without value");
  std::string structIdentifier = *nextToken.value;
  eatNextToken(); // eat identifier

  varOrReturn(fieldList,
              parseListWithTrailingComma<ast::FieldDecl>(
                  {TokenKind::Lbrace, "expected '{'"}, &Parser::parseFieldDecl,
                  {TokenKind::Rbrace, "expected '}'"}));

  return std::make_unique<ast::StructDecl>(location, structIdentifier,
                                           std::move(*fieldList));
}

// <functionDecl>
//  ::= 'fn' <identifier> <parameterList> ':' <type> <block>
//
// <parameterList>
//  ::= '(' (<paramDecl> (',' <paramDecl>)* ','?)? ')'
std::unique_ptr<ast::FunctionDecl> Parser::parseFunctionDecl() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat fn

  matchOrReturn(TokenKind::Identifier, "expected identifier");

  assert(nextToken.value && "identifier token without value");
  std::string functionIdentifier = *nextToken.value;
  eatNextToken(); // eat identifier

  varOrReturn(parameterList,
              parseListWithTrailingComma<ast::ParamDecl>(
                  {TokenKind::Lpar, "expected '('"}, &Parser::parseParamDecl,
                  {TokenKind::Rpar, "expected ')'"}));

  matchOrReturn(TokenKind::Colon, "expected ':'");
  eatNextToken(); // eat ':'

  varOrReturn(type, parseType());

  matchOrReturn(TokenKind::Lbrace, "expected function body");
  varOrReturn(block, parseBlock());

  return std::make_unique<ast::FunctionDecl>(location, functionIdentifier,
                                             *type, std::move(*parameterList),
                                             std::move(block));
}

// <paramDecl>
//  ::= 'var'? <identifier> ':' <type>
std::unique_ptr<ast::ParamDecl> Parser::parseParamDecl() {
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

  return std::make_unique<ast::ParamDecl>(location, std::move(identifier),
                                          std::move(*type), isVar);
}

// <varDecl>
//  ::= <identifier> (':' <type>)? ('=' <expr>)?
std::unique_ptr<ast::VarDecl> Parser::parseVarDecl(bool isLet) {
  SourceLocation location = nextToken.location;

  assert(nextToken.value && "identifier token without value");

  std::string identifier = *nextToken.value;
  eatNextToken(); // eat identifier

  std::optional<ast::Type> type;
  if (nextToken.kind == TokenKind::Colon) {
    eatNextToken(); // eat ':'

    type = parseType();
    if (!type)
      return nullptr;
  }

  if (nextToken.kind != TokenKind::Equal)
    return std::make_unique<ast::VarDecl>(location, identifier, type, !isLet);
  eatNextToken(); // eat '='

  varOrReturn(initializer, parseExpr());

  return std::make_unique<ast::VarDecl>(location, identifier, type, !isLet,
                                        std::move(initializer));
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

// <assignment>
//  ::= (<declRefExpr> | <memberExpr>) '=' <expr>
std::unique_ptr<ast::Assignment>
Parser::parseAssignmentRHS(std::unique_ptr<ast::AssignableExpr> lhs) {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat '='

  varOrReturn(rhs, parseExpr());

  return std::make_unique<ast::Assignment>(location, std::move(lhs),
                                           std::move(rhs));
}

// <declStmt>
//  ::= ('let'|'var') <varDecl>  ';'
std::unique_ptr<ast::DeclStmt> Parser::parseDeclStmt() {
  Token tok = nextToken;
  eatNextToken(); // eat 'let' | 'var'

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

  if (nextToken.kind == TokenKind::KwLet || nextToken.kind == TokenKind::KwVar)
    return parseDeclStmt();

  return parseAssignmentOrExpr();
}

std::unique_ptr<ast::Stmt> Parser::parseAssignmentOrExpr() {
  varOrReturn(lhs, parsePrefixExpr());

  if (nextToken.kind != TokenKind::Equal) {
    varOrReturn(expr, parseExprRHS(std::move(lhs), 0));

    matchOrReturn(TokenKind::Semi, "expected ';' at the end of expression");
    eatNextToken(); // eat ';'

    return expr;
  }

  auto *dre = dynamic_cast<ast::AssignableExpr *>(lhs.get());
  if (!dre)
    return report(lhs->location,
                  "expected variable on the LHS of an assignment");

  std::ignore = lhs.release();

  varOrReturn(assignment,
              parseAssignmentRHS(std::unique_ptr<ast::AssignableExpr>(dre)));

  matchOrReturn(TokenKind::Semi, "expected ';' at the end of assignment");
  eatNextToken(); // eat ';'

  return assignment;
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
//  ::= ('!' | '-')* <primaryExpr>
std::unique_ptr<ast::Expr> Parser::parsePrefixExpr() {
  Token tok = nextToken;

  if (tok.kind != TokenKind::Excl && tok.kind != TokenKind::Minus)
    return parsePostfixExpr();
  eatNextToken(); // eat '!' or '-'

  varOrReturn(rhs, parsePrefixExpr());

  return std::make_unique<ast::UnaryOperator>(tok.location, std::move(rhs),
                                              tok.kind);
}

// <postfixExpression>
//  ::= <primaryExpression> <argumentList>? <memberExpr>*
//
// <argumentList>
//  ::= '(' (<expr> (',' <expr>)* ','?)? ')'
//
// <memberExpr>
//  ::= '.' <identifier>
std::unique_ptr<ast::Expr> Parser::parsePostfixExpr() {
  varOrReturn(expr, parsePrimary());

  if (nextToken.kind == TokenKind::Lpar) {
    SourceLocation location = nextToken.location;
    varOrReturn(argumentList,
                parseListWithTrailingComma<ast::Expr>(
                    {TokenKind::Lpar, "expected '('"}, &Parser::parseExpr,
                    {TokenKind::Rpar, "expected ')'"}));

    expr = std::make_unique<ast::CallExpr>(location, std::move(expr),
                                           std::move(*argumentList));
  }

  while (nextToken.kind == TokenKind::Dot) {
    SourceLocation location = nextToken.location;
    eatNextToken(); // eat '.'

    matchOrReturn(TokenKind::Identifier, "expected field identifier");
    assert(nextToken.value && "identifier without value");

    expr = std::make_unique<ast::MemberExpr>(location, std::move(expr),
                                             *nextToken.value);
    eatNextToken(); // eat identifier
  }

  return expr;
}

// <primaryExpr>
//  ::= <numberLiteral>
//  |   <structInstantiation>
//  |   <declRefExpr>
//  |   '(' <expr> ')'
//
// <numberLiteral>
//  ::= <number>
//
// <structInstantiation>
//  ::= <identifier> <fieldInitList>
//
// <fieldInitList>
//  ::= '{' (<fieldInit> (',' <fieldInit>)* ','?)? '}'
//
// <declRefExpr>
//  ::= <identifier>
std::unique_ptr<ast::Expr> Parser::parsePrimary() {
  SourceLocation location = nextToken.location;

  if (nextToken.kind == TokenKind::Lpar) {
    eatNextToken(); // eat '('

    varOrReturn(expr, withNoRestrictions(&Parser::parseExpr));

    matchOrReturn(TokenKind::Rpar, "expected ')'");
    eatNextToken(); // eat ')'

    return std::make_unique<ast::GroupingExpr>(location, std::move(expr));
  }

  if (nextToken.kind == TokenKind::Number) {
    auto literal =
        std::make_unique<ast::NumberLiteral>(location, *nextToken.value);
    eatNextToken(); // eat number
    return literal;
  }

  if (nextToken.kind == TokenKind::Identifier) {
    std::string identifier = *nextToken.value;
    eatNextToken(); // eat identifier

    if (!(restrictions & StructNotAllowed) &&
        nextToken.kind == TokenKind::Lbrace) {
      auto fieldInitList = parseListWithTrailingComma<ast::FieldInitStmt>(
          {TokenKind::Lbrace, "expected '{'"}, &Parser::parseFieldInitStmt,
          {TokenKind::Rbrace, "expected '}'"});

      if (!fieldInitList) {
        synchronizeOn({TokenKind::Rbrace});
        eatNextToken(); // eat '}'
        return nullptr;
      }

      return std::make_unique<ast::StructInstantiationExpr>(
          location, std::move(identifier), std::move(*fieldInitList));
    }

    auto declRefExpr =
        std::make_unique<ast::DeclRefExpr>(location, std::move(identifier));
    return declRefExpr;
  }

  return report(location, "expected expression");
}

// <TList>
//  ::= <openingToken> (<T> (',' <T>)* ','?)? <closingToken>
template <typename T, typename F>
std::unique_ptr<std::vector<std::unique_ptr<T>>>
Parser::parseListWithTrailingComma(
    std::pair<TokenKind, const char *> openingToken,
    F parser,
    std::pair<TokenKind, const char *> closingToken) {
  matchOrReturn(openingToken.first, openingToken.second);
  eatNextToken(); // eat openingToken

  std::vector<std::unique_ptr<T>> list;
  while (true) {
    if (nextToken.kind == closingToken.first)
      break;

    varOrReturn(init, (this->*parser)());
    list.emplace_back(std::move(init));

    if (nextToken.kind != TokenKind::Comma)
      break;
    eatNextToken(); // eat ','
  }

  matchOrReturn(closingToken.first, closingToken.second);
  eatNextToken(); // eat closingToken

  return std::make_unique<decltype(list)>(std::move(list));
}

// <type>
//  ::= 'number'
//  |   'void'
//  |   <identifier>
std::optional<ast::Type> Parser::parseType() {
  TokenKind kind = nextToken.kind;

  if (kind != TokenKind::KwNumber && kind != TokenKind::KwVoid &&
      kind != TokenKind::Identifier) {
    report(nextToken.location, "expected type specifier");
    return std::nullopt;
  }

  SourceLocation location = nextToken.location;
  assert(nextToken.value && "identifier without value");
  std::string name = *nextToken.value;
  eatNextToken(); // eat identifier

  return ast::Type(location, std::move(name));
};

// <sourceFile>
//     ::= (<structDecl> | <functionDecl>)* EOF
std::pair<std::vector<std::unique_ptr<ast::Decl>>, bool>
Parser::parseSourceFile() {
  std::vector<std::unique_ptr<ast::Decl>> declarations;

  while (nextToken.kind != TokenKind::Eof) {
    if (nextToken.kind == TokenKind::KwFn) {
      if (auto fn = parseFunctionDecl()) {
        declarations.emplace_back(std::move(fn));
        continue;
      }
    } else if (nextToken.kind == TokenKind::KwStruct) {
      if (auto st = parseStructDecl()) {
        declarations.emplace_back(std::move(st));
        continue;
      }
    } else {
      report(nextToken.location,
             "expected function or struct declaration on the top level");
    }

    synchronizeOn({TokenKind::KwFn, TokenKind::KwStruct});
    continue;
  }

  assert(nextToken.kind == TokenKind::Eof && "expected to see end of file");

  // Only the lexer and the parser has access to the tokens, so to report an
  // error on the EOF token, we look for main() here.
  bool hasMainFunction = false;
  for (auto &&decl : declarations) {
    if (!dynamic_cast<const ast::FunctionDecl *>(decl.get()))
      continue;

    hasMainFunction |= decl->identifier == "main";
  }

  if (!hasMainFunction && !incompleteAST)
    report(nextToken.location, "main function not found");

  return {std::move(declarations), !incompleteAST && hasMainFunction};
}
} // namespace yl
