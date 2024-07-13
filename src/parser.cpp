#include <cassert>

#include "parser.h"
#include "utils.h"

namespace yl {
// <functionDecl>
//  ::= 'fn' <identifier> '(' ')' ':' <type> <block>
std::unique_ptr<FunctionDecl> Parser::parseFunctionDecl() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat fn

  if (nextToken.kind != TokenKind::Identifier)
    return report(nextToken.location, "expected identifier");

  assert(nextToken.value && "identifier token without value");
  std::string functionIdentifier = *nextToken.value;
  eatNextToken(); // eat identifier

  if (nextToken.kind != TokenKind::Lpar)
    return report(nextToken.location, "expected '('");
  eatNextToken(); // eat '('

  if (nextToken.kind != TokenKind::Rpar)
    return report(nextToken.location, "expected ')'");
  eatNextToken(); // eat ')'

  if (nextToken.kind != TokenKind::Colon)
    return report(nextToken.location, "expected ':'");
  eatNextToken(); // eat ':'

  varOrReturn(type, parseType());

  if (nextToken.kind != TokenKind::Lbrace)
    return report(nextToken.location, "expected function body");

  varOrReturn(block, parseBlock());

  return std::make_unique<FunctionDecl>(location, functionIdentifier, *type,
                                        std::move(block));
}

// <block>
//  ::= '{' '}'
std::unique_ptr<Block> Parser::parseBlock() {
  SourceLocation location = nextToken.location;
  eatNextToken(); // eat '{'

  if (nextToken.kind != TokenKind::Rbrace)
    return report(nextToken.location, "expected '}' at the end of a block");
  eatNextToken(); // eat '}'

  return std::make_unique<Block>(location);
}

// <type>
//  ::= 'void'
//  |   <identifier>
std::optional<Type> Parser::parseType() {
  TokenKind kind = nextToken.kind;

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