#include <cassert>
#include <fstream>
#include <iostream>
#include <sstream>

#include "lexer.h"

using namespace yl;

int main(int argc, const char **argv) {
  std::ifstream file(argv[1]);

  std::stringstream buffer;
  buffer << file.rdbuf();
  SourceFile sourceFile = {argv[1], buffer.str()};

  Lexer lexer(sourceFile);

  Token tok = lexer.getNextToken();
  while (tok.kind != TokenKind::Eof) {
    if (tok.kind == TokenKind::Identifier)
      std::cout << "identifier(" << *tok.value << ')';
    else if (tok.kind == TokenKind::KwFn)
      std::cout << "fn";
    else if (tok.kind == TokenKind::KwVoid)
      std::cout << "void";
    else if (tok.kind == TokenKind::Unk)
      std::cout << "unknown";
    else
      std::cout << static_cast<char>(tok.kind);

    std::cout << '\n';
    tok = lexer.getNextToken();
  }

  assert(tok.kind == TokenKind::Eof);
  std::cout << "eof\n";
}
