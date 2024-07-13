#include <fstream>
#include <iostream>
#include <sstream>

#include "lexer.h"
#include "parser.h"

using namespace yl;

int main(int argc, const char **argv) {
  std::ifstream file(argv[1]);

  std::stringstream buffer;
  buffer << file.rdbuf();
  SourceFile sourceFile = {argv[1], buffer.str()};

  Lexer lexer(sourceFile);
  Parser parser(lexer);

  auto [ast, success] = parser.parseSourceFile();

  for (auto &&fn : ast)
    fn->dump();

  return !success;
}
