#include <fstream>
#include <iostream>
#include <sstream>

#include "codegen.h"
#include "lexer.h"
#include "parser.h"
#include "sema.h"

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

  Sema sema(std::move(ast));
  auto res = sema.resolveAST();

  for (auto &&fn : res) {
    fn->dump();
  }

  Codegen codegen(std::move(res), argv[1]);
  codegen.generateIR()->dump();

  return 0;
}
