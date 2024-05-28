#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>

#include "codegen.h"
#include "lexer.h"
#include "parser.h"
#include "sema.h"

int main(int argc, const char **argv) {
  std::ifstream file(argv[1]);
  std::stringstream buffer;
  buffer << file.rdbuf();

  SourceFile sourceFile{argv[1], buffer.str()};

  // std::cout << sourceFile.buffer << '\n';

  TheLexer lexer{sourceFile};

  // Token tok{SourceLocation{"dummy", 0, 0}, TokenKind::TOK_EOF};
  // while ((tok = lexer.getNextToken()).kind != TokenKind::TOK_EOF) {
  //   auto [file, l, c] = tok.location;
  //   std::cout << file << ':' << l << ':' << c << ' ' << (int)tok.kind << ' '
  //             << tok.value << '\n';
  // }

  TheParser parser{lexer};
  auto TopLevel = parser.parseSourceFile();
  for (auto &&fn : TopLevel) {
    fn->dump();
  }

  Sema sema{std::move(TopLevel)};

  std::cerr << "\n\n";
  auto resolvedAST = sema.resolveSourceFile();
  for (auto &&fn : resolvedAST)
    fn->dump();

  std::cerr << "\n\n";
  Codegen codegen{std::move(resolvedAST)};
  codegen.generateIR("tmp.ll");

  std::stringstream command;
  command << "clang tmp.ll -Wno-override-module";
  std::system(command.str().c_str());

  // std::filesystem::remove("tmp.ll");

  return 0;
}