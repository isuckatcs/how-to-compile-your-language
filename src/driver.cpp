#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#include "cfg.h"
#include "codegen.h"
#include "lexer.h"
#include "parser.h"
#include "sema.h"

using namespace yl;

namespace {
void displayHelp() {
  std::cout << "Usage:\n"
            << "  compiler [options] <source_file>\n\n"
            << "Options:\n"
            << "  -h           display this message\n"
            << "  -o <file>    write executable to <file>\n"
            << "  -ast-dump    print the abstract syntax tree\n"
            << "  -res-dump    print the resolved syntax tree\n"
            << "  -llvm-dump   print the llvm module\n"
            << "  -cfg-dump    print the control flow graph\n";
}

[[noreturn]] void error(std::string_view msg) {
  std::cerr << "error: " << msg << '\n';
  std::exit(1);
}

struct CompilerOptions {
  std::filesystem::path source;
  std::filesystem::path output;
  bool displayHelp = false;
  bool astDump = false;
  bool resDump = false;
  bool llvmDump = false;
  bool cfgDump = false;
};

CompilerOptions parseArguments(int argc, const char **argv) {
  CompilerOptions options;

  int idx = 1;
  while (idx < argc) {
    std::string_view arg = argv[idx];

    if (arg[0] != '-') {
      if (!options.source.empty())
        error("unexpected argument '" + std::string(arg) + '\'');

      options.source = arg;
    } else {
      if (arg == "-h")
        options.displayHelp = true;
      else if (arg == "-o")
        options.output = ++idx >= argc ? "" : argv[idx];
      else if (arg == "-ast-dump")
        options.astDump = true;
      else if (arg == "-res-dump")
        options.resDump = true;
      else if (arg == "-llvm-dump")
        options.llvmDump = true;
      else if (arg == "-cfg-dump")
        options.cfgDump = true;
      else
        error("unexpected option '" + std::string(arg) + '\'');
    }

    ++idx;
  }

  return options;
}
} // namespace

int main(int argc, const char **argv) {
  CompilerOptions options = parseArguments(argc, argv);

  if (options.displayHelp) {
    displayHelp();
    return 0;
  }

  if (options.source.empty())
    error("no source file specified");

  if (options.source.extension() != ".yl")
    error("unexpected source file extension");

  std::ifstream file(options.source);
  if (!file)
    error("failed to open '" + options.source.string() + '\'');

  std::stringstream buffer;
  buffer << file.rdbuf();
  SourceFile sourceFile{options.source.c_str(), buffer.str()};

  Lexer lexer(sourceFile);
  Parser parser(lexer);
  auto [ast, success] = parser.parseSourceFile();

  if (options.astDump) {
    for (auto &&decl : ast.decls)
      decl->dump();
    return 0;
  }

  if (!success)
    return 1;

  Sema sema(std::move(ast));
  auto resolvedTree = sema.resolveAST();

  if (options.resDump) {
    if (resolvedTree.has_value()) {
      for (auto &&decl : resolvedTree->getStructs())
        decl->dump();

      for (auto &&decl : resolvedTree->getFunctions())
        decl->dump();
    }

    return 0;
  }

  if (options.cfgDump) {
    if (resolvedTree.has_value()) {
      for (auto &&fn : resolvedTree->getFunctions()) {
        std::cerr << fn->identifier << ':' << '\n';
        CFGBuilder().build(*fn).dump();
      }
    }
    return 0;
  }

  if (!resolvedTree.has_value())
    return 1;

  Codegen codegen(*resolvedTree, options.source.c_str());
  llvm::Module *llvmIR = codegen.generateIR();

  if (options.llvmDump) {
    llvmIR->print(llvm::errs(), nullptr);
    return 0;
  }

  // Theoretically this can still generate the same tmp files for 2 different
  // invocations and remove them later.
  std::stringstream path;
  path << "tmp-" << std::filesystem::hash_value(options.source) << ".ll";
  const std::string &llvmIRPath = path.str();

  std::error_code errorCode;
  llvm::raw_fd_ostream f(llvmIRPath, errorCode);
  llvmIR->print(f, nullptr);

  std::stringstream command;
  command << "clang-20 " << llvmIRPath;
  if (!options.output.empty())
    command << " -o " << options.output;

  int ret = std::system(command.str().c_str());
  std::filesystem::remove(llvmIRPath);

  return ret;
}
