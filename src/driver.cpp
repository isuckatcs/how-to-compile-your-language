#include <cstdlib>
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

#include <llvm/Support/Host.h>

void displayHelp() {
  std::cout << "Usage:\n"
            << "  your-compiler [options] <source_file>\n\n"
            << "Options:\n"
            << "  -h           display this message\n"
            << "  -o <file>    write executable to <file>\n"
            << "  -ast-dump    print the abstract syntax tree\n"
            << "  -res-dump    print the resolved syntax tree\n"
            << "  -llvm-dump   print the llvm module\n"
            << "  -cfg-dump   print the control flow graph\n";
}

[[noreturn]] void error(std::string_view msg) {
  std::cerr << "error: " << msg << '\n';
  std::exit(1);
}

struct CompilerOptions {
  std::optional<std::string_view> source;
  std::optional<std::string_view> output;
  bool displayHelp = false;
  bool astDump = false;
  bool resDump = false;
  bool llvmDump = false;
  bool cfgDump = false;
};

CompilerOptions parseArguments(int argc, const char **argv) {
  CompilerOptions options{std::nullopt, std::nullopt, false};

  int idx = 1;
  while (idx < argc) {
    std::string_view arg{argv[idx]};

    if (arg[0] != '-') {
      if (options.source)
        error("unexpected argument '" + std::string{arg} + '\'');

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
        error("unexpected option '" + std::string{arg} + '\'');
    }

    ++idx;
  }

  return options;
}

int main(int argc, const char **argv) {
  CompilerOptions options = parseArguments(argc, argv);

  if (options.displayHelp) {
    displayHelp();
    return 0;
  }

  if (!options.source)
    error("no source file specified");

  std::ifstream file{options.source->data()};
  if (!file)
    error("failed to open '" + std::string{*options.source} + '\'');

  std::stringstream buffer;
  buffer << file.rdbuf();
  SourceFile sourceFile{*options.source, buffer.str()};

  TheLexer lexer{sourceFile};
  TheParser parser{lexer};

  auto [functions, success] = parser.parseSourceFile();

  if (options.astDump) {
    for (auto &&fn : functions)
      fn->dump();
    return 0;
  }

  if (!success)
    return 1;

  Sema sema{std::move(functions)};

  auto resolvedFunctions = sema.resolveSourceFile();
  if (resolvedFunctions.empty())
    return 1;

  // FIXME: Is this the proper place to do this?
  if (options.cfgDump) {
    for (auto &&fn : resolvedFunctions) {
      std::cout << "----------" << '\n';
      std::cout << fn->identifier << '\n';
      std::cout << "----------" << '\n';
      CFGBuilder b;
      b.build(*fn).dump(1);
    }

    return 0;
  }

  if (options.resDump) {
    for (auto &&fn : resolvedFunctions)
      fn->dump();
    return 0;
  }

  Codegen codegen{std::move(resolvedFunctions)};
  std::unique_ptr<llvm::Module> ir = codegen.generateIR();

  // FIXME: Is this the proper place to do this?
  ir->setSourceFileName(*options.source);
  ir->setTargetTriple(llvm::sys::getDefaultTargetTriple());

  if (options.llvmDump) {
    ir->dump();
    return 0;
  }

  // Theoretically this can still generate the same tmp files for 2 different
  // invocations and remove them later.
  std::stringstream ss;
  ss << "tmp-" << std::hash<std::string_view>{}(*options.source) << ".ll";
  auto outLL = ss.str();

  std::error_code errorCode;
  llvm::raw_fd_ostream f{outLL, errorCode};
  ir->print(f, nullptr);

  std::stringstream command;
  command << "clang " << outLL;
  if (options.output)
    command << " -o " << *options.output;

  int ret = std::system(command.str().c_str());
  std::filesystem::remove(outLL);

  return ret;
}