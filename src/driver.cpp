#include <llvm/IR/Verifier.h>
#include <llvm/Support/FileSystem.h>

#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#include "codegen.h"
#include "lexer.h"
#include "mono.h"
#include "parser.h"
#include "sema.h"

using namespace yl;

namespace {
void displayHelp() {
  std::cout << "Usage:\n"
            << "  compiler [options] <source_file>\n\n"
            << "Options:\n"
            << "  -h              display this message\n"
            << "  -o <file>       write executable to <file>\n"
            << "  -verify-only    only verify the generated llvm module\n"
            << "  -ast-dump       print the abstract syntax tree\n"
            << "  -res-dump       print the resolved syntax tree\n"
            << "  -mono-dump      print the instantiated generics\n"
            << "  -llvm-dump      print the llvm module\n"
            << "  -cfg-dump       print the control flow graph\n";
}

struct CompilerOptions {
  std::filesystem::path source;
  std::filesystem::path output;
  bool displayHelp = false;
  bool verifyOnly = false;
  bool astDump = false;
  bool resDump = false;
  bool monoDump = false;
  bool llvmDump = false;
  bool cfgDump = false;
};

CompilerOptions parseArguments(int argc,
                               const char **argv,
                               diag::DiagnosticReporter *reporter) {
  CompilerOptions options;

  int idx = 1;
  while (idx < argc) {
    std::string_view arg = argv[idx];

    if (arg[0] != '-') {
      if (!options.source.empty()) {
        err::unexpectedArgument().with(arg).report(reporter);
        std::exit(1);
      }

      options.source = arg;
    } else {
      if (arg == "-h")
        options.displayHelp = true;
      else if (arg == "-o")
        options.output = ++idx >= argc ? "" : argv[idx];
      else if (arg == "-verify-only")
        options.verifyOnly = true;
      else if (arg == "-ast-dump")
        options.astDump = true;
      else if (arg == "-res-dump")
        options.resDump = true;
      else if (arg == "-mono-dump")
        options.monoDump = true;
      else if (arg == "-llvm-dump")
        options.llvmDump = true;
      else if (arg == "-cfg-dump")
        options.cfgDump = true;
      else {
        err::unexpectedOption().with(arg).report(reporter);
        std::exit(1);
      }
    }

    ++idx;
  }

  return options;
}

std::filesystem::path getLibDirPath(const char *argv0) {
  void *address = (void *)getLibDirPath;
  std::string exe = llvm::sys::fs::getMainExecutable(argv0, address);

  auto exeDir = std::filesystem::weakly_canonical(exe).parent_path();
  return exeDir.parent_path().append("lib");
}
} // namespace

int main(int argc, const char **argv) {
  diag::DiagnosticConsumer consumer;
  diag::DiagnosticReporter reporter(consumer);

  CompilerOptions options = parseArguments(argc, argv, &reporter);

  if (options.displayHelp) {
    displayHelp();
    return 0;
  }

  if (options.source.empty()) {
    err::noSourceFile().report(&reporter);
    std::exit(1);
  }

  if (options.source.extension() != ".yl") {
    err::unexpectedExtension().report(&reporter);
    std::exit(1);
  }

  std::ifstream file(options.source);
  if (!file) {
    err::failedToOpenFile().with(options.source.string()).report(&reporter);
    std::exit(1);
  }

  std::stringstream buffer;
  buffer << file.rdbuf();
  SourceFile sourceFile{options.source.c_str(), buffer.str()};

  Lexer lexer(sourceFile);
  Parser parser(reporter, lexer);
  std::unique_ptr<ast::SourceFile> ast = parser.parseSourceFile();

  if (options.astDump) {
    ast->dump();
    return 0;
  }

  if (!ast->isComplete)
    return 1;

  ConstExprEvaluator cee(true);
  Sema sema(reporter, cee, *ast);
  std::unique_ptr<res::Context> resolvedCtx = sema.resolveAST();

  if (options.resDump) {
    if (resolvedCtx)
      resolvedCtx->getTU()->dump();

    return 0;
  }

  if (options.cfgDump) {
    if (resolvedCtx)
      resolvedCtx->dumpEveryFunctionCFG();
    return 0;
  }

  if (!resolvedCtx)
    return 1;

  MonoCollector monoCollector(reporter, resolvedCtx.get());
  std::unique_ptr<mono::Context> monoCtx = monoCollector.collectMonoFunctions();

  if (options.monoDump) {
    if (monoCtx)
      monoCtx->dump();

    return 0;
  }

  if (!monoCtx)
    return 1;

  Codegen codegen(*monoCtx, options.source.c_str());
  llvm::Module *llvmIR = codegen.generateIR();

  if (options.verifyOnly)
    return llvm::verifyModule(*llvmIR, &llvm::errs());

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
  command << "clang-20 " << llvmIRPath << " -L" << getLibDirPath(argv[0])
          << " -lyl_runtime";
  if (!options.output.empty())
    command << " -o " << options.output;

#ifdef YL_LIBGCOV
  command << ' ' << YL_LIBGCOV;
#endif

  int ret = std::system(command.str().c_str());
  std::filesystem::remove(llvmIRPath);

  return ret == 0 ? 0 : 1;
}
