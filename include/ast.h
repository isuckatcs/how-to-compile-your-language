#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_AST_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_AST_H

#include <iostream>
#include <memory>

#include "utils.h"

namespace yl {
struct Type {
  enum class Kind { Void, Custom };

  Kind kind;
  std::string name;

  static Type builtinVoid() { return {Kind::Void, "void"}; }
  static Type custom(const std::string &name) { return {Kind::Custom, name}; }

private:
  Type(Kind kind, std::string name) : kind(kind), name(std::move(name)){};
};

struct Decl : public Dumpable {
  SourceLocation location;
  std::string identifier;

  Decl(SourceLocation location, std::string identifier)
      : location(location), identifier(std::move(identifier)) {}
  virtual ~Decl() = default;
};

struct Block : public Dumpable {
  SourceLocation location;

  Block(SourceLocation location) : location(location) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "Block\n";
  }
};

struct FunctionDecl : public Decl {
  Type type;
  std::unique_ptr<Block> body;

  FunctionDecl(SourceLocation location, std::string identifier, Type type,
               std::unique_ptr<Block> body)
      : Decl{location, std::move(identifier)}, type(std::move(type)),
        body(std::move(body)) {}

  void dump(size_t level = 0) const override {
    std::cerr << indent(level) << "FunctionDecl: " << identifier << ':'
              << type.name << '\n';

    body->dump(level + 1);
  }
};
} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_AST_H