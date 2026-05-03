#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_DIAG_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_DIAG_H

#include <sstream>
#include <vector>

#include "utils.h"

namespace yl {
namespace diag {
enum class Severity { Error, Warning, Note };

struct Diagnostic {
  Severity severity;
  SourceLocation location;
  std::string message;
  std::vector<Diagnostic> notes;

  Diagnostic &withNote(Diagnostic note);
};

class DiagBuilder {
  Severity severity;
  std::string format;
  // FIXME: mutable
  mutable std::vector<std::string> renderedArgs;

  template <typename T> void renderArgs(T t) const {
    std::stringstream ss;
    ss << t;
    renderedArgs.emplace_back(ss.str());
  }

public:
  DiagBuilder(Severity severity, std::string format)
      : severity(severity),
        format(std::move(format)) {}

  template <typename... Args>
  Diagnostic operator()(SourceLocation location, Args... args) const {
    std::stringstream message;
    renderedArgs.clear();
    (renderArgs(args), ...);

    int subIdx = 0;
    for (auto it = format.begin(); it != format.end(); ++it) {
      if (*it == '{')
        message << renderedArgs[subIdx++];
      else if (*it != '}')
        message << *it;
    }

    return Diagnostic{severity, location, message.str()};
  }
};

#define ERR(name, msg)                                                         \
  const diag::DiagBuilder name { diag::Severity::Error, msg }

#define WRN(name, msg)                                                         \
  const diag::DiagBuilder name { diag::Severity::Warning, msg }

struct DiagnosticConsumer {
  virtual ~DiagnosticConsumer() = default;
  virtual void consume(Diagnostic diagnostic) = 0;
};

struct PrintingDiagnosticConsumer : public DiagnosticConsumer {
  void consume(Diagnostic diagnostic) override;
};

class DiagnosticReporter {
  DiagnosticConsumer *consumer;

public:
  explicit DiagnosticReporter(DiagnosticConsumer &consumer)
      : consumer(&consumer) {}

  std::nullptr_t report(Diagnostic diagnostic) {
    consumer->consume(diagnostic);
    return nullptr;
  }
};
} // namespace diag

namespace err {
// clang-format off
// parser
ERR(expected, "expected {}");
ERR(expectedTopLevel, "expected function, struct or trait declaration on the top level");

ERR(unknownType, "the type of '{}' is unknown");
ERR(inferenceError, "{}");
ERR(failedToResolveType, "failed to resolve type '{}'");
ERR(annotationsNeeded, "explicit type annotations are needed to infer the type of '{}'");
ERR(initTyMismatch, "an expression of type '{}' cannot be used to initialize a variable of type '{}'");
ERR(typeParamShadowed, "declaring '{}' shadows outer type parameter");
ERR(typeArgCntMismatch, "type argument count mismatch, expected {}, but received {}");
ERR(expectedOperandTy, "expected '{}' operand");
ERR(invalidCallTy, "calling expression of type '{}'");
ERR(expectedBoolCondition, "expected 'bool' in condition");
ERR(unexpectedTypeParam, "expected value, found type parameter");
ERR(expectedReturnValue, "expected function to return a value");
ERR(expectedReturnValueOnEveryPath, "expected function to return a value on every path");
ERR(rvalueAssignment, "cannot assign to rvalue");
ERR(redeclaration, "redeclaration of '{}'");
ERR(missingSymbol, "symbol '{}' not found");
ERR(lookupInTypeFailed, "failed to find member '{}' in '{}'");
ERR(memberFnLookupFailed, "failed to find member function '{}' in '{}'");
ERR(notInitialized, "'{}' is not initialized");
ERR(cannotBeMutated, "'{}' cannot be mutated");
ERR(fieldAlreadyInitialized, "field '{}' is already initialized");
ERR(noFieldWithName, "'{}' has no field named '{}'");
ERR(fieldNotInitialized, "field '{}' is not initialized");
ERR(expectedMethodCall, "expected to call method");
ERR(unexpectedAmpParam, "only parameters can have '&' type");
ERR(ampOutsideArgList, "'&' can only be used to pass arguments to '&' parameters");
ERR(ampWrongCategory, "only mutable lvalues can be passed to '&' parameters");
ERR(mutableAmp, "unexpected 'mut' specifier, a '&' parameter is always mutable");
ERR(unaryOperandUnknown, "type of operand to unary operator is unknown");
ERR(binopOperandUnknown, "type of {} to binary operator is unknown");
ERR(binopIncompatibleOperands, "incompatible operands to binary operator ('{}' and '{}')");
ERR(incompatibleAssignment, "expected to assign '{}' but received '{}' instead");
ERR(noReturnValue, "expected a return value");
ERR(invalidReturnValue, "cannot return '{}' from a function returning '{}'");
ERR(cannotAccessMember, "cannot access member of '{}'");
ERR(ambigousMemberFn, "ambigous member function reference");
ERR(notGeneric, "'{}' is not a generic");
ERR(classMethodCallOnInstance, "class level methods cannot be called on an instance");
ERR(wrongArgCount, "wrong number of arguments in function call, expected {}, but received {}");

// trait
ERR(traitNotImplemented, "'{}' doesn't implement trait '{}'");
ERR(traitMissingMember, "trait '{}' has no member '{}'");
ERR(stricterParamTy, "cannot replace parameter of type '{}' with stricter implementation type '{}'");
ERR(fnSignatureMismatch, "trait function declaration has '{}' signature, but the given implementation is '{}'");
ERR(notATrait, "'{}' is not a trait");
ERR(alreadyImplementedTrait, "trait '{}' is already implemented for struct '{}'");
ERR(alreadyImplementedFn, "function '{}' is already implemented for trait '{}'");
ERR(missingTraitFn, "struct '{}' must implement function '{}' from trait '{}'");
ERR(conflictingTrait, "implementing trait '{}' conflicts with more specific implementation '{}'");
ERR(missingRequirement, "implementing trait '{}' requires implementing trait '{}'");

// struct
ERR(expectedInstance, "expected an instance of '{}'");
ERR(structImmutable, "expected mutable struct instance");
ERR(notStructInstance, "expected struct declaration to instantiate");
ERR(selfContainingStruct, "struct '{}' contains itself");

// main
ERR(wrongMainReturnTy, "'main' function is expected to return 'unit'");
ERR(wrongMainArgCount, "'main' function is expected to take no arguments");
ERR(mainIsGeneric, "'main' function cannot be generic");
ERR(mainNotFound, "'main' function not found");

// builtin
ERR(reservedPrintf, "'printf' is a reserved function name and cannot be used for user-defined functions");

// self
ERR(selfTyNotAllowed, "'Self' is only allowed inside structs and traits");
ERR(selfParamNotAllowed, "'self' parameter is only allowed in methods");
ERR(selfWrongPosition, "'self' can only be the first parameter");
ERR(selfWrongType, "the type of 'self' must reference 'Self'");
// clang-format on
}; // namespace err

namespace wrn {
WRN(unreachableStmt, "unreachable statement");
} // namespace wrn

} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_DIAG_H
