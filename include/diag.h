#ifndef HOW_TO_COMPILE_YOUR_LANGUAGE_DIAG_H
#define HOW_TO_COMPILE_YOUR_LANGUAGE_DIAG_H

#include <sstream>
#include <vector>

#include "utils.h"

namespace yl {
namespace diag {
struct Diagnostic {
  enum class Severity { Error, Warning };

  Severity severity;
  SourceLocation location;
  std::string message;
};

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

class DiagBuilder {
  Diagnostic::Severity severity;
  SourceLocation location;
  std::string_view format;
  std::vector<std::string> args;

public:
  DiagBuilder(Diagnostic::Severity severity,
              SourceLocation location,
              std::string_view format)
      : severity(severity),
        location(location),
        format(std::move(format)) {}

  template <typename T> DiagBuilder &with(T t) {
    std::stringstream ss;
    ss << t;
    args.emplace_back(ss.str());
    return *this;
  }

  std::nullptr_t report(DiagnosticReporter *reporter) {
    std::stringstream message;

    int argIdx = 0;
    for (const auto *it = format.begin(); it != format.end(); ++it) {
      if (*it == '{') {
        message << args[argIdx++];
        ++it;
        continue;
      }

      message << *it;
    }

    return reporter->report(Diagnostic{severity, location, message.str()});
  }
};

#define diag(severity, name, format)                                           \
  inline diag::DiagBuilder name(SourceLocation loc) {                          \
    return diag::DiagBuilder(diag::Diagnostic::Severity::severity, loc,        \
                             format);                                          \
  }
} // namespace diag

namespace err {
// clang-format off
// parser
diag(Error, expected, "expected {}");
diag(Error, expectedTopLevel, "expected function, struct or trait declaration on the top level");
diag(Error, expectedAtEnd, "expected '{}' at the end of {}");
diag(Error, expectedBody, "expected {} body");

diag(Error, unknownType, "the type of '{}' is unknown");
diag(Error, inferenceError, "{}");
diag(Error, failedToResolveType, "failed to resolve type '{}'");
diag(Error, annotationsNeeded, "explicit type annotations are needed to infer the type of '{}'");
diag(Error, initTyMismatch, "an expression of type '{}' cannot be used to initialize a variable of type '{}'");
diag(Error, typeParamShadowed, "declaring '{}' shadows outer type parameter");
diag(Error, typeArgCntMismatch, "type argument count mismatch, expected {}, but received {}");
diag(Error, expectedOperandTy, "expected '{}' operand");
diag(Error, invalidCallTy, "calling expression of type '{}'");
diag(Error, expectedBoolCondition, "expected 'bool' in condition");
diag(Error, unexpectedTypeParam, "expected value, found type parameter");
diag(Error, expectedReturnValue, "expected function to return a value");
diag(Error, expectedReturnValueOnEveryPath, "expected function to return a value on every path");
diag(Error, rvalueAssignment, "cannot assign to rvalue");
diag(Error, redeclaration, "redeclaration of '{}'");
diag(Error, missingSymbol, "symbol '{}' not found");
diag(Error, lookupInTypeFailed, "failed to find member '{}' in '{}'");
diag(Error, memberFnLookupFailed, "failed to find member function '{}' in '{}'");
diag(Error, notInitialized, "'{}' is not initialized");
diag(Error, cannotBeMutated, "'{}' cannot be mutated");
diag(Error, fieldAlreadyInitialized, "field '{}' is already initialized");
diag(Error, noFieldWithName, "'{}' has no field named '{}'");
diag(Error, fieldNotInitialized, "field '{}' is not initialized");
diag(Error, expectedMethodCall, "expected to call method");
diag(Error, unexpectedAmpParam, "only parameters can have '&' type");
diag(Error, ampOutsideArgList, "'&' can only be used to pass arguments to '&' parameters");
diag(Error, ampWrongCategory, "only mutable lvalues can be passed to '&' parameters");
diag(Error, mutableAmp, "unexpected 'mut' specifier, a '&' parameter is always mutable");
diag(Error, unaryOperandUnknown, "type of operand to unary operator is unknown");
diag(Error, binopOperandUnknown, "type of {} to binary operator is unknown");
diag(Error, binopIncompatibleOperands, "incompatible operands to binary operator ('{}' and '{}')");
diag(Error, incompatibleAssignment, "expected to assign '{}' but received '{}' instead");
diag(Error, noReturnValue, "expected a return value");
diag(Error, invalidReturnValue, "cannot return '{}' from a function returning '{}'");
diag(Error, cannotAccessMember, "cannot access member of '{}'");
diag(Error, ambigousMemberFn, "ambigous member function reference");
diag(Error, notGeneric, "'{}' is not a generic");
diag(Error, classMethodCallOnInstance, "class level methods cannot be called on an instance");
diag(Error, wrongArgCount, "wrong number of arguments in function call, expected {}, but received {}");

// trait
diag(Error, traitNotImplemented, "'{}' doesn't implement trait '{}'");
diag(Error, traitMissingMember, "trait '{}' has no member '{}'");
diag(Error, stricterParamTy, "cannot replace parameter of type '{}' with stricter implementation type '{}'");
diag(Error, fnSignatureMismatch, "trait function declaration has '{}' signature, but the given implementation is '{}'");
diag(Error, notATrait, "'{}' is not a trait");
diag(Error, alreadyImplementedTrait, "trait '{}' is already implemented for struct '{}'");
diag(Error, alreadyImplementedFn, "function '{}' is already implemented for trait '{}'");
diag(Error, missingTraitFn, "struct '{}' must implement function '{}' from trait '{}'");
diag(Error, conflictingTrait, "implementing trait '{}' conflicts with more specific implementation '{}'");
diag(Error, missingRequirement, "implementing trait '{}' requires implementing trait '{}'");

// struct
diag(Error, expectedInstance, "expected an instance of '{}'");
diag(Error, structImmutable, "expected mutable struct instance");
diag(Error, notStructInstance, "expected struct declaration to instantiate");
diag(Error, selfContainingStruct, "struct '{}' contains itself");

// main
diag(Error, wrongMainReturnTy, "'main' function is expected to return 'unit'");
diag(Error, wrongMainArgCount, "'main' function is expected to take no arguments");
diag(Error, mainIsGeneric, "'main' function cannot be generic");
diag(Error, mainNotFound, "'main' function not found");

// builtin
diag(Error, reservedPrintf, "'printf' is a reserved function name and cannot be used for user-defined functions");

// self
diag(Error, selfTyNotAllowed, "'Self' is only allowed inside structs and traits");
diag(Error, selfParamNotAllowed, "'self' parameter is only allowed in methods");
diag(Error, selfWrongPosition, "'self' can only be the first parameter");
diag(Error, selfWrongType, "the type of 'self' must reference 'Self'");
// clang-format on
}; // namespace err

namespace wrn {
// clang-format off
diag(Warning, unreachableStmt, "unreachable statement");
// clang-format on
} // namespace wrn

} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_DIAG_H
