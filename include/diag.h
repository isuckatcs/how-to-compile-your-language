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
  void consume(Diagnostic diagnostic);
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
diag(Error, expected2, "expected {} or {}");
diag(Error, expected3, "expected {}, {} or {}");
diag(Error, expected4, "expected {}, {}, {} or {}");
diag(Error, expectedTopLevel, "expected function, struct or trait declaration on the top level");
diag(Error, expectedAtEnd, "expected {} at the end of {}");
diag(Error, expectedBody, "expected {} body");

// symbols
diag(Error, redeclaration, "redeclaration of '{}'");
diag(Error, missingSymbol, "symbol '{}' not found");
diag(Error, lookupInTypeFailed, "failed to find member '{}' in '{}'");
diag(Error, memberFnLookupFailed, "failed to find member function '{}' in '{}'");
diag(Error, noFieldWithName, "'{}' has no field named '{}'");
diag(Error, cannotAccessMember, "cannot access member of '{}'");
diag(Error, ambigousMemberFn, "ambigous member function reference");

// types
diag(Error, unknownType, "the type of '{}' is unknown");
diag(Error, failedToResolveType, "failed to resolve type '{}'");
diag(Error, inferenceError, "{}");
diag(Error, annotationsNeeded, "explicit type annotations are needed to infer the type of '{}'");
diag(Error, initTyMismatch, "an expression of type '{}' cannot be used to initialize a variable of type '{}'");
diag(Error, incompatibleAssignment, "expected to assign '{}' but received '{}' instead");
diag(Error, invalidReturnValue, "cannot return '{}' from a function returning '{}'");
diag(Error, expectedOperandTy, "expected '{}' operand");
diag(Error, unaryOperandUnknown, "type of operand to unary operator is unknown");
diag(Error, binopOperandUnknown, "type of {} to binary operator is unknown");
diag(Error, binopIncompatibleOperands, "incompatible operands to binary operator ('{}' and '{}')");
diag(Error, expectedBoolCondition, "expected 'bool' in condition");
diag(Error, invalidCallTy, "calling expression of type '{}'");
diag(Error, expectedInstance, "expected an instance of '{}'");
diag(Error, notStructInstance, "expected struct declaration to instantiate");

// generics
diag(Error, typeParamShadowed, "declaring '{}' shadows outer type parameter");
diag(Error, typeArgCntMismatch, "type argument count mismatch, expected {}, but received {}");
diag(Error, unexpectedTypeParam, "expected value, found type parameter");
diag(Error, notGeneric, "'{}' is not a generic");

// functions
diag(Error, wrongArgCount, "wrong number of arguments in function call, expected {}, but received {}");
diag(Error, expectedMethodCall, "expected to call method");
diag(Error, classMethodCallOnInstance, "class level methods cannot be called on an instance");
diag(Error, expectedReturnValue, "expected function to return a value");
diag(Error, expectedReturnValueOnEveryPath, "expected function to return a value on every path");
diag(Error, noReturnValue, "expected a return value");

// values
diag(Error, rvalueAssignment, "cannot assign to rvalue");
diag(Error, cannotBeMutated, "'{}' cannot be mutated");
diag(Error, structImmutable, "expected mutable struct instance");
diag(Error, unexpectedAmpParam, "only parameters can have '&' type");
diag(Error, ampOutsideArgList, "'&' can only be used to pass arguments to '&' parameters");
diag(Error, ampWrongCategory, "only mutable lvalues can be passed to '&' parameters");
diag(Error, mutableAmp, "unexpected 'mut' specifier, a '&' parameter is always mutable");

// init
diag(Error, notInitialized, "'{}' is not initialized");
diag(Error, fieldAlreadyInitialized, "field '{}' is already initialized");
diag(Error, fieldNotInitialized, "field '{}' is not initialized");

// traits
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

// structs
diag(Error, selfContainingStruct, "struct '{}' contains itself");

// self
diag(Error, selfTyNotAllowed, "'Self' is only allowed inside structs and traits");
diag(Error, selfParamNotAllowed, "'self' parameter is only allowed in methods");
diag(Error, selfWrongPosition, "'self' can only be the first parameter");
diag(Error, selfWrongType, "the type of 'self' must reference 'Self'");

// main
diag(Error, wrongMainReturnTy, "'main' function is expected to return 'unit'");
diag(Error, wrongMainArgCount, "'main' function is expected to take no arguments");
diag(Error, mainIsGeneric, "'main' function cannot be generic");
diag(Error, mainNotFound, "'main' function not found");

// builtins
diag(Error, reservedPrintf, "'printf' is a reserved function name and cannot be used for user-defined functions");

// lambdas
diag(Error, outParamCapture, "capturing '&' parameter '{}' in lambda is not allowed");
// clang-format on
}; // namespace err

namespace wrn {
// clang-format off
diag(Warning, unreachableStmt, "unreachable statement");
// clang-format on
} // namespace wrn

} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_DIAG_H
