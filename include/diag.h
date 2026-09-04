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
  DiagBuilder(Diagnostic::Severity severity, std::string_view format)
      : severity(severity),
        format(std::move(format)) {}

  DiagBuilder &at(SourceLocation location) {
    this->location = location;
    return *this;
  }

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
  inline diag::DiagBuilder name() {                                            \
    return diag::DiagBuilder(diag::Diagnostic::Severity::severity, format);    \
  }
} // namespace diag

namespace err {
// clang-format off
// parser
diag(Error, expected, "expected {}");
diag(Error, expected2, "expected {} or {}");
diag(Error, expected3, "expected {}, {} or {}");
diag(Error, expected4, "expected {}, {}, {} or {}");
diag(Error, expectedAtEnd, "expected {} at the end of {}");
diag(Error, expectedBody, "expected {} body");

// symbols
diag(Error, redeclaration, "redeclaration of '{}'");
diag(Error, missingSymbol, "symbol '{}' not found");
diag(Error, fieldLookupBaseInvalid, "cannot look up field '{}' in primitive type '{}'");
diag(Error, fieldLookupFailed, "failed to find field '{}' in '{}'");
diag(Error, memberLookupFailed, "failed to find member '{}' in '{}'");
diag(Error, memberFnLookupFailed, "failed to find member function '{}' in '{}'");
diag(Error, noFieldWithName, "'{}' has no field named '{}'");
diag(Error, memberAccessInRawTrait, "cannot look up member in trait without a specific extension");
diag(Error, memberAccessInValue, "cannot look up member in value");
diag(Error, ambiguousAssociatedFn, "ambiguous associated function reference");
diag(Error, traitObjectMethodNotCalled, "expected to call member function on trait object");

// types
diag(Error, unknownType, "the type of '{}' is unknown");
diag(Error, failedToResolveType, "failed to resolve type '{}'");
diag(Error, unificationError, "cannot unify '{}' with '{}'")
diag(Error, unsatisfiedRequirement, "cannot satisfy requirement '{} : {}'")
diag(Error, ambiguousConformance, "'{}' ambiguously satisfies requirement '{} : {}'")
diag(Error, annotationsNeeded, "explicit type annotations are needed to infer the type of '{}'");
diag(Error, annotationsNeededForRequirements, "explicit type annotations needed to disambiguate requirements");
diag(Error, expectedOperandTy, "expected '{}' operand");
diag(Error, expectedPointerOperand, "only pointer operands can be dereferenced");
diag(Error, memberBaseUnknown, "type of base expression is unknown");
diag(Error, unaryOperandUnknown, "type of operand to unary operator is unknown");
diag(Error, binopOperandUnknown, "type of {} to binary operator is unknown");
diag(Error, binopIncompatibleOperands, "incompatible operands to binary operator ('{}' and '{}')");
diag(Error, expectedBoolCondition, "expected 'bool' in condition");
diag(Error, invalidCallTy, "calling expression of type '{}'");
diag(Error, rawTrait, "expected a specific extension or a trait object of '{}'");
diag(Error, wrongDeclKind, "referenced declaration has unexpected kind");
diag(Error, traitObjectWrongType, "only pointers and references to trait objects are allowed");
diag(Error, traitObjectTemplateMemberFn, "trait '{}' with a template method cannot be used as a trait object");
diag(Error, traitObjectStaticMemberFn, "trait '{}' with a static method cannot be used as a trait object");
diag(Error, traitObjectSelfParam, "trait '{}' with a method referencing 'Self' in a non-receiver parameter type cannot be used as a trait object");
diag(Error, traitObjectSelfReturn, "trait '{}' with a method returning a type referencing 'Self' cannot be used as a trait object");
diag(Error, superTraitNotTraitObjectCompatible, "super trait '{}' of trait '{}' is not trait object compatible");
diag(Error, traitNotTraitObjectCompatible, "trait '{}' is not trait object compatible");
diag(Error, traitObjectPtrDereference, "cannot dereference pointer to trait object");
diag(Error, traitObjectAssignment, "cannot assign to trait object");
diag(Error, universalTypeExtension, "expected a trait extension or an extension of a concrete type");
diag(Error, nonStructTypeExtension, "type '{}' can only be extended with traits");
diag(Error, infiniteStructType, "struct '{}' recurses infinitely in '{}'");
diag(Error, promoOperandUnknown, "cannot promote unknown '{}' type to '{}'");

// generics
diag(Error, typeParamShadowed, "declaring '{}' shadows outer type parameter");
diag(Error, extensionTypeParamUnused, "type parameter '{}' must be referenced by either the extended type or the extending trait");
diag(Error, typeArgCntMismatch, "type argument count mismatch, expected {}, but received {}");
diag(Error, notGeneric, "'{}' is not a generic");

// functions
diag(Error, wrongArgCount, "wrong number of arguments in function call, expected {}, but received {}");
diag(Error, classMethodCallOnInstance, "class level methods cannot be called on an instance");
diag(Error, expectedReturnValue, "expected function to return a value");
diag(Error, expectedReturnValueOnEveryPath, "expected function to return a value on every path");
diag(Error, noReturnValue, "expected a return value");

// values
diag(Error, rvalueAssignment, "cannot assign to rvalue");
diag(Error, immutableAssignment, "cannot assign to immutable value");
diag(Error, rvalueRef, "only lvalues can be taken by reference");
diag(Error, mutRefParameter, "a '&' parameter cannot be 'mut'");
diag(Error, numberLiteralOutOfRange, "number literal must be between {} and {}");

// init
diag(Error, notInitialized, "'{}' is not initialized");
diag(Error, fieldAlreadyInitialized, "field '{}' is already initialized");
diag(Error, fieldNotInitialized, "field '{}' is not initialized");

// traits
diag(Error, stricterParamTy, "cannot replace parameter of type '{}' with stricter implementation type '{}'");
diag(Error, fnSignatureMismatch, "trait function declaration has '{}' signature, but the given implementation is '{}'");
diag(Error, notATrait, "'{}' is not a trait");
diag(Error, missingTraitFn, "'{}' must implement function '{}' from trait '{}'");
diag(Error, conflictingExtensionForType, "extension '{} : {}' conflicts with extension '{} : {}' for type '{}'");
diag(Error, missingRequirement, "extension '{} : {}' requires extension '{} : {}'");
diag(Error, selfRequiringTrait, "trait '{}' requires conformance to itself");
diag(Error, overflow, "overflow while solving '{} : {}'");
diag(Error, monoOverflow, "overflow while instantiating '{}' with '{}'");
diag(Error, multipleTraitsProvideMethod, "multiple traits provide '{}' for '{}'");
diag(Error, traitProvidesMethod, "trait '{}' provides '{}' for '{}'");

// structs
diag(Error, selfContainingStruct, "struct '{}' contains itself");

// self
diag(Error, selfTyNotAllowed, "'Self' is only allowed inside traits, structs and extensions");
diag(Error, selfParamNotAllowed, "'self' parameter is only allowed in methods");
diag(Error, selfWrongPosition, "'self' can only be the first parameter");
diag(Error, selfWrongType, "'self' can only have '&Self' or '&mut Self' types");

// main
diag(Error, wrongMainReturnTy, "'main' function is expected to return 'unit'");
diag(Error, wrongMainArgCount, "'main' function is expected to take no arguments");
diag(Error, mainIsGeneric, "'main' function cannot be generic");
diag(Error, mainNotFound, "'main' function not found");

// lambdas
diag(Error, refParamCapture, "capturing '&' parameter in lambda is not allowed");
diag(Error, lamdaSelfParam, "'self' in lambda parameter list is not allowed");

// driver
diag(Error, noSourceFile, "no source file specified");
diag(Error, unexpectedExtension, "unexpected source file extension");
diag(Error, failedToOpenFile, "failed to open '{}'");
diag(Error, unexpectedArgument, "unexpected argument '{}'");
diag(Error, unexpectedOption, "unexpected option '{}'");
diag(Error, expectedFlagArgument, "expected argument to '{}'");
diag(Error, failedToFindInPath, "failed to find '{}' in PATH");
// clang-format on
}; // namespace err

namespace wrn {
// clang-format off
diag(Warning, unreachableStmt, "unreachable statement");
// clang-format on
} // namespace wrn

} // namespace yl

#endif // HOW_TO_COMPILE_YOUR_LANGUAGE_DIAG_H
