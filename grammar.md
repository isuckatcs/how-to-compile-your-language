# Grammar

```bnf
<sourceFile> 
    ::= (<traitDecl> | <structDecl> | <functionDecl>)* EOF

<traitDecl>
    ::= 'trait' <identifier> <typeParamList>? <traitList>? '{' <traitFunctionDecl>* '}'

<traitFunctionDecl>
    ::= 'fn' <functionSignature> (';' | <block>)

<traitList>
    ::= ':' <traitInstance> ('&' <traitInstance>)*

<traitInstance>
    ::= <identifier> <typeList>?

<structDecl>
    ::= 'struct' <identifier> <typeParamList>? '{' <memberList>? '}'

<typeParamList>
    ::= '<' <typeParamDecl> (',' <typeParamDecl>)* ','? '>'

<typeParamDecl>
    ::= <identifier> <traitList>?

<memberList>
    ::= (<fieldList> | <implDecl> | <functionDecl>)*

<fieldList>
    ::= (<fieldDecl> (',' <fieldDecl>)* ','?)?

<fieldDecl>
    ::= <identifier> <typeAnnotation>

<implDecl> 
    ::= <implSpecifier> (';' | ('{' <functionDecl>* '}'))

<implSpecifier>
    ::= 'impl' <traitInstance>

<functionDecl> 
    ::= 'fn' <functionSignature> <block>

<functionSignature>
    ::= <identifier> <typeParamList>? <parameterList> <typeAnnotation>?

<parameterList>
    ::= '(' (<paramDecl> (',' <paramDecl>)* ','?)? ')'

<paramDecl>
    ::= 'mut'? <identifier> <typeAnnotation>

<varDecl>
    ::= <identifier> <typeAnnotation>? ('=' <expr>)?

<block>
    ::= '{' <statement>* '}'

<statement>
    ::= <expr> ';'
    |   <ifStatement>
    |   <whileStatement>
    |   <returnStmt>
    |   <assignment>
    |   <declStmt>

<whileStatement>
    ::= 'while' <expr> <block>

<ifStatement>
    ::= 'if' <expr> <block> ('else' (<ifStatement> | <block>))?

<declStmt>
    ::= ('let' | 'mut') <varDecl> ';'

<assignment>
    ::= <expr> '=' <expr> ';'

<memberExpr>
    ::= '.' <declRefExpr>

<returnStmt>
    ::= 'return' <expr>? ';'

<expr>
    ::= <disjunction>
    
<disjunction>
    ::= <conjunction> ('||' <conjunction>)*

<conjunction>
    ::= <equality> ('&&' <equality>)*

<equality>
    ::= <comparison> ('==' <comparison>)*

<comparison>
    ::= <additiveExpression> (('<' | '>') <additiveExpression>)*

<additiveExpression>
    ::= <multiplicativeExpression> (('+' | '-') <multiplicativeExpression>)*

<multiplicativeExpression>
    ::= <prefixExpression> (('*' | '/') <prefixExpression>)*

<prefixExpression>
    ::= ('!' | '-' | '&' | '*')* <postfixExpression>

<postfixExpression>
    ::= <primaryExpression> (<argumentList> | <memberExpr>)*

<argumentList>
    ::= '(' (<expr> (',' <expr>)* ','?)? ')'

<primaryExpression>
    ::= 'unit'
    |   <numberLiteral>
    |   <boolLiteral>
    |   <lambda>
    |   <pathExpr> <fieldInitList>?
    |   '(' <expr> ')'

<lambda>
    ::= '->' <lambdaParamList>? <block>

<lambdaParamList>
    ::= '(' (<lambdaParamDecl> (',' <lambdaParamDecl>)* ','?)? ')' <typeAnnotation>?

<lambdaParamDecl>
    ::= 'mut'? <identifier> <typeAnnotation>?

<typeAnnotation>
    ::= ':' <type>

<pathExpr>
    ::= (<traitSpecifier> '::')? <declRefExpr> ('::' <declRefExpr>)*

<traitSpecifier>
    ::= '@' '<' <type> <implSpecifier> '>'

<declRefExpr>
    ::= (<identifier> | 'Self') <typeArgumentList>?

<typeArgumentList>
    ::= '@' <typeList>

<typeList>
    ::= '<' <type> (',' <type>)* ','? '>'

<fieldInitList>
    ::= '{' (<fieldInit> (',' <fieldInit>)* ','?)? '}'

<fieldInit>
    ::= <identifier> ':' <expr>

<type>
    ::= <builtinType>
    |   <userDefinedType>
    |   <functionType>
    |   <outParamType>
    |   <pointerType>
    |   <implType>

<builtinType>
    ::= 'number'
    |   'bool'
    |   'unit'
    |   'Self'

<userDefinedType>
    ::= <identifier> <typeList>?

<functionType>
    ::= '(' <type> (',' <type>)* ','? ')' -> type

<outParamType>
    ::= '&' <type>

<pointerType>
    ::= '*' 'mut'? <type>

<implType>
    ::= 'impl' <traitInstance> ('&' <traitInstance>)*

<identifier>
    ::= ('a'..'z' | 'A'..'Z')+ ('a'..'z' | 'A'..'Z' | '0'..'9')*

<numberLiteral>
    ::= ('0'..'9')+ ('.' ('0'..'9')+)?

<boolLiteral>
    ::= 'true' | 'false'
```
# Operators

| Precedence | Type           | Symbols |
|------------|----------------|---------|
| Highest    | Prefix         | !, -    |
|            | Multiplicative | *, /    |
|            | Additive       | +, -    |
|            | Comparison     | <, >    |
|            | Equality       | ==      |
|            | Conjunction    | &&      |
| Lowest     | Disjunction    | \|\|    |

