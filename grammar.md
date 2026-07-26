# Grammar

```bnf
<sourceFile> 
    ::= (<traitDecl> | <structDecl> | <functionDecl> | <typeExtension>)* EOF

<traitDecl>
    ::= 'trait' <identifier> <typeParamList>? <traitConformance>? '{' <traitFunctionDecl>* '}'

<traitFunctionDecl>
    ::= 'fn' <functionSignature> (';' | <block>)

<traitConformance>
    ::= ':' <userDefinedType> ('&' <userDefinedType>)*

<structDecl>
    ::= 'struct' <identifier> <typeParamList>? '{' (<fieldList> | <functionDecl>)* '}'

<typeParamList>
    ::= '<' <typeParamDecl> (',' <typeParamDecl>)* ','? '>'

<typeParamDecl>
    ::= <identifier> <traitConformance>?

<fieldList>
    ::= (<fieldDecl> (',' <fieldDecl>)* ','?)?

<fieldDecl>
    ::= <identifier> <typeAnnotation>

<typeExtension> 
    ::= 'extension' <typeParamList> <type> <traitConformance> '{' <functionDecl>* '}'

<functionDecl> 
    ::= 'fn' <functionSignature> <block>

<functionSignature>
    ::= <identifier> <typeParamList>? <parameterList> <typeAnnotation>?

<parameterList>
    ::= '(' (<paramDecl> (',' <paramDecl>)* ','?)? ')'

<paramDecl>
    ::= 'mut'? <identifier> ':' <borrowedModifier>? <type>

<borrowedModifier>
    ::= 'borrowed' 'mut'?

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
    ::= ('!' | '-' | '*')* <postfixExpression>

<postfixExpression>
    ::= <primaryExpression> (<argumentList> | <memberExpr>)*

<argumentList>
    ::= '(' (<expr> (',' <expr>)* ','?)? ')'

<primaryExpression>
    ::= 'unit'
    |   <numberLiteral>
    |   <boolLiteral>
    |   <gcExpr>
    |   <lambda>
    |   <pathExpr> <fieldInitList>?
    |   '(' <expr> ')'

<gcExpr>
    ::= ('gc' | 'gcMut') '(' <expr> ')'

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
    ::= '@' '<' <type> ':' <userDefinedType> '>'

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
    |   <pointerType>
    |   <anyType>

<builtinType>
    ::= 'number'
    |   'bool'
    |   'unit'
    |   'Self'

<userDefinedType>
    ::= <identifier> <typeList>?

<functionType>
    ::= '(' <type> (',' <type>)* ','? ')' -> type

<pointerType>
    ::= '*' 'mut'? <type>

<anyType>
    ::= 'any' <userDefinedType>

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

