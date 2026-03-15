# Grammar

```bnf
<sourceFile> 
    ::= (<traitDecl> | <structDecl> | <functionDecl>)* EOF

<traitDecl>
    ::= 'trait' <identifier> <typeParamList>? <traitList>? '{' <traitFunctionDecl>* '}'

<traitFunctionDecl>
    ::= 'fn' <functionSignature> (';' | <block>)

<traitList>
    ::= ':' <userDefinedType> ('&' <userDefinedType>)*

<structDecl>
    ::= 'struct' <identifier> <typeParamList>? <traitList>? '{' <memberList>? '}'

<typeParamList>
    ::= '<' <typeParamDecl> (',' <typeParamDecl>)* ','? '>'

<typeParamDecl>
    ::= <identifier> <traitList>?

<memberList>
    ::= (<fieldList> | <memberFunctionList>)*

<fieldList>
    ::= (<fieldDecl> (',' <fieldDecl>)* ','?)?

<fieldDecl>
    ::= <identifier> ':' <type>

<memberFunctionList>
    ::= (<implDecl> | <functionDecl>)*

<implDecl> 
    ::= 'impl' <userDefinedType> '::' <functionSignature> <block>

<functionDecl> 
    ::= 'fn' <functionSignature> <block>

<functionSignature>
    ::= <identifier> <typeParamList>? <parameterList> ':' <type>?

<parameterList>
    ::= '(' (<paramDecl> (',' <paramDecl>)* ','?)? ')'

<paramDecl>
    ::= 'mut'? <identifier> ':' <type>

<varDecl>
    ::= <identifier> (':' <type>)? ('=' <expr>)?

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
    ::= ('!' | '-' | '&')* <postfixExpression>

<postfixExpression>
    ::= <primaryExpression> (<argumentList> | <memberExpr>)*

<argumentList>
    ::= '(' (<expr> (',' <expr>)* ','?)? ')'

<primaryExpression>
    ::= 'unit'
    |   <numberLiteral>
    |   <pathExpr> <fieldInitList>?
    |   '(' <expr> ')'

<pathExpr>
    ::= <declRefExpr> ('::' <declRefExpr>)*

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

<builtinType>
    ::= 'number'
    |   'unit'
    |   'Self'

<userDefinedType>
    ::= <identifier> <typeList>?

<functionType>
    ::= '(' <type> (',' <type>)* ','? ')' -> type

<outParamType>
    ::= '&' <type>

<identifier>
    ::= ('a'..'z' | 'A'..'Z')+ ('a'..'z' | 'A'..'Z' | '0'..'9')*

<numberLiteral>
    ::= ('0'..'9')+ ('.' ('0'..'9')+)?
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

