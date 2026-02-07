# Grammar

```bnf
<sourceFile> 
    ::= (<structDecl> | <functionDecl>)* EOF

<structDecl>
    ::= 'struct' <identifier> <typeParamList>? <fieldList>

<typeParamList>
    ::= '<' <typeParamDecl> (',' <typeParamDecl>)* ','? '>'

<typeParamDecl>
    ::= <identifier>

<fieldList>
    ::= '{' (<fieldDecl> (',' <fieldDecl>)* ','?)? '}'

<fieldDecl>
    ::= <identifier> ':' <type>

<functionDecl> 
    ::= 'fn' <identifier> <typeParamList>? <parameterList> ':' <type> <block>

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
    ::= ('!' | '-')* <postfixExpression>

<postfixExpression>
    ::= <primaryExpression> (<argumentList> | <memberExpr>)*

<argumentList>
    ::= '(' (<expr> (',' <expr>)* ','?)? ')'

<primaryExpression>
    ::= <numberLiteral>
    |   <declRefExpr> <fieldInitList>?
    |   '(' <expr> ')'

<numberLiteral>
    ::= <number>

<declRefExpr>
    ::= <identifier> <typeArgumentList>?

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

<builtinType>
    ::= 'number'
    |   'void'

<userDefinedType>
    ::= <identifier> <typeList>?

<functionType>
    ::= '(' <type> (',' <type>)* ','? ')' -> type

<identifier>
    ::= ('a'..'z' | 'A'..'Z')+ ('a'..'z' | 'A'..'Z' | '0'..'9')*

<number>
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

