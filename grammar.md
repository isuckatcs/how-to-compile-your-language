# Grammar

```bnf
<sourceFile> 
    ::= <functionDecl>* EOF

<functionDecl> 
    ::= 'fn' <identifier> <parameterList> ':' <type> <block>

<parameterList>
    ::= '(' (<paramDecl> (',' <paramDecl>)* ','?)? ')'

<paramDecl>
    ::= 'var'? <identifier> ':' <type>

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
    ::= ('let' | 'var') <varDecl> ';'

<assignment>
    ::= <declRefExpr> '=' <expr> ';'

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
    ::= <primaryExpression> <argumentList>

<argumentList>
    ::= '(' (<expr> (',' <expr>)* ','?)? ')'

<primaryExpression>
    ::= <numberLiteral>
    |   <declRefExpr>
    |   '(' <expr> ')'

<numberLiteral>
    ::= <number>

<declRefExpr>
    ::= <identifier>

<type>
    ::= 'number'
    |   'void'
    |   <identifier>

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

