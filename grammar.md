# Grammar

```bnf
<sourceFile> 
    ::= (<structDecl> | <functionDecl>)* EOF

<structDecl>
    ::= 'struct' <identifier> <memberList>

<memberList>
    ::= '{' (<memberDecl> (',' <memberDecl>)* ','?)? '}'

<memberDecl>
    ::= <identifier> ':' <type>

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
    ::= (<declRefExpr> | <memberExpr>) '=' <expr> ';'

<memberExpr>
    ::= '.' <identifier>

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
    ::= <primaryExpression> <argumentList>? <memberExpr>*

<argumentList>
    ::= '(' (<expr> (',' <expr>)* ','?)? ')'

<primaryExpression>
    ::= <numberLiteral>
    |   <structInstantiation>
    |   <declRefExpr>
    |   '(' <expr> ')'

<numberLiteral>
    ::= <number>

<structInstantiation>
    ::= <identifier> <memberInitList>

<memberInitList>
    ::= '{' (<memberInit> (',' <memberInit>)* ','?)? '}'

<memberInit>
    ::= <identifier> ':' <expr>

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

