# Grammar

```bnf
<mangledSymbol> 
    ::= '_Yl' <identifier> <genericArgs>?

<identifier>
    ::= ('0'..'9')+ ('a'..'z' | 'A'..'Z')+ ('a'..'z' | 'A'..'Z' | '0'..'9')*

<genericArgs>
    ::= 'G' <type>+ 'E'

<type>
    ::= 'n'
    |   'v'
    |   'S' <identifier> <genericArgs>?
    |   'F' <type>* 'R' <type>
```
