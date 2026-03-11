# Grammar

```bnf
<mangledSymbol> 
    ::= '_Yl' <structType>* <identifier> <genericArgs>?

<identifier>
    ::= ('0'..'9')+ ('a'..'z' | 'A'..'Z')+ ('a'..'z' | 'A'..'Z' | '0'..'9')*

<genericArgs>
    ::= 'G' <type>+ 'E'

<type>
    ::= 'n'
    |   'v'
    |   <structType>
    |   <functionType>

<structType>
    ::= 'S' <identifier> <genericArgs>?

<functionType>
    ::= 'F' <type>* 'R' <type>
```
