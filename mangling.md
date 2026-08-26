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
    |   'u'
    |   'b'
    |   'p' <type>
    |   'm' <type>
    |   'r' <type>
    |   'i' <type>
    |   <structType>
    |   <anyTraitType>
    |   <traitType>
    |   <functionType>

<structType>
    ::= 'S' <identifier> <genericArgs>?

<anyTraitType>
    ::= 'A' <identifier> <genericArgs>?

<traitType>
    ::= 'T' <identifier> <genericArgs>?

<functionType>
    ::= 'F' <type>* 'R' <type>
```
