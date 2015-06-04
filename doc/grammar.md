---
title: Grammar
---


<card> ::= <ws> "card" <ws> "{" <cardContent> "}" <ws>
<cardContent> ::= <declaration> { <delim> <declaration> }
<declaration> ::= <deployment>

<deployment> ::= <spaces> <filepath> <spaces> "->" <spaces> <filepath> <spaces>

<spaces> ::= { " " | "\t" }
<ws> ::= { <wsc> }
<wsc> ::= " " | "\t" | "\r" | "\n"
<delim> ::= ";" | <eol>
<eol> ::= | "\n\r" | "\r\n" | "\n" | "\r"
