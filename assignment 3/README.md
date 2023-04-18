## Design Decisions
- BigInt is implemented using string
- In make_rat(a,b), rational number is stored in lowest form
- rational is stored as a tuple (bigint, bigint)
- for a positive number, no need to add a "+" . I have not implemented use of "+" anywhere( except for add in calculator)
- in the representation of a rational, only the numerator can be negative
- division by zero raises error 
- my calculator implementation implements simple calculation( doesnt include a symbol table)
- always end the line with ()
- I represent ~1/3 as ~.(3) and not ~0.(3)

## Grammar
1. For rational numbers
R -> SI | SF | SD 
S -> + | ~ | ε
I -> aI | a 
F -> I/bA
D -> A.A(I )
A -> ε | I 
a -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
b -> 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

2. For rational number expressions
E -> E + R | E - R | R 
R -> R * T | R / T | T 
T -> (E) | X
X -> V | S 
V -> [a-zA-Z]^+^[0-9]^*^[a-zA-Z]^*^    variable name 
S -> rational

## Acknowledgements
https://www.smlnj.org/doc/ML-Yacc/mlyacc007.html
https://rogerprice.org/ML-Lex-Yacc-Guide/ML-Lex-Yacc-Guide.pdf
http://www.smlnj.org/doc/ML-Lex/manual.html