### RPN transforms


When I was a kid I taught Reverse Polish Notation to myself ( http://www-stone.ch.cam.ac.uk/documentation/rrf/rpn.html ) and used a RPN calculator in high school. 

Today's challenge is to write a program which takes expressions in infix notation, such that 

```
( (2+1) * 8 )^2
```

and rewrite then in RPN. Such a rewrite could be

```
2 1 + 8 * 2 ^
```

To make things more precise, the tokens are integers, the following arithmetic operators: `+`, `-`,`*` ,` / `, `^` (addition, substraction, division and exponentiation), and parenthesis `(` and `)`.

Your program should tolerate spaces for instance `(2   +1)`, and should tolerate missing parentheses with repeated additions and multiplications, for instance `(1+2+4)` for `((1+2)+4)` or `(1*2*4)` for `((1*2)*4)`. In all other cases input expressions will be properly parenthesed. 

