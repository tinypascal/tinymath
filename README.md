# tinymath
All of this code (including bugs) is public domain.  
  
A simple interpreter study using a...
- lexer
- parser
- interpreter (rpn stack machine)

Its primary purpose is to provide a learning platform to build on, contains debugging information for each stage.  
Sample debugging output for the input '4 + 2 * (5 - 2)':
  
```
Result: *** Lexer ***
1 Number: 4
3 Operator: +
5 Number: 2
7 Operator: *
9 Single: (
10 Number: 5
12 Operator: -
14 Number: 2
15 Single: )

*** Parser ***

Program accepted
Number(4) Number(2) Number(5) Number(2) - * +

*** Interpreter ***
Workstack: Number(4)
Workstack: Number(2) Number(4)
Workstack: Number(5) Number(2) Number(4)
Workstack: Number(2) Number(5) Number(2) Number(4)
Workstack: Number(3) Number(2) Number(4)
Workstack: Number(6) Number(4)
Workstack: Number(10)

10.00000000
```
