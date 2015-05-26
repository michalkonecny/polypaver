/* 
    A small example theorem involving an integral.
    
    Proved, for example, with: -d 3
    The false version is disproved, for example, with: -d 2 -o bfs
*/ 
integral.
H1: x <- [0 .. 1].
H2: y <- [0 .. 1].
->
C1: 
/*    x/80 + // use this line to make the formula false */
    Exp(x+y) - Exp(x)
    |<-|
    [-0.01 .. 0.01] + /* comment this line to make the theorem touching */
    Integral(x, (x + y), Exp(t), t).
    
