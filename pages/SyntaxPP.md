---
title: PolyPaver | Documentation | .pp Language
author: Michal Konečný
date: 2015-06-16
mathjax: on
---

**Warning**  *This file currently documents a subset of the language, some newer features are not yet included.*

Example
=======

```
power.
H1: x <- [1 .. 10].
H2: n <- [1 .. 10].
H3: integer(n).
->
C1: x^2 <= x^(n+1) + 0.000000001.
```


Grammar
=======

Top level
---------

```
<conjecture>       ::= <identifier> "." [ { <hypothesis> } "->" ] <conclusion> { <conclusion> }
<hypothesis>       ::= "H" { <id> } ":" <predicate> "."
<conclusion>       ::= "C" { <id> } ":" <predicate> "."
<predicate>        ::= <type_declaration> | <expression> <relation_symbol> <expression> | ...
<type_declaration> ::= "integer(" <identifier> ")"
```


Relations
---------

**Relation**                  |**Explanation** 
----------------------------- |------------------------------------------------
`<-`                          |element of / within interval / sub-interval of
`<=`, `>=`, `==`, `=`, `!=`   |`=` and `==` both denote equality

Expressions
-----------

  **Constant**   **Explanation**
  -------------- ---------------------------------------------------------
  `Pi`           
  `FepsAbs`      2<sup>-126</sup> (IEEE Single FP least non-zero number)
  `FepsiAbs`     The interval `[` -2<sup>-126</sup>..2<sup>-126</sup>`]`
  `FepsRel`      2<sup>-24</sup> (IEEE Single FP machine epsilon)
  `FepsiRel`     The interval `[` -2<sup>-24</sup>..2<sup>-24</sup>`]`

  **Exact operator**   **IEEE-Single-rounded operator**
  -------------------- ---------------------------------------
  `+`                  `(+)`
  `-`                  `(-)`
  `*`                  `(*)`
  `/`                  `(/)`
  `^`                  *rounded integer power not available*

  **Function**          **Explanation**
  --------------------- --------------------------------------------------------
  `Interval(x,y)`       the least interval covering `x` and `y`
  `Sqrt(x)`             exact square root of `x`
  `Exp(x)`              exact natural exponential (e<sup><code>x</code></sup>)
  `Sin(x)`              exact sine
  `Cos(x)`              exact cosine
  `Integral(a,b,f,x)`   exact integral from `a` to `b` of `f(x)dx`

Please note that in the above, `x` and `y` can be arbitrary expressions.
