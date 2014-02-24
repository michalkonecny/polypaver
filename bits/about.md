---
title: test
author: Michal Konečný
date: 2014-02-24
mathjax: on
---

Some statements that PolyPaver proves automatically in a few seconds or minutes:

$$
\left(
\begin{array}{l}
a \in [-10, 10]\\
b \in [-10, 10]\\
b > a + 0.1
\end{array}
\right)
\implies
e^{a} - e^{b} > (b - a) e^{\frac{a+b}{2}}
$$

$$
\left(
\begin{array}{l}
x \in [0, 1]\\
y \in [0, 1]\\
\end{array}
\right)
\implies
e^{x+y}
 - e^{x}\in
    [-0.01, 0.01] +
    \int_x^{x + y} e^t\,\mathrm{d}t
$$ 

$$
\left(
\begin{array}{l}
x \in [0.5, 2]\\
r \in [0, 3]\\
r \in [-x^2/4+x, x^2/4+1]\\
r = (0.5 \otimes (r \oplus x \oslash r))\\
\end{array}
\right)
\implies
0.5 \otimes (r \oplus x \oslash r) \in (1+4\varepsilon)\sqrt{x}
$$

where $\otimes, \oplus, \oslash$ are double-precision floating-point operations
and $\varepsilon$ is the corresponding floating-point epsilon. 

#### Under the hood:

  * Multi-variate correctly rounded polynomial interval arithmetic for automatically reducing dependency effects
  * Uses (a version of) the [AERN library](https://code.google.com/p/aern/)
  
#### Origin:

  * PolyPaver was first developed by Jan Duracz and Michal Konečný during Jan's PhD study under Michal's supervision.
  * Sponsored by EPSRC and Altran Praxis 



