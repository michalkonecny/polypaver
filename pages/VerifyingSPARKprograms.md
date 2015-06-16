---
title: PolyPaver | Documentation | Verifying SPARK 2005 Programs
author: Michal Konečný
date: 2015-06-16
mathjax: on
---

**Table of contents**

PolyPaver provides facilities for automatically verifying
[SPARK Ada](http://www.adacore.com/sparkpro/language-toolsuite/)
programs with floating-point arithmetic.
The cabal package includes three SPARK Ada
numerical programs that can be verified using PolyPaver
(except one which is verified only partially, to illustrate some
of the limits of the current version).

This page documents a method to approach verifying
the provided programs and also gives some guidance
for the process of implementing and verifying
numerical SPARK Ada programs.

SPARK Ada and PolyPaver
=======================

[SPARK Ada](http://www.adacore.com/sparkpro/language-toolsuite/)
is a programming language and associated tools for efficiently
developing extremely reliable software systems.
SPARK 2014 differs substantially from previous versions of SPARK.
This page is about the older versions of SPARK, usually referred to as SPARK 2005.

The SPARK 2005 programming language is a subset of Ada with
annotations that allow the programmer to formally express a
specification of the program using first-order logic.
Such annotations can be used, for example, to encode
information about the intended mathematical interpretation
of the program and to give bounds on how the program deviates from it.

The SPARK 2005 Examiner tool checks some easier aspects of the specification
and produces a correctness theorem in the form of a collection of
verification conditions (VCs).
Such a correctness theorem implies that the program adheres to its
specification, which, in SPARK, includes exception freedom by default.
To complete the verification, it is necessary to prove all the VCs.
If the programs are of a certain kind and adhere to certain
restrictions (specified later), the VCs are generally numerical theorems that
PolyPaver can read and try to prove.

Section [Proving the generated problems](#Proving_the_generated_problems)
below demonstrates a simple strategy how to work with some of the
PolyPaver switches to guide and control the proof effort
when working with SPARK-generated VC collections.

Overview of included example SPARK Ada programs
===============================================

sqrt
----

*Code*:
[examples/SPARK2005/sqrt/src](https://github.com/michalkonecny/polypaver/tree/master/examples/SPARK2005/sqrt/src)

*Main procedure*: `Example.Sqrt`

*Input*: real value `X`

*Intended output*: the square root of `X`

erfriemann
----------

*Code*:
[examples/SPARK2005/erfriemann/src](https://github.com/michalkonecny/polypaver/tree/master/examples/SPARK2005/erfriemann/src)

*Main procedure*: `erfRiemann`

*Input*: real value `X` and integer `n`

*Intended output*: a Riemann sum over `n` segments, approximating the
value of (a scaled version of) the Gaussian error function for `X`

peak
----

*Code*:
[examples/SPARK2005/peak/src](https://github.com/michalkonecny/polypaver/tree/master/examples/SPARK2005/peak/src)

*Main procedure*: `PeakUnit`

*Input*: real values `Y1`, `Y2`, `Y3`

*Intended output*: the peak value of the *quadratic* interpolation of
`(-1,Y1), (0,Y2), (1,Y3)`

Generation of verification conditions (VCs)
===========================================

In the program main folder, execute:

``sh

>spark @peak
>sparksimp
```

The first command above produces VCs in `.vcg` files.
The second command applies symbolic reasoning to simplify the VCs and
saves them in `.siv` files.
One file is generated for each procedure or function to be verified.
For example, the VCs in the following files together form the
correctness theorem
for the peak program:

```sh

out/peak/max.siv
out/peak/coeffs.siv
out/peak/peakunit.siv
out/peak/peakq.siv```

In these four files, there are altogether 63 VC conclusions that result
in 63 problems to try to prove.

The following table summarises the numbers of polypaver problems
generated for the example programs:

  **Program**   **VCs**   **Problems**
  ------------- --------- --------------
  sqrt          19        31
  erfriemann    19        31
  peak          31        63

Proving the generated problems
==============================

The majority of the problems are usually trivial. To categorise the
problems, first run

```sh
>polypaver peak/out/peak/max.siv -t 1 -q
```

and analogously for all the other `.siv` files.
The above command applies PolyPaver for 1s on default settings to each
VC conclusion in the file.

PolyPaver will output a summary in the end where we can find out
which VC conclusions have been proved and which not.

Proving the easy problems
-------------------------

With the above statement, PolyPaver typically proves
42 out of the 63 problems in the peak program.

We list below the 21 conclusions that have not been proved by the above
statement, together with other information provided by PolyPaver:

```xml
coeffs\_10 conclusion 1: GAVE UP: REACHED MAXIMUM QUEUE SIZE after
6.4004e-2 s (0d, 0h, 0min, 0s) (proved fraction: -0.0)
coeffs\_10 conclusion 2: GAVE UP: REACHED MAXIMUM QUEUE SIZE after
6.0004e-2 s (0d, 0h, 0min, 0s) (proved fraction: -0.0)
coeffs\_10 conclusion 3: GAVE UP: REACHED MAXIMUM QUEUE SIZE after
6.0003e-2 s (0d, 0h, 0min, 0s) (proved fraction: -0.0)
coeffs\_10 conclusion 4: GAVE UP: REACHED MAXIMUM QUEUE SIZE after
6.0004e-2 s (0d, 0h, 0min, 0s) (proved fraction: -0.0)
```

```xml
peakq\_6 conclusion 1: GAVE UP: TIMED OUT after 1.0000630000000001 s
(0d, 0h, 0min, 1s) (proved fraction: 0.10546874999999502)
peakq\_6 conclusion 2: GAVE UP: TIMED OUT after 1.000062 s (0d, 0h,
0min, 1s) (proved fraction: 0.10876464843749406)
peakq\_6 conclusion 3: GAVE UP: TIMED OUT after 1.0000630000000001 s
(0d, 0h, 0min, 1s) (proved fraction: 0.10888671874999392)
peakq\_7 conclusion 1: GAVE UP: TIMED OUT after 1.0000630000000001 s
(0d, 0h, 0min, 1s) (proved fraction: 1.6708374023437313e-2)
peakq\_7 conclusion 2: GAVE UP: TIMED OUT after 1.0000630000000001 s
(0d, 0h, 0min, 1s) (proved fraction: 1.6708374023437313e-2)
peakq\_8 conclusion 1: GAVE UP: TIMED OUT after 1.000062 s (0d, 0h,
0min, 1s) (proved fraction: 7.069110870361201e-5)
peakq\_8 conclusion 2: GAVE UP: TIMED OUT after 1.0000630000000001 s
(0d, 0h, 0min, 1s) (proved fraction: 1.667630672454823e-2)
```

```xml
peakunit\_11 conclusion 1: GAVE UP: TIMED OUT after 1.0000630000000001 s
(0d, 0h, 0min, 1s) (proved fraction: 1.4705919020343043e-3)
peakunit\_11 conclusion 2: GAVE UP: TIMED OUT after 1.0000630000000001 s
(0d, 0h, 0min, 1s) (proved fraction: 1.1273574054939286e-3)
peakunit\_11 conclusion 3: GAVE UP: TIMED OUT after 1.0000630000000001 s
(0d, 0h, 0min, 1s) (proved fraction: 1.4707697555422319e-3)
peakunit\_12 conclusion 1: GAVE UP: TIMED OUT after 1.000062 s (0d, 0h,
0min, 1s) (proved fraction: 0.1913355886936148)
peakunit\_12 conclusion 2: GAVE UP: TIMED OUT after 1.000062 s (0d, 0h,
0min, 1s) (proved fraction: 0.1912115626037071)
peakunit\_12 conclusion 3: GAVE UP: TIMED OUT after 1.000062 s (0d, 0h,
0min, 1s) (proved fraction: 0.20239257812499625)
```

Those problems that were not decided due to reaching a maximum require
increasing
some parameters other than time.
The success rate is improved by switching from the default degree 0
enclosures
to affine enclosures:

```sh
>polypaver out/peak/max.siv -t 10 -d 1 -q
```

and its analogues for the other files result in having only the
following 8 out of 63 problems left to decide:

```xml
peakq\_8 conclusion 1: GAVE UP: TIMED OUT after 10.000625000000001 s
(0d, 0h, 0min, 10s) (proved fraction: 1.647949218749915e-3)
peakq\_8 conclusion 2: GAVE UP: TIMED OUT after 10.004626 s (0d, 0h,
0min, 10s) (proved fraction: 0.14423370361327945)
```

```xml
peakunit\_11 conclusion 1: GAVE UP: TIMED OUT after 10.052628 s (0d, 0h,
0min, 10s) (proved fraction: 1.1472702026367049e-3)
peakunit\_11 conclusion 2: GAVE UP: TIMED OUT after 10.008626000000001 s
(0d, 0h, 0min, 10s) (proved fraction: 1.119848340749728e-3)
peakunit\_11 conclusion 3: GAVE UP: TIMED OUT after 10.056627 s (0d, 0h,
0min, 10s) (proved fraction: 1.4683846384286679e-3)
peakunit\_12 conclusion 1: GAVE UP: TIMED OUT after 10.000626 s (0d, 0h,
0min, 10s) (proved fraction: 0.24522590637204436)
peakunit\_12 conclusion 2: GAVE UP: TIMED OUT after 10.000625000000001 s
(0d, 0h, 0min, 10s) (proved fraction: 0.24930596351619966)
peakunit\_12 conclusion 3: GAVE UP: TIMED OUT after 10.000625000000001 s
(0d, 0h, 0min, 10s) (proved fraction: 0.3779017329215653)
```

For those problems where PolyPaver timed out, the output shows how far
it got at proving it.
Typically, if the fraction is above 1 percent, it is possible to decide
the problem in reasonably time
using the same settings. For example, running

```sh
>polypaver out/peak/peakunit.siv peakunit\_12
```

results in:

```
>>>>>>>>>>> SUMMARY <<<<<<<<<<<
>>>>>>>>>>> peakunit_12 conclusion 1: PROVED in 32.142009 s
>>>>>>>>>>> (0d, 0h, 0min, 32s)
>>>>>>>>>>> peakunit_12 conclusion 2: PROVED in 17.073067 s
>>>>>>>>>>> (0d, 0h, 0min, 17s)
>>>>>>>>>>> peakunit_12 conclusion 3: PROVED in 49.231076 s
>>>>>>>>>>> (0d, 0h, 0min, 49s)
```

Similarly, `peakq_8` conclusion 2 is proved in 87s using the default
setting, which leaves only 4 conclusions unproved.

### Summary of easy problems

  **Program**   **Problem**   **Easy problems (ie proved with `-d 0 -t 120 -f or -d 1 -t 120 -f`)**
  ------------- ------------- -----------------------------------------------------------------------
  sqrt          21            20
  erfriemann    31            28
  peak          63            59

Proving the hard problems
-------------------------

### sqrt

`sqrt_13` conclusion 1

-   using switches: `-d 7 -z 5 -e 10`

-   proved in \<10min, using \<20000 boxes

### erfriemann

`erfriemann_10` conclusion 1

-   using switches: `-d 0 -I 4 -f`

-   proved in \<31h

`erfriemann_10` conclusion 2

-   using switches: `-d 0 -I 4 -f`

-   proved in \<7min

`erfriemann_19` conclusion 1

-   using switches: `-d 0 -I 4 -f`

-   proved in \<80min

### peak

`peakq_8` conclusion 1

-   using switches: `-d 1`

-   proved in \< 23min, using \<230000 boxes

`peakunit_11` conclusions 1,2,3

-   These are statements of a similar nature as
    [examples/pp/skewing2.pp](https://github.com/michalkonecny/polypaver/blob/master/examples/pp/skewing2.pp)
    but with 8 variables.

-   PolyPaver has not managed to prove any of them within a timeout of 3
    days.

-   Such problems can be solved using PolyPaver after a substitution that strengthens the formula.
    A result of such substitution made manually is in
    [examples/pp/skewing.pp](https://github.com/michalkonecny/polypaver/blob/master/examples/pp/skewing.pp).
    The tool `pp_simplify` (included in the PolyPaver package) finds and makes such substitutions automatically. 


How to write SPARK programs that can be verified using PolyPaver
================================================================

PolyPaver is best suited for verifying procedures and functions
that have a small number of input and output floating-point parameters.
It does not yet support verifying programs with arrays.
Moreover, the types or preconditions for the variables should
specify ranges for these variables. To be able to verify
tight accuracy properties, these ranges should be
much smaller than the full range of the floating-point type.

Programming floating-point operations
-------------------------------------

*TODO*

<!--

At present, the SPARK tools treat floating-point operations in SPARK
code as
exact real operations. To prevent this, it is currently necessary to
refrain
from using operations such as `*`, +, / and - with floating-point
types.
Instead, one has to use the functions defined in the `PolyPaver.Floats`
and
`PolyPaver.Long_Floats` packages that are included in folder
[examples/SPARK2005/packages](https://github.com/michalkonecny/polypaver/tree/master/examples/SPARK2005/packages).
The standard floating-point operators should be replaced with the
following:

  **operator**   **IEEE Single precision**<br />(any rounding mode)   **IEEE Double precision** <br />(any rounding mode)
  -------------- ---------------------------------------------------- -----------------------------------------------------
  `+`            `PolyPaver.Float.Add`                                `PolyPaver.Long_Float.Add`
  `-`            `PolyPaver.Float.Subtract`                           `PolyPaver.Long_Float.Subtract`
  `*`            `PolyPaver.Float.Multiply`                           `PolyPaver.Long_Float.Multiply`
  `/`            `PolyPaver.Float.Divide`                             `PolyPaver.Long_Float.Divide`

In future, it is planned to provide a pre-processor for
SPARK Ada programs that will automatically replace all occurrences
of these operators for floating-point types with PolyPaver
functions such as above. At the same time, it is envisaged that
the precision (and other parameters) of the floating point type will be
automatically
inferred by the pre-processor so that other base floating-point types
than
the IEEE Single and Double will be supported.

In addition to the floating-point operators,
the packages also contain wrappers for the Ada library
floating-point square root, exponentiation, sine and cosine.
It is important to use these variants instead of the standard
Ada functions. Otherwise, these functions will not be recogised
by Polypaver and the whole expression that start with such a function
treated as a variable.

-->

Operations and functions in annotations
---------------------------------------

PolyPaver provides a number of abstract operations (so-called proof
functions)
that may be used in annotations to encode accuracy properties of SPARK
floating-point programs.

To express the exact real operations `*`, /, +, - in SPARK annotations,
simply use these operators directly as the SPARK tools treat these
operators as exact real operators.
No substitutes for these exact operators are therefore needed.

PolyPaver does however provide a number of functions with intended
exact real semantics, which extend the expressiveness of the SPARK
annotation language.
These functions are defined in packages
`PolyPaver.Interval` and `PolyPaver.Exact` in folder
[examples/SPARK2005/packages](https://github.com/michalkonecny/polypaver/tree/master/examples/SPARK2005/packages).
Among them are the interval constructor, the integral operator for
continuous functions defined by algebraic expressions
and the interval containment relation.
The interval operators make it more convenient to express accuracy
constraints.
The integral operator facilitates verifying specifications
with special functions that have an integral form.

Ada type information is mostly lost during VC generation.
PolyPaver assumes by default that all variables are real.
To let PolyPaver know that a variable `n` is an integer variable,
add the proposition `PolyPaver.Integers.Is_Integer(n)` to
the precondition or loop invariant.
