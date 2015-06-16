---
title: PolyPaver | Features
author: Michal Konečný
date: 2014-02-24
mathjax: on
---

*(See also [Changelog](Changelog.html))*

### The master branch

* Deciding conjectures with expressions that contain
    * The operations +,-,*,/, abs, sqrt, exp <!--, sin, cos -->
    * Explicit **intervals** and **interval operations**
    * Definite **integrals** (with arbitrary expressions for endpoints)
    * Floating-point **rounded operations** (with adjustable precision)
* Supported input formats:
    * pp: based on fld language used in SPARK 2005 verification conditions
    * a modified tptp: only fof formulas, extended with common infix relations (eg <=, >=) and operations (eg +, *, /)
         * similar to the modified tptp supported by [MetiTarski](http://www.cl.cam.ac.uk/~lp15/papers/Arith/)
* Verification of **floating-point** [SPARK/Ada 2005](http://en.wikipedia.org/wiki/SPARK_(programming_language)) code
    * Full verification of floating-point code in cooperation with the official SPARK tools
         * Functional correctness
         * Freedom from numerical exceptions
    * Annotations can specify 
         * a bound on rounding errors, as a function of input values
         * a relation of floating-point results with intuitive mathematical meaning  
    * Specifications can contain interval operations and integrals
    * Effective for procedures and functions with up to 4-6 floating-point variables
       *(arrays are not supported yet)*
* Live graphical progress update for 2D problems

### Planned

  * Support for other elementary real functions
  * Support for calling from [Why3](http://why3.lri.fr/)
    * This will facilitate integration with [SPARK 2014](http://www.spark-2014.org/)
  * Integration with symbolic theorem provers, SMT
  * Increase trust in PolyPaver results
    * Verify the underlying function arithmetic
        * Fully specify this arithmetic *(almost done)*
        * Exhaustively property-test the arithmetic against its specification *(almost done)*
        * Verify the arithmetic against the specification
    * Generate independently verifiable proofs




