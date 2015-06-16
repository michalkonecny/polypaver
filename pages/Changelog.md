---
title: PolyPaver | Documentation | Changelog
author: Michal Konečný
date: 2015-06-16
mathjax: on
---

### **Recent**

-   Added support for loading conjectures in a modified version of the TPTP formula language.
-   Improved pruning of irrelevant hypotheses.
    - Eg ignoring the hypothesis $x + y \leq a$ if the variable $a$ does not appear anywhere else in the formula.
-   Removed support for loading problems specified as Haskell modules. 
-   Moved to ghc 7.8.4.\

### **PolyPaver 0.2** (2013-05-23)

[Linux 32-bit](http://code.google.com/p/polypaver/downloads/detail?name=polypaver-0.2-linux-i386-glibc215), 
[Windows 32-bit](http://code.google.com/p/polypaver/downloads/detail?name=polypaver-0.2-win7-32bit.exe), 
[Haskell source](http://code.google.com/p/polypaver/downloads/detail?name=polypaver-0.2.tar.gz)

-   Problems can be loaded from .vc files with human friendly
    syntax.
-   Optionally, problems can be loaded from Haskell modules (only if
    ghc is installed).
-   Support for the integral operator.
-   New parameter "maximum queue length". Its default value depends
    on the selected search mode.
-   Variables can be declared to range over integers only.
-   By default, PolyPaver now uses DFS and if this fails to decide,
    PolyPaver automatically runs BFS to search for counter-example
    near where DFS stopped.
-   Proving each conclusion of a Spark VC separately.
-   Sorting hypotheses in Spark VCs from smallest to largest.
-   Moved from ghc 6.12.3 to ghc 7.4.2.
-   Tested on Mac OSX and Windows in addition to Linux 32bit.

### **PolyPaver 0.1** (2011-09-07): 
[Linux executable](http://code.google.com/p/polypaver/downloads/detail?name=polypaver-0.1), 
platform-independent [source code](http://code.google.com/p/polypaver/downloads/detail?name=polypaver-0.1.tar.gz)

-   expressions with explicit intervals and interval operations
    -   field operations, abs, sqrt, exp, sin, cos
    -   floating point constants and rounded operations (with adjustable precision)
-   solving VCs from [SPARK 2005](https://en.wikipedia.org/wiki/SPARK_%28programming_language%29) siv files
-   live plotting of pavings for 2D problems (showing which areas of
    the domain are already proved true and/or showing the location
    of a counter example)

