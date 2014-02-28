---
title: Verifying a SPARK FP program, Part 1: Annotations (under construction)
author: Michal Konečný
date: 2014-02-28
mathjax: on
---

This tutorial shows the steps of writing a floating-point SPARK 2005 program
and proving it computes numbers that correspond to its intuitive meaning 
as well as proving the absence of numerical exceptions.  The complete code
for this program is available [here](https://github.com/michalkonecny/polypaver/tree/master/tools/SPARK2005/examples/erfRiemann).

The tutorial is divided into the following parts:

  1. **Annotations: Annotating a SPARK floating-point program** *(this page, under construction)* 
  2. Generation: Generating valid VCs *(coming soon)*
  3. Proving easier VCs: Proving most of the VCs and identifying challenging VCs *(coming soon)*
  4. Proving harder VCs: Attempting to prove the harder VCs *(coming soon)*
  5. Debugging: Detecting and locating mistakes in SPARK floating-point programs *(coming soon)* 

---

### The Ada program

The program computes an approximation of the integral

$$
\int_0^x e^{-t^2}\;\mathrm{d}t
$$

using the left Riemann sum over a partition of equal size.

This integral is an essential part of the [Gauss error function](http://en.wikipedia.org/wiki/Error_function) and it has no closed algebraic solution.

The Ada specification and body are as follows:

```ada
package Riemann is 

   function erf_Riemann(x : Float; n : Integer) return Float;

end Riemann;
```

The parameter $n$ determines the number of segments to be used in the partition.  The partition has $2^n$ segments.

```ada
with PP_F_Elementary;

package body Riemann is

   function erf_Riemann(x : Float; n : Integer) return Float
   is
      partitionSize : Integer;
      stepSize : Float;
      stepStart : Float;
      valueStart : Float;
      result : Float;
      step : Integer;
   begin
      partitionSize := 2 ** n;
      stepSize := x/Float(partitionSize);
      result := 0.0;
      for step in 0..((2^n)-1) loop
         stepStart := stepSize * Float(step);
         valueStart :=
           PP_F_Elementary.Exp(-(stepStart * stepStart));
         result := 
           result + stepSize * valueStart;
         step := step + 1;
      end loop;
      return result;
   end erf_Riemann;

end Riemann;
```

The package `PP_F_Elementary` is a PolyPaver-friendly alternative
to `Ada.Numeric.Elementary_Functions`.  
This package is included in the PolyPaver download bundle.  

### Precondition and postcondition

*bounds for variables*

*bounds on model error*

*bounds on rounding error* 

### The loop invariant

*incremental version of the postcondition*

*bounding local variables*