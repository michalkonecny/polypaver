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

This integral is an essential part of the [Gauss error function](http://en.wikipedia.org/wiki/Error_function) and it has no closed algebraic solution.

The integral is approximated using the left Riemann sum over a partition with $2^n$ segments of equal size.  

The following is an Ada implementation of this method.

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

#### Bounding the method error

Let us defer reasoning about rounding errors for later and first consider the
algorithm as if it was performed in exact real arithmetic and analyse its method error.

How does the Riemann sum relate to the integral?  We first restrict the domain of $x$ to non-negative numbers.
Thus the integrand $e^{-t^2}$ is considered only for non-negative $t$, where it is a decreasing function.
The left Riemann sum is therefore an *upper bound* on the exact integral.  

Moreover, the difference between the left Riemann sum and the integral can be bounded by the difference
between the left and rigth Riemann sums, which happens to be exactly 

$$
\frac{x}{n}\left(1-e^{-x^2}\right)
$$

**TODO**: *Add diagram illustrating the sums*

**TODO**: *Add SPARK specification with a postcondition expressing the method error*

#### Bounding the rounding error

#### Bounding the domain

### The loop invariant

*incremental version of the postcondition*

*bounding local variables*