---
title: Verifying a SPARK FP program, Part 1: Annotations
author: Michal Konečný
date: 2014-02-28
mathjax: on
---

This tutorial shows the steps of writing a floating-point SPARK 2005 program
and proving it computes numbers that correspond to its intuitive meaning 
as well as proving the absence of numerical exceptions.

The tutorial is divided into the following parts:

  1. **Annotations: Annotating a SPARK floating-point program** *(this page)* 
  2. Generation: Generating valid VCs *(coming soon)*
  3. Proving easier VCs: Proving most of the VCs and identifying challenging VCs *(coming soon)*
  4. Proving harder VCs: Attempting to prove the harder VCs *(coming soon)*
  5. Debugging: Detecting and locating mistakes in SPARK floating-point programs *(coming soon)* 

---

### The Ada program

### The preconditions

### The postcondition

### The loop invariant

<!--
Some math:

$$
\begin{aligned}
\nabla \times \vec{\mathbf{B}} -\, \frac1c\, \frac{\partial\vec{\mathbf{E}}}{\partial t} & = \frac{4\pi}{c}\vec{\mathbf{j}} \\   \nabla \cdot \vec{\mathbf{E}} & = 4 \pi \rho \\
\nabla \times \vec{\mathbf{E}}\, +\, \frac1c\, \frac{\partial\vec{\mathbf{B}}}{\partial t} & = \vec{\mathbf{0}} \\
\nabla \cdot \vec{\mathbf{B}} & = 0 \end{aligned}
$$

Some Hakell code:

```haskell
newtype MonadMonoid m a = MonadMonoid { unMonad :: a -> m a }

instance Monad m => Monoid (MonadMonoid m a) where
  mempty = MonadMonoid return
  mappend f g = MonadMonoid (f >=> g)
```

Some SPARK code:

```ada
with PP_F_Elementary;
with PP_LF_Elementary;

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
      step := 0;
      while step < partitionSize loop -- for step = 0..((2^n)-1) loop
         stepStart := stepSize * Float(step);
         --# assert 
         --#     PP_F_Exact.Contained_In(
         --#         result
         --#         ,
         --#         PP_F_Exact.Integral
         --#            (0.0, stepStart,
         --#             PP_F_Exact.Exp(-PP_F_Exact.Integration_Variable**2)
         --#            )
         --#         +
         --#         PP_F_Exact.Interval(
         --#              - c*Float(step+1)
         --#              ,
         --#              (1.0-PP_F_Exact.Exp(-(x * Float(step)/Float(partitionSize))**2))*x/Float(partitionSize)
         --#              + c*Float(step+1)
         --#         )
         --#     )
         --#     and PP_F_Exact.Is_Range(result,-10, 100.0)
         --#     and PP_Integer.Is_Range(n, 1, 10) 
         --#     and PP_Integer.Is_Range(step, 0, partitionSize-1)
         --#     and partitionSize = 2 ** n
         --#     and stepStart = PP_F_Rounded.Multiply(6, stepSize, Float(step))
         --#     and stepSize = PP_F_Rounded.Divide(6, x, Float(partitionSize));

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

Fin.
-->