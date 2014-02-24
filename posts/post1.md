---
title: New PolyPaver pages
author: Michal Konečný
date: 2014-02-24
mathjax: on
---

PolyPaver has been slowly settling on GitHub.  Today, these pages have been added as a part of this process.
The pages are built using Hakyll and Bootstrap.

I plan to soon add also:
 
  * Tutorials
  * Reference help pages
  * Related publications


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