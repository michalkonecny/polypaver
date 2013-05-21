# Verifying numerical SPARK Ada programs using PolyPaver 

PolyPaver provides facilities for automatically proving SPARK generated verification conditions (VCs) 
that make up correctness theorems for SPARK Ada programs. 
The verification approach relies on the SPAR toolset for VC generation and simplification. 
The resulting correctness theorems generally comprise the set of numerically nontrivial 
sub-theorems encoding exception freedom and (optionally) accuracy properties. 
Below follows a brief introduction to the method by which VCs may be derived that PolyPaver can process 
and a procedure for working with the options provided by PolyPaver to guide and control the proof effort. 

## Overview of the example programs

### peak

*Main procedure*: PeakUnit  
*Input*: real values Y1, Y2, Y3  
*Output*: the peak value of the *quadratic* interpolation of (-1,Y1), (0,Y2), (1,Y3)

### sqrt

*Main procedure*: Sqrt  
*Input*: real value X  
*Output*: the square root of X

### erfriemann

*Main procedure*: erfRiemann  
*Input*: real value X and integer n  
*Output*: a Riemann sum over n segments, approximating the value of the Gaussian error function for X

## How to write SPARK programs that can be verified using PolyPaver

TODO: 
* List replacements for Ada FP operations and functions.  
* State that in annotations, numerical operations retain their exact meaning.
* List annotation functions understood by polypaver (such as such as polypaver.exact.sqrt). 

To generate correctness theorems for a program, 
PolyPaver uses the SPARK Examiner VC generator to produce a set of VCs. 
As the currently available version of the Examiner treats flaoting point 
operations as exact real arithmetic, it is necessary to short-circuit the VC generation process. 
To thi end, PolyPaver provides a SPARK Ada package with an interface to the Ada numerics packages 
and 

In addition to the above operators, these packages also
provide a number of abstract operations (so-called proof functions) 
that may be used in annotations
to encode accuracy properties of SPARK floating-point programs. 
The SPARK tools also treat numeric operators in annotations 
as exact real operators and therefore abstract versions of the standard 
numeric operations are not needed, if this is what is intended. 
PolyPaver does however provide a number of functions with intended
exact real semantics, which extend the expressiveness of the SPARK annotation language.
Among such operators are the interval constructor, the integral operator for 
continuous functions defined by algebraic expressions 
and the interval containment relation.
The interval operators make it more convenient to express accuracy constraints.
The integral operator facilitates verifying specifications
with special functions that have an integral form.

## Generation of verification conditions (VCs):

In the program main folder, execute:

    > spark @peak
    > sparksimp

The first command above produces VCs in `.vcg` files.  
The second command applies symbolic reasoning to simplify the VCs and saves them in `.siv` files.  
One file is generated for each procedure or function to be verified.
For example, the VCs in the following files together form the correctness theorem 
for the peak program:  

    out/peak/max.siv
    out/peak/coeffs.siv
    out/peak/peakunit.siv
    out/peak/peakq.siv

In these four files, there are altogether 63 VC conclusions that result in 63 problems to try to prove.

The following table summarises the numbers of polypaver problems generated for the example programs:

<table border="1">
<tr>
<th>Program</th>
<th>VCs</th>
<th>Problems</th>
</tr>
<tr>
<td>peak</td>
<td>31</td>
<td>63</td>
</tr>
<tr>
<td>sqrt</td>
<td>13</td>
<td>21</td>
</tr>
<tr>
<td>erfriemann</td>
<td>19</td>
<td>31</td>
</tr>
</table>

## Proving the easy conclusions

The majority of the problems are usually trivial.  To categorise the problems, first run

    > polypaver peak/out/peak/max.siv -t 1 -q

and analogously for all the other `.siv` files.  
The above command applies PolyPaver for 1s on default settings to each VC conclusion in the file.

PolyPaver will output a summary in the end where we can find out
which VC conclusions have been proved and which not.

### Identifying easy conclusions

With the above statement, PolyPaver typically proves 
42 out of the 63 problems in the peak program.

We list below the 21 VC that have not been proved by the above
statement, together with other information provided by PolyPaver:

    coeffs_10 conclusion 1: GAVE UP: REACHED MAXIMUM QUEUE SIZE after 6.4004e-2 s (0d, 0h, 0min, 0s) (proved fraction: -0.0)
    coeffs_10 conclusion 2: GAVE UP: REACHED MAXIMUM QUEUE SIZE after 6.0004e-2 s (0d, 0h, 0min, 0s) (proved fraction: -0.0)
    coeffs_10 conclusion 3: GAVE UP: REACHED MAXIMUM QUEUE SIZE after 6.0003e-2 s (0d, 0h, 0min, 0s) (proved fraction: -0.0)
    coeffs_10 conclusion 4: GAVE UP: REACHED MAXIMUM QUEUE SIZE after 6.0004e-2 s (0d, 0h, 0min, 0s) (proved fraction: -0.0)

    peakq_6 conclusion 1: GAVE UP: TIMED OUT after 1.0000630000000001 s (0d, 0h, 0min, 1s) (proved fraction: 0.10546874999999502)
    peakq_6 conclusion 2: GAVE UP: TIMED OUT after 1.000062 s (0d, 0h, 0min, 1s) (proved fraction: 0.10876464843749406)
    peakq_6 conclusion 3: GAVE UP: TIMED OUT after 1.0000630000000001 s (0d, 0h, 0min, 1s) (proved fraction: 0.10888671874999392)
    peakq_7 conclusion 1: GAVE UP: TIMED OUT after 1.0000630000000001 s (0d, 0h, 0min, 1s) (proved fraction: 1.6708374023437313e-2)
    peakq_7 conclusion 2: GAVE UP: TIMED OUT after 1.0000630000000001 s (0d, 0h, 0min, 1s) (proved fraction: 1.6708374023437313e-2)
    peakq_8 conclusion 1: GAVE UP: TIMED OUT after 1.000062 s (0d, 0h, 0min, 1s) (proved fraction: 7.069110870361201e-5)
    peakq_8 conclusion 2: GAVE UP: TIMED OUT after 1.0000630000000001 s (0d, 0h, 0min, 1s) (proved fraction: 1.667630672454823e-2)

    peakunit_11 conclusion 1: GAVE UP: TIMED OUT after 1.0000630000000001 s (0d, 0h, 0min, 1s) (proved fraction: 1.4705919020343043e-3)
    peakunit_11 conclusion 2: GAVE UP: TIMED OUT after 1.0000630000000001 s (0d, 0h, 0min, 1s) (proved fraction: 1.1273574054939286e-3)
    peakunit_11 conclusion 3: GAVE UP: TIMED OUT after 1.0000630000000001 s (0d, 0h, 0min, 1s) (proved fraction: 1.4707697555422319e-3)
    peakunit_12 conclusion 1: GAVE UP: TIMED OUT after 1.000062 s (0d, 0h, 0min, 1s) (proved fraction: 0.1913355886936148)
    peakunit_12 conclusion 2: GAVE UP: TIMED OUT after 1.000062 s (0d, 0h, 0min, 1s) (proved fraction: 0.1912115626037071)
    peakunit_12 conclusion 3: GAVE UP: TIMED OUT after 1.000062 s (0d, 0h, 0min, 1s) (proved fraction: 0.20239257812499625)

Those problems that were not decided due to reaching a maximum require increasing
some parameters other than time.
The success rate is improved by switching from the default degree 0 enclosures
to affine enclosures:

    > polypaver out/peak/max.siv -t 10 -d 1 -q

and its analogues for the other files result in having only the following 8 out of 63 problems left to decide:

    peakq_8 conclusion 1: GAVE UP: TIMED OUT after 10.000625000000001 s (0d, 0h, 0min, 10s) (proved fraction: 1.647949218749915e-3)
    peakq_8 conclusion 2: GAVE UP: TIMED OUT after 10.004626 s (0d, 0h, 0min, 10s) (proved fraction: 0.14423370361327945)

    peakunit_11 conclusion 1: GAVE UP: TIMED OUT after 10.052628 s (0d, 0h, 0min, 10s) (proved fraction: 1.1472702026367049e-3)
    peakunit_11 conclusion 2: GAVE UP: TIMED OUT after 10.008626000000001 s (0d, 0h, 0min, 10s) (proved fraction: 1.119848340749728e-3)
    peakunit_11 conclusion 3: GAVE UP: TIMED OUT after 10.056627 s (0d, 0h, 0min, 10s) (proved fraction: 1.4683846384286679e-3)
    peakunit_12 conclusion 1: GAVE UP: TIMED OUT after 10.000626 s (0d, 0h, 0min, 10s) (proved fraction: 0.24522590637204436)
    peakunit_12 conclusion 2: GAVE UP: TIMED OUT after 10.000625000000001 s (0d, 0h, 0min, 10s) (proved fraction: 0.24930596351619966)
    peakunit_12 conclusion 3: GAVE UP: TIMED OUT after 10.000625000000001 s (0d, 0h, 0min, 10s) (proved fraction: 0.3779017329215653)


For those problems where PolyPaver timed out, the output shows how far it got at proving it.
Typically, if the fraction is above 1 percent, it is possible to decide the problem in reasonably time 
using the same settings.  For example, running

    > polypaver out/peak/peakunit.siv peakunit_12

results in:

    >>>>>>>>>>> SUMMARY <<<<<<<<<<<
    peakunit_12 conclusion 1: PROVED in 32.142009 s (0d, 0h, 0min, 32s)
    peakunit_12 conclusion 2: PROVED in 17.073067 s (0d, 0h, 0min, 17s)
    peakunit_12 conclusion 3: PROVED in 49.231076 s (0d, 0h, 0min, 49s)

Similarly, peakq_8 conclusion 2 is proved in 87s using the default setting, which leaves only 4 conclusions unproved.

### Summary of easy conlusions

<table border="1">
<tr>
<th>Program</th>
<th>Problems</th>
<th>Easy problems (ie proved with -d 0 -t 120 -f or -d 1 -t 120 -f)</th>
</tr>
<tr>
<td>peak</td>
<td>63</td>
<td>59</td>
</tr>
<tr>
<td>sqrt</td>
<td>21</td>
<td>20</td>
</tr>
<tr>
<td>erfriemann</td>
<td>31</td>
<td>28</td>
</tr>
</table>


## Proving the hard problems

### peak

peakq_8 conclusion 1

* using switches -d 1

    * proved in < 23min, using <230000 boxes

peakunit_11 conclusions 1,2,3

* These are statements of a similar nature as `examples/pp/skewing.pp` but with 8 variables.

* PolyPaver has not managed to prove any of them within a timeout of 3 days.

* An explanation why PolyPaver is not suited for such problems at present and how we plan to address this limitation in future is in file `examples/pp/skewing.pp`.


### sqrt

sqrt_13 conclusion 1

* using switches -d 7 -z 5 -e 10

* proved in <10min, using <20000 boxes


### erfriemann

erfriemann_10 conclusion 1

* -d 0 -I 4 -f

* proved in <31h

erfriemann_10 conclusion 2

* -d 0 -I 4 -f

* proved in <7min

erfriemann_19 conclusion 1

* -d 0 -I 4 -f

* proved in <80min






