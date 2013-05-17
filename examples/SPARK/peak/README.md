# Example verified SPARK Ada program: Peak

## Goal of the main procedure PeakUnit

Input: real values Y1, Y2, Y3

Output: the peak value of the *quadratic* interpolation of (-1,Y1), (0,Y2), (1,Y3)

## Verification using SPARK tools and PolyPaver

### Generation of verification conditions (VCs):

    > spark @peak
    > sparksimp

The first command above produces VCs in `.vcg` files.  
The second command applies symbolic reasoning to simplify the VCs and saves them in `.siv` files.  
The VCs in the following files together form the correctness theorem for the program:  

    out/peak/max.siv
    out/peak/coeffs.siv
    out/peak/peakunit.siv
    out/peak/peakq.siv

Altogether there are 63 VC conclusions that result in 63 problems to try to prove.

### Proving the simplified VCs using PolyPaver

#### Classifying problems as trivial or non-trivial

Majority of the problems are trivial.  To categorise the problems, first run

    > polypaver out/peak/max.siv -t 1 -q

and analogously for all the other `.siv` files.  
The above command applies PolyPaver for 1s on default settings to each VC conclusion in the file.

PolyPaver will output a summary in the end where we can find out
which VC conclusions have been proved and which not.
With the above statement, PolyPaver typically proves 
42 out of the 63 problems
(4/4 in max, 15/23 in coeffs, 9/16 in peakq, 14/20 in peakunit).

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



